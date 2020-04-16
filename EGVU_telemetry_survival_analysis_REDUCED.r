##########################################################################
#
# EGYPTIAN VULTURE MONTHLY SURVIVAL ANALYSIS FROM TELEMETRY
#
##########################################################################
# written by Steffen Oppel, July 2019
# data preparation by Evan Buechley, Ron Efrat, Louis Phipps, and Evan Buechley
# branched from "EGVU_telemetry_survival_analysis.r' on 9 Jan 2020
# removed all exploratory analyses, retained streamlined code for final model for inference (previously called m31)

## MAJOR CHANGES ON 7 APRIL 2020
## removed all residents
## removed age effect and simply retained 2 age groups (<18 months and >18 months)
## test 3 models with breed,mig, and winter stage and west, Balkan and Caucasus populations, either additive or interactive

## INCLUDED NEW DATA on 14 APRIL 2020 and re-ran models

library(jagsUI)
library(tidyverse)
library(data.table)
library(lubridate)
filter<-dplyr::filter
select<-dplyr::select


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DATA FROM SPREADSHEETS AND ASSIGN FINAL STATES OF EACH ANIMAL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

try(setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study"), silent=T) ## changed after re-cloning remote
EV<-fread("ev.tv.summary.proofed_RE4_migrantsonly.csv")   ## updated on 9 April 2020
EVcovar<-fread("ev.survival.prepared.csv")
names(EV)[1]<-'species'
head(EV)
dim(EV)
EV$id.tag = as.character(EV$id.tag)
EVcovar$id.tag = as.character(EVcovar$id.tag)

#EV<-EV %>% mutate(start=mdy_hm(start.date), end= mdy_hm(end.date)) %>%
EV<-EV %>% mutate(start=parse_date_time(start.date, c("mdy", "mdy HM")), end= parse_date_time(end.date, c("mdy", "mdy HM"))) %>%
  filter(!is.na(start)) %>%
  filter(species=="Neophron percnopterus") %>%
  filter(start<ymd_hm("2019-04-01 12:00")) %>%  ## remove birds only alive for a few months in 2019
  select(species,population,id.tag,sex,age.at.deployment,age.at.deployment.month,captive.raised,rehabilitated, start, end, fate, how.fate.determined.clean, mean.GPS.dist.last10fixes.degrees)
head(EV)
dim(EV)


### REDUCE COVARIATES TO THOSE TAGS IN EV DATA
EVcovar<-EVcovar %>% filter(id.tag %in% EV$id.tag)


####### ASSIGNMENT OF STATES ########
unique(EV$how.fate.determined.clean)
unique(EV$age.at.deployment)
unique(EV$fate.final)
#quest.fates<-EV %>% filter(fate!=fate.final) %>% select(id.tag,population,age.at.deployment,start,end,fate,fate.final,how.fate.determined.clean)

#### revert 3 fates to 'unknown'
#EV <- EV %>%
 # mutate(fate=ifelse(id.tag %in% quest.fates$id.tag[c(1,2,12)],"unknown",fate))




### SHOW INVENTORY OF POSSIBLE COMBINATIONS OF STATES
as.data.frame(table(EV$how.fate.determined.clean,EV$fate)) %>%
	filter(Freq!=0) %>%
	rename(fate=Var2, how.fate.det=Var1)
EV %>% filter(fate=="confirmed dead" & how.fate.determined.clean=="resighted / recaptured")

EV<-EV %>%

# True States (S) - these are often unknown and cannot be observed, we just need them to initialise the model (best guess)
# 1 dead
# 2 alive with functioning tag
# 3 alive with defunct tag OR without tag (when tag was lost)

  mutate(TS= ifelse(fate=="alive",2,
                    ifelse(fate %in% c("confirmed dead","likely dead","unknown"),1,3))) %>%
  # mutate(OS= ifelse(fate=="alive",1,
  #                   ifelse(fate %in% c("unknown","suspected transmitter failure"),5,
  #                          ifelse(fate=="verified transmitter failure",3,
  #                                 ifelse(fate=="dead",4,
  #                                        ifelse(fate=="suspected mortality",2,5)))))) %>%

# Observed States (O) - these are based on the actual transmission history
# 1 Tag ok, bird moving
# 2 Tag ok, bird not moving (dead)
# 3 Tag failed, bird observed alive
# 4 Dead bird recovered
# 5 No signal (=not seen)
  mutate(OS= ifelse(fate=="alive",1,
                    ifelse(fate %in% c("unknown","likely transmitter failure"),5,
                           ifelse(fate=="confirmed transmitter failure",3,
                                  ifelse(how.fate.determined.clean %in% c("carcass found","resighted / recaptured","transmitter recovered"),4,2))))) %>%
  arrange(id.tag)

head(EV)

### CHECK WHETHER STATE ASSIGNMENT IS PLAUSIBLE ###
table(EV$TS,EV$OS)

EV %>% filter(OS==3 & TS==3) %>% select(id.tag,population,age.at.deployment,start,end,fate,fate,how.fate.determined.clean)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# REMOVE RESIDENT BIRDS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
EV$population[EV$id.tag=="Ardi_182265"]<-"middle east"
unique(EV$population)
EV<-EV %>% filter(!(population %in% c("unknown","oman","horn of africa")))
dim(EV)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE CAPTURE HISTORY FOR SURVIVAL ESTIMATIONS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### CREATE A TIME SERIES DATA FRAME ###
mindate<-min(EV$start)
maxdate<-max(EV$end)
timeseries<-data.frame(date=seq(mindate, maxdate, "1 month")) %>%
  mutate(month=month(date),year=year(date)) %>%
  mutate(date=format(date, format="%m-%Y")) %>%
  mutate(season=ifelse(month %in% c(2,3,4,9,10), 'migration',ifelse(month %in% c(11,12,1),"winter","summer"))) %>%
  mutate(col=seq_along(date)+1)
dim(timeseries)


### CREATE BLANK MATRICES TO HOLD INFORMATION ABOUT TRUE AND OBSERVED STATES ###

EV.obs.matrix<-EV %>% select(id.tag) %>%
  arrange(id.tag)
EV.obs.matrix[,2:max(timeseries$col)]<-NA									

EV.state.matrix<-EV %>% select(id.tag) %>%
  arrange(id.tag)
EV.state.matrix[,2:max(timeseries$col)]<-NA								

tag.age<-EV %>% select(id.tag) %>%
  arrange(id.tag)
tag.age[,2:max(timeseries$col)]<-NA									



### FILL MATRICES WITH STATE INFORMATION ###

for(n in EV.obs.matrix$id.tag){
  xl<-EV %>% filter(id.tag==n)
  mindate<-format(xl$start, format="%m-%Y")
  maxdate<-format(xl$end, format="%m-%Y")
  startcol<-timeseries$col[timeseries$date==mindate]
  stopcol<-timeseries$col[timeseries$date==maxdate]
  stopcol<-ifelse(stopcol<=startcol,min(startcol+1,max(timeseries$col)),stopcol)

  ## CALCULATE TAG AGE (assuming tag was new when deployed)
  tag.age[EV.obs.matrix$id.tag==n,startcol:max(timeseries$col)]<-seq(1,(max(timeseries$col)-(startcol-1)),1)/12  ## specify tag age in years
    
  ## ASSIGN OBSERVED STATE
  EV.obs.matrix[EV.obs.matrix$id.tag==n,startcol:(stopcol-1)]<-1
  if(startcol==stopcol){EV.obs.matrix[EV.obs.matrix$id.tag==n,2:(stopcol-1)]<-NA} ## for the few cases where stopcol-1 is actually before startcol
  EV.obs.matrix[EV.obs.matrix$id.tag==n,stopcol:max(timeseries$col)]<-xl$OS   ## this assumes that state never changes - some birds may have gone off air and then been found dead - this needs manual adjustment!  
  
  ## ASSIGN INITIAL TRUE STATE (to initialise z-matrix of model)
  EV.state.matrix[EV.state.matrix$id.tag==n,(startcol+1):(stopcol-1)]<-2      ## state at first capture is known, hence must be NA in z-matrix
  EV.state.matrix[EV.state.matrix$id.tag==n,stopcol:max(timeseries$col)]<-xl$TS   ## this assumes that state never changes - some birds may have gone off air and then been found dead - this needs manual adjustment!
  EV.state.matrix[EV.state.matrix$id.tag==n,2:startcol]<-NA ## error occurs if z at first occ is not NA, so we need to specify that for birds alive for <1 month because stopcol-1 = startcol
  
}


# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # CREATE MATRIX OF SURVIVAL PARAMETERS BASED ON AGE AND SEASON
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0	No data
# 1	Stationary
# 3	Migratory - set this to 3 so we have states 1 and 2 for stationary, and 3,4,5 for migratory

EV.phi.states<-fread("Mig_stage_matrix_final.csv")
head(EV.phi.states)
dim(EV.phi.states)

### arrange into full matrix ###
EV.phi.matrix<-EV.phi.states %>%
  mutate(id.tag=substr(id.year,1,nchar(id.year)-9)) %>%
  mutate(year=as.numeric(substr(id.year,nchar(id.year)-7,nchar(id.year)-4))) %>%  
  select(-Seq,-id.year) %>%
  gather(key="month",value="state",-id.tag,-year) %>%
  mutate(date=ymd(paste(year,month,"01",sep="-"))) %>%
  mutate(date=format(date, format="%m-%Y")) %>%
  mutate(col=timeseries$col[match(date,timeseries$date)]) %>%
  filter(!is.na(col)) %>%
  mutate(state=ifelse(state==2,3,1)) %>%  ## replace 0 as there will be no parameter with index 0
  #mutate(state=ifelse(state==1,1,ifelse(month<7,2,3))) %>%  ## state==2 for spring migration and state==3 for fall migration - removed on 8 Jan 2020 to increase precision in estimates
  select(-year,-month,-date) %>%
  spread(key=col,value=state, fill=1) %>%
  arrange(id.tag)
dim(EV.phi.matrix)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIX ID TAG VALUES
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## fix provided by Evan Buechley on 3 Dec 2019

EV.phi.matrix$id.tag = as.character(EV.phi.matrix$id.tag)
EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="1_1"] <- "52027_1"
EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="93_14"] <- "81_14"
EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="AF5AF11F_NA"] <- "Bianca_IHB_AF5AF11F"
EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="B05AF11F_NA"] <- "Clara_IHC_B05AF11F"
EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="Provence_2016_Ad_wild_EO5018_Salomé_8P_5018"] <- "Provence_2016_Ad_wild_EO5018_Salome_8P_5018"
EV.phi.matrix<-EV.phi.matrix[!(EV.phi.matrix$id.tag=="Djibouti_127589"),]

EV.phi.matrix<-EV.phi.matrix %>% filter(id.tag %in% EV$id.tag) %>%
  arrange(id.tag)
dim(EV.phi.matrix)

#EV$id.tag[which((EV$id.tag %in% EV.phi.matrix$id.tag)==FALSE)]



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# USE NEW COVARIATE DATA FRAME TO SPECIFY THREE COVARIATE MATRICES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## for each occasion one value per individual that specifies age, dist moved, and lat
head(EVcovar)
dim(EVcovar)

## CREATE AGE MATRIx
age.matrix<-EVcovar %>% filter(id.tag %in% EV.obs.matrix$id.tag) %>%
  mutate(timestamp=ymd_hms(timestamp)) %>%
  mutate(yr.mo=format(timestamp, format="%m-%Y")) %>%
  mutate(col=timeseries$col[match(yr.mo,timeseries$date)]) %>%
  group_by(id.tag,col) %>%
  summarise(age=mean(age.in.months.capped)) %>%
  spread(key=col,value=age) %>%
  arrange(id.tag)

## CREATE LATITUDE MATRIX
lat.matrix<-EVcovar %>% filter(id.tag %in% EV.obs.matrix$id.tag) %>%
  mutate(timestamp=ymd_hms(timestamp)) %>%
  mutate(yr.mo=format(timestamp, format="%m-%Y")) %>%
  mutate(col=timeseries$col[match(yr.mo,timeseries$date)]) %>%
  group_by(id.tag,col) %>%
  summarise(lat=mean(mean.monthly.lat)) %>%
  spread(key=col,value=lat) %>%
  arrange(id.tag)

## CREATE LONGITUDE MATRIX AT ORIGIN (FIRST LONGITUDE VALUE FOR EACH BIRD)
long.orig<-EVcovar %>% filter(id.tag %in% EV.obs.matrix$id.tag) %>%
  mutate(timestamp=ymd_hms(timestamp)) %>%
  arrange(id.tag,timestamp) %>%
  group_by(id.tag) %>%
  summarise(long=first(mean.monthly.long)) %>%
  arrange(id.tag)

### THE ABOVE MATRICES HAVE GAPS
### FILL GAPS IN MATRICES BY LOOPING ###

for(n in EV.obs.matrix$id.tag){
  xl<-EV %>% filter(id.tag==n)
  xage<-age.matrix %>% filter(id.tag==n)
  mindate<-format(xl$start, format="%m-%Y")
  maxdate<-format(xl$end, format="%m-%Y")
  startcol<-timeseries$col[timeseries$date==mindate]
  endcol<-timeseries$col[timeseries$date==maxdate]
  xage[startcol:endcol]

  ### ASSIGN VALUES TO ALL SUBSEQUENT INTERVALS
  for (col in (endcol+1):max(timeseries$col)){
    age.matrix[age.matrix$id.tag==n,col]<-min((age.matrix[age.matrix$id.tag==n,(col-1)]+1),54)
    lat.matrix[lat.matrix$id.tag==n,col]<-lat.matrix[lat.matrix$id.tag==n,(col-1)]

  } ## end loop over each occasion
  
  
  misscol<-which(is.na(age.matrix[age.matrix$id.tag==n,]))
  misscol<-misscol[misscol>startcol]
  
  ### INTERPOLATE VALUES IF NA IN COLUMNS THAT SHOULD HAVE VALUES
  for (col in misscol){
    age.matrix[age.matrix$id.tag==n,col]<-min((age.matrix[age.matrix$id.tag==n,(col-1)]+1),54)
    lat.matrix[lat.matrix$id.tag==n,col]<-lat.matrix[lat.matrix$id.tag==n,(col-1)]

  } ## end loop over each occasion

} ## end loop over each animal







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ADJUST PHI MATRIX TO REFLECT THE STAGES WE WANT TO TEST
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Option 1: stationary for summer and winter, migration for 3 geographic populations
# Option 2: stationary for summer and winter, migration

### CREATE A VULNERABLE MATRIX WITH 1/0 and 1 for juveniles from Italy/Balkans on migration 
vul.mat<-EV.phi.matrix


dim(lat.matrix)

for(row in 1:nrow(EV.phi.matrix)) {
  id<-EV.phi.matrix$id.tag[row]
  pop<-EV$population[match(id,EV$id.tag)]
  
  for(col in 2:ncol(EV.phi.matrix)) {
    age<-age.matrix[match(id,age.matrix$id.tag),col]
    age<-ifelse(is.na(age),1,age)
    if(EV.phi.matrix[row,col]==1){
      vul.mat[row,col]<-0
      EV.phi.matrix[row,col]<-ifelse(lat.matrix[row,col]>30,1,2)
      EV.phi.matrix[row,col]<-ifelse(is.na(lat.matrix[row,col]),1,EV.phi.matrix[row,col])
      EV.phi.matrix[row,col]<-ifelse(age<10,2,EV.phi.matrix[row,col])     ### b.phi.mig[1] never converges for juveniles because there are too few data, so we set this to 2 for the month after fledging.
    }
    if(EV.phi.matrix[row,col]==3){
      EV.phi.matrix[row,col]<-ifelse(pop=="western europe",3,ifelse(pop %in% c("italy","balkans"),4,5))
      vul.mat[row,col]<-ifelse(age<19,ifelse(pop %in% c("italy","balkans"),1,0),0)
    }
  }
}

EV.phi.matrix



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE INPUT DATA FOR JAGS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Convert to numeric matrices that JAGS can loop over
y.telemetry<-as.matrix(EV.obs.matrix[,2:max(timeseries$col)])
z.telemetry<-as.matrix(EV.state.matrix[,2:max(timeseries$col)])
age.mat<-as.matrix(age.matrix[,2:max(timeseries$col)])
lat.mat<-as.matrix(lat.matrix[,2:max(timeseries$col)])

range(age.mat, na.rm=T)
range(lat.mat, na.rm=T)



#### create vector of first marking and of last alive record
get.first.telemetry<-function(x)min(which(!is.na(x)))
get.last.telemetry<-function(x)max(which(!is.na(x) & x==1))
f.telemetry<-apply(y.telemetry,1,get.first.telemetry)
l.telemetry<-apply(y.telemetry,1,get.last.telemetry)

#### extract and standardise covariates
tag.fail.indicator<-EV$mean.GPS.dist.last10fixes.degrees

#### BUNDLE DATA INTO A LIST
INPUT.telemetry <- list(y = y.telemetry,
                        f = f.telemetry,
                        l = l.telemetry,
                        age = age.mat,
                        adult = ifelse(age.mat>18,0,1), ### provide a simple classification for adults and non-adults
                        mig = as.matrix(EV.phi.matrix[,2:max(timeseries$col)]),
                        lat = lat.mat,
                        pop = ifelse(EV$pop %in% c("italy","western europe"),1,0),
                        vul = as.matrix(vul.mat[,2:max(timeseries$col)]),
                        #long = long.orig$long, ##long.mat,      ### if we want this as a continuous pop definition we would need to use just one value per bird, not a monthly value
                        capt = ifelse(EV$captive.raised=="N",0,1),
                        #resid = ifelse(EV$population %in% c("unknown","oman","horn of africa"),0,1),
				                tfail = as.numeric(tag.fail.indicator),
				                tag.age = as.matrix(tag.age[,2:max(timeseries$col)]),
                        nind = dim(y.telemetry)[1],
                        n.occasions = dim(y.telemetry)[2])



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPECIFY AND SET UP MODEL RUNS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Parameters monitored
parameters.telemetry <- c("p.seen.alive","base.obs","base.fail","base.recover","beta1","beta2","beta3","beta4",
                          "mean.phi","lp.mean","b.phi.mig","b.phi.capt","b.phi.pop","b.phi.age","b.phi.vul")

# MCMC settings
ni <- 5000
nt <- 4
nb <- 2000
nc <- 3




#### LATEST SIMPLEST AND MAYBE EASIEST MODEL TO INTERPRET ##########
INPUT.telemetry$mig<-ifelse(INPUT.telemetry$mig>2,1,0)
INPUT.telemetry$pop<-ifelse(INPUT.telemetry$pop>1,0,1)

inits.telemetry <- function(){list(z = z.telemetry,
                                   mean.phi = runif(1, 0.9, 1), ### two intercepts for juvenile and adults
                                   base.obs = rnorm(1,0, 0.001),                # Prior for intercept of observation probability on logit scale
                                   base.fail = rnorm(1,0, 0.001),               # Prior for intercept of tag failure probability on logit scale
                                   beta2 = rnorm(1,0, 0.001),         # Prior for slope parameter for 
                                   beta3 = rnorm(1,0, 0.001))} 

# Call JAGS from R (took 92.958 min DIC = 3352.662)
EGVU_surv_mod_full_additive <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
                                        "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVU_binary_additive.jags",
                                        n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni)

# # Call JAGS from R (took 68.06 min DIC = 3360.895)
# EGVU_surv_mod_no_mig <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                                         "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVU_binary_additive_nomig.jags",
#                                         n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni)
# 
# # Call JAGS from R (took 69.078 min DIC = 3363.334)
# EGVU_surv_mod_no_capt <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                                         "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVU_binary_additive_nocapt.jags",
#                                         n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni)



############## PREVIOUS MODELS ALSO TRIED ##########################


# # Initial values for some parameters
# inits.telemetry <- function(){list(z = z.telemetry,
#                                    mean.phi = runif(2, 0.5, 0.999), ### two intercepts for juvenile and adults
#                                    base.obs = rnorm(1,0, 0.001),                # Prior for intercept of observation probability on logit scale
#                                    base.fail = rnorm(1,0, 0.001),               # Prior for intercept of tag failure probability on logit scale
#                                    beta1 = rnorm(1,0, 0.001),         # Prior for slope parameter for obs prob with time since
#                                    beta2 = rnorm(1,0, 0.001),         # Prior for slope parameter for 
#                                    beta3 = rnorm(1,0, 0.001))} 
# 
# 
# # Call JAGS from R (took 70 min DIC = 3350.327)
# EGVU_surv_mod_5stage <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                      "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVUsurv_simplage_5migstage.jags",
#                      n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni)
# 
# ##### CHANGE INPUT DATA FOR A SINGLE RESIDENT STAGE AND 3 migratory stages #####
# INPUT.telemetry$mig<-as.matrix(EV.phi.matrix[,2:max(timeseries$col)])
# INPUT.telemetry$mig<-ifelse(INPUT.telemetry$mig==2,1,INPUT.telemetry$mig)
# INPUT.telemetry$mig<-ifelse(INPUT.telemetry$mig==5,2,INPUT.telemetry$mig)
# 
# 
# # Call JAGS from R (took 70 min DIC = 3347.666)
# EGVU_surv_mod_4stage <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                           "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVUsurv_simplage_4migstage.jags",
#                           n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni)
# 
# 
# 
# ### SIMPLEST POSSIBLE MODEL ############################
# ##### CHANGE INPUT DATA FOR ADDITIVE POPULATION EFFECT #####
# INPUT.telemetry$mig<-as.matrix(EV.phi.matrix[,2:max(timeseries$col)])
# INPUT.telemetry$mig<-ifelse(INPUT.telemetry$mig>2,1,0)
# 
# # Call JAGS from R (took 70 min DIC = 3353.559)
# EGVU_surv_mod_2stage_addpop <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                           "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVUsurv_simplage_2migstage_addpop.jags",
#                           n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni)
# 
# # Call JAGS from R (took 90 min DIC = 3383.845)
# EGVU_surv_mod_2stage_addpop_agemig <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                                         "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVUsurv_simplage_2migstage_addpop_agemig.jags",
#                                         n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni)
# 
# # Call JAGS from R (took 79 min DIC = 3349.905)
# EGVU_surv_mod_2stage_intpop <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                                         "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVUsurv_simplage_2migstage_intpop.jags",
#                                         n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T)#, n.iter = ni)
# 
# # Call JAGS from R (took 79 min DIC = 3397.236)
# EGVU_surv_mod_2stage_intpop_mig <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                                     "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVUsurv_simplage_2migstage_intpop_mig.jags",
#                                     n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T)#, n.iter = ni)
# 
# 
# # Call JAGS from R (took 79 min DIC = 3349.905)
# # Initial values for some parameters
# inits.telemetry <- function(){list(z = z.telemetry,
#                                    mean.phi = runif(1, 0.5, 0.999), ### two intercepts for juvenile and adults
#                                    base.obs = rnorm(1,0, 0.001),                # Prior for intercept of observation probability on logit scale
#                                    base.fail = rnorm(1,0, 0.001),               # Prior for intercept of tag failure probability on logit scale
#                                    beta1 = rnorm(1,0, 0.001),         # Prior for slope parameter for obs prob with time since
#                                    beta2 = rnorm(1,0, 0.001),         # Prior for slope parameter for 
#                                    beta3 = rnorm(1,0, 0.001))} 
# EGVU_surv_mod_2stage_intpop_AGE <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                                         "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVUsurv_age_2migstage_intpop.jags",
#                                         n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T)#, n.iter = ni)
# 
# 
# 
# 
# ### MIGRATION ONLY MODELS WITH NO GEOGRAPHIC STRUCTURE ############################
# 
# # Call JAGS from R (took 70 min DIC = 3364.502)
# EGVU_surv_mod_2stage <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                                         "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVUsurv_simplage_2migstage.jags",
#                                         n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni)
# 
# # Call JAGS from R (took 137 min DIC = 3362.817)
# EGVU_surv_mod_2stage_int <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                                  "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVUsurv_simplage_2migstage.jags",
#                                  n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni)
# 
# 
# ### MODEL WITH 3 stages - breeding, winter, and migration
# INPUT.telemetry$mig<-as.matrix(EV.phi.matrix[,2:max(timeseries$col)])
# INPUT.telemetry$mig<-ifelse(INPUT.telemetry$mig>2,3,INPUT.telemetry$mig)
# 
# EGVU_surv_mod_3stage <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                                  "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVUsurv_simplage_3migstage.jags",
#                                  n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni)
# 
# 
# ### MODEL WITH 4 stages - breeding, winter, spring, fall migration (DIC= 3373.8)
# ## this requires creating the mig.stage matrix from scratch to separate spring and fall migration ##
# 
# EV.phi.states<-fread("Mig_stage_matrix.csv")
# EV.phi.matrix<-EV.phi.states %>%
#   mutate(id.tag=substr(id.year,1,nchar(id.year)-9)) %>%
#   mutate(year=as.numeric(substr(id.year,nchar(id.year)-7,nchar(id.year)-4))) %>%  
#   select(-Seq,-id.year) %>%
#   gather(key="month",value="state",-id.tag,-year) %>%
#   mutate(date=ymd(paste(year,month,"01",sep="-"))) %>%
#   mutate(date=format(date, format="%m-%Y")) %>%
#   mutate(col=timeseries$col[match(date,timeseries$date)]) %>%
#   filter(!is.na(col)) %>%
#   mutate(state=ifelse(state==1,1,ifelse(month<7,3,4))) %>%  ## state==3 for spring migration and state==4 for fall migration - updated on 10 April to allow for 2 resident states
#   select(-year,-month,-date) %>%
#   spread(key=col,value=state, fill=1) %>%
#   arrange(id.tag)
# 
# EV.phi.matrix$id.tag = as.character(EV.phi.matrix$id.tag)
# EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="1_1"] <- "52027_1"
# EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="93_14"] <- "81_14"
# EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="AF5AF11F_NA"] <- "Bianca_IHB_AF5AF11F"
# EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="B05AF11F_NA"] <- "Clara_IHC_B05AF11F"
# EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="Provence_2016_Ad_wild_EO5018_Salomé_8P_5018"] <- "Provence_2016_Ad_wild_EO5018_Salome_8P_5018"
# EV.phi.matrix<-EV.phi.matrix[!(EV.phi.matrix$id.tag=="Djibouti_127589"),]
# 
# EV.phi.matrix<-EV.phi.matrix %>% filter(id.tag %in% EV$id.tag) %>%
#   arrange(id.tag)
# 
# for(row in 1:nrow(EV.phi.matrix)) {
#   id<-EV.phi.matrix$id.tag[row]
# 
#   for(col in 2:ncol(EV.phi.matrix)) {
#     if(EV.phi.matrix[row,col]==1){
#       EV.phi.matrix[row,col]<-ifelse(lat.matrix[row,col]>30,1,2)
#       EV.phi.matrix[row,col]<-ifelse(is.na(lat.matrix[row,col]),1,EV.phi.matrix[row,col])
#     }
#   }
# }
# 
# INPUT.telemetry$mig<-as.matrix(EV.phi.matrix[,2:max(timeseries$col)])
# 
# EGVU_surv_mod_4stage_fallmig <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                                          "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVUsurv_simplage_4migstage.jags",
#                                          n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni)
# 
# 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXPORT THE OUTPUT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
try(setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study"), silent=T)
save.image("EGVU_survival_output_simplage.RData") 

load("EGVU_survival_output_full_additive.RData")







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPECIFY JAGS MODELS [NEEDS TO BE RUN FIRST]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Specify model in BUGS language
sink("EGVU_binary_additive.jags")
cat("
  model {
    
    # -------------------------------------------------
    # Parameters:
    # phi: monthly survival probability intercept
    # tag.fail: probability that tag will fail
    # tag.loss: probability that tag will fall off - not identifiable, so not included
    
    # p.obs: probability to be tracked with functioning tag (=1)
    # p.found.dead: probability for carcass to be recovered
    # p.seen.alive: probability to be observed alive despite the tag being defunct
    
    # -------------------------------------------------
    # States (S):
    # 1 dead
    # 2 alive with functioning tag
    # 3 alive with defunct tag or tag lost
    
    # Observations (O):
    # 1 Tag ok, bird moving
    # 2 Tag ok, bird not moving (dead, or tag lost and no longer on bird)
    # 3 Tag failed, bird observed alive
    # 4 Dead bird recovered
    # 5 No signal (=not seen)
    
    # -------------------------------------------------
    
    # Priors and constraints
    
    
    #### MONTHLY SURVIVAL PROBABILITY
    for (i in 1:nind){
      for (t in f[i]:(n.occasions)){
        logit(phi[i,t]) <- lp.mean +      ### age category-specific intercept 
          b.phi.mig*(mig[i,t]) +       ### survival dependent on migratory stage of the month (stationary or migratory)
          b.phi.capt*(capt[i]) +     ### survival dependent on captive-release (captive-raised or other)
          b.phi.age*(adult[i,t]) +     ### survival dependent on age (wild or juvenile or other)
          b.phi.pop*(pop[i])  +    ### survival dependent on population (western Europe or other)
          b.phi.vul*(vul[i,t])      ### survival dependent on highly vulnerable stage (juveniles on migration in Balkans/Italy)
      } #t
    } #i
    
    #### BASELINE FOR SURVIVAL PROBABILITY (wild adult stationary from east)
    mean.phi ~ dunif(0.9, 1)   # uninformative prior for all MONTHLY survival probabilities
    lp.mean <- log(mean.phi/(1 - mean.phi))    # logit transformed survival intercept
    
    #### SLOPE PARAMETERS FOR SURVIVAL PROBABILITY
    b.phi.capt ~ dnorm(0, 0.01)         # Prior for captive effect on survival probability on logit scale
    b.phi.mig ~ dnorm(0, 0.01)          # Prior for migration effect on survival probability on logit scale
    b.phi.age ~ dnorm(-2, 0.01)          # Prior for age effect on survival probability on logit scale
    b.phi.pop ~ dnorm(0, 0.01)          # Prior for population effect on survival probability on logit scale
    b.phi.vul ~ dunif(-0.01,-3)         # Prior for vulnerable state on survival probability on logit scale
    
    
    #### TAG FAILURE AND LOSS PROBABILITY
    for (i in 1:nind){
      for (t in f[i]:(n.occasions)){
        logit(p.obs[i,t]) <- base.obs + beta1*(t-l[i]) #### probability of observation GIVEN THAT TAG IS WORKING is reciprocal to time since last good record
        logit(tag.fail[i,t]) <- base.fail + beta2*tag.age[i,t] + beta3*tfail[i] #### probability of TAG FAILURE is influenced by tag type and tag age
        logit(p.found.dead[i,t]) <- base.recover + beta4*lat[i,t] #### probability of recovery is influenced by latitude
      } #t
    } #i
    
    
    ##### SLOPE PARAMETERS FOR OBSERVATION PROBABILITY
    base.obs ~ dnorm(0, 0.001)                # Prior for intercept of observation probability on logit scale
    base.fail ~ dnorm(0, 0.001)               # Prior for intercept of tag failure probability on logit scale
    base.recover ~ dnorm(0, 0.001)               # Prior for intercept of tag failure probability on logit scale
    beta1 ~ dnorm(0, 0.001)T(-10, 10)         # Prior for slope parameter for obs prob with time since
    beta2 ~ dnorm(0, 0.001)T(-10, 10)         # Prior for slope parameter for fail probability with tag age
    beta3 ~ dnorm(0, 0.001)T(-10, 10)         # Prior for slope parameter for fail probability with tage movement during last 10 GPS fixes
    beta4 ~ dnorm(0, 0.001)T(-10, 10)         # Prior for slope parameter for dead detection with latitude
    sigma ~ dunif(0, 10)                     # Prior on standard deviation for random error term
    tau <- pow(sigma, -2)
    
    p.seen.alive ~ dunif(0, 1)    # Prior for probability that bird with defunct or lost tag is observed alive
    
    
    # -------------------------------------------------
    # Define state-transition and observation matrices 
    # -------------------------------------------------
    
    for (i in 1:nind){
      
      for (t in f[i]:(n.occasions-1)){
        
        # Define probabilities of state S(t+1) [last dim] given S(t) [first dim]
        
        ps[1,i,t,1]<-1    ## dead birds stay dead
        ps[1,i,t,2]<-0
        ps[1,i,t,3]<-0
        
        ps[2,i,t,1]<-(1-phi[i,t])
        ps[2,i,t,2]<-phi[i,t] * (1-tag.fail[i,t])
        ps[2,i,t,3]<-phi[i,t] * tag.fail[i,t]
        
        ps[3,i,t,1]<-(1-phi[i,t])
        ps[3,i,t,2]<-0
        ps[3,i,t,3]<-phi[i,t]
        
        # Define probabilities of O(t) [last dim] given S(t)  [first dim]
        
        po[1,i,t,1]<-0
        po[1,i,t,2]<-p.obs[i,t] * (1-tag.fail[i,t]) * (1-p.found.dead[i,t])
        po[1,i,t,3]<-0
        po[1,i,t,4]<-p.found.dead[i,t]
        po[1,i,t,5]<-(1-p.obs[i,t]) * tag.fail[i,t] * (1-p.found.dead[i,t])
        
        po[2,i,t,1]<-p.obs[i,t] * (1-tag.fail[i,t])
        po[2,i,t,2]<-0
        po[2,i,t,3]<-0
        po[2,i,t,4]<-0
        po[2,i,t,5]<-(1-p.obs[i,t]) * tag.fail[i,t]
        
        po[3,i,t,1]<-0
        po[3,i,t,2]<-0
        po[3,i,t,3]<-p.seen.alive
        po[3,i,t,4]<-0
        po[3,i,t,5]<-(1-p.seen.alive)
        
      } #t
    } #i
    
    # Likelihood 
    for (i in 1:nind){
      # Define latent state at first capture
      z[i,f[i]] <- 2 ## alive when first marked
      for (t in (f[i]+1):n.occasions){
        # State process: draw S(t) given S(t-1)
        z[i,t] ~ dcat(ps[z[i,t-1], i, t-1,])
        # Observation process: draw O(t) given S(t)
        y[i,t] ~ dcat(po[z[i,t], i, t-1,])
      } #t
    } #i
  }
    ",fill = TRUE)
sink()
