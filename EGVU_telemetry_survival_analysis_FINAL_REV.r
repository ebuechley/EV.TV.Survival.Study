##########################################################################
#
# EGYPTIAN VULTURE MONTHLY SURVIVAL ANALYSIS FROM TELEMETRY
#
##########################################################################
# written by Steffen Oppel, July 2019
# data preparation by Evan Buechley, Ron Efrat, Louis Phipps, and Evan Buechley
# branched from "EGVU_telemetry_survival_analysis.r' on 9 Jan 2020
## 25 Nov 2020: explored effect of re-classifying drowning in Med as 'likely dead' (rather than 'confirmed dead') - no problem at all!
## FINAL DISCUSSION WITH RON AND EVAN - decided to use 2pop model and examine continuous age, quadratic age, and effect of removing the rehab/recap birds
## 4 DEC 2020: after re-reading manuscript created a new branch with 3 lat band effects

## reduced from "EGVU_telemetry_survival_analysis_LATITUDE.r'on 11 DEC 2020: FINAL RUN OF FINAL MODEL

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
#EV<-fread("ev.tv.summary.proofed_RE4_migrantsonly.csv")   ## updated on 9 April 2020
EV<-fread("ev.summary.final.Rev1.survival.prepared.csv")   ## REVISED DATA 10 NOV 2020
EV<-EV %>% filter(id.tag!="R2_190604")
#EV<-fread("ev.summary.final.csv")   ## new file provided by Evan on 27 May 2020
#EV<-fread("FINAL_ANALYSIS_DATA.csv")   ## new file reated by Steffen on 28 May 2020 - just changed from RE4 by switching 'Apollo' from unknown to confirmed dead
EVcovar<-fread("ev.final.Rev1.survival.prepared.csv")
EVcovar %>% filter(id.tag=="R2_190604")
EV<-EV %>% filter(id.tag!="R2_190604")   ## remove one individual that is the same of a failed tag
#names(EV)[1]<-'species'
head(EV)
dim(EV)
EV$id.tag = as.character(EV$id.tag)
EVcovar$id.tag = as.character(EVcovar$id.tag)
EV$id.tag[EV$id.tag=="Clara_IHC_NA"] <- "Clara_IHC_B05AF11F" ### changed in input file of mig matrix, so we need to change that here as well
EVcovar$id.tag[EVcovar$id.tag=="Clara_IHC_NA"] <- "Clara_IHC_B05AF11F" ### changed in input file of mig matrix, so we need to change that here as well

unique(EV$fate)

#EV<-EV %>% mutate(start=mdy_hm(start.date), end= mdy_hm(end.date)) %>%
EV<-EV %>% #mutate(start=parse_date_time(start.date, c("mdy", "mdy HM")), end= parse_date_time(end.date, c("mdy", "mdy HM"))) %>%
  #mutate(start=parse_date_time(start.date, c("mdy", "mdy HM")), end= as.POSIXct(as.Date(as.numeric(end.date), origin="1970-01-01"))) %>% ### revised file has awkward date for end.date
  #mutate(start=ymd_hms(start.date), end= as.POSIXct(as.Date(as.numeric(end.date), origin="1970-01-01"))) %>% ### revised file has awkward date for end.date
  #mutate(start=ymd_hms(start.date), end= min(as.POSIXct(ymd(end.date)), ymd_hms(mortality.date))) %>% ### revised file has awkward date for end.date
  mutate(start=ymd_hms(start.date), end= if_else(ymd_hms(end.date)<ymd_hms(mortality.date),ymd_hms(end.date),ymd_hms(mortality.date))) %>% ### USE THE EARLIER OF END / MORTALITY DATE
  filter(!is.na(start)) %>%
  filter(species=="Neophron percnopterus") %>%
  filter(start<ymd_hm("2020-09-01 12:00")) %>%  ## remove birds only alive for a few months in 2020 (removes 1 bird: Baronnies_2019_Imm_wild_OR181635_5T_181635)
  #filter(rehabilitated=="N") %>%  ## remove rehabilitated adult birds
  #filter(!(fate %in% c("returned to captivity","confirmed dead") & how.fate.determined %in% c("recaptured","resighted / recaptured"))) %>%  ## remove imprinted juveniles that were recaptured
  mutate(fate=if_else(fate=="returned to captivity","confirmed dead",fate)) %>%  ## set recaptured individuals to 'confirmed dead'
  select(species,population,id.tag,sex,age.at.deployment.months,captive.raised,rehabilitated, start, end, fate, how.fate.determined,mean.GPS.dist.last10fixes, cause.of.death)  ## , mean.GPS.dist.last10fixes.degrees
head(EV)
dim(EV)


### ADD DATA NOT IN 28 MAY 2020 version of final summary
#EV$mean.GPS.dist.last10fixes.degrees<-EVtagfail$mean.GPS.dist.last10fixes.degrees[match(EV$id.tag,EVtagfail$id.tag)]

### CHANGE FATE OF APOLLO - inserted after Guido Ceccolini's comments
EV %>% filter(grepl("Apollo",id.tag))
EV$fate[EV$id.tag=="Apollo_16093"]<-"confirmed dead"
EV$how.fate.determined[EV$id.tag=="Apollo_16093"]<-"carcass found"

### CHANGE END DATE OF ANDI, HEDHJET, AND R2
EV$end[EV$id.tag=="Andi_182261"]<-ymd_hms("2020-10-30 12:00:00")
EV$end[EV$id.tag=="R2_16015"]<-ymd_hms("2020-10-30 12:00:00")
EV$end[EV$id.tag=="Hedjet_171349"]<-ymd_hms("2020-10-30 12:00:00")

### CHANGE FATE OF GURMAN (Resighted by Igor)
EV$how.fate.determined[EV$id.tag =="Gurman_114"]<-"resighted / recaptured"
EV$fate[EV$id.tag =="Gurman_114"]<-"confirmed transmitter failure"
  
### SHOW END DATES OF BIRDS ALIVE
set.to.unknown<-EV %>% filter(fate=="alive") %>% filter(end<ymd_hms("2020-08-10 23:59:59"))
update.end.date<-EV %>% filter(fate=="alive") %>% filter(end<ymd_hms("2020-09-30 23:59:59"))

### FIX END DATES AND FATE
#EV$end[EV$id.tag %in% update.end.date$id.tag]<-ymd_hms("2020-10-30 23:59:59")       ## temporary hack until Evan has sorted out final dates
#fwrite(update.end.date, "EV.end.date_update_needed.csv")

### SUM TOTAL OF TRACKING EFFORT
EV %>% mutate(end=if_else(is.na(end), ymd_hms("2020-10-30 00:00:00"), end)) %>%
  mutate(tracklength=difftime(end,start, unit="days")) %>% summarise(TOTAL=sum(tracklength)/30)

### REDUCE COVARIATES TO THOSE TAGS IN EV DATA
EVcovar<-EVcovar %>% filter(id.tag %in% EV$id.tag)


####### ASSIGNMENT OF STATES ########
unique(EV$how.fate.determined)
unique(EV$age.at.deployment)
unique(EV$fate)
unique(EV$cause.of.death)

#quest.fates<-EV %>% filter(fate!=fate.final) %>% select(id.tag,population,age.at.deployment,start,end,fate,fate.final,how.fate.determined.clean)

#### revert 3 fates to 'unknown'
#EV <- EV %>%
 # mutate(fate=ifelse(id.tag %in% quest.fates$id.tag[c(1,2,12)],"unknown",fate))


#### SELECT THE BIRDS THAT TERMINATED OVER WATER AND ARE 'CONFIRMED' DEAD
change.fate<-EV %>% filter(cause.of.death %in% c("drowned","drop in the sea","Natural barrier","Drop in the sea")) %>%
  filter(how.fate.determined  %in% c("terminated over water","inferred from transmissions")) %>%
  filter(fate=="confirmed dead") %>%
  select(population,id.tag,fate, how.fate.determined,cause.of.death)
EV$fate[EV$id.tag %in% change.fate$id.tag]<-"likely dead"


### SHOW INVENTORY OF POSSIBLE COMBINATIONS OF STATES
as.data.frame(table(EV$how.fate.determined,EV$fate)) %>%
	filter(Freq!=0) %>%
	rename(fate=Var2, how.fate.det=Var1)
EV %>% filter(fate=="confirmed dead" & how.fate.determined=="resighted / recaptured")  ### WHY CAN A CONFIRMED DEAD BIRD BE RECAPTURED?
EV %>% filter(fate=="returned to captivity")  ### check the two birds returned to captivity - now set to 'confirmed dead'
EV %>% filter(fate=="unknown" & how.fate.determined=="active")  ### WHY CAN AN ACTIVE BIRD BE UNKNOWN?



### STATE ASSIGNMENT

EV<-EV %>%

# True States (S) - these are often unknown and cannot be observed, we just need them to initialise the model (best guess)
# 1 dead
# 2 alive with functioning tag
# 3 alive with defunct tag OR without tag (when tag was lost)

  mutate(TS= ifelse(tolower(fate)=="alive",2,
                    ifelse(tolower(fate) %in% c("confirmed dead","likely dead","unknown"),1,3))) %>%
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
                                  ifelse(how.fate.determined %in% c("carcass found","resighted / recaptured","transmitter recovered"),4,2))))) %>%
  arrange(id.tag)

head(EV)

### CHECK WHETHER STATE ASSIGNMENT IS PLAUSIBLE ###
table(EV$TS,EV$OS)

EV %>% filter(OS==3 & TS==3) %>% select(id.tag,population,age.at.deployment.months,start,end,fate,fate,how.fate.determined) ## the birds observed after having lost transmitter
EV %>% filter(OS==2 & TS==3) %>% select(id.tag,population,age.at.deployment.months,start,end,fate,fate,how.fate.determined) ## birds with failed tag but not moving


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# REASSIGN POPULATIONS FOR BIRDS MIGRATING OUT OF ETHIOPIA AND REMOVE RESIDENT BIRDS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
EV$population[EV$id.tag=="Ardi_182265"]<-"middle east"
EV$population[EV$id.tag=="Awash_182257"]<-"middle east"
EV$population[EV$id.tag=="Dalol_182250"]<-"middle east"
EV$population[EV$id.tag=="Chuupa_182258"]<-"caucasus"
EV$population[EV$id.tag=="Loma_182260"]<-"caucasus"
EV$population[EV$id.tag=="Wesal_182264"]<-"caucasus"

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

### CREATE YEAR AND MONTH VECTORS TO USE FOR RANDOM TIME EFFECT
years<-timeseries$year-2006
months<-timeseries$month

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

EV.phi.states<-fread("Mig_stage_matrix.Rev1.csv")
head(EV.phi.states)
dim(EV.phi.states)

### arrange into full matrix ###
EV.phi.matrix<-EV.phi.states %>%
  #mutate(id.tag=substr(id.year,1,nchar(id.year)-9)) %>%
  mutate(id.tag=substr(id.tag.year,1,nchar(id.tag.year)-5)) %>%
  mutate(year=as.numeric(substr(id.tag.year,nchar(id.tag.year)-3,nchar(id.tag.year)))) %>%  
  select(-id.tag.year) %>%
  gather(key="month",value="state",-id.tag,-year) %>%
  mutate(month=as.numeric(str_replace(month,"X",""))) %>%
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
## Mig.matrix csv file amended by Ron Efrat and Steffen Oppel on 13 Nov 2020
## none of these manual re-labels should be necessary after the correction of input files

EV.phi.matrix$id.tag = as.character(EV.phi.matrix$id.tag)
#EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="1_1"] <- "52027_1"
#EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="93_14"] <- "81_14"
#EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="AF5AF11F_NA"] <-"Bianca_IHB_AF5AF11F"
#EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="B05AF11F_NA"] <- "Clara_IHC_NA" ### changed in input file to "Clara_IHC_B05AF11F"
#EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="EnciÃ±a-9FJ_5783"] <- "Encina-9FJ_5783"

EV.phi.matrix<-EV.phi.matrix %>% filter(id.tag %in% EV$id.tag) %>%
  arrange(id.tag)
dim(EV.phi.matrix)



#### FIND MISMATCH BETWEEN EV STATE MATRIX AND MIG MATRIX
unique(EV$id.tag)
EV$id.tag[which((EV$id.tag %in% EV.phi.matrix$id.tag)==FALSE)]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# USE NEW COVARIATE DATA FRAME TO SPECIFY THREE COVARIATE MATRICES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## for each occasion one value per individual that specifies age, dist moved, and lat
head(EVcovar)
dim(EVcovar)

## CREATE AGE MATRIx
age.matrix<-EVcovar %>% filter(id.tag %in% EV.obs.matrix$id.tag) %>%
  mutate(timestamp=ymd_hms(timestamp)) %>%
  #mutate(timestamp=dmy_hm(timestamp)) %>% ## necessary because Excel automatically converted date format on 13 Nov 2020
  mutate(yr.mo=format(timestamp, format="%m-%Y")) %>%
  mutate(col=timeseries$col[match(yr.mo,timeseries$date)]) %>%
  group_by(id.tag,col) %>%
  summarise(age=mean(age.in.months)) %>%   ### changed on 20 Nov 2020 from age.in.months.capped
  spread(key=col,value=age) %>%
  arrange(id.tag)

## CREATE LATITUDE MATRIX
lat.matrix<-EVcovar %>% filter(id.tag %in% EV.obs.matrix$id.tag) %>%
  mutate(timestamp=ymd_hms(timestamp)) %>%
  #mutate(timestamp=dmy_hm(timestamp)) %>% ## necessary because Excel automatically converted date format on 13 Nov 2020
  mutate(yr.mo=format(timestamp, format="%m-%Y")) %>%
  mutate(col=timeseries$col[match(yr.mo,timeseries$date)]) %>%
  group_by(id.tag,col) %>%
  summarise(lat=mean(mean.monthly.lat)) %>%
  spread(key=col,value=lat) %>%
  arrange(id.tag)

## CREATE LONGITUDE MATRIX AT ORIGIN (FIRST LONGITUDE VALUE FOR EACH BIRD)
long.orig<-EVcovar %>% filter(id.tag %in% EV.obs.matrix$id.tag) %>%
  mutate(timestamp=ymd_hms(timestamp)) %>%
  #mutate(timestamp=dmy_hm(timestamp)) %>% ## necessary because Excel automatically converted date format on 13 Nov 2020
  arrange(id.tag,timestamp) %>%
  group_by(id.tag) %>%
  summarise(long=first(mean.monthly.long)) %>%
  arrange(id.tag)

### THE ABOVE MATRICES HAVE GAPS
### FILL GAPS IN MATRICES BY LOOPING ###

for(n in EV.obs.matrix$id.tag){
  xl<-EV %>% filter(id.tag==n)
  xage<-age.matrix %>% dplyr::filter(id.tag==n)
  mindate<-format(xl$start, format="%m-%Y")
  maxdate<-format(xl$end, format="%m-%Y")
  startcol<-timeseries$col[timeseries$date==mindate]
  endcol<-timeseries$col[timeseries$date==maxdate]
  xage[startcol:endcol]

  ### ASSIGN VALUES TO ALL SUBSEQUENT INTERVALS
  for (col in min(max(timeseries$col),(endcol+1)):max(timeseries$col)){
    #age.matrix[age.matrix$id.tag==n,col]<-min((age.matrix[age.matrix$id.tag==n,(col-1)]+1),54)
    age.matrix[age.matrix$id.tag==n,col]<-(age.matrix[age.matrix$id.tag==n,(col-1)]+1) ### continuous age for sensecence analysis
    lat.matrix[lat.matrix$id.tag==n,col]<-lat.matrix[lat.matrix$id.tag==n,(col-1)]

  } ## end loop over each occasion
  
  
  misscol<-which(is.na(age.matrix[age.matrix$id.tag==n,]))
  misscol<-misscol[misscol>startcol]
  
  ### INTERPOLATE VALUES IF NA IN COLUMNS THAT SHOULD HAVE VALUES
  for (col in misscol){
    #age.matrix[age.matrix$id.tag==n,col]<-min((age.matrix[age.matrix$id.tag==n,(col-1)]+1),54)
    age.matrix[age.matrix$id.tag==n,col]<-(age.matrix[age.matrix$id.tag==n,(col-1)]+1) ### continuous age for sensecence analysis
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

dim(EV.phi.matrix)
dim(EV.obs.matrix)
dim(lat.mat)
dim(age.mat)

EV$id.tag[which((EV$id.tag %in% age.matrix$id.tag)==FALSE)]




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPECIFY FINAL MODEL WITH SIMPLE 2 CATEGORY LATITUDE AND MIGRATION AND  CONTINUOUS AGE EFFECT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Specify model for continuous age effect
sink("EGVU_mig_lat_survival_model.jags")
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
    
    ## RANDOM ANNUAL TIME EFFECT ON SURVIVAL
    for (ny in 1:(nyears)){
    surv.raneff[ny] ~ dnorm(0, tau.surv)
    }
    
    ### PRIORS FOR RANDOM EFFECTS
    sigma.surv ~ dunif(0, 2)                     # Prior for standard deviation of survival
    tau.surv <- pow(sigma.surv, -2)
    
    #### MONTHLY SURVIVAL PROBABILITY
    for (i in 1:nind){
    for (t in f[i]:(n.occasions)){
    logit(phi[i,t]) <- lp.mean +      ### intercept for mean survival 
    b.phi.mig*(mig[i,t]) +       ### survival dependent on migratory stage of the month (stationary or migratory)
    b.phi.capt*(capt[i]) +     ### survival dependent on captive-release (captive-raised or other)
    b.phi.lat1*(lat1[i,t])*(1-(adult[i,t])) +       ### survival dependent on migratory stage of the month (stationary or migratory)
    b.phi.age*(age[i,t]) +     ### survival dependent on age (juvenile or other)
    b.phi.pop*(pop[i])  +    ### survival dependent on population (western Europe or other)
    surv.raneff[year[t]]
    } #t
    } #i
    
    #### BASELINE FOR SURVIVAL PROBABILITY (wild adult stationary from east)
    mean.phi ~ dunif(0.9, 1)   # uninformative prior for all MONTHLY survival probabilities
    lp.mean <- log(mean.phi/(1 - mean.phi))    # logit transformed survival intercept
    
    #### SLOPE PARAMETERS FOR SURVIVAL PROBABILITY
    b.phi.capt ~ dnorm(0, 0.5)     # Prior for captive effect on survival probability on logit scale
    b.phi.mig ~ dnorm(0, 0.5)      # Prior for migration effect on survival probability on logit scale
    b.phi.lat1 ~ dnorm(0, 0.5)     # Prior for effect of AFRICA on survival probability on logit scale
    b.phi.age ~ dnorm(0, 0.5)      # Prior for age effect on survival probability on logit scale
    b.phi.pop ~ dunif(0,4)         # Prior for population effect on survival probability on logit scale
    
    
    #### TAG FAILURE AND LOSS PROBABILITY
    for (i in 1:nind){
    for (t in f[i]:(n.occasions)){
    logit(p.obs[i,t]) <- base.obs ##+ beta1*(t-l[i]) #### probability of observation GIVEN THAT TAG IS WORKING is reciprocal to time since last good record
    logit(tag.fail[i,t]) <- base.fail + beta2*tag.age[i,t] + beta3*tfail[i] #### probability of TAG FAILURE is influenced by tag type and tag age
    logit(p.found.dead[i,t]) <- base.recover + beta4*lat[i,t] #### probability of recovery is influenced by latitude
    } #t
    } #i
    
    
    ##### SLOPE PARAMETERS FOR OBSERVATION PROBABILITY
    base.obs ~ dnorm(0, 0.001)                # Prior for intercept of observation probability on logit scale
    base.fail ~ dnorm(0, 0.001)               # Prior for intercept of tag failure probability on logit scale
    base.recover ~ dnorm(0, 0.001)               # Prior for intercept of tag failure probability on logit scale
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





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE INPUT DATA FOR JAGS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Convert to numeric matrices that JAGS can loop over
y.telemetry<-as.matrix(EV.obs.matrix[,2:max(timeseries$col)])
z.telemetry<-as.matrix(EV.state.matrix[,2:max(timeseries$col)])
age.mat<-as.matrix(age.matrix[,2:max(timeseries$col)])
lat.mat<-as.matrix(lat.matrix[,2:max(timeseries$col)])
#mig.mat<- as.matrix(EV.phi.matrix[,2:max(timeseries$col)])

#y.telemetry[74,157:159]<-
#z.telemetry[74,159]<-1   ###  bird lost at sea, but erroneously classified as transmitter failure because of 'Confirmed dead' misspelling

range(age.mat, na.rm=T)
table(age.mat)
range(lat.mat, na.rm=T)
#range(mig.mat)


#### create vector of first marking and of last alive record
get.first.telemetry<-function(x)min(which(!is.na(x)))
get.last.telemetry<-function(x)max(which(!is.na(x) & x==1))
f.telemetry<-apply(y.telemetry,1,get.first.telemetry)
l.telemetry<-apply(y.telemetry,1,get.last.telemetry)
l.telemetry[3]<-85

#### extract and standardise covariates
tag.fail.indicator<-EV$mean.GPS.dist.last10fixes

#### BUNDLE DATA INTO A LIST
INPUT.telemetry <- list(y = y.telemetry,
                        f = f.telemetry,
                        l = l.telemetry,
                        age = age.mat,
                        adult = ifelse(age.mat>18,0,1), ### provide a simple classification for adults and non-adults
                        mig = as.matrix(EV.phi.matrix[,2:max(timeseries$col)]), ### provide a simple binary classification for stationary and migratory periods
                        lat = lat.mat,
                        lat1 = ifelse(lat.mat<25,1,0), ### provide a simple classification for AFRICA
                        lat2 = ifelse(lat.mat>30,1,0), ### provide a simple classification for EUROPE
                        pop = ifelse(EV$pop %in% c("western europe"),1,0),  ##"italy",
                        pop.num = ifelse(EV$pop %in% c("western europe"),1,ifelse(EV$pop %in% c("italy","balkans"),2,3)),  ##numeric three level pop index
                        vul = as.matrix(vul.mat[,2:max(timeseries$col)]),
                        capt = ifelse(EV$captive.raised=="N",0,1),
				                tfail = as.numeric(tag.fail.indicator),
				                tag.age = as.matrix(tag.age[,2:max(timeseries$col)]),
                        nind = dim(y.telemetry)[1],
                        n.occasions = dim(y.telemetry)[2],
				                year=years,
				                nyears=length(unique(years)))
INPUT.telemetry$mig<-ifelse(INPUT.telemetry$mig>2,1,0) 

# EV %>% filter(population %in% c("italy","balkans")) %>% filter(age.at.deployment=="juv") %>%
#   group_by(population, fate) %>%
#   summarise(n=length(id.tag))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPECIFY AND SET UP MODEL RUNS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Parameters monitored
parameters.telemetry <- c("p.seen.alive","base.obs","base.fail","base.recover","beta1","beta2","beta3","beta4",
                          "mean.phi","lp.mean","b.phi.mig","b.phi.capt","b.phi.pop","b.phi.age","b.phi.vul","b.phi.age2","b.phi.lat",
                          "b.phi.lat1","b.phi.lat2")

# MCMC settings
ni <- 25000
nt <- 5
nb <- 5000
nc <- 3




#### PREPARE INITS ##########

inits.telemetry <- function(){list(z = z.telemetry,
                                   mean.phi = runif(1, 0.9, 1), ### two intercepts for juvenile and adults
                                   base.obs = rnorm(1,0, 0.001),                # Prior for intercept of observation probability on logit scale
                                   base.fail = rnorm(1,0, 0.001),               # Prior for intercept of tag failure probability on logit scale
                                   beta2 = rnorm(1,0, 0.001),         # Prior for slope parameter for 
                                   beta3 = rnorm(1,0, 0.001))} 


#### TOP MODEL WITH SIMPLE 2 LATITUDINAL BAND

FINAL_MODEL <- jags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
                                     "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVU_mig_lat_survival_model.jags",
                                     n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T, n.iter = ni)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SAVE THE OUTPUT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
try(setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study"), silent=T)
#save.image("EGVU_survival_output_REV1_FINAL.RData")
load("EGVU_survival_output_REV1_FINAL.RData")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############ PREPARE SUMMARY TABLES AND FIGURES  #############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CALCULATE PREDICTED SURVIVAL BASED ON MODEL with binary additive parameters
## MIG STAGES ARE: 0=stationary, 1=migratory
## POPULATION CLASSES ARE: 1=western Europe, 0=elsewhere
## AGE: 0=adult, 1=juvenile
## CAPT: 0=wild, 1=captive
## LAT: 0=north, 1=Africa
## summarise annual survival by using 10*stationary, 1*spring mig and 1*fall 

### PLOT PARAMETERS ON LOGIT SCALE
out10<-as.data.frame(FINAL_MODEL$summary)
out10$parameter<-row.names(FINAL_MODEL$summary)
fwrite(out10,"EGVU_REV1_FINAL_parameter_estimates.csv")

out10 %>% filter(grepl("b.phi",parameter)) %>%
  mutate(parameter=ifelse(parameter=="b.phi.mig","migration",parameter)) %>%
  mutate(parameter=ifelse(parameter=="b.phi.age","age",parameter)) %>%
  mutate(parameter=ifelse(parameter=="b.phi.pop","western Europe",parameter)) %>%
  mutate(parameter=ifelse(parameter=="b.phi.capt","captive-reared",parameter)) %>%
  mutate(parameter=ifelse(parameter=="b.phi.lat1","south of 25 N",parameter)) %>%
  ggplot()+
  geom_point(aes(x=parameter, y=mean))+
  geom_errorbar(aes(x=parameter, ymin=`2.5%`, ymax=`97.5%`), width=.1) +
  geom_hline(aes(yintercept=0), colour="darkgrey") +
  
  ## format axis ticks
  xlab("Parameter") +
  scale_y_continuous(name="estimate (logit scale)", limits=c(-2,2), breaks=seq(-2,2,0.5)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=18, color="black"),
        axis.text.x=element_text(size=12, color="black",angle=45, vjust = 1, hjust=1), 
        axis.title=element_text(size=18), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("EGVU_parameter_estimates_FINAL_REV1.jpg", height=7, width=10)



### PREPARE RAW MCMC OUTPUT
parmcols<-dimnames(FINAL_MODEL$samples[[1]])[[2]]

### COMBINE SAMPLES ACROSS CHAINS
MCMCout<-rbind(FINAL_MODEL$samples[[1]],FINAL_MODEL$samples[[2]],FINAL_MODEL$samples[[3]])
str(MCMCout)



#### TABLE FOR PREDICTED ANNUAL SURVIVAL FOR ADULT AND JUVENILE FOR EACH POPULATION
### SET UP ANNUAL TABLE

AnnTab<-data.frame(pop=rep(c(0,1), each=24),
                   capt=0,
                   age=rep(c(seq(2:13),rep(54,12)),2),
                   mig=c(c(0,1,0,0,0,0,0,0,0,0,0,0), ## juveniles east
                         c(0,1,0,0,0,0,1,0,0,0,0,0), ## adults east
                         c(0,1,0,0,0,0,0,0,0,0,0,0),  ## juveniles west
                         c(0,1,0,0,0,0,0,1,0,0,0,0)),
                   lat=c(c(0,0,0,0,1,1,1,1,1,1,1,1), ## juveniles east
                         c(0,0,1,1,1,1,1,1,1,0,0,0), ## adults east
                         c(0,0,0,0,1,1,1,1,1,1,1,1),  ## juveniles west
                         c(0,0,1,1,1,1,1,1,1,0,0,0)))   ## adults  west
Xin<-AnnTab %>% mutate(capt=1) %>% bind_rows(AnnTab) %>% 
  mutate(Population=ifelse(pop==1,"western Europe","central and east")) %>%
  #mutate(scaleage=agescale[age]) %>%
  mutate(adult=ifelse(age<19,1,0))


### CALCULATE PREDICTED VALUE FOR EACH SAMPLE
MCMCpred<-data.frame()
for(s in 1:nrow(MCMCout)) {
  
  X<-  Xin %>%
    
    ### CALCULATE MONTHLY SURVIVAL
    mutate(logit.surv=as.numeric(MCMCout[s,match("lp.mean",parmcols)])+
             as.numeric(MCMCout[s,match("b.phi.mig",parmcols)])*mig +
             as.numeric(MCMCout[s,match("b.phi.age",parmcols)])*age +
             as.numeric(MCMCout[s,match("b.phi.capt",parmcols)])*capt +
             as.numeric(MCMCout[s,match("b.phi.lat1",parmcols)])*lat*(1-adult) +
             as.numeric(MCMCout[s,match("b.phi.pop",parmcols)])*pop) %>%
    ### BACKTRANSFORM TO NORMAL SCALE
    mutate(surv=plogis(logit.surv)) %>%
    
    ### CALCULATE ANNUAL SURVIVAL
    mutate(Ageclass=ifelse(age==54,"adult","juvenile")) %>%
    group_by(Ageclass,Population,capt) %>%
    summarise(ann.surv=prod(surv)) %>%
    mutate(simul=s)            
  
  
  MCMCpred<-rbind(MCMCpred,as.data.frame(X)) 
  
}


### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

TABLE2<-  MCMCpred %>% 
  
  ### ANNOTATE GROUPS
  #mutate(Ageclass=ifelse(age==54,"adult","juvenile")) %>%
  mutate(Origin=ifelse(capt==0,"wild","captive")) %>%
  
  ### CALCULATE CREDIBLE INTERVALS
  group_by(Population,Ageclass,Origin) %>%
  summarise(med.surv=quantile(ann.surv,0.5),lcl.surv=quantile(ann.surv,0.025),ucl.surv=quantile(ann.surv,0.975)) %>%
  arrange(Population,Ageclass,Origin) %>%
  filter(!(Population=="western Europe" & Origin=="captive"))

TABLE2
fwrite(TABLE2,"EGVU_AnnSurv_Table1_REV1_FINAL.csv")




##### FIGURE OF MONTHLY SURVIVAL WITH MIGRATORY STAGE
### SET UP ANNUAL TABLE

##### FIGURE OF MONTHLY SURVIVAL WITH MIGRATORY STAGE
### SET UP ANNUAL TABLE

MigTab<-expand.grid(age=seq(1,54,3),mig=c(0,1),pop=c(0,1),lat=c(0,1), capt=c(0,1)) %>%
  mutate(Population=ifelse(pop==1,"western Europe","central and east")) %>%
  mutate(adult=ifelse(age<19,1,0))
dim(MigTab)*12000

### CALCULATE PREDICTED VALUE FOR EACH SAMPLE
MCMCpred<-data.frame()
for(s in 1:nrow(MCMCout)) {
  
  X<-  MigTab %>%
    
    ### CALCULATE MONTHLY SURVIVAL
    mutate(logit.surv=as.numeric(MCMCout[s,match("lp.mean",parmcols)])+
             as.numeric(MCMCout[s,match("b.phi.capt",parmcols)])*capt +
             as.numeric(MCMCout[s,match("b.phi.mig",parmcols)])*mig +
             as.numeric(MCMCout[s,match("b.phi.age",parmcols)])*age +
             as.numeric(MCMCout[s,match("b.phi.lat1",parmcols)])*lat*(1-adult) +
             as.numeric(MCMCout[s,match("b.phi.pop",parmcols)])*pop)
  
  MCMCpred<-rbind(MCMCpred,X) 
}


### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

MCMCpred %>% group_by(age,mig, lat) %>%
  summarise(med.surv=quantile(logit.surv,0.5),lcl.surv=quantile(logit.surv,0.025),ucl.surv=quantile(logit.surv,0.975)) %>%
  filter(!(mig==1 & lat==1)) %>%
  filter(!(age<19 & lat==1)) %>%
  
  ### BACKTRANSFORM TO NORMAL SCALE
  mutate(surv=plogis(med.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  
  ### ANNOTATE GROUPS
  mutate(Season=ifelse(lat==1,"south of 25°N",ifelse(mig==1,"migration","north of 25°N"))) %>%
  #mutate(stage=ifelse(mig==0,"stationary","migrating")) %>%
  ggplot()+
  geom_ribbon(aes(x=age, ymin=lcl, ymax=ucl, fill=Season), alpha=0.2) +   ##, type=Origin
  geom_line(aes(x=age, y=surv, color=Season))+     ## , linetype=Origin
  
  ## format axis ticks
  scale_x_continuous(name="Age in years", limits=c(1,54), breaks=seq(1,54,6), labels=seq(0,4,0.5)) +
  scale_y_continuous(name="Monthly survival probability", limits=c(0.7,1), breaks=seq(0.,1,0.05)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=16, color="black"),  
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("FIG3_EGVU_Monthly_Surv_mig_tradeoff.jpg", width=11,height=9, quality=100)



