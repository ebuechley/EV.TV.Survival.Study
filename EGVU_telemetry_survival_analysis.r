##########################################################################
#
# EGYPTIAN AND TURKEY VULTURE MONTHLY SURVIVAL ANALYSIS FROM TELEMETRY
#
##########################################################################
# written by Steffen Oppel, July 2019
# data preparation by Evan Buechley

## updated 5 Nov 2019 to include suggestions by Ron Efrat, Louis Phipps, and Evan Buechley
## include age of transmitter in prob of signal (p.obs)
## include type of transmitter (GSM/PTT) in prob of signal
## include area where signal dropped off to gauge whether GSM coverage may have been poor [requires GSM coverage maps]
## consider date of loss as function of time with respect to end of study period (May 2019)
## add tag loss probability (tag ok, not moving, but bird is alive because it lost the tag)


## UPDATE 1 NOV 2019: after discussion with Evan decided to focus on EV first
## re-write model with linear predictor for survival to vary with (1) age, (2) migration, and (3) captive status
## build full model first, then consider interactions and/or parameter selection

## UPDATE 17 NOV 2019: included new data sent by Evan


library(jagsUI)
library(tidyverse)
library(data.table)
library(lubridate)
filter<-dplyr::filter
select<-dplyr::select


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DATA FROM SPREADSHEETS AND ASSIGN FINAL STATES OF EACH ANIMAL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

try(setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study"), silent=T)
#try(setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study"), silent=T)
#EV<-fread("Google Sheets\\Egyptian Vulture tracking summary - EV summary.csv")
#EV<-fread("Google Sheets\\EGVU_fate_summary_Balkans.csv")
EV<-fread("ev.tv.summary.proofed.csv")
EVcovar<-fread("ev.survival.prepared.csv")
names(EV)[1]<-'species'
head(EV)


#EV<-EV %>% mutate(start=mdy_hm(start.date), end= mdy_hm(end.date)) %>%
EV<-EV %>% mutate(start=parse_date_time(start.date, c("mdy", "mdy HM")), end= parse_date_time(end.date, c("mdy", "mdy HM"))) %>%
  filter(!is.na(start)) %>%
  filter(species=="Neophron percnopterus") %>%
  filter(start<ymd_hm("2019-04-01 12:00")) %>%  ## remove birds only alive for a few months in 2019
  select(species,population,id.tag,sex,age.at.deployment,age.at.deployment.month,captive.raised,rehabilitated, start, end, fate, how.fate.determined.clean, mean.GPS.dist.last10fixes.degrees)
head(EV)
dim(EV)


####### ASSIGNMENT OF STATES ########
## this may be better done manually and should certainly be validated by data owners!

unique(EV$how.fate.determined.clean)
unique(EV$age.at.deployment)
unique(EV$fate)
EV %>% filter(is.na(fate))

### SHOW INVENTORY OF POSSIBLE COMBINATIONS OF STATES
as.data.frame(table(EV$how.fate.determined.clean,EV$fate)) %>%
	filter(Freq!=0) %>%
	rename(fate=Var2, how.fate.det=Var1)


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
                                  ifelse(how.fate.determined.clean %in% c("carcass found","resighted / recaptured","transmitter recovered"),4,2)))))

head(EV)

### CHECK WHETHER STATE ASSIGNMENT IS PLAUSIBLE ###
table(EV$TS,EV$OS)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE CAPTURE HISTORY FOR SURVIVAL ESTIMATIONS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## MAJOR UPDATE 5 Nov 2019 - SHOULD WE REMOVE YEAR AND REDUCE CH from 187 to 134 OCCASIONS?
### CALCULATE LONGEST POSSIBLE EXPOSURE
EV %>% mutate(dur=as.numeric(difftime(end,start,"days"))/30) %>% group_by(species) %>%
	summarise(totlength=max(dur))
## small reduction, but a lot of effort - omitted for now!
	

### CREATE A TIME SERIES DATA FRAME ###
mindate<-min(EV$start)
maxdate<-max(EV$end)
timeseries<-data.frame(date=seq(mindate, maxdate, "1 month")) %>%
  mutate(month=month(date),year=year(date)) %>%
  mutate(date=format(date, format="%m-%Y")) %>%
  mutate(season=ifelse(month %in% c(2,3,4,9,10), 'migration',ifelse(month %in% c(11,12,1),"winter","summer"))) %>%
  mutate(col=seq_along(date)+1)
dim(timeseries)


### CREATE A BLANK MATRICES TO HOLD INFORMATION ABOUT TRUE AND OBSERVED STATES ###

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
# commented out on 17 NOV 2019 when inserting new covariate data frame
# 
# # phi[1]: juvenile survival probability during migration
# # phi[2]: juvenile survival probability during winter
# # phi[3]: immature survival probability during stationary period (winter or summer)
# # phi[4]: immature survival probability during migration
# # phi[5]: adult survival probability during summer (breeding season)
# # phi[6]: adult survival probability during migration
# # phi[7]: adult survival probability during winter (non-breeding season)
# 
# phi.lookup.ev<-data.frame(age=c("juv","imm","adult"),summer=c(1,3,5),migration=c(1,4,6),winter=c(2,3,7), species="Neophron percnopterus")
# phi.lookup.tv<-data.frame(age=c("juv","imm","adult"),summer=c(8,10,12),migration=c(8,11,13),winter=c(9,10,14), species="Cathartes aura")
# phi.lookup<-rbind(phi.lookup.ev,phi.lookup.tv)
# age.lookup<-data.frame(month=seq(1:max(timeseries$col)), age=c(rep("juv",10),rep("imm",43),rep("adult",(max(timeseries$col)-53))))  ## CHANGED ADULT FROM 72 to 54 MONTHS BASED ON EVAN'S DEPLOYMENT AGE
# unique(EV$age.at.deployment.month)
# EV$age.at.deployment.month[is.na(EV$age.at.deployment.month)]<-32      ### FILL IN BLANKS WITH MEAN AGE AT DEPLOYMENT mean(EV$age.at.deployment.month,na.rm=T)
# 
# EV.phi.matrix<-EV %>% select(id.tag)
# EV.phi.matrix[,2:max(timeseries$col)]<-NA									### NEEDS MANUAL ADJUSTMENT IF REPEATED FOR LONGER TIME SERIES
# 
# 
# 
# ### FILL MATRICES WITH STATE INFORMATION ###
# 
# for(n in EV.obs.matrix$id.tag){
#   xl<-EV %>% filter(id.tag==n)
#   species<-xl$species
#   mindate<-format(xl$start, format="%m-%Y")
#   maxdate<-format(xl$end, format="%m-%Y")
#   startcol<-timeseries$col[timeseries$date==mindate]
#   #startagecat<-ifelse(xl$age.at.deployment=="adult","adult", ifelse(xl$age.at.deployment=="juv","juv","imm")) ### very simplistic assumes all immatures are 25 months old!
#   startagecat<-age.lookup$age[match(xl$age.at.deployment.month,age.lookup$month)]
#   startagemonth<-xl$age.at.deployment.month
#   
#   ## ASSIGN SURVIVAL PARAMETERS FOR FIRST SURVIVAL 
#   EV.phi.matrix[EV.phi.matrix$id.tag==n,startcol]<-phi.lookup[phi.lookup$age==startagecat & phi.lookup$species==species,match(timeseries$season[timeseries$col==startcol],names(phi.lookup))]
#   
#   ### ASSIGN SURVIVAL PARAMETERS FOR ALL SUBSEQUENT INTERVALS
#   for (col in (startcol+1):max(timeseries$col)){
#     age.progress<-col-startcol
#     curr.age<-if_else(startagecat=="adult","adult",
#                      as.character(age.lookup$age[age.progress+startagemonth]))
#     curr.season<-timeseries$season[timeseries$col==col]
#     EV.phi.matrix[EV.phi.matrix$id.tag==n,col]<-phi.lookup[phi.lookup$age==curr.age & phi.lookup$species==species,match(curr.season,names(phi.lookup))]
#     
#   } ## end loop over each occasion
#   
# } ## end loop over each animal
# 




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


## CREATE MOVE DISTANCE (MIGRATION) MATRIX
mig.matrix<-EVcovar %>% filter(id.tag %in% EV.obs.matrix$id.tag) %>%
  mutate(timestamp=ymd_hms(timestamp)) %>%
  mutate(yr.mo=format(timestamp, format="%m-%Y")) %>%
  mutate(col=timeseries$col[match(yr.mo,timeseries$date)]) %>%
  group_by(id.tag,col) %>%
  summarise(mig=mean(sum.monthly.dist)) %>%
  spread(key=col,value=mig) %>%
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
    mig.matrix[age.matrix$id.tag==n,col]<-mig.matrix[mig.matrix$id.tag==n,(col-1)]

  } ## end loop over each occasion
  
  
  misscol<-which(is.na(age.matrix[age.matrix$id.tag==n,]))
  misscol<-misscol[misscol>startcol]
  
  ### INTERPOLATE VALUES IF NA IN COLUMNS THAT SHOULD HAVE VALUES
  for (col in misscol){
    age.matrix[age.matrix$id.tag==n,col]<-min((age.matrix[age.matrix$id.tag==n,(col-1)]+1),54)
    lat.matrix[lat.matrix$id.tag==n,col]<-lat.matrix[lat.matrix$id.tag==n,(col-1)]
    mig.matrix[age.matrix$id.tag==n,col]<-mig.matrix[mig.matrix$id.tag==n,(col-1)]
    
  } ## end loop over each occasion

} ## end loop over each animal





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE INPUT DATA FOR JAGS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CHECK THAT MATRICES HAVE IDENTICAL DIMENSIONS AND SORT ORDER
# head(EV.obs.matrix)
# dim(EV.obs.matrix)
# head(EV.state.matrix)
# dim(EV.state.matrix)
# head(age.matrix)
# dim(age.matrix)
# head(mig.matrix)
# dim(mig.matrix)
# head(lat.matrix)
# dim(lat.matrix)

#### Convert to numeric matrices that JAGS can loop over
y.telemetry<-as.matrix(EV.obs.matrix[,2:max(timeseries$col)])
z.telemetry<-as.matrix(EV.state.matrix[,2:max(timeseries$col)])
phi.mat<-as.matrix(EV.phi.matrix[,2:max(timeseries$col)])
age.mat<-as.matrix(age.matrix[,2:max(timeseries$col)])
mig.mat<-as.matrix(mig.matrix[,2:max(timeseries$col)])
lat.mat<-as.matrix(lat.matrix[,2:max(timeseries$col)])


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
                        mig = mig.mat,
                        lat = lat.mat,
                        capt = ifelse(EV$captive.raised=="N",0,1),
				                tfail = as.numeric(tag.fail.indicator),
				                tag.age = as.matrix(tag.age[,2:max(timeseries$col)]),
                        nind = dim(y.telemetry)[1],
                        n.occasions = dim(y.telemetry)[2],
                        #phi.mat=phi.mat,
                        nsurv=max(phi.mat,na.rm=T))
#rm(list=setdiff(ls(), "INPUT.telemetry"))





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPECIFY JAGS MODEL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Specify model in BUGS language
sink("EGVU_telemetry_multistate_tagfail_phi_lp.jags")
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
  # 2 Tag ok, bird not moving (dead, or tag lost)
  # 3 Tag failed, bird observed alive
  # 4 Dead bird recovered
  # 5 No signal (=not seen)
  
  # -------------------------------------------------
  
  # Priors and constraints


  # MONTHLY SURVIVAL PROBABILITY
  for (i in 1:nind){
    for (t in f[i]:(n.occasions)){
        logit(phi[i,t]) <- lp.mean + b.phi.age*(age[i,t]) + b.phi.mig*(mig[i,t])+ b.phi.capt*(capt[i]) + b.phi.lat*(lat[i,t])  #### probability of monthly survival dependent on age, migration status, and captive origin
    } #t
  } #i
  

  #### SLOPE PARAMETERS FOR SURVIVAL PROBABILITY
  mean.phi ~ dunif(0.5, 0.9999)   # uninformative prior for all MONTHLY survival probabilities
  lp.mean <- log(mean.phi/(1 - mean.phi))    # logit transformed survival intercept
  b.phi.age ~ dnorm(0, 0.001)                # Prior for slope of age on survival probability on logit scale
  b.phi.mig ~ dnorm(0, 0.001)               # Prior for slope of migration on survival probability on logit scale
  b.phi.capt ~ dnorm(0, 0.001)         # Prior for slope of captive origin on survival probability on logit scale
  b.phi.lat ~ dnorm(0, 0.001)         # Prior for slope of captive origin on survival probability on logit scale


  # TAG FAILURE AND LOSS PROBABILITY
  for (i in 1:nind){
   for (t in f[i]:(n.occasions)){
      logit(p.obs[i,t]) <- base.obs + beta1*(t-l[i]) + obs.error[t]   #### probability of observation GIVEN THAT TAG IS WORKING is reciprocal to time since last good record
      logit(tag.fail[i,t]) <- base.fail + beta2*tag.age[i,t] + beta3*tfail[i] + tag.fail.error[t]     #### probability of TAG FAILURE is influenced by tag type and tag age
      } #t
   } #i

  for (t in 1:(n.occasions)){
    tag.fail.error[t] ~ dnorm(0, tau)
    obs.error[t] ~ dnorm(0, tau)
  }

  # SLOPE PARAMETERS FOR OBSERVATION PROBABILITY
	base.obs ~ dnorm(0, 0.001)                # Prior for intercept of observation probability on logit scale
	base.fail ~ dnorm(0, 0.001)               # Prior for intercept of tag failure probability on logit scale
	beta1 ~ dnorm(0, 0.001)T(-10, 10)         # Prior for slope parameter for obs prob with time since
	beta2 ~ dnorm(0, 0.001)T(-10, 10)         # Prior for slope parameter for fail probability with tag age
	beta3 ~ dnorm(0, 0.001)T(-10, 10)         # Prior for slope parameter for fail probability with tage movement during last 10 GPS fixes
	sigma ~ dunif(0, 10)                     # Prior on standard deviation for random error term
	tau <- pow(sigma, -2)

  p.found.dead ~ dunif(0, 1)   # Prior for probability that dead bird carcass is found
  p.seen.alive ~ dunif(0, 1)    # Prior for probability that bird with defunct or lost tag is observed alive
    #p.obs ~ dunif(0.5, 1)       # Prior for probability to 'observe' a bird with functional tag (=should be 1?)


  
  # Define state-transition and observation matrices 
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
      ps[3,i,t,2]<-0 ###phi[i,t] * (1-tag.fail[i,t]) ### since we do not have a monthly transition matrix this transition is not possible
      ps[3,i,t,3]<-phi[i,t] ###* tag.fail[i,t]
      
      # Define probabilities of O(t) [last dim] given S(t)  [first dim]
      
      po[1,i,t,1]<-0
      po[1,i,t,2]<-p.obs[i,t] * (1-tag.fail[i,t]) * (1-p.found.dead)
      po[1,i,t,3]<-0
      po[1,i,t,4]<-p.found.dead
      po[1,i,t,5]<-(1-p.obs[i,t]) * tag.fail[i,t] * (1-p.found.dead)
      
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
    z[i,f[i]] <- 2 ## y[i,f[i]]                  ### THIS MAY NEED TO BE FIXED AS THE OBS STATES DO NOT MATCH TRUES STATES
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
# SPECIFY AND SET UP MODEL RUN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Parameters monitored
parameters.telemetry <- c("mean.phi","p.seen.alive","p.found.dead","b.phi.age","b.phi.mig","b.phi.capt","b.phi.lat")

# Initial values
#inits.telemetry <- function(){list(z = z.telemetry,
#                                   phi = runif(INPUT.telemetry$nsurv, 0.5, 0.999),
#                                   p.obs = runif(1, 0.5, 1))}  

inits.telemetry <- function(){list(z = z.telemetry,
                                   mean.phi = runif(1, 0.5, 0.999),
					     base.obs = rnorm(1,0, 0.001),                # Prior for intercept of observation probability on logit scale
						base.fail = rnorm(1,0, 0.001),               # Prior for intercept of tag failure probability on logit scale
						beta1 = rnorm(1,0, 0.001),         # Prior for slope parameter for obs prob with time since
						beta2 = rnorm(1,0, 0.001),         # Prior for slope parameter for 
						beta3 = rnorm(1,0, 0.001))} 

# MCMC settings
ni <- 5
nt <- 4
nb <- 100
nc <- 4

# Call JAGS from R (took 30 min for Balkan data)
EVsurv <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
			"C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study\\EGVU_telemetry_multistate_tagfail_phi_lp.jags",
			n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni) 

save.image("EGVU_survival_output_v2.RData")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXPORT THE OUTPUT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
out<-as.data.frame(TV_EVsurv$summary)
out$parameter<-row.names(TV_EVsurv$summary)
write.table(out,"TUVU_EGVU_telemetry_survival_estimates.csv", sep=",", row.names=F)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT SURVIVAL ESTIMATES 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
phi.labels<-phi.lookup %>% gather(key="season", value="parameter",-age,-species) %>% mutate(label=paste(age,season, sep=".")) %>%
  arrange(parameter) %>%
  filter(!grepl("juv.summer",label)) %>%
  filter(!grepl("imm.summer",label))
                                                                         
## retrieve the population projections
plotdat<-out[(grep("phi",out$parameter)),c(12,1,3,7)] %>%
  mutate(parameter= phi.labels$label) %>%
  mutate(species= phi.labels$species)
names(plotdat)[1:4]<-c('parm','mean','lcl','ucl')



### produce plot 

pdf("TV_EV_survival_estimates.pdf", width=7, height=10)

ggplot(plotdat)+
  geom_point(aes(x=parm, y=mean), size=1,col='darkgrey')+
  geom_errorbar(aes(x=parm, ymin=lcl, ymax=ucl), width=.1)+
  facet_wrap(~species, ncol=1, scales="fixed") +

  ## format axis ticks
  scale_y_continuous(name="monthly survival probability", limits=c(0.7,1),breaks=seq(0.7,1,0.05), labels=seq(0.7,1,0.05))+

  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=18, color="black"),
        axis.text.x=element_text(size=12, color="black",angle=45, vjust = 1, hjust=1), 
        axis.title=element_text(size=18), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

dev.off()


