##########################################################################
#
# EGYPTIAN AND TURKEY VULTURE MONTHLY SURVIVAL ANALYSIS FROM TELEMETRY
#
##########################################################################
# written by Steffen Oppel, July 2019
# data preparation by Evan Buechley


library(jagsUI)
library(tidyverse)
library(data.table)
library(lubridate)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DATA FROM SPREADSHEETS AND ASSIGN FINAL STATES OF EACH ANIMAL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

try(setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study"), silent=T)
try(setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study"), silent=T)
EV<-fread("Google Sheets\\Egyptian Vulture tracking summary - EV summary.csv")


EV<-EV %>% mutate(start=mdy_hm(start.date), end= mdy_hm(end.date)) %>%
  filter(!is.na(start)) %>%
  filter(start<ymd_hm("2019-04-01 12:00")) %>%  ## remove birds only alive for a few months in 2019
  select(id.tag,sex,age.at.deployment,captive.raised,rehabilitated, start, end, fate, how.fate.determined)
head(EV)


####### ASSIGNMENT OF STATES ########
## this may be better done manually and should certainly be validated by data owners!

# True States (S) - these are often unknown and cannot be observed, we just need them to initialise the model (best guess)
# 1 dead
# 2 alive with functioning tag
# 3 alive with defunct tag

# Observed States (O) - these are based on the actual transmission history
# 1 Tag ok, bird moving
# 2 Tag ok, bird not moving (dead)
# 3 Tag failed, bird observed alive
# 4 Dead bird recovered
# 5 No signal (=not seen)

unique(EV$how.fate.determined)
unique(EV$fate)
EV %>% filter(is.na(fate))

EV<-EV %>%
  mutate(TS= ifelse(fate=="alive",2,
                    ifelse(fate %in% c("dead","suspected mortality"),1,3))) %>%
  mutate(OS= ifelse(fate=="alive",1,
                    ifelse(fate %in% c("unknown","suspected transmitter failure"),5,
                           ifelse(fate=="verified transmitter failure",3,
                                  ifelse(fate=="dead",4,
                                         ifelse(fate=="suspected mortality",2,5)))))) %>%
  mutate(TS=ifelse(is.na(TS),3,TS),OS=ifelse(is.na(OS),5,OS))       ## WE SHOULD MAKE SURE THAT THIS IS NOT NEEDED!


head(EV)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE CAPTURE HISTORY FOR SURVIVAL ESTIMATIONS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### CREATE A TIME SERIES DATA FRAME ###
mindate<-min(EV$start)
maxdate<-max(EV$end)
timeseries<-data.frame(date=seq(mindate, maxdate, "1 month")) %>%
  mutate(month=month(date),year=year(date)) %>%
  mutate(date=format(date, format="%m-%Y")) %>%
  mutate(col=seq_along(date)+1)
dim(timeseries)


### CREATE A BLANK MATRICES TO HOLD INFORMATION ABOUT TRUE AND OBSERVED STATES ###

EV.obs.matrix<-EV %>% select(id.tag)
EV.obs.matrix[,2:max(timeseries$col)]<-NA									### NEEDS MANUAL ADJUSTMENT IF REPEATED FOR LONGER TIME SERIES

EV.state.matrix<-EV %>% select(id.tag)
EV.state.matrix[,2:max(timeseries$col)]<-NA									### NEEDS MANUAL ADJUSTMENT IF REPEATED FOR LONGER TIME SERIES

EV.phi.matrix<-EV %>% select(id.tag)
EV.phi.matrix[,2:max(timeseries$col)]<-NA									### NEEDS MANUAL ADJUSTMENT IF REPEATED FOR LONGER TIME SERIES


### FILL MATRICES WITH STATE INFORMATION ###

for(n in EV.obs.matrix$id.tag){
  xl<-EV %>% filter(id.tag==n)
  mindate<-format(xl$start, format="%m-%Y")
  maxdate<-format(xl$end, format="%m-%Y")
  startcol<-timeseries$col[timeseries$date==mindate]
  stopcol<-timeseries$col[timeseries$date==maxdate]
  stopcol<-ifelse(stopcol<=startcol,min(startcol+1,max(timeseries$col)),stopcol)
  
  ## ASSIGN OBSERVED STATE
  EV.obs.matrix[EV.obs.matrix$id.tag==n,startcol:(stopcol-1)]<-1
  if(startcol==stopcol){EV.obs.matrix[EV.obs.matrix$id.tag==n,2:(stopcol-1)]<-NA} ## for the few cases where stopcol-1 is actually before startcol
  EV.obs.matrix[EV.obs.matrix$id.tag==n,stopcol:max(timeseries$col)]<-xl$OS   ## this assumes that state never changes - some birds may have gone off air and then been found dead - this needs manual adjustment!  
  
  ## ASSIGN INITIAL TRUE STATE (to initialise z-matrix of model)
  EV.state.matrix[EV.state.matrix$id.tag==n,(startcol+1):(stopcol-1)]<-2      ## state at first capture is known, hence must be NA in z-matrix
  EV.state.matrix[EV.state.matrix$id.tag==n,stopcol:max(timeseries$col)]<-xl$TS   ## this assumes that state never changes - some birds may have gone off air and then been found dead - this needs manual adjustment!
  EV.state.matrix[EV.state.matrix$id.tag==n,2:startcol]<-NA ## error occurs if z at first occ is not NA, so we need to specify that for birds alive for <1 month because stopcol-1 = startcol
  
  ## ASSIGN SURVIVAL PARAMETERS FOR TRANSITIONS (NEEDS MORE WORK!)
  # phi[1]: juvenile survival probability during migration
  # phi[2]: juvenile survival probability during winter
  # phi[3]: immature survival probability during stationary period (winter or summer)
  # phi[4]: immature survival probability during migration
  # phi[5]: adult survival probability during summer (breeding season)
  # phi[6]: adult survival probability during migration
  # phi[7]: adult survival probability during winter (non-breeding season)
  
  ### NEED TO DEFINE SEASONS AND SPECIFY COLUMNS
  
  EV.phi.matrix[EV.phi.matrix$id.tag==n,2:max(timeseries$col)]<-rep(1:2,53)   ### NEEDS WORK TO SPECIFY PROPERLY BY AGE, MIG, RELEASE STATUS
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE INPUT DATA FOR JAGS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#### Convert to numeric matrices that JAGS can loop over

y.telemetry<-as.matrix(EV.obs.matrix[,2:max(timeseries$col)])
z.telemetry<-as.matrix(EV.state.matrix[,2:max(timeseries$col)])
phi.mat<-as.matrix(EV.phi.matrix[,2:max(timeseries$col)])


#### create vector of first marking (this is 1 by default)
get.first.telemetry<-function(x)min(which(!is.na(x)))
f.telemetry<-apply(y.telemetry,1,get.first.telemetry)


INPUT.telemetry <- list(y = y.telemetry,
                        f = f.telemetry,
                        nind = dim(y.telemetry)[1],
                        n.occasions = dim(y.telemetry)[2],
                        phi.mat=phi.mat,
                        nsurv=max(phi.mat,na.rm=T))
#rm(list=setdiff(ls(), "INPUT.telemetry"))





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPECIFY JAGS MODEL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Specify model in BUGS language
sink("EGVU_telemetry_multistate.jags")
cat("
model {
  
  # -------------------------------------------------
  # Parameters:
  # phi[1]: juvenile survival probability during migration
  # phi[2]: juvenile survival probability during winter
  # phi[3]: immature survival probability during stationary period (winter or summer)
  # phi[4]: immature survival probability during migration
  # phi[5]: adult survival probability during summer (breeding season)
  # phi[6]: adult survival probability during migration
  # phi[7]: adult survival probability during winter (non-breeding season)
  # tag.fail: probability that tag will fail
  
  # p.obs: probability to be tracked with functioning tag (=1)
  # p.found.dead: probability for carcass to be recovered
  # p.seen.alive: probability to be observed alive despite the tag being defunct
  
  # -------------------------------------------------
  # States (S):
  # 1 dead
  # 2 alive with functioning tag
  # 3 alive with defunct tag
  
  # Observations (O):
  # 1 Tag ok, bird moving
  # 2 Tag ok, bird not moving (dead)
  # 3 Tag failed, bird observed alive
  # 4 Dead bird recovered
  # 5 No signal (=not seen)
  
  # -------------------------------------------------
  
  # Priors and constraints
  for (s in 1:nsurv){
    phi[s] ~ dunif(0.5, 0.9999)   # Equal uninformative prior for all MONTHLY survival probabilities
  }
  
  tag.fail ~ dunif(0, 1)   # Prior for MONTHLY tag failure probability
  p.found.dead ~ dunif(0, 1)   # Prior for probability that dead bird carcass is found
  p.seen.alive ~ dunif(0, 1)    # Prior for probability that bird with defunct tag is observed alive
  p.obs ~ dunif(0.5, 1)       # Prior for probability to 'observe' a bird with functional tag (=should be 1?)
  
  
  
  # Define state-transition and observation matrices 
  for (i in 1:nind){
    
    for (t in f[i]:(n.occasions-1)){
      
      # Define probabilities of state S(t+1) [last dim] given S(t) [first dim]
      
      ps[1,i,t,1]<-1    ## dead birds stay dead
      ps[1,i,t,2]<-0
      ps[1,i,t,3]<-0
      
      ps[2,i,t,1]<-(1-phi[phi.mat[i,t]])
      ps[2,i,t,2]<-phi[phi.mat[i,t]] * (1-tag.fail)
      ps[2,i,t,3]<-phi[phi.mat[i,t]] * tag.fail
      
      ps[3,i,t,1]<-(1-phi[phi.mat[i,t]])
      ps[3,i,t,2]<-phi[phi.mat[i,t]] * (1-tag.fail)
      ps[3,i,t,3]<-phi[phi.mat[i,t]] * tag.fail
      
      # Define probabilities of O(t) [last dim] given S(t)  [first dim]
      
      po[1,i,t,1]<-0
      po[1,i,t,2]<-p.obs * (1-tag.fail) * (1-p.found.dead)
      po[1,i,t,3]<-0
      po[1,i,t,4]<-p.found.dead
      po[1,i,t,5]<-(1-p.obs) * tag.fail * (1-p.found.dead)
      
      po[2,i,t,1]<-p.obs
      po[2,i,t,2]<-0
      po[2,i,t,3]<-0
      po[2,i,t,4]<-0
      po[2,i,t,5]<-(1-p.obs)
      
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
parameters.telemetry <- c("phi","p.obs","p.seen.alive","p.found.dead", "tag.fail")

# Initial values
inits.telemetry <- function(){list(z = z.telemetry,
                                   phi = runif(INPUT.telemetry$nsurv, 0.5, 0.999),
                                   p.obs = runif(1, 0.5, 1))}  


# MCMC settings
ni <- 20000
nt <- 4
nb <- 5000
nc <- 3

# Call JAGS from R
EVsurv <- jags(INPUT.telemetry, inits.telemetry, parameters.telemetry, "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study\\EGVU_telemetry_multistate.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, n.cores=nc, parallel=T)

