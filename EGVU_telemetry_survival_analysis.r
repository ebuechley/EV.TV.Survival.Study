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

setwd("~/Documents/GitHub/EV - TV Survival Study/")
#EV<-read.csv("Google Sheets/EGVU_fate_summary_Balkans.csv")
EV<-read.csv("ev.tv.summary.merged.final.csv")
summary(EV)

#EV<-EV %>% mutate(start=mdy_hm(start.date), end= mdy_hm(end.date)) %>%
EV<-EV %>% mutate(start=parse_date_time(start.date, c("mdy", "mdy HM")), end= parse_date_time(end.date, c("mdy", "mdy HM"))) %>%
  filter(!is.na(start)) %>%
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
                    ifelse(how.fate.determined.clean %in% c("unknown","suspected transmitter failure","transmission ended"),5,
                           ifelse(how.fate.determined.clean =="verified transmitter failure",3,
                                  ifelse(how.fate.determined.clean %in% c("dead","recaptured","retrieved transmitter and carcass","retrieved transmitter and asked locals","found carcass","carcass retrieved","tag retrieved, asked locals","found feathers"),4,
                                         ifelse(how.fate.determined.clean %in% c("suspected mortality","inferred from transmissions"),2,5)))))) %>%
  mutate(TS=ifelse(is.na(TS),3,TS),OS=ifelse(is.na(OS),ifelse(TS==1,2,5),OS))       ## WE SHOULD MAKE SURE THAT THIS IS NOT NEEDED!
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

EV.obs.matrix<-EV %>% select(id.tag)
EV.obs.matrix[,2:max(timeseries$col)]<-NA									

EV.state.matrix<-EV %>% select(id.tag)
EV.state.matrix[,2:max(timeseries$col)]<-NA								

tag.age<-EV %>% select(id.tag)
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




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE MATRIX OF SURVIVAL PARAMETERS BASED ON AGE AND SEASON
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# phi[1]: juvenile survival probability during migration
# phi[2]: juvenile survival probability during winter
# phi[3]: immature survival probability during stationary period (winter or summer)
# phi[4]: immature survival probability during migration
# phi[5]: adult survival probability during summer (breeding season)
# phi[6]: adult survival probability during migration
# phi[7]: adult survival probability during winter (non-breeding season)

phi.lookup<-data.frame(age=c("juv","imm","adult"),summer=c(1,3,5),migration=c(1,4,6),winter=c(2,3,7))
age.lookup<-data.frame(month=seq(1:max(timeseries$col)), age=c(rep("juv",10),rep("imm",43),rep("adult",(max(timeseries$col)-53))))  ## CHANGED ADULT FROM 72 to 54 MONTHS BASED ON EVAN'S DEPLOYMENT AGE
unique(EV$age.at.deployment.month)
EV$age.at.deployment.month[is.na(EV$age.at.deployment.month)]<-32      ### FILL IN BLANKS WITH MEAN AGE AT DEPLOYMENT mean(EV$age.at.deployment.month,na.rm=T)

EV.phi.matrix<-EV %>% select(id.tag)
EV.phi.matrix[,2:max(timeseries$col)]<-NA									### NEEDS MANUAL ADJUSTMENT IF REPEATED FOR LONGER TIME SERIES



### FILL MATRICES WITH STATE INFORMATION ###

for(n in EV.obs.matrix$id.tag){
  xl<-EV %>% filter(id.tag==n)
  mindate<-format(xl$start, format="%m-%Y")
  maxdate<-format(xl$end, format="%m-%Y")
  startcol<-timeseries$col[timeseries$date==mindate]
  #startagecat<-ifelse(xl$age.at.deployment=="adult","adult", ifelse(xl$age.at.deployment=="juv","juv","imm")) ### very simplistic assumes all immatures are 25 months old!
  startagecat<-age.lookup$age[match(xl$age.at.deployment.month,age.lookup$month)]
  startagemonth<-xl$age.at.deployment.month
  
  ## ASSIGN SURVIVAL PARAMETERS FOR FIRST SURVIVAL 
  EV.phi.matrix[EV.phi.matrix$id.tag==n,startcol]<-phi.lookup[phi.lookup$age==startagecat,match(timeseries$season[timeseries$col==startcol],names(phi.lookup))]
  
  ### ASSIGN SURVIVAL PARAMETERS FOR ALL SUBSEQUENT INTERVALS
  for (col in (startcol+1):max(timeseries$col)){
    age.progress<-col-startcol
    curr.age<-if_else(startagecat=="adult","adult",
                     as.character(age.lookup$age[age.progress+startagemonth]))
    curr.season<-timeseries$season[timeseries$col==col]
    EV.phi.matrix[EV.phi.matrix$id.tag==n,col]<-phi.lookup[phi.lookup$age==curr.age,match(curr.season,names(phi.lookup))]
    
  } ## end loop over each occasion
  
} ## end loop over each animal





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE INPUT DATA FOR JAGS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#### Convert to numeric matrices that JAGS can loop over
y.telemetry<-as.matrix(EV.obs.matrix[,2:max(timeseries$col)])
z.telemetry<-as.matrix(EV.state.matrix[,2:max(timeseries$col)])
phi.mat<-as.matrix(EV.phi.matrix[,2:max(timeseries$col)])


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
				tfail = tag.fail.indicator,
				tag.age = as.matrix(tag.age[,2:max(timeseries$col)]),
                        nind = dim(y.telemetry)[1],
                        n.occasions = dim(y.telemetry)[2],
                        phi.mat=phi.mat,
                        nsurv=max(phi.mat,na.rm=T))
#rm(list=setdiff(ls(), "INPUT.telemetry"))





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPECIFY JAGS MODEL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Specify model in BUGS language
sink("EGVU_telemetry_multistate_tagfail.jags")
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
  for (s in 1:nsurv){
    phi[s] ~ dunif(0.5, 0.9999)   # Equal uninformative prior for all MONTHLY survival probabilities
  }
  
  #tag.fail ~ dunif(0, 1)   # Prior for MONTHLY tag failure probability
  #tag.loss ~ dunif(0, 1)   # Prior for MONTHLY tag loss probability - this will be very difficult to resolve
  p.found.dead ~ dunif(0, 1)   # Prior for probability that dead bird carcass is found
  p.seen.alive ~ dunif(0, 1)    # Prior for probability that bird with defunct or lost tag is observed alive
  #p.obs ~ dunif(0.5, 1)       # Prior for probability to 'observe' a bird with functional tag (=should be 1?)


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
	base.obs ~ dnorm(0, 0.001)                # Prior for intercept of observation probability on logit scale
	base.fail ~ dnorm(0, 0.001)               # Prior for intercept of tag failure probability on logit scale
	beta1 ~ dnorm(0, 0.001)T(-10, 10)         # Prior for slope parameter for obs prob with time since
	beta2 ~ dnorm(0, 0.001)T(-10, 10)         # Prior for slope parameter for fail probability with tag age
	beta3 ~ dnorm(0, 0.001)T(-10, 10)         # Prior for slope parameter for fail probability with tage movement during last 10 GPS fixes
	sigma ~ dunif(0, 10)                     # Prior on standard deviation for random error term
	tau <- pow(sigma, -2)

  
  # Define state-transition and observation matrices 
  for (i in 1:nind){
    
    for (t in f[i]:(n.occasions-1)){
      
      # Define probabilities of state S(t+1) [last dim] given S(t) [first dim]
      
      ps[1,i,t,1]<-1    ## dead birds stay dead
      ps[1,i,t,2]<-0
      ps[1,i,t,3]<-0
      
      ps[2,i,t,1]<-(1-phi[phi.mat[i,t]])
      ps[2,i,t,2]<-phi[phi.mat[i,t]] * (1-tag.fail[i,t])
      ps[2,i,t,3]<-phi[phi.mat[i,t]] * tag.fail[i,t]
      
      ps[3,i,t,1]<-(1-phi[phi.mat[i,t]])
      ps[3,i,t,2]<-0 ###phi[phi.mat[i,t]] * (1-tag.fail[i,t]) ### since we do not have a monthly transition matrix this transition is not possible
      ps[3,i,t,3]<-phi[phi.mat[i,t]] ###* tag.fail[i,t]
      
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
parameters.telemetry <- c("phi","p.seen.alive","p.found.dead")

# Initial values
#inits.telemetry <- function(){list(z = z.telemetry,
#                                   phi = runif(INPUT.telemetry$nsurv, 0.5, 0.999),
#                                   p.obs = runif(1, 0.5, 1))}  

inits.telemetry <- function(){list(z = z.telemetry,
                                   phi = runif(INPUT.telemetry$nsurv, 0.5, 0.999),
					     base.obs = rnorm(1,0, 0.001),                # Prior for intercept of observation probability on logit scale
						base.fail = rnorm(1,0, 0.001),               # Prior for intercept of tag failure probability on logit scale
						beta1 = rnorm(1,0, 0.001),         # Prior for slope parameter for obs prob with time since
						beta2 = rnorm(1,0, 0.001),         # Prior for slope parameter for 
						beta3 = rnorm(1,0, 0.001))} 

# MCMC settings
ni <- 20000
nt <- 4
nb <- 5000
nc <- 3

# Call JAGS from R (took 30 min for Balkan data)
EVsurv <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
			"C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study\\EGVU_telemetry_multistate_tagfail.jags",
			n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T,#n.iter = ni, 
			max.iter=150.000)
save.image("EGVU_survival_output.RData")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXPORT THE OUTPUT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
out<-as.data.frame(EVsurv$summary)
out$parameter<-row.names(EVsurv$summary)
write.table(out,"EGVU_telemetry_survival_estimates.csv", sep=",", row.names=F)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT SURVIVAL ESTIMATES 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
phi.labels<-phi.lookup %>% gather(key="season", value="parameter",-age) %>% mutate(label=paste(age,season, sep=".")) %>%
  arrange(parameter) %>%
  filter(label!="juv.summer") %>%
  filter(label!="imm.summer")
                                                                         
## retrieve the population projections
plotdat<-out[(grep("phi",out$parameter)),c(12,1,3,7)] %>%
  mutate(parameter= phi.labels$label)
names(plotdat)[1:4]<-c('parm','mean','lcl','ucl')



### produce plot 

pdf("EV_survival_estimates.pdf", width=10, height=7)

ggplot(plotdat)+
  geom_point(aes(x=parm, y=mean), size=1,col='darkgrey')+
  geom_errorbar(aes(x=parm, ymin=lcl, ymax=ucl), width=.1)+

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


