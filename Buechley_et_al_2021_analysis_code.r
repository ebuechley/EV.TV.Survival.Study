##########################################################################
#
# EGYPTIAN VULTURE MONTHLY SURVIVAL ANALYSIS FROM TELEMETRY DATA
#
##########################################################################
# Citation:



# written by Steffen Oppel, finalised 5 February 2021
# data preparation by Evan Buechley, Ron Efrat, Louis Phipps, and Evan Buechley


library(jagsUI)
library(tidyverse)
library(data.table)
library(lubridate)
filter<-dplyr::filter
select<-dplyr::select


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DATA FROM ASSEMBLED R WORKSPACE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### download this file from "https:\\github.com\\steffenoppel\\vultures\\EgyptianVulture_data.RData",destfile="EgyptianVulture_data.RData"
load("EgyptianVulture_data.RData")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXPLANATION OF INPUT DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### the object 'z.telemetry' is a matrix of the initial values for the true life state of 220 tracked birds (rows) over 159 months (columns)
# Initial true life states are as follows:
# 1 dead
# 2 alive with functioning tag
# 3 alive with defunct tag or tag lost


### the object 'INPUT.telemetry' is a list with the following elements:
str(INPUT.telemetry)
# y: matrix of observed states (1-5) for 220 tracked birds (rows) over 159 months (columns):
# Observation states are as follows:
# 1 Tag ok, bird moving
# 2 Tag ok, bird not moving (dead)
# 3 Tag failed, bird observed alive
# 4 Dead bird recovered
# 5 No signal (=not seen)

# f: vector with one value per tracked individual specifying the first month of tracking
# l: vector with one value per tracked individual specifying the last month of tracking
# age: matrix of individual age per month for 220 tracked birds (rows) over 159 months (columns) - age is capped at 54 months for adult birds
# adult: binary matrix specifying whether an individual was inexperienced (younger than 18 months; 1) or 18 months and older (0) in a given month for 220 tracked birds (rows) over 159 months (columns)
# mig: matrix specifying whether an individual migrated (1) or not (0) in a given month for 220 tracked birds (rows) over 159 months (columns)
# lat: matrix specifying the mean latitude of an individual bird in a given month for 220 tracked birds (rows) over 159 months (columns)
# lat1: binary matrix of latitude specifying whether an individual was south of 25N (1) or north (0) in a given month for 220 tracked birds (rows) over 159 months (columns)
# pop: vector with one value per tracked individual specifying whether an individual originated from western Europe (Spain, France, Portugal; 1) or not (0)
# capt: vector with one value per tracked individual specifying whether the bird was captive-bred (1) or wild (0)
# tfail: vector with one value per tracked individual specifying the signal strength of the last 10 GPS positions acquired as an indication of tag functionality
# tag.age: matrix of the age of the transmitter for 220 tracked birds (rows) over 159 months (columns) - transmitters were sometimes re-used and birds were equipped with older transmitters
# nind: number of individual birds tracked with satellite telemetry (220)
# n.occasions: number of months over which individuals were tracked (159)
# years: vector indicating the year for each month of the tracking period to accommodate annual effects
# nyears: number of years over which 





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPECIFY JAGS MODEL WITH LATITUDE, MIGRATION AND CONTINUOUS AGE EFFECT
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
      logit(p.obs[i,t]) <- base.obs #### probability of observation GIVEN THAT TAG IS WORKING
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
# SPECIFY AND SET UP MODEL RUN
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
                                   beta2 = rnorm(1,0, 0.001),         
                                   beta3 = rnorm(1,0, 0.001))} 


#### TOP MODEL WITH SIMPLE 2 LATITUDINAL BAND

FINAL_MODEL <- jags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
                                     "EGVU_mig_lat_survival_model.jags",
                                     n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T, n.iter = ni)





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
  geom_line(aes(x=age, y=surv, color=Season),size=2)+     ## , linetype=Origin
  
  ## format axis ticks
  scale_x_continuous(name="Age in years", limits=c(1,54), breaks=seq(1,54,6), labels=seq(0,4,0.5)) +
  scale_y_continuous(name="Monthly survival probability", limits=c(0.8,1), breaks=seq(0.,1,0.05)) +
  scale_fill_viridis_d(begin = .4, direction = -1) +
  scale_color_viridis_d(begin = .4, direction = -1) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=16, color="black"),
        legend.position=c(0.8,0.2), 
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))


Fig2data<-MCMCpred %>% group_by(age,mig, lat, pop, capt) %>%
  summarise(med.surv=quantile(logit.surv,0.5),lcl.surv=quantile(logit.surv,0.025),ucl.surv=quantile(logit.surv,0.975)) %>%
  filter(!(mig==1 & lat==1)) %>%
  filter(!(age<19 & lat==1)) %>%
  
  ### BACKTRANSFORM TO NORMAL SCALE
  mutate(surv=plogis(med.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  
  ### ANNOTATE GROUPS
  mutate(Origin=ifelse(capt==0,"wild","captive")) %>%
  mutate(Population=ifelse(pop==1,"western Europe","central and east")) %>%
  mutate(Season=ifelse(lat==1,"south of 25°N",ifelse(mig==1,"migration","north of 25°N"))) %>%
  ungroup() %>%
  select(Population, Origin, Season, age, surv,lcl,ucl)
