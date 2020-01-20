##########################################################################
#
# EGYPTIAN VULTURE MONTHLY SURVIVAL ANALYSIS FROM TELEMETRY
#
##########################################################################
# plotting model estimates obtained from EGVU_telemetry_survival_analysis_FINAL.r
# reduced from EGVU_telemetry_survival_result_summaries.r
## finalised on 10 Jan 2020 to include final model output

library(jagsUI)
library(tidyverse)
library(data.table)
library(lubridate)
library(gtools)
filter<-dplyr::filter
select<-dplyr::select



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD FINAL MODEL RESULTS 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


try(setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study"), silent=T)
load("EGVU_survival_output_FINAL.RData")  ### need to load whole workspace for input matrices to create plotting data range
out31<-fread("EGVU_telemetry_survival_estimates_FINAL.csv") ## this causes some weird list error in the PLOTDAT creation below
out31<-as.data.frame(EGVU_surv_mod$summary)
out31$parameter<-row.names(EGVU_surv_mod$summary)
out31$model<-"m31"

### PREPARE RAW MCMC OUTPUT
parmcols<-dimnames(EGVU_surv_mod$samples[[1]])[[2]]

### COMBINE SAMPLES ACROSS CHAINS
MCMCout<-rbind(EGVU_surv_mod$samples[[1]],EGVU_surv_mod$samples[[2]],EGVU_surv_mod$samples[[3]],EGVU_surv_mod$samples[[4]])
str(MCMCout)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIG S1 - PARAMETER ESTIMATES ON LOGIT SCALE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

out31 %>% mutate(parameter=ifelse(parameter=="lp.mean[1]","adult.intercept",parameter)) %>%
  mutate(parameter=ifelse(parameter=="lp.mean[2]","juv.intercept",parameter)) %>%
  filter(grepl("b.phi",parameter)| grepl("intercept",parameter)) %>%
  
  ggplot()+
  geom_point(aes(x=parameter, y=mean))+
  geom_errorbar(aes(x=parameter, ymin=`2.5%`, ymax=`97.5%`), width=.1) +
  geom_hline(aes(yintercept=0), colour="darkgrey") +
  
  ## format axis ticks
  xlab("Parameter") +
  ylab("Parameter estimate (logit scale)") +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=18, color="black"),
        axis.text.x=element_text(size=12, color="black",angle=45, vjust = 1, hjust=1), 
        axis.title=element_text(size=18), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))


ggsave("FigS1_parameter_estimates_logit.jpg", height=10, width=11)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIG. 2 - SURVIVAL WITH LATITUDE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### CALCULATE PREDICTED VALUE FOR EACH SAMPLE
MCMCpred<-data.frame()
for(s in 1:nrow(MCMCout)) {
  
  X<-  expand.grid(age=c(3,54), lat=seq(min(lat.mat, na.rm=T),max(lat.mat, na.rm=T),length=200)) %>%
    mutate(adult=ifelse(age==54,0,1)) %>%
    
    ### CALCULATE MONTHLY SURVIVAL
    mutate(logit.surv=ifelse(adult==1,as.numeric(MCMCout[s,match("lp.mean[2]",parmcols)]),as.numeric(MCMCout[s,match("lp.mean[1]",parmcols)]))+
             as.numeric(MCMCout[s,match("b.phi.age",parmcols)])*age*adult +
             as.numeric(MCMCout[s,match("b.phi.mig[1]",parmcols)]) +
             as.numeric(MCMCout[s,match("b.phi.lat",parmcols)])*lat) 
  
  MCMCpred<-rbind(MCMCpred,X) 
  
}


### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

PLOTDAT<-  MCMCpred %>% group_by(age,adult,lat) %>%
  summarise(med.surv=quantile(logit.surv,0.5),lcl.surv=quantile(logit.surv,0.025),ucl.surv=quantile(logit.surv,0.975)) %>%
  
  ### BACKTRANSFORM TO NORMAL SCALE
  mutate(surv=plogis(med.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  
  ### ANNOTATE GROUPS
  mutate(Ageclass=ifelse(age==54,"adult","juvenile"))

head(PLOTDAT)


## PLOT 

ggplot(PLOTDAT)+
  geom_ribbon(aes(x=lat, ymin=lcl, ymax=ucl, fill=Ageclass), alpha=0.2) +
  geom_line(aes(x=lat, y=surv,color=Ageclass))+
  
  ## format axis ticks
  scale_x_continuous(name="Latitude", limits=c(1.2,45), breaks=seq(5,45,5), labels=seq(5,45,5)) +
  scale_y_continuous(name="Monthly survival probability", limits=c(0.6,1), breaks=seq(0.6,1,0.1)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=16, color="black"),  
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("Fig2_Surv_by_Latitude.jpg", width=10,height=9)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIG. 3 - SURVIVAL WITH LONGITUDE OF ORIGIN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### CALCULATE PREDICTED VALUE FOR EACH SAMPLE
MCMCpred<-data.frame()
for(s in 1:nrow(MCMCout)) {
  
  X<-  expand.grid(age=c(3,54), long=seq(min(long.orig$long, na.rm=T),max(long.orig$long, na.rm=T),length=200)) %>%
    mutate(adult=ifelse(age==54,0,1)) %>%
    
    ### CALCULATE MONTHLY SURVIVAL
    mutate(logit.surv=ifelse(adult==1,as.numeric(MCMCout[s,match("lp.mean[2]",parmcols)]),as.numeric(MCMCout[s,match("lp.mean[1]",parmcols)]))+
             as.numeric(MCMCout[s,match("b.phi.age",parmcols)])*age*adult +
             as.numeric(MCMCout[s,match("b.phi.mig[1]",parmcols)]) +
             as.numeric(MCMCout[s,match("b.phi.long",parmcols)])*long) 
  
  MCMCpred<-rbind(MCMCpred,X) 
  
}


### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

PLOTDAT<-  MCMCpred %>% group_by(age,adult,long) %>%
  summarise(med.surv=quantile(logit.surv,0.5),lcl.surv=quantile(logit.surv,0.025),ucl.surv=quantile(logit.surv,0.975)) %>%
  
  ### BACKTRANSFORM TO NORMAL SCALE
  mutate(surv=plogis(med.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  
  ### ANNOTATE GROUPS
  mutate(Ageclass=ifelse(age==54,"adult","juvenile"))

head(PLOTDAT)


## PLOT 

ggplot(PLOTDAT)+
  geom_ribbon(aes(x=long, ymin=lcl, ymax=ucl, fill=Ageclass), alpha=0.2) +
  geom_line(aes(x=long, y=surv,color=Ageclass))+
  
  ## format axis ticks
  scale_x_continuous(name="Longitude of origin", limits=c(-8,45), breaks=seq(-5,45,5), labels=seq(-5,45,5)) +
  scale_y_continuous(name="Monthly survival probability", limits=c(0.6,1), breaks=seq(0.6,1,0.1)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=16, color="black"),  
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("Fig3_Surv_by_Longitude.jpg", width=10,height=9)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIG. 4 - SURVIVAL WITH AGE AND CAPTIVE ORIGIN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### CALCULATE PREDICTED VALUE FOR EACH SAMPLE
MCMCpred<-data.frame()
for(s in 1:nrow(MCMCout)) {
  
  X<-  expand.grid(age=seq(min(age.mat, na.rm=T),max(age.mat, na.rm=T),1), capt=c(0,1)) %>%

    ### CALCULATE MONTHLY SURVIVAL
    mutate(logit.surv=as.numeric(MCMCout[s,match("lp.mean[2]",parmcols)])+
             as.numeric(MCMCout[s,match("b.phi.age",parmcols)])*age +
             as.numeric(MCMCout[s,match("b.phi.mig[1]",parmcols)]) +
             as.numeric(MCMCout[s,match("b.phi.capt",parmcols)])*capt) 
  
  MCMCpred<-rbind(MCMCpred,X) 
  
}


### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

PLOTDAT<-  MCMCpred %>% group_by(age,capt) %>%
  summarise(med.surv=quantile(logit.surv,0.5),lcl.surv=quantile(logit.surv,0.025),ucl.surv=quantile(logit.surv,0.975)) %>%
  
  ### BACKTRANSFORM TO NORMAL SCALE
  mutate(surv=plogis(med.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  mutate(Origin=ifelse(capt==0,"wild","captive-reared"))

head(PLOTDAT)


## PLOT 

ggplot(PLOTDAT)+
  geom_ribbon(aes(x=age, ymin=lcl, ymax=ucl, fill=Origin), alpha=0.2) +
  geom_line(aes(x=age, y=surv,color=Origin))+
  
  ## format axis ticks
  scale_x_continuous(name="Age in years", limits=c(1,54), breaks=seq(1,54,6), labels=seq(0,4,0.5)) +
  scale_y_continuous(name="Monthly survival probability", limits=c(0.75,1), breaks=seq(0.75,1,0.05)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=16, color="black"),  
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("Fig4_Surv_by_AgeCapt.jpg", width=10,height=9)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIG. 5 - SURVIVAL WITH MIGRATORY STAGE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### CALCULATE PREDICTED VALUE FOR EACH SAMPLE
MCMCpred<-data.frame()
for(s in 1:nrow(MCMCout)) {
  
  X<-  data.frame(age=c(3,3,3,3,54,54), mig=c(1,2,1,2,1,2), capt=c(0,0,1,1,0,0)) %>%
    mutate(adult=ifelse(age==54,0,1)) %>%
    
    ### CALCULATE MONTHLY SURVIVAL
    mutate(logit.surv=ifelse(adult==1,as.numeric(MCMCout[s,match("lp.mean[2]",parmcols)]),as.numeric(MCMCout[s,match("lp.mean[1]",parmcols)]))+
             as.numeric(MCMCout[s,match("b.phi.age",parmcols)])*age*adult +
             ifelse(mig==1,as.numeric(MCMCout[s,match("b.phi.mig[1]",parmcols)]),as.numeric(MCMCout[s,match("b.phi.mig[2]",parmcols)]))+
             as.numeric(MCMCout[s,match("b.phi.capt",parmcols)])*capt) 
  
  MCMCpred<-rbind(MCMCpred,X) 
}


### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

PLOTDAT<-  MCMCpred %>% group_by(adult,capt,mig) %>%
  summarise(med.surv=quantile(logit.surv,0.5),lcl.surv=quantile(logit.surv,0.025),ucl.surv=quantile(logit.surv,0.975)) %>%
  
  ### BACKTRANSFORM TO NORMAL SCALE
  mutate(surv=plogis(med.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  mutate(Stage=ifelse(mig==1,"stationary","migratory")) %>%
  ungroup() %>%
  mutate(plotseq=c(0.7,0.9,1.9,2.1,2.9,3.1)) 

head(PLOTDAT)


## PLOT 

ggplot(PLOTDAT)+
  geom_point(aes(x=plotseq, y=surv,colour=Stage), alpha=0.2) +
  geom_errorbar(aes(x=plotseq, ymin=lcl, ymax=ucl, y=surv,color=Stage))+
  
  ## format axis ticks
  scale_x_continuous(name="", limits=c(0,4), breaks=c(0.8,2,3), labels=c("wild adult","wild juvenile","captive-reared \n juvenile")) +
  scale_y_continuous(name="Monthly survival probability", limits=c(0.85,1), breaks=seq(0.85,1,0.05)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=16, color="black"),  
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("Fig5_Surv_by_MigStage.jpg", width=10,height=9)












#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OUTPUT TABLE FOR PREDICTED ANNUAL SURVIVAL FOR ADULT AND JUVENILE FOR EAST AND WEST POPULATION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL
## summarise annual survival by using 10*stationary, 1*spring mig and 1*fall mig
## west population for spain lon=-5, lat=41.8, winter at lat = 15
## east population for bulgaria lon=25.9, lat=41.7, winter at lat = 11
## resident African population for ethiopia lon=40.8, lat = 11

west<-data.frame(age=c(1:12,rep(54,12)),
             mig=c(c(1,2,1,1,1,1,1,1,1,1,1,1),c(1,2,1,1,1,1,2,2,1,1,1,1)),
             lat=c(c(41.8,25,15,15,15,15,15,15,15,15,15,15),c(41.8,25,15,15,15,15,25,35,41.8,41.8,41.8,41.8)),
             long=rep(-5,24),
             pop="west")

east<-data.frame(age=c(1:12,rep(54,12)),
                 mig=c(c(1,2,1,1,1,1,1,1,1,1,1,1),c(1,2,1,1,1,1,2,2,1,1,1,1)),
                 lat=c(c(41.7,25,11,11,11,11,11,11,11,11,11,11),c(41.7,25,11,11,11,11,25,35,41.7,41.7,41.7,41.7)),
                 long=rep(25.9,24),
                 pop="east")

south<-data.frame(age=c(1:12,rep(54,12)),
                 mig=rep(1,24),
                 lat=rep(11,24),
                 long=rep(40.8,24),
                 pop="south")
  


### CALCULATE PREDICTED VALUE FOR EACH SAMPLE
MCMCpred<-data.frame()
for(s in 1:nrow(MCMCout)) {
  
  X<-  rbind(west,east,south) %>%
    mutate(adult=ifelse(age==54,0,1)) %>%
    
    ### CALCULATE MONTHLY SURVIVAL
    mutate(logit.surv=ifelse(adult==1,as.numeric(MCMCout[s,match("lp.mean[2]",parmcols)]),as.numeric(MCMCout[s,match("lp.mean[1]",parmcols)]))+
             as.numeric(MCMCout[s,match("b.phi.age",parmcols)])*age*adult +
             ifelse(mig==1,as.numeric(MCMCout[s,match("b.phi.mig[1]",parmcols)]),as.numeric(MCMCout[s,match("b.phi.mig[2]",parmcols)]))+
             as.numeric(MCMCout[s,match("b.phi.long",parmcols)])*long +
             as.numeric(MCMCout[s,match("b.phi.lat",parmcols)])*lat) %>%
    
    ### BACKTRANSFORM TO NORMAL SCALE
    mutate(surv=plogis(logit.surv)) %>%
    
    ### CALCULATE ANNUAL SURVIVAL
    group_by(adult,pop) %>%
    summarise(ann.surv=prod(surv))

  MCMCpred<-bind_rows(MCMCpred,X)
}








TABLE2 <- MCMCpred %>% group_by(adult,pop) %>%
  summarise(med.surv=quantile(ann.surv,0.5),lcl.surv=quantile(ann.surv,0.025),ucl.surv=quantile(ann.surv,0.975)) %>%
  
  ### ANNOTATE GROUPS
  mutate(Population=ifelse(pop=="west","Europe (West)",
                           ifelse(pop=="east","Europe (East)","Southern"))) %>%
  mutate(Age=ifelse(adult==0,"adult","juvenile"))


fwrite(TABLE2,"EGVU_ann_survival_estimates.csv")






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIGURE S2 - PLOT DESIRED BY RON
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## PLOT 
TABLE2 %>% ungroup() %>%
  arrange(Population,Age) %>%
  mutate(plotseq=c(0.9,1.1,1.9,2.1,2.9,3.1)) %>%

ggplot()+
  geom_point(aes(x=plotseq, y=med.surv,colour=Age), alpha=0.2) +
  geom_errorbar(aes(x=plotseq, ymin=lcl.surv, ymax=ucl.surv, y=med.surv,color=Age))+
  
  ## format axis ticks
  scale_x_continuous(name="", limits=c(0,4), breaks=c(1,2,3), labels=c("Europe (West)","Europe (East)","Resident \n Africa")) +
  scale_y_continuous(name="Annual survival probability", limits=c(0,1), breaks=seq(0,1,0.2)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=16, color="black"),  
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("Fig6_AnnualSurvival.jpg", width=10,height=9)







