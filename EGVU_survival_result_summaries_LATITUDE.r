##########################################################################
#
# EGYPTIAN VULTURE MONTHLY SURVIVAL ANALYSIS FROM TELEMETRY
#
##########################################################################
## plotting model estimates obtained from EGVU_telemetry_survival_analysis_FINAL.r
## revised on 6 December 2020 after 3 alternative models were run


library(jagsUI)
library(tidyverse)
library(data.table)
library(lubridate)
library(gtools)
filter<-dplyr::filter
select<-dplyr::select



try(setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study"), silent=T)
load("EGVU_survival_output_REV1_lat_explore.RData")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############ OUTPUT FROM FINAL SIMPLIFIED MODEL WITHOUT SEA CROSSING EFFECT  #############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### REVISED TO USE MODEL WITH RANDOM YEAR EFFECT DURING REVISION 1

### CALCULATE PREDICTED SURVIVAL BASED ON MODEL with binary additive parameters
## MIG STAGES ARE: 0=stationary, 1=migratory
## POPULATION CLASSES ARE: 1=western Europe, 0=elsewhere
## AGE: 0=adult, 1=juvenile
## CAPT: 0=wild, 1=captive
## LAT: 0=north, 1=Africa
## summarise annual survival by using 10*stationary, 1*spring mig and 1*fall 

### PLOT PARAMETERS ON LOGIT SCALE
out10<-as.data.frame(REV1_2lat_cat_age$summary)
out10$parameter<-row.names(REV1_2lat_cat_age$summary)
out10$model<-"2_lat"
fwrite(out10,"EGVU_2_lat_model_parameter_estimates.csv")

out10 %>% filter(grepl("b.phi",parameter)) %>%
  mutate(parameter=ifelse(parameter=="b.phi.mig","migration",parameter)) %>%
  mutate(parameter=ifelse(parameter=="b.phi.age","juvenile",parameter)) %>%
  mutate(parameter=ifelse(parameter=="b.phi.pop","western Europe",parameter)) %>%
  mutate(parameter=ifelse(parameter=="b.phi.capt","captive-reared",parameter)) %>%
  mutate(parameter=ifelse(parameter=="b.phi.lat1","Africa",parameter)) %>%
  ggplot()+
  geom_point(aes(x=parameter, y=mean))+
  geom_errorbar(aes(x=parameter, ymin=`2.5%`, ymax=`97.5%`), width=.1) +
  geom_hline(aes(yintercept=0), colour="darkgrey") +
  
  ## format axis ticks
  xlab("Parameter") +
  ylab("estimate (logit scale)") +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=18, color="black"),
        axis.text.x=element_text(size=12, color="black",angle=45, vjust = 1, hjust=1), 
        axis.title=element_text(size=18), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("EGVU_parameter_estimates_2lat.pdf", height=7, width=10)



### PREPARE RAW MCMC OUTPUT
parmcols<-dimnames(REV1_2lat_cat_age$samples[[1]])[[2]]

### COMBINE SAMPLES ACROSS CHAINS
MCMCout<-rbind(REV1_2lat_cat_age$samples[[1]],REV1_2lat_cat_age$samples[[2]],REV1_2lat_cat_age$samples[[3]])
str(MCMCout)



#### TABLE FOR PREDICTED ANNUAL SURVIVAL FOR ADULT AND JUVENILE FOR EACH POPULATION
### SET UP ANNUAL TABLE

AnnTab<-data.frame(pop=rep(c(0,1), each=24),
                   capt=0,
                   age=rep(c(rep(1,12),rep(0,12)),2),
                   mig=c(c(0,1,0,0,0,0,0,0,0,0,0,0), ## juveniles east
                         c(0,1,0,0,0,0,1,0,0,0,0,0), ## adults east
                         c(0,1,0,0,0,0,0,0,0,0,0,0),  ## juveniles west
                         c(0,1,0,0,0,0,0,1,0,0,0,0)),
                   lat=c(c(0,0,0,0,1,1,1,1,1,1,1,1), ## juveniles east
                         c(0,0,1,1,1,1,1,1,1,0,0,0), ## adults east
                         c(0,0,0,0,1,1,1,1,1,1,1,1),  ## juveniles west
                         c(0,0,1,1,1,1,1,1,1,0,0,0)))   ## adults  west
Xin<-AnnTab %>% mutate(capt=1) %>% bind_rows(AnnTab) %>% 
  mutate(Population=ifelse(pop==1,"western Europe","central and eastern"))

### CALCULATE PREDICTED VALUE FOR EACH SAMPLE
MCMCpred<-data.frame()
for(s in 1:nrow(MCMCout)) {
  
  X<-  Xin %>%
    
    ### CALCULATE MONTHLY SURVIVAL
    mutate(logit.surv=as.numeric(MCMCout[s,match("lp.mean",parmcols)])+
             as.numeric(MCMCout[s,match("b.phi.mig",parmcols)])*mig +
             as.numeric(MCMCout[s,match("b.phi.age",parmcols)])*age +
             as.numeric(MCMCout[s,match("b.phi.capt",parmcols)])*capt +
             as.numeric(MCMCout[s,match("b.phi.lat1",parmcols)])*lat +
             as.numeric(MCMCout[s,match("b.phi.pop",parmcols)])*pop) %>%
    ### BACKTRANSFORM TO NORMAL SCALE
    mutate(surv=plogis(logit.surv)) %>%
    
    ### CALCULATE ANNUAL SURVIVAL
    group_by(age,Population,capt) %>%
    summarise(ann.surv=prod(surv)) %>%
    mutate(simul=s)            
  
  
  MCMCpred<-rbind(MCMCpred,as.data.frame(X)) 
  
}


### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

TABLE2<-  MCMCpred %>% 

  ### ANNOTATE GROUPS
  mutate(Ageclass=ifelse(age==0,"adult","juvenile")) %>%
  mutate(Origin=ifelse(capt==0,"wild","captive")) %>%
  
  ### CALCULATE CREDIBLE INTERVALS
  group_by(Population,Ageclass,Origin) %>%
  summarise(med.surv=quantile(ann.surv,0.5),lcl.surv=quantile(ann.surv,0.025),ucl.surv=quantile(ann.surv,0.975)) %>%
  arrange(Population,Ageclass,Origin) %>%
  #filter(!(Ageclass=="adult" & Origin=="captive")) %>%  ## removed to show adult captive
  filter(!(Population=="western Europe" & Origin=="captive"))

TABLE2

fwrite(TABLE2,"EGVU_AnnSurv_2lat_model.csv")


##### FIGURE OF MONTHLY SURVIVAL WITH MIGRATORY STAGE
### SET UP ANNUAL TABLE

MigTab<-expand.grid(age=c(0,1),mig=c(0,1),pop=c(1,0), capt=c(0,1), lat=c(0,1)) %>%
  mutate(Population=ifelse(pop==1,"western Europe","central and eastern Med"))

### CALCULATE PREDICTED VALUE FOR EACH SAMPLE
MCMCpred<-data.frame()
for(s in 1:nrow(MCMCout)) {
  
  X<-  MigTab %>%
    
    ### CALCULATE MONTHLY SURVIVAL
    mutate(logit.surv=as.numeric(MCMCout[s,match("lp.mean",parmcols)])+
             as.numeric(MCMCout[s,match("b.phi.capt",parmcols)])*capt +
             as.numeric(MCMCout[s,match("b.phi.mig",parmcols)])*mig +
             as.numeric(MCMCout[s,match("b.phi.age",parmcols)])*age +
             as.numeric(MCMCout[s,match("b.phi.lat1",parmcols)])*lat +
             as.numeric(MCMCout[s,match("b.phi.pop",parmcols)])*pop)
  
  MCMCpred<-rbind(MCMCpred,X) 
}


### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

MCMCpred %>% group_by(age,mig, lat) %>%
  summarise(med.surv=quantile(logit.surv,0.5),lcl.surv=quantile(logit.surv,0.025),ucl.surv=quantile(logit.surv,0.975)) %>%
  
  ### BACKTRANSFORM TO NORMAL SCALE
  mutate(surv=plogis(med.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  
  ### ANNOTATE GROUPS
  mutate(Ageclass=ifelse(age==0,"adult","juvenile")) %>%
  mutate(Latitude=ifelse(lat==0,"> 20 N","< 20 N")) %>%
  mutate(stage=ifelse(mig==0,"stationary","migrating")) %>%
  ggplot()+
  geom_point(aes(x=Ageclass, y=surv,colour=Latitude, shape=stage), size=1.5, position=position_dodge(width=0.2)) +
  geom_errorbar(aes(x=Ageclass, ymin=lcl, ymax=ucl, color=Latitude, shape=stage), width=0.05, position=position_dodge(width=0.2))+
  #facet_wrap(~Ageclass, ncol=1) +
  
  ## format axis ticks
  #scale_x_continuous(name="", limits=c(0,5), breaks=c(1,2,3,4), labels=c("wild adult","wild juvenile","captive-reared \n juvenile")) +
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

ggsave("Monthly_Surv_mig_tradeoff.pdf", width=11,height=9)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############ FINAL SIMPLIFIED MODEL WITH CONTINUOUS AGE #############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## AGE: continuous in months from 1 to 54

### PLOT PARAMETERS ON LOGIT SCALE
out10<-as.data.frame(REV1_2lat_adult_cont_age$summary)
out10$parameter<-row.names(REV1_2lat_adult_cont_age$summary)
out10$model<-"cont_age"
fwrite(out10,"EGVU_cont_age_2lat_adult_parameter_estimates.csv")

out10 %>% filter(grepl("b.phi",parameter)) %>%
  mutate(parameter=ifelse(parameter=="b.phi.mig","migration",parameter)) %>%
  mutate(parameter=ifelse(parameter=="b.phi.age","age",parameter)) %>%
  mutate(parameter=ifelse(parameter=="b.phi.pop","western Europe",parameter)) %>%
  mutate(parameter=ifelse(parameter=="b.phi.capt","captive-reared",parameter)) %>%
  mutate(parameter=ifelse(parameter=="b.phi.lat1","Africa",parameter)) %>%
  ggplot()+
  geom_point(aes(x=parameter, y=mean))+
  geom_errorbar(aes(x=parameter, ymin=`2.5%`, ymax=`97.5%`), width=.1) +
  geom_hline(aes(yintercept=0), colour="darkgrey") +
  
  ## format axis ticks
  xlab("Parameter") +
  ylab("estimate (logit scale)") +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=18, color="black"),
        axis.text.x=element_text(size=12, color="black",angle=45, vjust = 1, hjust=1), 
        axis.title=element_text(size=18), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))
ggsave("Tradeoff_Cont_age_2lat_adult_parameter_estimates.pdf", width=11,height=9)


### PREPARE RAW MCMC OUTPUT
parmcols<-dimnames(REV1_2lat_adult_cont_age$samples[[1]])[[2]]

### COMBINE SAMPLES ACROSS CHAINS
MCMCout<-rbind(REV1_2lat_adult_cont_age$samples[[1]],REV1_2lat_adult_cont_age$samples[[2]],REV1_2lat_adult_cont_age$samples[[3]])
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


fwrite(TABLE2,"EGVU_AnnSurv_continuous_age_2lat_adult.csv")


##### FIGURE OF MONTHLY SURVIVAL WITH MIGRATORY STAGE
### SET UP ANNUAL TABLE

MigTab<-expand.grid(age=seq(1,54),mig=c(0,1),pop=c(0,1),lat=c(0,1), capt=c(0,1)) %>%
  mutate(Population=ifelse(pop==1,"western Europe","central and east")) %>%
  mutate(adult=ifelse(age<19,1,0))


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
  mutate(Season=ifelse(lat==1,"stationary (<20 N)",ifelse(mig==1,"migration","stationary (>20 N)"))) %>%
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


ggsave("Monthly_Surv_continuous_age_TRADEOFF.pdf", width=11,height=9)


