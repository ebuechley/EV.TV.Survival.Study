##########################################################################
#
# EGYPTIAN VULTURE MONTHLY SURVIVAL ANALYSIS FROM TELEMETRY
#
##########################################################################
## plotting model estimates obtained from EGVU_telemetry_survival_analysis_FINAL.r
## revised on 6 April 2020 after 3 alternative models were run
## revised on 9 April 2020 to adjust for lack of age effect

## updated on 10 April to include several models with 2-5 migration stages

library(jagsUI)
library(tidyverse)
library(data.table)
library(lubridate)
library(gtools)
filter<-dplyr::filter
select<-dplyr::select




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD ALL 3 MODEL RESULTS 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

try(setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study"), silent=T)
load("EGVU_survival_output_simplage.RData")  ### need to load whole workspace for input matrices to create plotting data range

### LINEAR PREDICTOR EQUATION
# logit(phi[i,t]) <- lp.mean[adult[i,t]+1] + b.phi.age*(age[i,t])*(adult[i,t])  +   ### age category-specific intercept and slope for non-adult bird to increase survival with age
#   b.phi.mig[mig[i,t]] +       ### survival dependent on migratory stage of the month (stationary or migratory)
#   b.phi.capt*(capt[i]) +      ### survival dependent on captive-release (wild or captive-raised)
#   b.phi.resident*(resid[i])


out1<-as.data.frame(EGVU_surv_mod_5stage$summary)
out1$parameter<-row.names(EGVU_surv_mod_5stage$summary)
out1$model<-"5_mig_stages_geog"

out2<-as.data.frame(EGVU_surv_mod_4stage$summary)
out2$parameter<-row.names(EGVU_surv_mod_4stage$summary)
out2$model<-"4_mig_stages_geog"

out3<-as.data.frame(EGVU_surv_mod_2stage_addpop$summary)
out3$parameter<-row.names(EGVU_surv_mod_2stage_addpop$summary)
out3$model<-"2_mig_stage_addpop"

out4<-as.data.frame(EGVU_surv_mod_2stage$summary)
out4$parameter<-row.names(EGVU_surv_mod_2stage$summary)
out4$model<-"2_mig_stage"

out5<-as.data.frame(EGVU_surv_mod_4stage_fallmig$summary)
out5$parameter<-row.names(EGVU_surv_mod_4stage_fallmig$summary)
out5$model<-"4_mig_stages_season"


### COMBINE OUTPUT FROM ALL 3 MODELS
out<-bind_rows(out1,out2,out3,out4,out5)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXTRACT DIC TO COMPARE MODELS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pd_dic <- function(x) {
  data.frame(n.parameters=x$pD, DIC=x$DIC)
}
DIC_tab<-bind_rows(pd_dic(EGVU_surv_mod_5stage),pd_dic(EGVU_surv_mod_4stage),pd_dic(EGVU_surv_mod_2stage_addpop),pd_dic(EGVU_surv_mod_2stage),pd_dic(EGVU_surv_mod_4stage_fallmig)) %>%
  mutate(model=c("5_mig_stages_geog","4_mig_stages_geog","2_mig_stage_addpop","2_mig_stage","4_mig_stages_season")) %>%
  arrange(DIC) %>%
  mutate(deltaDIC=DIC-DIC[1])
DIC_tab

ModSelTab<-out %>% dplyr::select(model, parameter,mean) %>%
  filter(grepl("b.phi",parameter)) %>%
  mutate(mean=round(mean,3)) %>%
  spread(key=parameter, value=mean, fill="not included") %>%
  left_join(DIC_tab, by="model")%>%
  arrange(DIC) 

fwrite(ModSelTab,"EGVU_surv_model_selection_simplage.csv")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT SURVIVAL PARAMETER ESTIMATES FROM ALL 3 MODELS ON LOGIT SCALE 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

out %>% filter(grepl("b.phi",parameter)) %>%
  left_join(DIC_tab, by="model") %>%
  mutate(header= paste(model,"delta DIC:",as.integer(deltaDIC)," ")) %>%
  
  ggplot()+
  geom_point(aes(x=parameter, y=mean))+
  geom_errorbar(aes(x=parameter, ymin=`2.5%`, ymax=`97.5%`), width=.1) +
  geom_hline(aes(yintercept=0), colour="darkgrey") +
  facet_wrap(~header, ncol=2) +
  
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

ggsave("EGVU_surv_parameter_estimates_simplage.pdf", height=16, width=10)








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############ MODEL 1: 4 MIGRATORY STAGES, RESIDENTS ASSIGNED THE SAME VALUE AS MIGRATORY WINTER STATIONARY    #############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### CALCULATE PREDICTED SURVIVAL BASED ON MODEL with 4 migratory stages
## MIG STAGES ARE: 1=breeding, 2=migration EAST, 3=wintering, 4=migration WEST
## summarise annual survival by using 10*stationary, 1*spring mig and 1*fall mig FOR WEST populations, 2* fall mig for EAST populations

### PREPARE RAW MCMC OUTPUT
parmcols<-dimnames(EGVU_surv_mod_4stage$samples[[1]])[[2]]

### COMBINE SAMPLES ACROSS CHAINS
MCMCout<-rbind(EGVU_surv_mod_4stage$samples[[1]],EGVU_surv_mod_4stage$samples[[2]],EGVU_surv_mod_4stage$samples[[3]],EGVU_surv_mod_4stage$samples[[4]])
str(MCMCout)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OUTPUT TABLE FOR PREDICTED ANNUAL SURVIVAL FOR ADULT AND JUVENILE FOR EACH POPULATION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### SET UP ANNUAL TABLE
AnnTab<-data.frame(pop=rep(c("western Europe","Italy/Balkans","Caucasus/Middle East"), each=24),
             capt=0,
             age=rep(c(rep(1,12),rep(2,12)),3),
             mig=c(c(1,3,1,1,1,1,1,1,1,1,1,1), ## juveniles west
                       c(1,3,1,1,1,1,3,1,1,1,1,1), ## adults west
                       c(1,4,4,1,1,1,1,1,1,1,1,1),  ## juveniles east
                       c(1,4,1,1,1,1,1,4,1,1,1,1),   ## adults east
                      c(1,2,2,1,1,1,1,1,1,1,1,1),  ## juveniles caucasus
                   c(1,2,1,1,1,1,1,2,1,1,1,1)))  ## adults caucasus
Xin<-AnnTab %>% mutate(capt=1) %>% bind_rows(AnnTab) %>% filter(!(age==2 & capt==1))


### CALCULATE PREDICTED VALUE FOR EACH SAMPLE
MCMCpred<-data.frame()
for(s in 1:nrow(MCMCout)) {
  
  X<-  Xin %>%
    
    ### CALCULATE MONTHLY SURVIVAL
    mutate(logit.surv=ifelse(age==1,as.numeric(MCMCout[s,match("lp.mean[2]",parmcols)]),as.numeric(MCMCout[s,match("lp.mean[1]",parmcols)]))+
             as.numeric(MCMCout[s,match("b.phi.capt",parmcols)])*capt +
             ifelse(mig==1,as.numeric(MCMCout[s,match("b.phi.mig[1]",parmcols)]),
                    ifelse(mig==2,as.numeric(MCMCout[s,match("b.phi.mig[2]",parmcols)]),
                           ifelse(mig==3,as.numeric(MCMCout[s,match("b.phi.mig[3]",parmcols)]),
                                  ifelse(mig==4,as.numeric(MCMCout[s,match("b.phi.mig[4]",parmcols)]),as.numeric(MCMCout[s,match("b.phi.mig[5]",parmcols)])))))) %>%
    
    ### BACKTRANSFORM TO NORMAL SCALE
    mutate(surv=plogis(logit.surv)) %>%
    
    ### CALCULATE ANNUAL SURVIVAL
    group_by(age,pop, capt) %>%
    summarise(ann.surv=prod(surv)) %>%
    mutate(simul=s)            
                        
  
  MCMCpred<-rbind(MCMCpred,as.data.frame(X)) 
  
}


### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

TABLE2<-  MCMCpred %>% 
  
  ### ANNOTATE GROUPS
  mutate(Ageclass=ifelse(age==2,"adult","juvenile")) %>%
  mutate(Origin=ifelse(capt==0,"wild","captive")) %>%
  
  ### CALCULATE CREDIBLE INTERVALS
  group_by(pop,Ageclass,Origin) %>%
  summarise(med.surv=quantile(ann.surv,0.5),lcl.surv=quantile(ann.surv,0.025),ucl.surv=quantile(ann.surv,0.975)) %>%
  arrange(pop,Ageclass,Origin)

TABLE2


fwrite(TABLE2,"EGVU_AnnSurv_simpleage_4stage.csv")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SURVIVAL WITH MIGRATORY STAGE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### SET UP ANNUAL TABLE

MigTab<-data.frame(age=rep(c(1,2),4), ### adult=1, juvenile=2
                   mig=rep(1:4,each=2))


### CALCULATE PREDICTED VALUE FOR EACH SAMPLE
MCMCpred<-data.frame()
for(s in 1:nrow(MCMCout)) {
  
  X<-  MigTab %>%
    
    ### CALCULATE MONTHLY SURVIVAL
    mutate(logit.surv=ifelse(age==2,as.numeric(MCMCout[s,match("lp.mean[2]",parmcols)]),as.numeric(MCMCout[s,match("lp.mean[1]",parmcols)]))+
             ifelse(mig==1,as.numeric(MCMCout[s,match("b.phi.mig[1]",parmcols)]),
                    ifelse(mig==2,as.numeric(MCMCout[s,match("b.phi.mig[2]",parmcols)]),
                           ifelse(mig==3,as.numeric(MCMCout[s,match("b.phi.mig[3]",parmcols)]),
                                  ifelse(mig==4,as.numeric(MCMCout[s,match("b.phi.mig[4]",parmcols)]),as.numeric(MCMCout[s,match("b.phi.mig[5]",parmcols)]))))))
  
  
  MCMCpred<-rbind(MCMCpred,X) 
}


### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

PLOTDAT<-  MCMCpred %>% filter(mig<5) %>% group_by(age,mig) %>%
  summarise(med.surv=quantile(logit.surv,0.5),lcl.surv=quantile(logit.surv,0.025),ucl.surv=quantile(logit.surv,0.975)) %>%
  
  ### BACKTRANSFORM TO NORMAL SCALE
  mutate(surv=plogis(med.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  mutate(Stage= ifelse(mig==1,"stationary",
                       ifelse(mig==2,"migration (Caucasus)",
                              ifelse(mig==3,"migration (west)",
                                     ifelse(mig==4,"migration (Balkans/Italy)","resident"))))) %>%
  ### ANNOTATE GROUPS
  mutate(Ageclass=ifelse(age==1,"adult","juvenile"))

head(PLOTDAT)


## PLOT 

ggplot(PLOTDAT)+
  geom_point(aes(x=Stage, y=surv,colour=Ageclass), alpha=0.2, position=position_dodge(width=0.1)) +
  geom_errorbar(aes(x=Stage, ymin=lcl, ymax=ucl, color=Ageclass), width=0.05, position=position_dodge(width=0.1))+
  
  ## format axis ticks
  #scale_x_continuous(name="", limits=c(0,5), breaks=c(1,2,3,4), labels=c("wild adult","wild juvenile","captive-reared \n juvenile")) +
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

ggsave("Monthly_Surv_by_4MigStage_simplage.jpg", width=11,height=9)
#fwrite(PLOTDAT,"Fig5_data.csv")








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############ MODEL 2: 4 MIGRATORY STAGES AND ADDITIVE EFFECT OF RESIDENTS VS. MIGRATORY POPULATION            #############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### CALCULATE PREDICTED SURVIVAL BASED ON MODEL with 4 migratory stages and a residency parameter
## MIG STAGES ARE: 1=breeding, 2=migration EAST, 3=wintering, 4=migration WEST
## summarise annual survival by using 10*stationary, 1*spring mig and 1*fall mig FOR WEST populations, 2* fall mig for EAST populations

### PREPARE RAW MCMC OUTPUT
parmcols<-dimnames(EGVU_surv_resident$samples[[1]])[[2]]

### COMBINE SAMPLES ACROSS CHAINS
MCMCout<-rbind(EGVU_surv_resident$samples[[1]],EGVU_surv_resident$samples[[2]],EGVU_surv_resident$samples[[3]],EGVU_surv_resident$samples[[4]])
str(MCMCout)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OUTPUT TABLE FOR PREDICTED ANNUAL SURVIVAL FOR ADULT AND JUVENILE FOR EACH POPULATION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### SET UP ANNUAL TABLE

AnnTab<-data.frame(pop=rep(c("west","east","resident"), each=24),
                   capt=0,
                   age=rep(c(1:12,rep(54,12)),3),
                   mig=c(c(1,4,3,3,3,3,3,3,3,3,3,3), ## juveniles west
                         c(1,4,3,3,3,3,4,1,1,1,1,1), ## adults west
                         c(1,2,2,3,3,3,3,3,3,3,3,3),  ## juveniles east
                         c(1,2,3,3,3,3,2,1,1,1,1,1),   ## adults east
                         c(3,3,3,3,3,3,3,3,3,3,3,3),   ## juveniles resident
                         c(3,3,3,3,3,3,3,3,3,3,3,3))) %>%  ## adults resident
  mutate(adult=ifelse(age<54,1,0)) %>%  ### adult=0, juvenile=1)
  mutate(resid=ifelse(pop=="resident",0,1))
Xin<-AnnTab %>% mutate(capt=1) %>% bind_rows(AnnTab) %>% filter(!(adult==0 & capt==1))


### CALCULATE PREDICTED VALUE FOR EACH SAMPLE
MCMCpred<-data.frame()
for(s in 1:nrow(MCMCout)) {
  
  X<-  Xin %>%
    
    ### CALCULATE MONTHLY SURVIVAL
    mutate(logit.surv=ifelse(adult==1,as.numeric(MCMCout[s,match("lp.mean[2]",parmcols)]),as.numeric(MCMCout[s,match("lp.mean[1]",parmcols)]))+
             as.numeric(MCMCout[s,match("b.phi.age",parmcols)])*age*adult +
             as.numeric(MCMCout[s,match("b.phi.capt",parmcols)])*capt +
             as.numeric(MCMCout[s,match("b.phi.resident",parmcols)])*resid +
             ifelse(mig==1,as.numeric(MCMCout[s,match("b.phi.mig[1]",parmcols)]),
                    ifelse(mig==2,as.numeric(MCMCout[s,match("b.phi.mig[2]",parmcols)]),
                           ifelse(mig==3,as.numeric(MCMCout[s,match("b.phi.mig[3]",parmcols)]),
                                  ifelse(mig==4,as.numeric(MCMCout[s,match("b.phi.mig[4]",parmcols)]),as.numeric(MCMCout[s,match("b.phi.mig[5]",parmcols)])))))) %>%
    
    ### BACKTRANSFORM TO NORMAL SCALE
    mutate(surv=plogis(logit.surv)) %>%
    
    ### CALCULATE ANNUAL SURVIVAL
    group_by(adult,pop, capt) %>%
    summarise(ann.surv=prod(surv)) %>%
    mutate(simul=s)            
  
  
  MCMCpred<-rbind(MCMCpred,as.data.frame(X)) 
  
}


### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

TABLE2<-  MCMCpred %>% 
  
  ### ANNOTATE GROUPS
  mutate(Ageclass=ifelse(adult==0,"adult","juvenile")) %>%
  mutate(Origin=ifelse(capt==0,"wild","captive")) %>%
  
  ### CALCULATE CREDIBLE INTERVALS
  group_by(pop,Ageclass,Origin) %>%
  summarise(med.surv=quantile(ann.surv,0.5),lcl.surv=quantile(ann.surv,0.025),ucl.surv=quantile(ann.surv,0.975)) %>%
  arrange(pop,Ageclass,Origin)

TABLE2


fwrite(TABLE2,"EGVU_AnnSurv_nolatlong_resident_model.csv")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SURVIVAL WITH MIGRATORY STAGE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### SET UP ANNUAL TABLE

MigTab<-data.frame(age=rep(c(3,54),5),
                   mig=c(rep(1:4,each=2),3,3),
                   resid=c(0,0,0,0,0,0,0,0,1,1)) %>%
  mutate(adult=ifelse(age<54,1,0))  ### adult=0, juvenile=1


### CALCULATE PREDICTED VALUE FOR EACH SAMPLE
MCMCpred<-data.frame()
for(s in 1:nrow(MCMCout)) {
  
  X<-  MigTab %>%
    
    ### CALCULATE MONTHLY SURVIVAL
    mutate(logit.surv=ifelse(adult==1,as.numeric(MCMCout[s,match("lp.mean[2]",parmcols)]),as.numeric(MCMCout[s,match("lp.mean[1]",parmcols)]))+
             as.numeric(MCMCout[s,match("b.phi.age",parmcols)])*age*adult +
             ifelse(mig==1,as.numeric(MCMCout[s,match("b.phi.mig[1]",parmcols)]),
                    ifelse(mig==2,as.numeric(MCMCout[s,match("b.phi.mig[2]",parmcols)]),
                           ifelse(mig==3,as.numeric(MCMCout[s,match("b.phi.mig[3]",parmcols)]),
                                  ifelse(mig==4,as.numeric(MCMCout[s,match("b.phi.mig[4]",parmcols)]),as.numeric(MCMCout[s,match("b.phi.mig[5]",parmcols)]))))))
  
  
  MCMCpred<-rbind(MCMCpred,X) 
}


### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

PLOTDAT<-  MCMCpred %>% filter(mig<5) %>% group_by(adult,mig, resid) %>%
  summarise(med.surv=quantile(logit.surv,0.5),lcl.surv=quantile(logit.surv,0.025),ucl.surv=quantile(logit.surv,0.975)) %>%
  
  ### BACKTRANSFORM TO NORMAL SCALE
  mutate(surv=plogis(med.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  mutate(Stage= ifelse(resid==1,"resident (Africa)",
                       ifelse(mig==1,"stationary breeding",
                            ifelse(mig==2,"migration (east)",
                              ifelse(mig==3,"stationary non-breeding",
                                     ifelse(mig==4,"migration (west)","resident")))))) %>%
  ### ANNOTATE GROUPS
  mutate(Ageclass=ifelse(adult==0,"adult","juvenile")) %>%
  arrange(Stage,Ageclass)

head(PLOTDAT)


## PLOT 

ggplot(PLOTDAT)+
  geom_point(aes(x=Stage, y=surv,colour=Ageclass), alpha=0.2, position=position_dodge(width=0.1)) +
  geom_errorbar(aes(x=Stage, ymin=lcl, ymax=ucl, color=Ageclass), width=0.05, position=position_dodge(width=0.1))+
  
  ## format axis ticks
  #scale_x_continuous(name="", limits=c(0,5), breaks=c(1,2,3,4), labels=c("wild adult","wild juvenile","captive-reared \n juvenile")) +
  scale_y_continuous(name="Monthly survival probability", limits=c(0.50,1), breaks=seq(0.50,1,0.05)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=16, color="black"),  
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("Monthly_Surv_by_4MigStage_resident.jpg", width=11,height=9)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############ MODEL 3: 5 MIGRATORY STAGES WITH SEPARATE EFFECT FOR RESIDENTS VS. MIGRATORY WINTERING           #############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL
## MIG STAGES ARE: 1=breeding, 2=migration EAST, 3=wintering, 4=migration WEST, 5= resident
## summarise annual survival by using 10*stationary, 1*spring mig and 1*fall mig FOR WEST populations, 2* fall mig for EAST populations

### PREPARE RAW MCMC OUTPUT
parmcols<-dimnames(EGVU_surv_mod_5stage$samples[[1]])[[2]]

### COMBINE SAMPLES ACROSS CHAINS
MCMCout<-rbind(EGVU_surv_mod_5stage$samples[[1]],EGVU_surv_mod_5stage$samples[[2]],EGVU_surv_mod_5stage$samples[[3]],EGVU_surv_mod_5stage$samples[[4]])
str(MCMCout)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OUTPUT TABLE FOR PREDICTED ANNUAL SURVIVAL FOR ADULT AND JUVENILE FOR EACH POPULATION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### SET UP ANNUAL TABLE
AnnTab<-data.frame(pop=rep(c("western Europe","Italy/Balkans","Caucasus/Middle East"), each=24),
                   capt=0,
                   age=rep(c(rep(1,12),rep(2,12)),3),
                   mig=c(c(1,3,2,2,2,2,2,2,2,2,2,2), ## juveniles west
                         c(1,3,2,2,2,2,3,1,1,1,1,1), ## adults west
                         c(1,4,4,2,2,2,2,2,2,2,2,2),  ## juveniles east
                         c(1,4,2,2,2,2,2,4,1,1,1,1),   ## adults east
                         c(1,5,5,2,2,2,2,2,2,2,2,2),  ## juveniles caucasus
                         c(1,5,2,2,2,2,2,5,1,1,1,1)))  ## adults caucasus
Xin<-AnnTab %>% mutate(capt=1) %>% bind_rows(AnnTab) %>% filter(!(age==1 & capt==1))


### CALCULATE PREDICTED VALUE FOR EACH SAMPLE
MCMCpred<-data.frame()
for(s in 1:nrow(MCMCout)) {
  
  X<-  Xin %>%
    
    ### CALCULATE MONTHLY SURVIVAL
    mutate(logit.surv=ifelse(age==2,as.numeric(MCMCout[s,match("lp.mean[2]",parmcols)]),as.numeric(MCMCout[s,match("lp.mean[1]",parmcols)]))+
             as.numeric(MCMCout[s,match("b.phi.capt",parmcols)])*capt +
             ifelse(mig==1,as.numeric(MCMCout[s,match("b.phi.mig[1]",parmcols)]),
                    ifelse(mig==2,as.numeric(MCMCout[s,match("b.phi.mig[2]",parmcols)]),
                           ifelse(mig==3,as.numeric(MCMCout[s,match("b.phi.mig[3]",parmcols)]),
                                  ifelse(mig==4,as.numeric(MCMCout[s,match("b.phi.mig[4]",parmcols)]),as.numeric(MCMCout[s,match("b.phi.mig[5]",parmcols)])))))) %>%
    
    ### BACKTRANSFORM TO NORMAL SCALE
    mutate(surv=plogis(logit.surv)) %>%
    
    ### CALCULATE ANNUAL SURVIVAL
    group_by(age,pop, capt) %>%
    summarise(ann.surv=prod(surv)) %>%
    mutate(simul=s)            
  
  
  MCMCpred<-rbind(MCMCpred,as.data.frame(X)) 
  
}


### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

TABLE2<-  MCMCpred %>% 
  
  ### ANNOTATE GROUPS
  mutate(Ageclass=ifelse(age==1,"adult","juvenile")) %>%
  mutate(Origin=ifelse(capt==0,"wild","captive")) %>%
  
  ### CALCULATE CREDIBLE INTERVALS
  group_by(pop,Ageclass,Origin) %>%
  summarise(med.surv=quantile(ann.surv,0.5),lcl.surv=quantile(ann.surv,0.025),ucl.surv=quantile(ann.surv,0.975)) %>%
  arrange(pop,Ageclass,Origin)

TABLE2


fwrite(TABLE2,"EGVU_AnnSurv_simplage_5stage_model.csv")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SURVIVAL WITH MIGRATORY STAGE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### SET UP ANNUAL TABLE

MigTab<-data.frame(age=rep(c(1,2),5), ### adult=1, juvenile=2
                   mig=rep(1:5,each=2))


### CALCULATE PREDICTED VALUE FOR EACH SAMPLE
MCMCpred<-data.frame()
for(s in 1:nrow(MCMCout)) {
  
  X<-  MigTab %>%
    
    ### CALCULATE MONTHLY SURVIVAL
    mutate(logit.surv=ifelse(age==2,as.numeric(MCMCout[s,match("lp.mean[2]",parmcols)]),as.numeric(MCMCout[s,match("lp.mean[1]",parmcols)]))+
             ifelse(mig==1,as.numeric(MCMCout[s,match("b.phi.mig[1]",parmcols)]),
                    ifelse(mig==2,as.numeric(MCMCout[s,match("b.phi.mig[2]",parmcols)]),
                           ifelse(mig==3,as.numeric(MCMCout[s,match("b.phi.mig[3]",parmcols)]),
                                  ifelse(mig==4,as.numeric(MCMCout[s,match("b.phi.mig[4]",parmcols)]),as.numeric(MCMCout[s,match("b.phi.mig[5]",parmcols)]))))))
  
  
  MCMCpred<-rbind(MCMCpred,X) 
}


### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

PLOTDAT<-  MCMCpred %>% group_by(age,mig) %>%
  summarise(med.surv=quantile(logit.surv,0.5),lcl.surv=quantile(logit.surv,0.025),ucl.surv=quantile(logit.surv,0.975)) %>%
  
  ### BACKTRANSFORM TO NORMAL SCALE
  mutate(surv=plogis(med.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  mutate(Stage= ifelse(mig==1,"stationary breeding",
                              ifelse(mig==4,"migration (east)",
                                     ifelse(mig==2,"stationary non-breeding",
                                            ifelse(mig==3,"migration (west)","migration (Caucasus)"))))) %>%
  ### ANNOTATE GROUPS
  mutate(Ageclass=ifelse(age==1,"adult","juvenile"))

head(PLOTDAT)


## PLOT 

ggplot(PLOTDAT)+
  geom_point(aes(x=Stage, y=surv,colour=Ageclass), alpha=0.2, position=position_dodge(width=0.1)) +
  geom_errorbar(aes(x=Stage, ymin=lcl, ymax=ucl, color=Ageclass), width=0.05, position=position_dodge(width=0.1))+
  
  ## format axis ticks
  #scale_x_continuous(name="", limits=c(0,5), breaks=c(1,2,3,4), labels=c("wild adult","wild juvenile","captive-reared \n juvenile")) +
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

ggsave("Monthly_Surv_by_5MigStage_simplage.jpg", width=11,height=9)


















#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIGURE - SURVIVAL WITH AGE AND CAPTIVE ORIGIN
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
fwrite(PLOTDAT,"Fig4_data.csv")

