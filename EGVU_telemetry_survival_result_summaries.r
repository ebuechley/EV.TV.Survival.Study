##########################################################################
#
# EGYPTIAN AND TURKEY VULTURE MONTHLY SURVIVAL ANALYSIS FROM TELEMETRY
#
##########################################################################
# plotting model estimates obtained from EGVU_telemetry_survival_analysis.r

## revised on 26 Nov 2019 after 4 alternative models were run

## revised on 30 Nov 2019 to include categorical classification of migratory and stationary periods

library(jagsUI)
library(tidyverse)
library(data.table)
library(lubridate)
library(gtools)
filter<-dplyr::filter
select<-dplyr::select


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD MODEL RESULTS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


try(setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study"), silent=T)
load("EGVU_survival_output_v3.RData")

### COMBINE OUTPUT FROM ALL 5 MODELS
out<-bind_rows(out1,out2,out3,out4,out5,out6, out7, out8)

### CONVERT mean.phi to logit
head(out)
out.intercept<-out %>% filter(grepl("mean.phi",parameter)) %>%
  mutate(mean=logit(mean),`2.5%`=logit(`2.5%`),`97.5%`=logit(`97.5%`))
out<-out %>% filter(!grepl("mean.phi",parameter)) %>%
  bind_rows(out.intercept)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXTRACT DIC TO COMPARE MODELS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pd_dic <- function(x) {
  data.frame(n.parameters=x$pD, DIC=x$DIC)
}
DIC_tab<-bind_rows(pd_dic(EVsurv1),pd_dic(EVsurv2),pd_dic(EVsurv3),pd_dic(EVsurv4),pd_dic(EVsurv5),pd_dic(EVsurv6),pd_dic(EVsurv7),pd_dic(EVsurv8)) %>%
  mutate(model=c("m1","m2","m3","m4","m5","m6","m7","m8")) %>%
  arrange(DIC) %>%
  mutate(deltaDIC=DIC-DIC[1])
DIC_tab

ModSelTab<-out %>% dplyr::select(model, parameter,mean) %>%
  mutate(mean=round(mean,3)) %>%
  spread(key=parameter, value=mean, fill="not included") %>%
  left_join(DIC_tab, by="model")%>%
  arrange(DIC) 

fwrite(ModSelTab,"EGVU_surv_model_selection_table.csv")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT PARAMETER ESTIMATES FROM ALL 6 MODELS ON LOGIT SCALE 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


out %>% filter(grepl("phi",parameter)) %>%
  left_join(DIC_tab, by="model") %>%
  mutate(header= paste(model,"delta DIC:",as.integer(deltaDIC)," ")) %>%

ggplot()+
  geom_point(aes(x=parameter, y=mean))+
  geom_errorbar(aes(x=parameter, ymin=`2.5%`, ymax=`97.5%`), width=.1) +
  geom_hline(aes(yintercept=0), colour="darkgrey") +
  facet_wrap(~header, ncol=4) +
  
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


ggsave("EGVU_surv_parameter_estimates_allmodels.pdf", height=10, width=15)









#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE DATA FRAME WITH ALL POSSIBLE COMBINATIONS (causes memory allocation problems)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CHANGED On 2 DEC 2019 to exclude models with a continuous 'migration' covariate due to the confounding  problem of dead birds travelling less
## SELECT ONLY PARAMETERS RELEVANT FOR MODEL 8


### LINEAR PREDICTOR EQUATION
# logit(phi[i,t]) <- lp.mean[adult[i,t]+1] + b.phi.age*(age[i,t])*(adult[i,t])  +   ### age category-specific intercept and slope for non-adult bird to increase survival with age
#   b.phi.mig*(mig[i,t]) * (resid[i]) +                           ### survival dependent on mean daily movement distance averaged over month for migratory populations
#   b.phi.capt*(capt[i]) + b.phi.free*(free[i,t])*(capt[i])*(adult[i,t])   +     ### survival dependent on captive-release and time since the captive bird was released as long as captive-released bird is not an adult
#   b.phi.lat*(lat[i,t]) + b.phi.lat2 * pow((lat[i,t]),2) + b.phi.long*(long[i]) +  #### probability of monthly survival dependent on latitude and longitude
#   b.phi.resident* (resid[i])    


## CREATE DATAFRAME OF ALL POSSIBLE COVARIATE COMBINATIONS FOR PLOTTING
PLOTSUBAD<-expand.grid(#resid=c(0,1),
                     mig=c(1,2),    #seq(min(mig.mat, na.rm=T),max(mig.mat, na.rm=T),length=30),
                     #free=seq(min(free.mat, na.rm=T),max(free.mat, na.rm=T),length=10),
                     #lat=seq(min(lat.mat.st, na.rm=T),max(lat.mat.st, na.rm=T),length=30),
                     #long=seq(min(long.st, na.rm=T),max(long.st, na.rm=T),length=30),
                     capt=c(0,1),
                     age=seq(min(age.mat, na.rm=T),max(age.mat, na.rm=T),length=30)) %>%
  mutate(adult=1)

PLOTAD<-expand.grid(#resid=c(0,1),
                        mig=c(1,2),    #seq(min(mig.mat, na.rm=T),max(mig.mat, na.rm=T),length=20),
                       #free=seq(min(free.mat, na.rm=T),max(free.mat, na.rm=T),length=10),
                       #lat=seq(min(lat.mat.st, na.rm=T),max(lat.mat.st, na.rm=T),length=30),
                       #long=seq(min(long.st, na.rm=T),max(long.st, na.rm=T),length=30),
                       capt=c(0,1)) %>%
  mutate(age=54,adult=0)



### CALCULATE PREDICTED SURVIVAL BASED ON MODEL 8
out8<-out %>% filter(model=="m8")

PLOTDAT<-  bind_rows(PLOTSUBAD,PLOTAD) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(mig)) %>%
  mutate(logit.surv=ifelse(adult==0,out8$mean[out8$parameter=="mean.phi[1]"],out8$mean[out8$parameter=="mean.phi[2]"])+
           out8$mean[out8$parameter=="b.phi.age"]*age*adult+
           out8$mean[out8$parameter=="b.phi.capt"]*capt +
           out8$mean[out8$parameter=="b.phi.mig"]*(mig-1)) %>% #+
           #out8$mean[out8$parameter=="b.phi.resident"]*resid) %>%
  mutate(lcl.surv=ifelse(adult==0,out8[out8$parameter=="mean.phi[1]",3],out8[out8$parameter=="mean.phi[2]",3])+
           out8[out8$parameter=="b.phi.age",3]*age*adult+
           out8[out8$parameter=="b.phi.capt",3]*capt +
           out8[out8$parameter=="b.phi.mig",3]*(mig-1)) %>% # +
           #out8[out8$parameter=="b.phi.resident",3]*resid) %>%
  mutate(ucl.surv=ifelse(adult==0,out8[out8$parameter=="mean.phi[1]",7],out8[out8$parameter=="mean.phi[2]",7])+
           out8[out8$parameter=="b.phi.age",7]*age*adult+
           out8[out8$parameter=="b.phi.capt",7]*capt +
           out8[out8$parameter=="b.phi.mig",7]*(mig-1)) %>% #+
           #out8[out8$parameter=="b.phi.resident",7]*resid) %>%
  mutate(surv=plogis(logit.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  mutate(Origin=ifelse(capt==1,"captive bred","wild")) %>%
  mutate(Migratory=ifelse(mig==1,"stationary","migratory")) %>%
  arrange(Origin,Migratory,age)

head(PLOTDAT)


### CALCULATE PREDICTED SURVIVAL BASED ON MODEL 7
out7<-out %>% filter(model=="m7")

PLOTDAT<-  bind_rows(PLOTSUBAD,PLOTAD) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(mig)) %>%
  mutate(logit.surv=ifelse(adult==0 & mig==1,out7$mean[out7$parameter=="mean.phi[1,1]"],
                           ifelse(adult==0 & mig==2,out7$mean[out7$parameter=="mean.phi[1,2]"],
                                  ifelse(adult==1 & mig==1,out7$mean[out7$parameter=="mean.phi[2,1]"],out7$mean[out7$parameter=="mean.phi[2,2]"])))+
           out7$mean[out7$parameter=="b.phi.age"]*age*adult+
           out7$mean[out7$parameter=="b.phi.capt"]*capt) %>% # +
           #out7$mean[out7$parameter=="b.phi.mig"]*(mig-1)) %>% #+
  #out7$mean[out7$parameter=="b.phi.resident"]*resid) %>%
  mutate(lcl.surv=ifelse(adult==0 & mig==1,out7[out7$parameter=="mean.phi[1,1]",3],
                         ifelse(adult==0 & mig==2,out7[out7$parameter=="mean.phi[1,2]",3],
                                ifelse(adult==1 & mig==1,out7[out7$parameter=="mean.phi[2,1]",3],out7[out7$parameter=="mean.phi[2,2]",3])))+
           out7[out7$parameter=="b.phi.age",3]*age*adult+
           out7[out7$parameter=="b.phi.capt",3]*capt) %>% # +
           #out7[out7$parameter=="b.phi.mig",3]*(mig-1)) %>% # +
  #out7[out7$parameter=="b.phi.resident",3]*resid) %>%
  mutate(ucl.surv=ifelse(adult==0 & mig==1,out7[out7$parameter=="mean.phi[1,1]",7],
                           ifelse(adult==0 & mig==2,out7[out7$parameter=="mean.phi[1,2]",7],
                                  ifelse(adult==1 & mig==1,out7[out7$parameter=="mean.phi[2,1]",7],out7[out7$parameter=="mean.phi[2,2]",7])))+
           out7[out7$parameter=="b.phi.age",7]*age*adult+
           out7[out7$parameter=="b.phi.capt",7]*capt) %>% # +
           #out7[out7$parameter=="b.phi.mig",7]*(mig-1)) %>% #+
  #out7[out7$parameter=="b.phi.resident",7]*resid) %>%
  mutate(surv=plogis(logit.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  mutate(Origin=ifelse(capt==1,"captive bred","wild")) %>%
  mutate(Migratory=ifelse(mig==1,"stationary","migratory")) %>%
  arrange(Origin,Migratory,age)

head(PLOTDAT)




### CALCULATE PREDICTED SURVIVAL BASED ON MODEL 3


PLOTDAT<-  bind_rows(PLOTSUBAD,PLOTAD) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(mig)) %>%
  mutate(logit.surv=ifelse(adult==0,out3$mean[out3$parameter=="mean.phi[1]"],out3$mean[out3$parameter=="mean.phi[2]"])+
           out3$mean[out3$parameter=="b.phi.age"]*age*adult+
           out3$mean[out3$parameter=="b.phi.capt"]*capt +
           out3$mean[out3$parameter=="b.phi.mig"]*(mig-1)) %>% #+
  mutate(lcl.surv=ifelse(adult==0,out3[out3$parameter=="mean.phi[1]",3],out3[out3$parameter=="mean.phi[2]",3])+
           out3[out3$parameter=="b.phi.age",3]*age*adult+
           out3[out3$parameter=="b.phi.capt",3]*capt +
           out3[out3$parameter=="b.phi.mig",3]*(mig-1)) %>% # +
  mutate(ucl.surv=ifelse(adult==0,out3[out3$parameter=="mean.phi[1]",7],out3[out3$parameter=="mean.phi[2]",7])+
           out3[out3$parameter=="b.phi.age",7]*age*adult+
           out3[out3$parameter=="b.phi.capt",7]*capt +
           out3[out3$parameter=="b.phi.mig",7]*(mig-1)) %>% #+
  mutate(surv=plogis(logit.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  mutate(Origin=ifelse(capt==1,"captive bred","wild")) %>%
  mutate(Migratory=ifelse(mig==1,"stationary","migratory")) %>%
  arrange(Origin,Migratory,age)

head(PLOTDAT)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT MONTHLY SURVIVAL PROBABILITIES ON REAL SCALE ACROSS RANGE OF COVARIATES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## PLOT AGE INCREASE FOR SUBADULTS

  ggplot(PLOTDAT[PLOTDAT$adult==1,])+
  geom_ribbon(aes(x=age, ymin=lcl, ymax=ucl, fill=Origin), alpha=0.2) +
  geom_line(aes(x=age, y=surv, colour=Origin))+
  facet_wrap(~Migratory) +
    
  geom_hline(data=PLOTDAT[PLOTDAT$adult==0,],aes(yintercept=surv, colour=Origin))+  
  
  ## format axis ticks
  scale_x_continuous(name="Age in years", limits=c(1,54), breaks=seq(1,54,6), labels=seq(0,4,0.5)) +
  scale_y_continuous(name="Monthly survival probability", limits=c(0,1), breaks=seq(0,1,0.2), labels=seq(0,1,0.2)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("EGVU_subad_surv_by_age.pdf")





## PLOT ADULT SURVIVAL ACROSS DAILY MOVEMENT DISTANCES

ggplot(PLOTDAT[PLOTDAT$adult==0,])+
  #geom_ribbon(aes(x=mig, ymin=lcl, ymax=ucl, fill=Origin), alpha=0.2) +
  geom_hline(data=PLOTDAT[PLOTDAT$adult==0,],aes(yintercept=surv, colour=Origin))+
  facet_wrap(~Migratory) +
  
  ## format axis ticks
  scale_x_continuous(name="Mean daily movement per month (km)", limits=c(0,6), breaks=seq(0,6,1), labels=seq(0,600,100)) +
  scale_y_continuous(name="Monthly survival probability", limits=c(0,1), breaks=seq(0,1,0.2), labels=seq(0,1,0.2)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("EGVU_ad_surv_by_daily_move.pdf")





## PLOT ADULT SURVIVAL ACROSS LATITUDE


## CREATE DATAFRAME OF LATITUDE RANGE AND PLOT
expand.grid(lat=unique(as.numeric(lat.mat), na.rm=T), capt=c(0,1)) %>%
  filter(!is.na(lat)) %>%
  mutate(logit.surv=out$mean[out$parameter=="mean.phi[2]"]+ out$mean[out$parameter=="b.phi.lat"]*lat+ out$mean[out$parameter=="b.phi.lat2"]*(lat^2)+ out$mean[out$parameter=="b.phi.capt"]*capt) %>%
  mutate(lcl.surv=out[out$parameter=="mean.phi[2]",3]+ out[out$parameter=="b.phi.lat",3]*lat+out[out$parameter=="b.phi.lat2",3]*(lat^2)+ out[out$parameter=="b.phi.capt",3]*capt) %>%
  mutate(ucl.surv=out[out$parameter=="mean.phi[2]",7]+ out[out$parameter=="b.phi.lat",7]*lat+ out[out$parameter=="b.phi.lat2",7]*(lat^2)+ out[out$parameter=="b.phi.capt",7]*capt) %>%
  mutate(surv=plogis(logit.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  mutate(Origin=ifelse(capt==1,"captive bred","wild")) %>%
  arrange(lat) %>%
  
  
  ggplot()+
  geom_ribbon(aes(x=lat, ymin=lcl, ymax=ucl, fill=Origin), alpha=0.2) +
  geom_line(aes(x=lat, y=surv, colour=Origin))+
  
  ## format axis ticks
  #scale_x_continuous(name="Latitude", limits=c(-10,10), breaks=seq(-10,10,5), labels=seq(5,45,10)) +
  scale_y_continuous(name="Monthly survival probability", limits=c(0,1), breaks=seq(0,1,0.2), labels=seq(0,1,0.2)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("EGVU_surv_by_latitude.pdf")



