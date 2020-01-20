##########################################################################
#
# EGYPTIAN AND TURKEY VULTURE MONTHLY SURVIVAL ANALYSIS FROM TELEMETRY
#
##########################################################################
# plotting model estimates obtained from EGVU_telemetry_survival_analysis.r

## revised on 26 Nov 2019 after 4 alternative models were run

## revised on 30 Nov 2019 to include categorical classification of migratory and stationary periods

## finalised on 3 Dec to only use code for model 7 (other code moved down) - model comparison also moved down

library(jagsUI)
library(tidyverse)
library(data.table)
library(lubridate)
library(gtools)
filter<-dplyr::filter
select<-dplyr::select

## STEFFEN'S CODE, EDITED BY EVAN BUECHLEY TO EXPERIMENT WITH ALTERNATIVE FIGURE FORMATS





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD M8 RESULTS 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CHANGED On 2 DEC 2019 to exclude models with a continuous 'migration' covariate due to the confounding  problem of dead birds travelling less
## SELECT ONLY PARAMETERS RELEVANT FOR MODEL 8

try(setwd("~/Documents/GitHub/EV - TV Survival Study/"), silent=T)
load("EGVU_survival_output_v3.RData")  ### need to load whole workspace for input matrices to create plotting data range
out8<-fread("EGVU_telemetry_survival_estimates_m8.csv")

### LINEAR PREDICTOR EQUATION
# logit(phi[i,t]) <- lp.mean[adult[i,t]+1,mig[i,t]] +
#                     b.phi.age*(age[i,t])*(adult[i,t])  +   ### age and migratory stage category-specific intercept and slope for non-adult bird to increase survival with age
#                     b.phi.capt*(capt[i]) +     ### survival dependent on captive-release and time since the captive bird was released as long as captive-released bird is not an adult
#                     b.phi.lat*(lat[i,t]) +
#                     b.phi.long*(long[i])  #### probability of monthly survival dependent on latitude and longitude 


### CALCULATE PREDICTED SURVIVAL BASED ON MODEL 8

PLOTDAT<-  expand.grid(mig=c(1,2),capt=c(0,1),age=seq(min(age.mat, na.rm=T),max(age.mat, na.rm=T),1)) %>%
  mutate(logit.surv=ifelse(mig==1,out8$mean[out8$parameter=="lp.mean[1]"],out8$mean[out8$parameter=="lp.mean[2]"])+
           out8$mean[out8$parameter=="b.phi.age"]*age+
           out8$mean[out8$parameter=="b.phi.capt"]*capt) %>% # +
  mutate(lcl.surv=ifelse(mig==1,as.numeric(out8[out8$parameter=="lp.mean[1]",3]),as.numeric(out8[out8$parameter=="lp.mean[2]",3])) +
          as.numeric(out8[out8$parameter=="b.phi.age",3])*age +
          as.numeric(out8[out8$parameter=="b.phi.capt",3])*capt) %>% # +
  mutate(ucl.surv=ifelse(mig==1,as.numeric(out8[out8$parameter=="lp.mean[1]",7]),as.numeric(out8[out8$parameter=="lp.mean[2]",7])) +
          as.numeric(out8[out8$parameter=="b.phi.age",7])*age+
          as.numeric(out8[out8$parameter=="b.phi.capt",7])*capt) %>% # +
  mutate(surv=plogis(logit.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  mutate(Origin=ifelse(capt==1,"captive bred","wild")) %>%
  mutate(Migratory=ifelse(mig==1,"stationary","migratory")) %>%
  arrange(Origin,Migratory,age)

PLOTDAT
head(PLOTDAT)
unique(PLOTDAT$Origin)
unique(PLOTDAT$Migratory)
summary(PLOTDAT)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIGURE 1 - PLOT MONTHLY SURVIVAL PROBABILITIES ON REAL SCALE ACROSS AGE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## PLOT AGE INCREASE FOR SUBADULTS
ggplot(PLOTDAT)+
  geom_ribbon(aes(x=age, ymin=lcl, ymax=ucl, fill=Origin), alpha=0.2) + #
  geom_line(aes(x=age, y=surv, color=Origin)) + #
  facet_wrap(~Migratory,ncol=1) + 
  
  ## ADD ADULT SURVIVAL  
  #geom_hline(data=PLOTDAT[PLOTDAT$age==54,],aes(yintercept=surv), colour="firebrick")+  
  
  ## format axis ticks
  scale_x_continuous(name="age in years", limits=c(1,54), breaks=seq(1,54,12), labels=seq(0,4,1)) +
  scale_y_continuous(name="monthly survival probability", limits=c(0.59,1), breaks=seq(0.60,1,0.1)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=14, color="black"),
        #legend.title=element_text(size=16, color="black"),  
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"),
        legend.title=element_blank())

ggsave("Fig1_survival.by.age.pdf", width=6,height=9)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIG. 2 - MONTHLY SURVIVAL PROBABILITIES ACROSS LATITUDE AND LONGITUDE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NEED TO FIX AGE TO A CERTAIN LEVEL
## PLOT SHOWS WEIRD KINK DUE TO 


## CREATE DATAFRAME OF ALL POSSIBLE COVARIATE COMBINATIONS FOR PLOTTING LATITUDE AND LONGITUDE
PLOTDAT<-expand.grid(mig=c(1,2),lat=seq(min(lat.mat.st, na.rm=T),max(lat.mat.st, na.rm=T),length=50),long=0,capt=c(0,1)) %>%
  bind_rows(expand.grid(mig=c(1,2),lat=0,long=seq(min(long.st, na.rm=T),max(long.st, na.rm=T),length=50),capt=c(0,1))) %>%
  mutate(age=54) %>%
  mutate(logit.surv=ifelse(mig==1,out8$mean[out8$parameter=="lp.mean[1]"],out8$mean[out8$parameter=="lp.mean[2]"])+
           as.numeric(out8$mean[out8$parameter=="b.phi.age"])*age+
           as.numeric(out8$mean[out8$parameter=="b.phi.capt"])*capt +
          as.numeric(out8$mean[out8$parameter=="b.phi.lat"])*lat+
          as.numeric(out8$mean[out8$parameter=="b.phi.long"])*long) %>%
  mutate(lcl.surv=ifelse(mig==1,as.numeric(out8[out8$parameter=="lp.mean[1]",3]),as.numeric(out8[out8$parameter=="lp.mean[2]",3])) +
           as.numeric(out8[out8$parameter=="b.phi.age",3])*age+
           as.numeric(out8[out8$parameter=="b.phi.capt",3])*capt +
           as.numeric(out8[out8$parameter=="b.phi.lat",3])*lat+
           as.numeric(out8[out8$parameter=="b.phi.long",3])*long) %>%
  mutate(ucl.surv=ifelse(mig==1,as.numeric(out8[out8$parameter=="lp.mean[1]",7]),as.numeric(out8[out8$parameter=="lp.mean[2]",7])) +
           as.numeric(out8[out8$parameter=="b.phi.age",7])*age+
           as.numeric(out8[out8$parameter=="b.phi.capt",7])*capt +
           as.numeric(out8[out8$parameter=="b.phi.lat",7])*lat+
           as.numeric(out8[out8$parameter=="b.phi.long",7])*long) %>%
  mutate(surv=plogis(logit.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  mutate(Origin=ifelse(capt==1,"captive bred","wild")) %>%
  mutate(Migratory=ifelse(mig==1,"stationary","migratory")) %>%
  mutate(Latitude=(lat*sd.lat)+mean.lat) %>% ## back transform latitude
  mutate(Longitude=(long*sd.long)+mean.long) %>% ## back transform longitude
  arrange(Origin,Migratory) %>%
  select(Origin, Migratory,surv,lcl,ucl,Latitude,Longitude) %>%
  gather(key='direction',value='degree',-Origin,-Migratory,-surv,-lcl,-ucl) %>%
  filter(!(direction=="Latitude" & degree==mean.lat)) %>%
  filter(!(direction=="Longitude" & degree==mean.long))
head(PLOTDAT)
PLOTDAT$direction[PLOTDAT$direction == "Longitude"] <- "longitude"
PLOTDAT$direction[PLOTDAT$direction == "Latitude"] <- "latitude"
summary(PLOTDAT)

## PLOT ADULT SURVIVAL ACROSS LATITUDE AND LONGITUDE

PLOTDAT %>%
  ggplot()+
  geom_ribbon(aes(x=degree, ymin=lcl, ymax=ucl, fill=Origin), alpha=0.2) +
  geom_line(aes(x=degree, y=surv,colour=Origin))+
    #geom_rect(aes(xmin=min(Longitude),ymin=min(Latitude),xmax=max(Longitude),ymax=max(Latitude), fill = surv)) +
    facet_grid(Migratory~direction, scales="free") +

  
  ## format axis ticks
  ylab("monthly survival probability") +
  xlab("degrees") +
  scale_y_continuous(name="monthly survival probability", limits=c(0.9,1), breaks=seq(0.9,1,0.05)) +
  
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        legend.text=element_text(size=14, color="black"),
        #legend.title=element_text(size=16, color="black"),  
        axis.title=element_text(size=18), 
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"),
        legend.title=element_blank())

ggsave("Fig2_ad.surv.by.lat.long.pdf", width=10,height=9)
#ggsave("Fig2_juv_surv_by_geography.pdf")









#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD RESULTS OF ALL MODELS TO COMPARE DIC AMONG MODELS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

try(setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study"), silent=T)
load("EGVU_survival_output_v3.RData")

### COMBINE OUTPUT FROM ALL 5 MODELS
out<-bind_rows(out1,out2,out3,out4,out5,out6, out7, out8)


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
  filter(grepl("b.phi",parameter)) %>%
  mutate(mean=round(mean,3)) %>%
  spread(key=parameter, value=mean, fill="not included") %>%
  left_join(DIC_tab, by="model")%>%
  arrange(DIC) 

fwrite(ModSelTab,"EGVU_surv_model_selection_table.csv")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT PARAMETER ESTIMATES FROM ALL 8 MODELS ON LOGIT SCALE 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#phimean<-out %>% filter(grepl("lp.mean",parameter))
out %>% filter(grepl("b.phi",parameter)) %>%
  #bind_rows(phimean) %>%
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
# ABANDONED CODE FOR PREDICTIONS FROM OTHER MODELS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## CREATE DATAFRAME OF ALL POSSIBLE COVARIATE COMBINATIONS FOR PLOTTING LATITUDE AND LONGITUDE
PLOTDAT<-expand.grid(mig=c(1,2),lat=seq(min(lat.mat.st, na.rm=T),max(lat.mat.st, na.rm=T),length=30),long=0,capt=c(0,1)) %>%
  bind_rows(expand.grid(mig=c(1,2),lat=0,long=seq(min(long.st, na.rm=T),max(long.st, na.rm=T),length=30),capt=c(0,1))) %>%
  mutate(adult=0, age=54) %>%
  mutate(logit.surv=ifelse(adult==0 & mig==1,as.numeric(out7$mean[out7$parameter=="lp.mean[1,1]"]),
                           ifelse(adult==0 & mig==2,as.numeric(out7$mean[out7$parameter=="lp.mean[1,2]"]),
                                  ifelse(adult==1 & mig==1,as.numeric(out7$mean[out7$parameter=="lp.mean[2,1]"]),as.numeric(out7$mean[out7$parameter=="lp.mean[2,2]"]))))+
           as.numeric(out7$mean[out7$parameter=="b.phi.age"])*age*adult+
           as.numeric(out7$mean[out7$parameter=="b.phi.capt"])*capt +
           as.numeric(out7$mean[out7$parameter=="b.phi.lat"])*lat+
           as.numeric(out7$mean[out7$parameter=="b.phi.long"])*long) %>%
  mutate(lcl.surv=ifelse(adult==0 & mig==1,as.numeric(out7[out7$parameter=="lp.mean[1,1]",3]),
                         ifelse(adult==0 & mig==2,as.numeric(out7[out7$parameter=="lp.mean[1,2]",3]),
                                ifelse(adult==1 & mig==1,as.numeric(out7[out7$parameter=="lp.mean[2,1]",3]),as.numeric(out7[out7$parameter=="lp.mean[2,2]",3]))))+
           as.numeric(out7[out7$parameter=="b.phi.age",3])*age*adult+
           as.numeric(out7[out7$parameter=="b.phi.capt",3])*capt +
           as.numeric(out7[out7$parameter=="b.phi.lat",3])*lat+
           as.numeric(out7[out7$parameter=="b.phi.long",3])*long) %>%
  mutate(ucl.surv=ifelse(adult==0 & mig==1,as.numeric(out7[out7$parameter=="lp.mean[1,1]",7]),
                         ifelse(adult==0 & mig==2,as.numeric(out7[out7$parameter=="lp.mean[1,2]",7]),
                                ifelse(adult==1 & mig==1,as.numeric(out7[out7$parameter=="lp.mean[2,1]",7]),as.numeric(out7[out7$parameter=="lp.mean[2,2]",7]))))+
           as.numeric(out7[out7$parameter=="b.phi.age",7])*age*adult+
           as.numeric(out7[out7$parameter=="b.phi.capt",7])*capt +
           as.numeric(out7[out7$parameter=="b.phi.lat",7])*lat+
           as.numeric(out7[out7$parameter=="b.phi.long",7])*long) %>%
  mutate(surv=plogis(logit.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  mutate(Origin=ifelse(capt==1,"captive bred","wild")) %>%
  mutate(Migratory=ifelse(mig==1,"stationary","migratory")) %>%
  mutate(Latitude=(lat*sd.lat)+mean.lat) %>% ## back transform latitude
  mutate(Longitude=(long*sd.long)+mean.long) %>% ## back transform longitude
  arrange(Origin,Migratory) %>%
  select(Origin, Migratory,surv,lcl,ucl,Latitude,Longitude) %>%
  gather(key='direction',value='degree',-Origin,-Migratory,-surv,-lcl,-ucl) %>%
  filter(!(direction=="Latitude" & degree==mean.lat)) %>%
  filter(!(direction=="Longitude" & degree==mean.long))
head(PLOTDAT)
PLOTDAT %>% filter(Origin=="captive bred" & Migratory=="migratory" & direction=="Latitude") 


### CALCULATE PREDICTED SURVIVAL BASED ON MODEL 3 - continuous daily movement distance
out3<-out %>% filter(model=="m3")

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





# ## PLOT ADULT SURVIVAL ACROSS DAILY MOVEMENT DISTANCES
# 
# ggplot(PLOTDAT[PLOTDAT$adult==0,])+
#   #geom_ribbon(aes(x=mig, ymin=lcl, ymax=ucl, fill=Origin), alpha=0.2) +
#   geom_hline(data=PLOTDAT[PLOTDAT$adult==0,],aes(yintercept=surv, colour=Origin))+
#   facet_wrap(~Migratory) +
#   
#   ## format axis ticks
#   scale_x_continuous(name="Mean daily movement per month (km)", limits=c(0,6), breaks=seq(0,6,1), labels=seq(0,600,100)) +
#   scale_y_continuous(name="Monthly survival probability", limits=c(0,1), breaks=seq(0,1,0.2), labels=seq(0,1,0.2)) +
#   
#   ## beautification of the axes
#   theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         axis.text.y=element_text(size=14, color="black"),
#         axis.text.x=element_text(size=14, color="black"), 
#         axis.title=element_text(size=18), 
#         strip.text.x=element_text(size=18, color="black"), 
#         strip.background=element_rect(fill="white", colour="black"))
# 
# ggsave("EGVU_ad_surv_by_daily_move.pdf")










### CALCULATE PREDICTED SURVIVAL BASED ON MODEL 7

PLOTDAT<-  expand.grid(adult=c(0,1),mig=c(1,2),capt=c(0,1),age=seq(min(age.mat, na.rm=T),max(age.mat, na.rm=T),length=30)) %>%
  mutate(logit.surv=ifelse(adult==0 & mig==1,out7$mean[out7$parameter=="lp.mean[1,1]"],
                           ifelse(adult==0 & mig==2,out7$mean[out7$parameter=="lp.mean[1,2]"],
                                  ifelse(adult==1 & mig==1,out7$mean[out7$parameter=="lp.mean[2,1]"],out7$mean[out7$parameter=="lp.mean[2,2]"])))+
           out7$mean[out7$parameter=="b.phi.age"]*age*adult+
           out7$mean[out7$parameter=="b.phi.capt"]*capt) %>% # +
  mutate(lcl.surv=ifelse(adult==0 & mig==1,as.numeric(out7[out7$parameter=="lp.mean[1,1]",3]),
                         ifelse(adult==0 & mig==2,as.numeric(out7[out7$parameter=="lp.mean[1,2]",3]),
                                ifelse(adult==1 & mig==1,as.numeric(out7[out7$parameter=="lp.mean[2,1]",3]),as.numeric(out7[out7$parameter=="lp.mean[2,2]",3]))))+
           as.numeric(out7[out7$parameter=="b.phi.age",3])*age*adult +
           as.numeric(out7[out7$parameter=="b.phi.capt",3])*capt) %>% # +
  mutate(ucl.surv=ifelse(adult==0 & mig==1,as.numeric(out7[out7$parameter=="lp.mean[1,1]",7]),
                         ifelse(adult==0 & mig==2,as.numeric(out7[out7$parameter=="lp.mean[1,2]",7]),
                                ifelse(adult==1 & mig==1,as.numeric(out7[out7$parameter=="lp.mean[2,1]",7]),as.numeric(out7[out7$parameter=="lp.mean[2,2]",7]))))+
           as.numeric(out7[out7$parameter=="b.phi.age",7])*age*adult+
           as.numeric(out7[out7$parameter=="b.phi.capt",7])*capt) %>% # +
  mutate(surv=plogis(logit.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  mutate(Origin=ifelse(capt==1,"captive bred","wild")) %>%
  mutate(Migratory=ifelse(mig==1,"stationary","migratory")) %>%
  arrange(Origin,Migratory,age)

head(PLOTDAT)
