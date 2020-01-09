##########################################################################
#
# EGYPTIAN AND TURKEY VULTURE MONTHLY SURVIVAL ANALYSIS FROM TELEMETRY
#
##########################################################################
# plotting model estimates obtained from EGVU_telemetry_survival_analysis.r

## revised on 26 Nov 2019 after 4 alternative models were run

## revised on 30 Nov 2019 to include categorical classification of migratory and stationary periods

## finalised on 3 Dec to only use code for model 7 (other code moved down) - model comparison also moved down

## adjusted on 29 Dec 2019 to new model output (m10)

## revised on 3 Jan 2020 to curtail model output to existing ranges

library(jagsUI)
library(tidyverse)
library(data.table)
library(lubridate)
library(gtools)
filter<-dplyr::filter
select<-dplyr::select






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD M10 RESULTS 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


try(setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study"), silent=T)
load("EGVU_survival_output_v3.RData")  ### need to load whole workspace for input matrices to create plotting data range
#out10<-fread("EGVU_telemetry_survival_estimates_m10.csv") ## this causes some weird list error in the PLOTDAT creation below
out10<-as.data.frame(EVsurv10$summary)
out10$parameter<-row.names(EVsurv10$summary)
out10$model<-"m10"

### LINEAR PREDICTOR EQUATION
# logit(phi[i,t]) <- lp.mean[mig[i,t]] + b.phi.age*(age[i,t])  +   ### age and migratory stage category-specific intercept and slope for non-adult bird to increase survival with age
#   b.phi.free*(free[i,t])*(capt[i])*(adult[i,t])   +     ### survival dependent on captive-release and time since the captive bird was released as long as captive-released bird is not an adult
#   b.phi.lat*(lat[i,t]) +                      #### probability of monthly survival dependent on latitude
#   b.phi.pop[pop[i]]                             #### probability of survival varies by population








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OUTPUT TABLE FOR PREDICTED ANNUAL SURVIVAL FOR ADULT AND JUVENILE FOR EACH POPULATION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL
## summarise annual survival by using 10*stationary, 1*spring mig and 1*fall mig

TABLE2<-data.frame(pop=rep(seq(1:4), each=48),
             capt=rep(c(0,1,0,1), each=24),
             age=rep(c(1:12,rep(54,12)),4), 
             mig=rep(c(c(1,3,1,1,1,1,1,1,1,1,1,1),c(1,3,1,1,1,1,1,1,1,1,2,1)),4)) %>%
  
  ### REMOVE MIGRATORY PHASES FROM SOUTHERN POPULATION
  mutate(mig=ifelse(pop==4,1,mig)) %>%
  
  ### CALCULATE MONTHLY SURVIVAL
  mutate(logit.surv=ifelse(mig==1,out10$mean[out10$parameter=="lp.mean[1]"],ifelse(mig==2,out10$mean[out10$parameter=="lp.mean[2]"],out10$mean[out10$parameter=="lp.mean[3]"]))+
           out10$mean[out10$parameter=="b.phi.age"]*age+
           out10$mean[grepl("b.phi.pop",out10$parameter)][pop]+
           out10$mean[out10$parameter=="b.phi.capt"]*capt)  %>% 
  mutate(lcl.surv=ifelse(mig==1,as.numeric(out10[out10$parameter=="lp.mean[1]",3]),ifelse(mig==2,as.numeric(out10[out10$parameter=="lp.mean[2]",3]),as.numeric(out10[out10$parameter=="lp.mean[3]",3]))) +
           as.numeric(out10[out10$parameter=="b.phi.age",3])*age +
           as.numeric(out10[grepl("b.phi.pop",out10$parameter),3][pop])+
           as.numeric(out10[out10$parameter=="b.phi.capt",3])*capt) %>% # +
  mutate(ucl.surv=ifelse(mig==1,as.numeric(out10[out10$parameter=="lp.mean[1]",7]),ifelse(mig==2,as.numeric(out10[out10$parameter=="lp.mean[2]",7]),as.numeric(out10[out10$parameter=="lp.mean[3]",7]))) +
           as.numeric(out10[out10$parameter=="b.phi.age",7])*age+
           as.numeric(out10[grepl("b.phi.pop",out10$parameter),7][pop])+
           as.numeric(out10[out10$parameter=="b.phi.capt",7])*capt) %>% # +
  
  ### BACKTRANSFORM TO NORMAL SCALE
  mutate(surv=plogis(logit.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  
  ### ANNOTATE GROUPS
  mutate(Population=ifelse(pop==1,"Europe (West)",
                           ifelse(pop==2,"Europe (East)",
                                  ifelse(pop==3,"Middle East","Southern")))) %>%
  mutate(Ageclass=ifelse(age==54,"adult","juvenile")) %>%
  mutate(Origin=ifelse(capt==1,"captive-raised","wild")) %>%
  
  ### REMOVE CAPTIVE RAISED BIRDS FROM SOUTHERN AND WESTERN POPULATION
  filter(!(capt==1 & pop %in% c(1,4)))  %>%
  
  ### REMOVE CAPTIVE RAISED ADULTS
  filter(!(capt==1 & age==54))  %>%
  
  ### CALCULATE ANNUAL SURVIVAL
  group_by(Population,Origin,Ageclass) %>%
  summarise(ann.surv=prod(surv),ann.surv.lcl=prod(lcl),ann.surv.ucl=prod(ucl))



fwrite(TABLE2,"EGVU_ann_survival_estimates.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIGURE 1 - PLOT DESIRED BY RON
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



ggplot(TABLE2, aes(y=ann.surv, x=Ageclass, colour=Origin))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=ann.surv.lcl, ymax=ann.surv.ucl, colour=Origin), width=.1)+
  facet_wrap(~Population,ncol=2) +
  
  ## format axis ticks
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




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIGURE 1 - PLOT MONTHLY SURVIVAL PROBABILITIES ON REAL SCALE ACROSS AGE FOR ALL 4 POPULATIONS AND 3 MIG STAGES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

PLOTDAT<-  expand.grid(mig=c(1,2,3),pop=seq(1:4),age=seq(min(age.mat, na.rm=T),max(age.mat, na.rm=T),1), capt=c(0,1)) %>%
  mutate(logit.surv=ifelse(mig==1,out10$mean[out10$parameter=="lp.mean[1]"],ifelse(mig==2,out10$mean[out10$parameter=="lp.mean[2]"],out10$mean[out10$parameter=="lp.mean[3]"]))+
           out10$mean[out10$parameter=="b.phi.age"]*age+
           out10$mean[grepl("b.phi.pop",out10$parameter)][pop]+
           out10$mean[out10$parameter=="b.phi.capt"]*capt)  %>% 
           
  mutate(lcl.surv=ifelse(mig==1,as.numeric(out10[out10$parameter=="lp.mean[1]",3]),ifelse(mig==2,as.numeric(out10[out10$parameter=="lp.mean[2]",3]),as.numeric(out10[out10$parameter=="lp.mean[3]",3]))) +
           as.numeric(out10[out10$parameter=="b.phi.age",3])*age +
           as.numeric(out10[grepl("b.phi.pop",out10$parameter),3][pop])+
           as.numeric(out10[out10$parameter=="b.phi.capt",3])*capt) %>% # +
  mutate(ucl.surv=ifelse(mig==1,as.numeric(out10[out10$parameter=="lp.mean[1]",7]),ifelse(mig==2,as.numeric(out10[out10$parameter=="lp.mean[2]",7]),as.numeric(out10[out10$parameter=="lp.mean[3]",7]))) +
           as.numeric(out10[out10$parameter=="b.phi.age",7])*age+
           as.numeric(out10[grepl("b.phi.pop",out10$parameter),7][pop])+
           as.numeric(out10[out10$parameter=="b.phi.capt",7])*capt) %>% # +
  mutate(surv=plogis(logit.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  mutate(Population=ifelse(pop==1,"Europe (West)",
                            ifelse(pop==2,"Europe (East)",
                                    ifelse(pop==3,"Middle East","Southern")))) %>%
  mutate(Season=ifelse(mig==1,"stationary",ifelse(mig==2,"spring migration","fall migration"))) %>%
  mutate(Origin=ifelse(capt==1,"captive-raised","wild")) %>%
  arrange(Population,Season,age) %>%
  
  ### REMOVE SPRING MIGRATION FOR JUVENILE BIRDS
  filter(!(Season=="spring migration" & age<16)) %>%

  ### REMOVE MIGRATION FOR SOUTHERN POPULATION
  filter(!(Season %in% c("spring migration","fall migration") & pop==4)) %>%
  
  ### REMOVE CAPTIVE RAISED BIRDS FROM SOUTHERN AND WESTERN POPULATION
  filter(!(capt==1 & pop %in% c(1,4)))

head(PLOTDAT)



## PLOT 
PLOTDAT %>% filter(capt==0) %>%

ggplot()+
  geom_ribbon(aes(x=age, ymin=lcl, ymax=ucl, fill=Season), alpha=0.2) +   ##, type=Origin
  geom_line(aes(x=age, y=surv, color=Season))+     ## , linetype=Origin
  facet_wrap(~Population,ncol=2) +
  
  ## format axis ticks
  scale_x_continuous(name="Age in years", limits=c(1,54), breaks=seq(1,54,6), labels=seq(0,4,0.5)) +
  scale_y_continuous(name="Monthly survival probability", limits=c(0.3,1), breaks=seq(0.3,1,0.1)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=16, color="black"),  
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("Fig1_Surv_by_Age.pdf", width=10,height=9)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIGURE 2 - PLOT MONTHLY SURVIVAL PROBABILITIES ON REAL SCALE ACROSS LATITUDE FOR ALL 4 POPULATIONS AND 3 MIG STAGES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### ASSESS RANGE OF LATITUDE FOR EACH POPULATION
poplatrange<-EVcovar %>% filter(id.tag %in% EV.obs.matrix$id.tag) %>%
  mutate(pop=ifelse(population %in% c("western europe","italy"),1,
                    ifelse(population=="balkans",2,
                           ifelse(population %in% c("middle east","caucasus"),3,4)))) %>%
  group_by(pop) %>%
  summarise(lat.min=min(lat),lat.max=max(lat))



### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

PLOTDAT<-  expand.grid(mig=c(1,2,3),pop=seq(1:4),lat=seq(min(lat.mat.st, na.rm=T),max(lat.mat.st, na.rm=T),length=50)) %>%
  mutate(age=3) %>%
  mutate(logit.surv=ifelse(mig==1,out10$mean[out10$parameter=="lp.mean[1]"],ifelse(mig==2,out10$mean[out10$parameter=="lp.mean[2]"],out10$mean[out10$parameter=="lp.mean[3]"]))+
           out10$mean[out10$parameter=="b.phi.lat"]*lat+
           out10$mean[out10$parameter=="b.phi.age"]*age+
           out10$mean[grepl("b.phi.pop",out10$parameter)][pop])  %>% 
  mutate(lcl.surv=ifelse(mig==1,as.numeric(out10[out10$parameter=="lp.mean[1]",3]),ifelse(mig==2,as.numeric(out10[out10$parameter=="lp.mean[2]",3]),as.numeric(out10[out10$parameter=="lp.mean[3]",3]))) +
           as.numeric(out10[out10$parameter=="b.phi.lat",3])*lat +
           as.numeric(out10[out10$parameter=="b.phi.age",3])*age +
           as.numeric(out10[grepl("b.phi.pop",out10$parameter),3][pop])) %>% # +
  mutate(ucl.surv=ifelse(mig==1,as.numeric(out10[out10$parameter=="lp.mean[1]",7]),ifelse(mig==2,as.numeric(out10[out10$parameter=="lp.mean[2]",7]),as.numeric(out10[out10$parameter=="lp.mean[3]",7]))) +
           as.numeric(out10[out10$parameter=="b.phi.lat",7])*lat+
           as.numeric(out10[out10$parameter=="b.phi.age",7])*age +
           as.numeric(out10[grepl("b.phi.pop",out10$parameter),7][pop])) %>% # +
  mutate(surv=plogis(logit.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  mutate(Population=ifelse(pop==1,"Europe (West)",
                           ifelse(pop==2,"Europe (East)",
                                  ifelse(pop==3,"Middle East","Southern")))) %>%
  mutate(Season=ifelse(mig==1,"stationary",ifelse(mig==2,"spring migration","fall migration"))) %>%
  arrange(Population,Season,lat) %>%
  mutate(Latitude=(lat*sd.lat)+mean.lat) %>% ## back transform latitude

  ### REMOVE MIGRATION FOR SOUTHERN POPULATION
  filter(!(Season %in% c("spring migration","fall migration") & pop==4)) %>%
  
  ### REMOVE LATITUDES OUTSIDE EACH POPULATION'S RANGE
  left_join(poplatrange, by="pop") %>%
  filter(!(Latitude>lat.max)) %>%
  filter(!(Latitude<lat.min))

head(PLOTDAT)
range(PLOTDAT$Latitude)


## PLOT 

ggplot(PLOTDAT)+
  geom_ribbon(aes(x=Latitude, ymin=lcl, ymax=ucl, fill=Season), alpha=0.2) +
  geom_line(aes(x=Latitude, y=surv, color=Season))+
  facet_wrap(~Population,ncol=2) +
  
  ## format axis ticks
  scale_x_continuous(name="Latitude", limits=c(1.2,45), breaks=seq(5,45,5), labels=seq(5,45,5)) +
  scale_y_continuous(name="Monthly survival probability", limits=c(0.1,1), breaks=seq(0.1,1,0.1)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=16, color="black"),  
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("Fig2_JUV_Surv_by_Latitude.pdf", width=10,height=9)






### PLOT EFFECT FOR LATITUDE

PLOTDAT<-  expand.grid(mig=c(1,2,3),lat=seq(min(lat.mat.st, na.rm=T),max(lat.mat.st, na.rm=T),length=50)) %>%
  mutate(logit.surv=ifelse(mig==1,out10$mean[out10$parameter=="lp.mean[1]"],ifelse(mig==2,out10$mean[out10$parameter=="lp.mean[2]"],out10$mean[out10$parameter=="lp.mean[3]"]))+
           out10$mean[out10$parameter=="b.phi.lat"]*lat)  %>% 
  mutate(lcl.surv=ifelse(mig==1,as.numeric(out10[out10$parameter=="lp.mean[1]",3]),ifelse(mig==2,as.numeric(out10[out10$parameter=="lp.mean[2]",3]),as.numeric(out10[out10$parameter=="lp.mean[3]",3]))) +
           as.numeric(out10[out10$parameter=="b.phi.lat",3])*lat) %>% # +
  mutate(ucl.surv=ifelse(mig==1,as.numeric(out10[out10$parameter=="lp.mean[1]",7]),ifelse(mig==2,as.numeric(out10[out10$parameter=="lp.mean[2]",7]),as.numeric(out10[out10$parameter=="lp.mean[3]",7]))) +
           as.numeric(out10[out10$parameter=="b.phi.lat",7])*lat) %>% # +
  mutate(surv=plogis(logit.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  mutate(Season=ifelse(mig==1,"stationary",ifelse(mig==2,"spring migration","fall migration"))) %>%
  arrange(Season,lat) %>%
  mutate(Latitude=(lat*sd.lat)+mean.lat) %>%
  filter(Season=="stationary")## back transform latitude
  

head(PLOTDAT)
range(PLOTDAT$Latitude)


## PLOT 

ggplot(PLOTDAT)+
  geom_ribbon(aes(x=Latitude, ymin=lcl, ymax=ucl), alpha=0.2) +
  geom_line(aes(x=Latitude, y=surv))+
  
  ## format axis ticks
  scale_x_continuous(name="Latitude", limits=c(1.2,45), breaks=seq(5,45,5), labels=seq(5,45,5)) +
  scale_y_continuous(name="Monthly survival probability", limits=c(0.1,1), breaks=seq(0.1,1,0.1)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=16, color="black"),  
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("Fig2_Surv_by_Latitude_MEAN.pdf", width=10,height=9)







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIGURE 3- PLOT MONTHLY SURVIVAL PROBABILITIES ON REAL SCALE ACROSS TIME SINCE RELEASE FOR ALL 4 POPULATIONS AND 3 MIG STAGES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### REMOVED ON 3 JAN AFTER REVISING MODEL TO NOT INCLUDE TIME SINCE RELEASE

# ### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL
# 
# PLOTDAT<-  #expand.grid(mig=c(1,2,3),pop=seq(1:4),free=seq(min(free.matrix[,-1], na.rm=T),max(free.matrix[,-1], na.rm=T),1)) %>%
#   expand.grid(mig=c(1,2,3),pop=seq(1:4),free=seq(0,10,1)) %>%
#   mutate(age=free+2) %>%
#   mutate(logit.surv=ifelse(mig==1,out10$mean[out10$parameter=="lp.mean[1]"],ifelse(mig==2,out10$mean[out10$parameter=="lp.mean[2]"],out10$mean[out10$parameter=="lp.mean[3]"]))+
#            out10$mean[out10$parameter=="b.phi.free"]*free+
#            out10$mean[out10$parameter=="b.phi.age"]*age+
#            out10$mean[grepl("b.phi.pop",out10$parameter)][pop])  %>% 
#   mutate(lcl.surv=ifelse(mig==1,as.numeric(out10[out10$parameter=="lp.mean[1]",3]),ifelse(mig==2,as.numeric(out10[out10$parameter=="lp.mean[2]",3]),as.numeric(out10[out10$parameter=="lp.mean[3]",3]))) +
#            as.numeric(out10[out10$parameter=="b.phi.free",3])*free +
#            as.numeric(out10[out10$parameter=="b.phi.age",3])*age +
#            as.numeric(out10[grepl("b.phi.pop",out10$parameter),3][pop])) %>% # +
#   mutate(ucl.surv=ifelse(mig==1,as.numeric(out10[out10$parameter=="lp.mean[1]",7]),ifelse(mig==2,as.numeric(out10[out10$parameter=="lp.mean[2]",7]),as.numeric(out10[out10$parameter=="lp.mean[3]",7]))) +
#            as.numeric(out10[out10$parameter=="b.phi.free",7])*free+
#            as.numeric(out10[out10$parameter=="b.phi.age",7])*age +
#            as.numeric(out10[grepl("b.phi.pop",out10$parameter),7][pop])) %>% # +
#   mutate(surv=plogis(logit.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
#   mutate(Population=ifelse(pop==1,"Europe (West)",
#                            ifelse(pop==2,"Europe (East)",
#                                   ifelse(pop==3,"Middle East","Southern")))) %>%
#   mutate(Season=ifelse(mig==1,"stationary",ifelse(mig==2,"spring migration","fall migration"))) %>%
#   arrange(Population,Season,free) %>%
#   
#   ### REMOVE MIGRATION FOR SOUTHERN POPULATION
#   filter(!(Season %in% c("spring migration","fall migration") & pop==4)) %>%
#   
#   ### REMOVE LATITUDES OUTSIDE EACH POPULATION'S RANGE
#   left_join(poplatrange, by="pop") %>%
#   filter(!(Latitude>lat.max)) %>%
#   filter(!(Latitude<lat.min))
# 
# head(PLOTDAT)
# 
# 
# ## PLOT 
# 
# ggplot(PLOTDAT)+
#   geom_ribbon(aes(x=free, ymin=lcl, ymax=ucl, fill=Season), alpha=0.2) +
#   geom_line(aes(x=free, y=surv, color=Season))+
#   facet_wrap(~Population,ncol=2) +
#   
#   ## format axis ticks
#   scale_x_continuous(name="Time since release (months)", limits=c(0,10), breaks=seq(0,10,2)) +
#   scale_y_continuous(name="Monthly survival probability", limits=c(0,1), breaks=seq(0.2,1,0.1)) +
#   
#   ## beautification of the axes
#   theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         axis.text.y=element_text(size=14, color="black"),
#         axis.text.x=element_text(size=14, color="black"), 
#         axis.title=element_text(size=18),
#         legend.text=element_text(size=14, color="black"),
#         legend.title=element_text(size=16, color="black"),  
#         strip.text=element_text(size=18, color="black"), 
#         strip.background=element_rect(fill="white", colour="black"))
# 
# ggsave("Fig3_JUV_Surv_after_release.pdf", width=10,height=9)
# 





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIG4 - PARAMETER ESTIMATES ON LOGIT SCALE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

out10$parameter<-str_replace(out10$parameter, pattern='lp.mean', replacement='b.phi.mig') 
out10 %>% filter(grepl("b.phi",parameter)) %>%

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


ggsave("Fig3_parameter_estimates_logit.pdf", height=10, width=15)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PREVIOUS TOP MODEL M8 --- LOAD M8 RESULTS 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CHANGED On 2 DEC 2019 to exclude models with a continuous 'migration' covariate due to the confounding  problem of dead birds travelling less
## SELECT ONLY PARAMETERS RELEVANT FOR MODEL 8

try(setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study"), silent=T)
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

head(PLOTDAT)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIGURE 1 - PLOT MONTHLY SURVIVAL PROBABILITIES ON REAL SCALE ACROSS AGE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## PLOT AGE INCREASE FOR SUBADULTS

ggplot(PLOTDAT)+
  geom_ribbon(aes(x=age, ymin=lcl, ymax=ucl, fill=Origin), alpha=0.2) +
  geom_line(aes(x=age, y=surv, color=Origin))+
  facet_wrap(~Migratory,ncol=1) +
  
  ## ADD ADULT SURVIVAL  
  #geom_hline(data=PLOTDAT[PLOTDAT$age==54,],aes(yintercept=surv), colour="firebrick")+  
  
  ## format axis ticks
  scale_x_continuous(name="Age in years", limits=c(1,54), breaks=seq(1,54,6), labels=seq(0,4,0.5)) +
  scale_y_continuous(name="Monthly survival probability", limits=c(0.59,1), breaks=seq(0.60,1,0.1)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=16, color="black"),  
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("Fig1_Age.pdf", width=10,height=9)






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


## PLOT ADULT SURVIVAL ACROSS LATITUDE AND LONGITUDE

PLOTDAT %>%
  ggplot()+
  geom_ribbon(aes(x=degree, ymin=lcl, ymax=ucl, fill=Origin), alpha=0.2) +
  geom_line(aes(x=degree, y=surv,colour=Origin))+
    #geom_rect(aes(xmin=min(Longitude),ymin=min(Latitude),xmax=max(Longitude),ymax=max(Latitude), fill = surv)) +
    facet_grid(Migratory~direction, scales="free") +

  
  ## format axis ticks
  ylab("Monthly survival probability") +
  xlab("degrees (WGS 84)") +

  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=16, color="black"),  
        axis.title=element_text(size=18), 
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("Fig2_ad_surv_by_geography.pdf")
#ggsave("Fig2_juv_surv_by_geography.pdf")









#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD RESULTS OF ALL MODELS TO COMPARE DIC AMONG MODELS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

try(setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study"), silent=T)
load("EGVU_survival_output_v3.RData")

### COMBINE OUTPUT FROM ALL 5 MODELS
out<-bind_rows(out1,out2,out3,out4,out5,out6, out7, out8,out10,out31)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXTRACT DIC TO COMPARE MODELS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pd_dic <- function(x) {
  data.frame(n.parameters=x$pD, DIC=x$DIC)
}
DIC_tab<-bind_rows(pd_dic(EVsurv1),pd_dic(EVsurv2),pd_dic(EVsurv3),pd_dic(EVsurv4),pd_dic(EVsurv5),pd_dic(EVsurv6),pd_dic(EVsurv7),pd_dic(EVsurv8),pd_dic(EVsurv10),pd_dic(EVsurv31)) %>%
  mutate(model=c("m1","m2","m3","m4","m5","m6","m7","m8","m10","m31")) %>%
  arrange(DIC) %>%
  mutate(deltaDIC=DIC-DIC[1])
DIC_tab

ModSelTab<-out %>% dplyr::select(model, parameter,mean) %>%
  filter(grepl("b.phi",parameter)) %>%
  mutate(mean=round(mean,3)) %>%
  spread(key=parameter, value=mean, fill="not included") %>%
  left_join(DIC_tab, by="model")%>%
  arrange(DIC) 

fwrite(ModSelTab,"EGVU_surv_model_selection_table_redvar.csv")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT PARAMETER ESTIMATES FROM ALL 10 MODELS ON LOGIT SCALE 
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


ggsave("EGVU_surv_parameter_estimates_allmodels_redvar.pdf", height=15, width=10)











#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OUTPUT TABLE FOR PREDICTED ANNUAL SURVIVAL FOR ADULT AND JUVENILE FROM MODEL M31
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL
## summarise annual survival by using 10*stationary, 1*spring mig and 1*fall mig

TABLE2<-data.frame(capt=rep(c(0,1), each=24),
                   age=rep(c(1:12,rep(54,12)),2),
                   adult=rep(c(1,0,1,0),each=12),
                   #mig=rep(c(c(0.1,4,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1),c(0.1,4,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,4,0.1)),2)) %>%
                  mig=rep(c(c(1,2,1,1,1,1,1,1,1,1,1,1),c(1,2,1,1,1,1,1,1,1,1,2,1)),2)) %>%
  
  ### CALCULATE MONTHLY SURVIVAL
  mutate(logit.surv=ifelse(adult==1,as.numeric(out31[out31$parameter=="lp.mean[2]",5]),as.numeric(out31[out31$parameter=="lp.mean[1]",5]))+
          as.numeric(out31[out31$parameter=="b.phi.age",5])*age*adult +
          #as.numeric(out3[out3$parameter=="b.phi.mig",5])*mig +
          as.numeric(out31[grepl("b.phi.mig",out31$parameter),5])[mig]+
          as.numeric(out31[out31$parameter=="b.phi.capt",5])*capt) %>% # +
  mutate(lcl.surv=ifelse(adult==1,as.numeric(out31[out31$parameter=="lp.mean[2]",5]),as.numeric(out31[out31$parameter=="lp.mean[1]",3]))+
           as.numeric(out31[out31$parameter=="b.phi.age",3])*age*adult +
           #as.numeric(out3[out3$parameter=="b.phi.mig",3])*mig +
           as.numeric(out31[grepl("b.phi.mig",out31$parameter),3])[mig]+
           as.numeric(out31[out31$parameter=="b.phi.capt",3])*capt) %>% # +
  mutate(ucl.surv=ifelse(adult==1,as.numeric(out31[out31$parameter=="lp.mean[2]",5]),as.numeric(out31[out31$parameter=="lp.mean[1]",7]))+
           as.numeric(out31[out31$parameter=="b.phi.age",7])*age*adult +
           #as.numeric(out3[out3$parameter=="b.phi.mig",7])*mig +
           as.numeric(out31[grepl("b.phi.mig",out31$parameter),7])[mig]+
           as.numeric(out31[out31$parameter=="b.phi.capt",7])*capt) %>% # +
  
  ### BACKTRANSFORM TO NORMAL SCALE
  mutate(surv=plogis(logit.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  
  ### ANNOTATE GROUPS
  mutate(Ageclass=ifelse(age==54,"adult","juvenile")) %>%
  mutate(Origin=ifelse(capt==1,"captive-raised","wild")) %>%
  
  ### REMOVE CAPTIVE RAISED ADULTS
  filter(!(capt==1 & age==54))  %>%
  
  ### CALCULATE ANNUAL SURVIVAL
  group_by(Origin,Ageclass) %>%
  summarise(ann.surv=prod(surv),ann.surv.lcl=prod(lcl),ann.surv.ucl=prod(ucl))



fwrite(TABLE2,"EGVU_ann_survival_estimates_m31.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIGURE 1 - PLOT DESIRED BY RON
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ggplot(TABLE2, aes(y=ann.surv, x=Ageclass, colour=Origin))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=ann.surv.lcl, ymax=ann.surv.ucl, colour=Origin), width=.1)+
  
  ## format axis ticks
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




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIGURE 1 - PLOT MONTHLY SURVIVAL ACROSS AGE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

PLOTDAT<-  expand.grid(age=seq(min(age.mat, na.rm=T),max(age.mat, na.rm=T),1), capt=c(0,1)) %>%
  mutate(adult=ifelse(age==54,0,1)) %>%

  ### CALCULATE MONTHLY SURVIVAL
  mutate(logit.surv=ifelse(adult==1,as.numeric(out31[out31$parameter=="lp.mean[2]",5]),as.numeric(out31[out31$parameter=="lp.mean[1]",5]))+
           as.numeric(out31[out31$parameter=="b.phi.age",5])*age*adult +
           as.numeric(out31[out31$parameter=="b.phi.capt",5])*capt) %>% # +
  mutate(lcl.surv=ifelse(adult==1,as.numeric(out31[out31$parameter=="lp.mean[2]",5]),as.numeric(out31[out31$parameter=="lp.mean[1]",3]))+
           as.numeric(out31[out31$parameter=="b.phi.age",3])*age*adult +
           as.numeric(out31[out31$parameter=="b.phi.capt",3])*capt) %>% # +
  mutate(ucl.surv=ifelse(adult==1,as.numeric(out31[out31$parameter=="lp.mean[2]",5]),as.numeric(out31[out31$parameter=="lp.mean[1]",7]))+
           as.numeric(out31[out31$parameter=="b.phi.age",7])*age*adult +
           as.numeric(out31[out31$parameter=="b.phi.capt",7])*capt) %>% # +
  
  ### BACKTRANSFORM TO NORMAL SCALE
  mutate(surv=plogis(logit.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  
  ### ANNOTATE GROUPS
  mutate(Origin=ifelse(capt==1,"captive-raised","wild")) %>%
  
  ### REMOVE CAPTIVE RAISED ADULTS
  filter(!(adult==0))
head(PLOTDAT)



## PLOT 
PLOTDAT %>%
  
  ggplot()+
  geom_ribbon(aes(x=age, ymin=lcl, ymax=ucl, fill=Origin), alpha=0.2) +   ##, type=Origin
  geom_line(aes(x=age, y=surv, color=Origin))+     ## , linetype=Origin
  
  ## format axis ticks
  scale_x_continuous(name="Age in years", limits=c(1,54), breaks=seq(1,54,6), labels=seq(0,4,0.5)) +
  scale_y_continuous(name="Monthly survival probability", limits=c(0.3,1), breaks=seq(0.3,1,0.1)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=16, color="black"),  
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("Fig1_Surv_by_Age_m31.pdf", width=10,height=9)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIGURE 2 - PLOT MONTHLY SURVIVAL ACROSS LATITUDE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### ASSESS RANGE OF LATITUDE FOR EACH POPULATION
poplatrange<-EVcovar %>% filter(id.tag %in% EV.obs.matrix$id.tag) %>%
  mutate(pop=ifelse(population %in% c("western europe","italy"),1,
                    ifelse(population=="balkans",2,
                           ifelse(population %in% c("middle east","caucasus"),3,4)))) %>%
  group_by(pop) %>%
  summarise(lat.min=min(lat),lat.max=max(lat))



### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

PLOTDAT<-  expand.grid(age=c(3,54), lat=seq(min(lat.mat.st, na.rm=T),max(lat.mat.st, na.rm=T),length=50)) %>%
  mutate(adult=ifelse(age==54,0,1)) %>%
  
  ### CALCULATE MONTHLY SURVIVAL
  mutate(logit.surv=ifelse(adult==1,as.numeric(out31[out31$parameter=="lp.mean[2]",5]),as.numeric(out31[out31$parameter=="lp.mean[1]",5]))+
           as.numeric(out31[out31$parameter=="b.phi.age",5])*age*adult +
           #as.numeric(out31[grepl("b.phi.mig",out31$parameter),5])[mig]+
           as.numeric(out31[out31$parameter=="b.phi.lat",5])*lat) %>% # +
  mutate(lcl.surv=ifelse(adult==1,as.numeric(out31[out31$parameter=="lp.mean[2]",3]),as.numeric(out31[out31$parameter=="lp.mean[1]",3]))+
           as.numeric(out31[out31$parameter=="b.phi.age",3])*age*adult +
           #as.numeric(out31[grepl("b.phi.mig",out31$parameter),3])[mig]+
           as.numeric(out31[out31$parameter=="b.phi.lat",3])*lat) %>% # +
  mutate(ucl.surv=ifelse(adult==1,as.numeric(out31[out31$parameter=="lp.mean[2]",7]),as.numeric(out31[out31$parameter=="lp.mean[1]",7]))+
           as.numeric(out31[out31$parameter=="b.phi.age",7])*age*adult +
           #as.numeric(out31[grepl("b.phi.mig",out31$parameter),7])[mig]+
           as.numeric(out31[out31$parameter=="b.phi.lat",7])*lat) %>% # +
  
  ### BACKTRANSFORM TO NORMAL SCALE
  mutate(surv=plogis(logit.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  
  ### ANNOTATE GROUPS
  mutate(Ageclass=ifelse(age==54,"adult","juvenile")) %>%
  mutate(Latitude=(lat*sd.lat)+mean.lat)


head(PLOTDAT)
tail(PLOTDAT)
range(PLOTDAT$Latitude)


## PLOT 

ggplot(PLOTDAT)+
  geom_ribbon(aes(x=Latitude, ymin=lcl, ymax=ucl, fill=Ageclass), alpha=0.2) +
  geom_line(aes(x=Latitude, y=surv,color=Ageclass))+
  
  ## format axis ticks
  scale_x_continuous(name="Latitude", limits=c(1.2,45), breaks=seq(5,45,5), labels=seq(5,45,5)) +
  scale_y_continuous(name="Monthly survival probability", limits=c(0.1,1), breaks=seq(0.1,1,0.1)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=16, color="black"),  
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("Fig2_Surv_by_Latitude_m31.pdf", width=10,height=9)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIGURE 3 - PLOT MONTHLY SURVIVAL ACROSS LONGITUDE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

PLOTDAT<-  expand.grid(age=c(3,54), long=seq(min(long.st, na.rm=T),max(long.st, na.rm=T),length=50), mig=c(1,2)) %>%
  mutate(adult=ifelse(age==54,0,1)) %>%
  
  ### CALCULATE MONTHLY SURVIVAL
  mutate(logit.surv=ifelse(adult==1,as.numeric(out31[out31$parameter=="lp.mean[2]",5]),as.numeric(out31[out31$parameter=="lp.mean[1]",5]))+
           as.numeric(out31[out31$parameter=="b.phi.age",5])*age*adult +
           as.numeric(out31[out31$parameter=="b.phi.long",5])*long) %>% # +
  mutate(lcl.surv=ifelse(adult==1,as.numeric(out31[out31$parameter=="lp.mean[2]",3]),as.numeric(out31[out31$parameter=="lp.mean[1]",3]))+
           as.numeric(out31[out31$parameter=="b.phi.age",3])*age*adult +
           as.numeric(out31[out31$parameter=="b.phi.long",3])*long) %>% # +
  mutate(ucl.surv=ifelse(adult==1,as.numeric(out31[out31$parameter=="lp.mean[2]",7]),as.numeric(out31[out31$parameter=="lp.mean[1]",7]))+
           as.numeric(out31[out31$parameter=="b.phi.age",7])*age*adult +
           as.numeric(out31[out31$parameter=="b.phi.long",7])*long) %>% # +
  
  ### BACKTRANSFORM TO NORMAL SCALE
  mutate(surv=plogis(logit.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  
  ### ANNOTATE GROUPS
  mutate(Ageclass=ifelse(age==54,"adult","juvenile")) %>%
  mutate(longitude=(long*sd.long)+mean.long)


head(PLOTDAT)
range(PLOTDAT$longitude)


## PLOT 

ggplot(PLOTDAT)+
  geom_ribbon(aes(x=longitude, ymin=lcl, ymax=ucl, fill=Ageclass), alpha=0.2) +
  geom_line(aes(x=longitude, y=surv,color=Ageclass))+
  
  ## format axis ticks
  scale_x_continuous(name="longitude", limits=c(1.2,45), breaks=seq(5,45,5), labels=seq(5,45,5)) +
  scale_y_continuous(name="Monthly survival probability", limits=c(0.1,1), breaks=seq(0.1,1,0.1)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=16, color="black"),  
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("Fig3_Surv_by_longitude_m31.pdf", width=10,height=9)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIGURE 4 - PLOT MONTHLY SURVIVAL ACROSS MOVEMENT DISTANCE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### CALCULATE PREDICTED SURVIVAL BASED ON M3

# PLOTDAT<-  expand.grid(age=c(3,54), mig=seq(min(mig.mat, na.rm=T),max(mig.mat, na.rm=T),length=500)) %>%
#   mutate(adult=ifelse(age==54,0,1)) %>%
#   
#   ### CALCULATE MONTHLY SURVIVAL
#   mutate(logit.surv=ifelse(adult==1,as.numeric(out31[out31$parameter=="lp.mean[2]",5]),as.numeric(out31[out31$parameter=="lp.mean[1]",5]))+
#            as.numeric(out31[out31$parameter=="b.phi.age",5])*age*adult +
#            as.numeric(out31[out31$parameter=="b.phi.mig",5])*mig) %>% # +
#   mutate(lcl.surv=ifelse(adult==1,as.numeric(out31[out31$parameter=="lp.mean[2]",5]),as.numeric(out31[out31$parameter=="lp.mean[1]",3]))+
#            as.numeric(out31[out31$parameter=="b.phi.age",3])*age*adult +
#            as.numeric(out31[out31$parameter=="b.phi.mig",3])*mig) %>% # +
#   mutate(ucl.surv=ifelse(adult==1,as.numeric(out31[out31$parameter=="lp.mean[2]",5]),as.numeric(out31[out31$parameter=="lp.mean[1]",7]))+
#            as.numeric(out31[out31$parameter=="b.phi.age",7])*age*adult +
#            as.numeric(out31[out31$parameter=="b.phi.mig",7])*mig) %>% # +
#   ### BACKTRANSFORM TO NORMAL SCALE
#   mutate(surv=plogis(logit.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
#   
#   ### ANNOTATE GROUPS
#   mutate(Ageclass=ifelse(age==54,"adult","juvenile")) %>%
#   mutate(Distance=mig*110)
  
  
  PLOTDAT<-  expand.grid(age=c(3,54), mig=c(1,2),capt=c(1,0)) %>%
    mutate(adult=ifelse(age==54,0,1)) %>%
  
    ### CALCULATE MONTHLY SURVIVAL
    mutate(logit.surv=ifelse(adult==1,as.numeric(out31[out31$parameter=="lp.mean[2]",5]),as.numeric(out31[out31$parameter=="lp.mean[1]",5]))+
             as.numeric(out31[out31$parameter=="b.phi.age",5])*age*adult +
             as.numeric(out31[grepl("b.phi.mig",out31$parameter),5])[mig]+
             as.numeric(out31[out31$parameter=="b.phi.capt",5])*capt) %>% # +
    mutate(lcl.surv=ifelse(adult==1,as.numeric(out31[out31$parameter=="lp.mean[2]",5]),as.numeric(out31[out31$parameter=="lp.mean[1]",3]))+
             as.numeric(out31[out31$parameter=="b.phi.age",3])*age*adult +
             as.numeric(out31[grepl("b.phi.mig",out31$parameter),3])[mig]+
             as.numeric(out31[out31$parameter=="b.phi.capt",3])*capt) %>% # +
    mutate(ucl.surv=ifelse(adult==1,as.numeric(out31[out31$parameter=="lp.mean[2]",5]),as.numeric(out31[out31$parameter=="lp.mean[1]",7]))+
             as.numeric(out31[out31$parameter=="b.phi.age",7])*age*adult +
             as.numeric(out31[grepl("b.phi.mig",out31$parameter),7])[mig]+
             as.numeric(out31[out31$parameter=="b.phi.capt",7])*capt) %>% # +
    
    ### BACKTRANSFORM TO NORMAL SCALE
    mutate(surv=plogis(logit.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%

  
    ### ANNOTATE GROUPS
    mutate(Ageclass=ifelse(age==54,"adult","juvenile")) %>%
    mutate(category=ifelse(mig==1,"stationary","migratory")) %>%
    mutate(Origin=ifelse(capt==1,"captive-raised","wild")) %>%
    filter(!(capt==1 & age==54))


head(PLOTDAT)


## PLOT 

ggplot(PLOTDAT)+
  geom_errorbar(aes(x=category, ymin=lcl, ymax=ucl,color=Origin), alpha=0.2) +
  geom_point(aes(x=category, y=surv,color=Origin))+
  facet_wrap(~Ageclass, ncol=1, scales="free_y") +
  
  
  ## format axis ticks
  #scale_x_continuous(name="Monthly distance (km)", limits=c(0,650), breaks=seq(0,650,50)) +
  #scale_y_continuous(name="Monthly survival probability", limits=c(0.1,1), breaks=seq(0.1,1,0.1)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=16, color="black"),  
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("Fig5_Surv_by_movement_m31.pdf", width=10,height=9)













#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PROPER PLOTTING USING RAW MCMC OUTPUT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parmcols<-dimnames(EVsurv31$samples[[1]])[[2]]

### COMBINE SAMPLES ACROSS CHAINS
MCMCout<-rbind(EVsurv31$samples[[1]],EVsurv31$samples[[2]],EVsurv31$samples[[3]],EVsurv31$samples[[4]])
str(MCMCout)


### CALCULATE PREDICTED VALUE FOR EACH SAMPLE
MCMCpred<-data.frame()
for(s in 1:nrow(MCMCout)) {
  
  X<-  expand.grid(age=c(3,54), lat=seq(min(lat.mat.st, na.rm=T),max(lat.mat.st, na.rm=T),length=500)) %>%
    mutate(adult=ifelse(age==54,0,1)) %>%
    
    ### CALCULATE MONTHLY SURVIVAL
    mutate(logit.surv=ifelse(adult==1,as.numeric(MCMCout[s,match("lp.mean[2]",parmcols)]),as.numeric(MCMCout[s,match("lp.mean[1]",parmcols)]))+
             as.numeric(MCMCout[s,match("b.phi.age",parmcols)])*age*adult +
             as.numeric(MCMCout[s,match("b.phi.lat",parmcols)])*lat) 
  
  MCMCpred<-rbind(MCMCpred,X) 
  
}


### CALCULATE PREDICTED SURVIVAL BASED ON FINAL MODEL

PLOTDAT<-  MCMCpred %>% group_by(age,adult,lat) %>%
  summarise(med.surv=quantile(logit.surv,0.5),lcl.surv=quantile(logit.surv,0.025),ucl.surv=quantile(logit.surv,0.975)) %>%
  
  ### BACKTRANSFORM TO NORMAL SCALE
  mutate(surv=plogis(med.surv),lcl=plogis(lcl.surv),ucl=plogis(ucl.surv)) %>%
  
  ### ANNOTATE GROUPS
  mutate(Ageclass=ifelse(age==54,"adult","juvenile")) %>%
  mutate(Latitude=(lat*sd.lat)+mean.lat)


head(PLOTDAT)
tail(PLOTDAT)
range(PLOTDAT$Latitude)


## PLOT 

ggplot(PLOTDAT)+
  geom_ribbon(aes(x=Latitude, ymin=lcl, ymax=ucl, fill=Ageclass), alpha=0.2) +
  geom_line(aes(x=Latitude, y=surv,color=Ageclass))+
  
  ## format axis ticks
  scale_x_continuous(name="Latitude", limits=c(1.2,45), breaks=seq(5,45,5), labels=seq(5,45,5)) +
  scale_y_continuous(name="Monthly survival probability", limits=c(0.1,1), breaks=seq(0.1,1,0.1)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=14, color="black"),
        axis.text.x=element_text(size=14, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=16, color="black"),  
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

ggsave("Fig2_Surv_by_Latitude_m31.pdf", width=10,height=9)





















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
out31<-out %>% filter(model=="m3")

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
