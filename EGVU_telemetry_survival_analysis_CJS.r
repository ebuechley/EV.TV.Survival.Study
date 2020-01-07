##########################################################################
#
# EGYPTIAN VULTURE MONTHLY SURVIVAL ANALYSIS FROM TELEMETRY
#
##########################################################################
# written by Steffen Oppel, July 2019
# based on 'EGVU_telemetry_survival_analysis.r'

## updated 7 Jan 2020
## reverted from multi-event to simple CJS model due to massive uncertainty in parameter estimates

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
dim(EV)
EV$id.tag = as.character(EV$id.tag)
EVcovar$id.tag = as.character(EVcovar$id.tag)

#EV<-EV %>% mutate(start=mdy_hm(start.date), end= mdy_hm(end.date)) %>%
EV<-EV %>% mutate(start=parse_date_time(start.date, c("mdy", "mdy HM")), end= parse_date_time(end.date, c("mdy", "mdy HM"))) %>%
  filter(!is.na(start)) %>%
  filter(species=="Neophron percnopterus") %>%
  filter(start<ymd_hm("2019-04-01 12:00")) %>%  ## remove birds only alive for a few months in 2019
  select(species,population,id.tag,sex,age.at.deployment,age.at.deployment.month,captive.raised,rehabilitated, start, end, fate, how.fate.determined.clean, mean.GPS.dist.last10fixes.degrees)
head(EV)
dim(EV)


### REDUCE COVARIATES TO THOSE TAGS IN EV DATA
EVcovar<-EVcovar %>% filter(id.tag %in% EV$id.tag)


####### ASSIGNMENT OF STATES ########
## simplified to a simple 0/1 for simple CJS, with a terminal 1 for confirmed transmitter failures

unique(EV$fate)
unique(EV$age.at.deployment)


EV<-EV %>%
  mutate(OS= ifelse(fate=="confirmed transmitter failure",1,0)) %>%
  arrange(id.tag)
head(EV)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE CAPTURE HISTORY FOR SURVIVAL ESTIMATIONS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## MAJOR UPDATE 5 Nov 2019 - SHOULD WE REMOVE YEAR AND REDUCE CH from 187 to 134 OCCASIONS?
### CALCULATE LONGEST POSSIBLE EXPOSURE
EV %>% mutate(dur=as.numeric(difftime(end,start,"days"))/30) %>% group_by(species) %>%
	summarise(totlength=max(dur))

### CREATE A TIME SERIES DATA FRAME ###
mindate<-min(EV$start)
maxdate<-max(EV$end)
timeseries<-data.frame(date=seq(mindate, maxdate, "1 month")) %>%
  mutate(month=month(date),year=year(date)) %>%
  mutate(date=format(date, format="%m-%Y")) %>%
  mutate(season=ifelse(month %in% c(2,3,4,9,10), 'migration',ifelse(month %in% c(11,12,1),"winter","summer"))) %>%
  mutate(col=seq_along(date)+1)
dim(timeseries)


### CREATE BLANK MATRICES TO HOLD INFORMATION ABOUT STATE ###

EV.obs.matrix<-EV %>% select(id.tag) %>%
  arrange(id.tag)
EV.obs.matrix[,2:max(timeseries$col)]<-0

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
  #EV.obs.matrix[EV.obs.matrix$id.tag==n,stopcol:max(timeseries$col)]<-xl$OS   ## this assumes that state never changes - some birds may have gone off air and then been found dead - this needs manual adjustment!  
  
  ## ASSIGN INITIAL TRUE STATE (to initialise z-matrix of model)
  EV.obs.matrix[EV.obs.matrix$id.tag==n,max(timeseries$col)]<-ifelse(EV$OS[EV$id.tag==n]==1,1,0)   ## this sets the last observation to 1 for those birds that had confirmed transmitter failure
  
  # EV.state.matrix[EV.state.matrix$id.tag==n,(startcol+1):(stopcol-1)]<-2      ## state at first capture is known, hence must be NA in z-matrix
  # EV.state.matrix[EV.state.matrix$id.tag==n,stopcol:max(timeseries$col)]<-xl$TS   ## this assumes that state never changes - some birds may have gone off air and then been found dead - this needs manual adjustment!
  # EV.state.matrix[EV.state.matrix$id.tag==n,2:startcol]<-NA ## error occurs if z at first occ is not NA, so we need to specify that for birds alive for <1 month because stopcol-1 = startcol
  # 
}


# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # CREATE MATRIX OF SURVIVAL PARAMETERS BASED ON AGE AND SEASON
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# commented out on 17 NOV 2019 when inserting new covariate data frame
# resurrected on 30 Nov 2019 based on manually provided file
# 0	No data
# 1	Stationary
# 2	Migratory

EV.phi.states<-fread("Mig_stage_matrix.csv")
head(EV.phi.states)

### arrange into full matrix ###
EV.phi.matrix<-EV.phi.states %>%
  #separate(id.year,into=c("id","tag","year"), sep="_") %>% ## does not work as some id have _
  #mutate(id.tag=paste(id,tag,sep="_")) %>%
  mutate(id.tag=substr(id.year,1,nchar(id.year)-9)) %>%
  mutate(year=as.numeric(substr(id.year,nchar(id.year)-7,nchar(id.year)-4))) %>%  
  select(-Seq,-id.year) %>%
  gather(key="month",value="state",-id.tag,-year) %>%
  mutate(date=ymd(paste(year,month,"01",sep="-"))) %>%
  mutate(date=format(date, format="%m-%Y")) %>%
  mutate(col=timeseries$col[match(date,timeseries$date)]) %>%
  filter(!is.na(col)) %>%
  mutate(state=ifelse(state==2,2,1)) %>%  ## replace 0 as there will be no parameter with index 0
  mutate(state=ifelse(state==1,1,ifelse(month<7,2,3))) %>%  ## state==2 for spring migration and state==3 for fall migration
  select(-year,-month,-date) %>%
  spread(key=col,value=state, fill=1) %>%
  arrange(id.tag)




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIX ID TAG VALUES
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## these were edits made to the summary table by data owners that I am reincorporating back to the original data
## fix provided by Evan Buechley on 3 Dec 2019

EV.phi.matrix$id.tag = as.character(EV.phi.matrix$id.tag)
EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="1_1"] <- "52027_1"
EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="93_14"] <- "81_14"
EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="AF5AF11F_NA"] <- "Bianca_IHB_AF5AF11F"
EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="B05AF11F_NA"] <- "Clara_IHC_B05AF11F"
EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="Provence_2016_Ad_wild_EO5018_Salomé_8P_5018"] <- "Provence_2016_Ad_wild_EO5018_Salome_8P_5018"
EV.phi.matrix<-EV.phi.matrix[!(EV.phi.matrix$id.tag=="Djibouti_127589"),]

EV.phi.matrix<-EV.phi.matrix %>% filter(id.tag %in% EV$id.tag) %>%
  arrange(id.tag)
dim(EV.phi.matrix)


### double check - should reveal no inconsistencies
#merge(EV.phi.matrix[,1:2],EV, by="id.tag", all=T) %>% filter(is.na(`2`)) %>% select(id.tag,population,age.at.deployment)
#merge(EV.phi.matrix[,1:2],EV, by="id.tag", all=T) %>% filter(is.na(population)) %>% select(id.tag,population,age.at.deployment)





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
  spread(key=col,value=age, fill=0) %>%
  arrange(id.tag)

# age.matrix[,1:2] %>%
#   rename(test=`2`) %>%
#   mutate(test=1) %>%
#   full_join(EV, by="id.tag") %>% filter(is.na(population)) %>% select(id.tag,population,age.at.deployment)


## CREATE MOVE DISTANCE (MIGRATION) MATRIX
mig.matrix<-EVcovar %>% filter(id.tag %in% EV.obs.matrix$id.tag) %>%
  mutate(timestamp=ymd_hms(timestamp)) %>%
  mutate(yr.mo=format(timestamp, format="%m-%Y")) %>%
  mutate(col=timeseries$col[match(yr.mo,timeseries$date)]) %>%
  group_by(id.tag,col) %>%
  summarise(mig=mean(mean.monthly.dist)) %>%
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

## CREATE LONGITUDE MATRIX
long.matrix<-EVcovar %>% filter(id.tag %in% EV.obs.matrix$id.tag) %>%
  mutate(timestamp=ymd_hms(timestamp)) %>%
  mutate(yr.mo=format(timestamp, format="%m-%Y")) %>%
  mutate(col=timeseries$col[match(yr.mo,timeseries$date)]) %>%
  group_by(id.tag,col) %>%
  summarise(lat=mean(mean.monthly.long)) %>%
  spread(key=col,value=lat) %>%
  arrange(id.tag)

long.orig<-EVcovar %>% filter(id.tag %in% EV.obs.matrix$id.tag) %>%
  mutate(timestamp=ymd_hms(timestamp)) %>%
  arrange(id.tag,timestamp) %>%
  group_by(id.tag) %>%
  summarise(long=first(mean.monthly.long)) %>%
  arrange(id.tag)

## create population vector
pop.orig<-EVcovar %>% filter(id.tag %in% EV.obs.matrix$id.tag) %>%
  mutate(timestamp=ymd_hms(timestamp)) %>%
  arrange(id.tag,timestamp) %>%
  group_by(id.tag) %>%
  summarise(pop=first(population)) %>%
  mutate(pop=ifelse(pop %in% c("balkans","italy"),2,
                    ifelse(pop=="western europe",1,
                           ifelse(pop %in% c("middle east","caucasus"),3,4)))) %>%
  arrange(id.tag)


## AGE SINCE TAGGING
free.matrix<-EVcovar %>% filter(id.tag %in% EV.obs.matrix$id.tag) %>%
  mutate(timestamp=ymd_hms(timestamp)) %>%
  mutate(yr.mo=format(timestamp, format="%m-%Y")) %>%
  mutate(col=timeseries$col[match(yr.mo,timeseries$date)]) %>%
  group_by(id.tag,col) %>%
  summarise(lat=mean(months.from.tagging)) %>%
  mutate(lat=ifelse(lat>53,54,lat)) %>%
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
    mig.matrix[mig.matrix$id.tag==n,col]<-mig.matrix[mig.matrix$id.tag==n,(col-1)]
    long.matrix[long.matrix$id.tag==n,col]<-long.matrix[long.matrix$id.tag==n,(col-1)]
    free.matrix[free.matrix$id.tag==n,col]<-min((free.matrix[free.matrix$id.tag==n,(col-1)]+1),54)
    free.matrix[free.matrix$id.tag==n,col]<-ifelse(EV$captive.raised[EV$id.tag==n]=="N",0,free.matrix[free.matrix$id.tag==n,col]) ## set value to 0 for all wild birds

  } ## end loop over each occasion
  
  
  misscol<-which(is.na(age.matrix[age.matrix$id.tag==n,]))
  misscol<-misscol[misscol>startcol]
  
  ### INTERPOLATE VALUES IF NA IN COLUMNS THAT SHOULD HAVE VALUES
  for (col in misscol){
    age.matrix[age.matrix$id.tag==n,col]<-min((age.matrix[age.matrix$id.tag==n,(col-1)]+1),54)
    lat.matrix[lat.matrix$id.tag==n,col]<-lat.matrix[lat.matrix$id.tag==n,(col-1)]
    mig.matrix[mig.matrix$id.tag==n,col]<-mig.matrix[mig.matrix$id.tag==n,(col-1)]
    long.matrix[long.matrix$id.tag==n,col]<-long.matrix[long.matrix$id.tag==n,(col-1)]
    free.matrix[free.matrix$id.tag==n,col]<-min((free.matrix[free.matrix$id.tag==n,(col-1)]+1),54)
    free.matrix[free.matrix$id.tag==n,col]<-ifelse(EV$captive.raised[EV$id.tag==n]=="N",0,free.matrix[free.matrix$id.tag==n,col]) ## set value to 0 for all wild birds
    
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
age.mat<-as.matrix(age.matrix[,2:max(timeseries$col)])
mig.mat<-as.matrix(mig.matrix[,2:max(timeseries$col)])  ## commented out on 30 NOV 2019 and replaced with categorical migratory vs stationary matrix; reinstated 4 Dec to re-run models
lat.mat<-as.matrix(lat.matrix[,2:max(timeseries$col)])
free.mat<-as.matrix(free.matrix[,2:max(timeseries$col)])
long.mat<-as.matrix(long.matrix[,2:max(timeseries$col)])

range(age.mat, na.rm=T)
range(mig.mat, na.rm=T)
range(lat.mat, na.rm=T)
range(long.mat, na.rm=T)
range(free.mat, na.rm=T)
free.matrix %>% gather(key=occ, value=free,-id.tag) %>% filter(free>50) %>% group_by(id.tag) %>% summarise(start=min(occ))



#### Standardise lat and long because lat^2 results in extremely large numerical values
mean.lat<-mean(lat.mat, na.rm=T)
sd.lat<-sd(lat.mat, na.rm=T)
lat.mat.st<-(lat.mat-mean.lat)/sd.lat

mean.long<-mean(long.orig$long, na.rm=T)
sd.long<-sd(long.mat, na.rm=T)
long.st<-(long.orig$long-mean.long)/sd.long



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
                        adult = ifelse(age.mat>53,0,1), ### provide a simple classification for adults and non-adults
                        mig = as.matrix(EV.phi.matrix[,2:max(timeseries$col)]),
                        lat = lat.mat.st,
                        pop = pop.orig$pop, ##vector that assigns each bird to 1 in 4 populations - used instead of longitude as continuous covariate
                        capt = ifelse(EV$captive.raised=="N",0,1),
				                tfail = as.numeric(tag.fail.indicator),
				                tag.age = as.matrix(tag.age[,2:max(timeseries$col)]),
                        nind = dim(y.telemetry)[1],
                        n.occasions = dim(y.telemetry)[2])
#rm(list=setdiff(ls(), "INPUT.telemetry"))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPECIFY AND SET UP MODEL RUN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Parameters monitored
parameters.telemetry <- c("mean.p","mean.phi","lp.mean","b.phi.age","b.phi.mig","b.phi.capt","b.phi.lat","b.phi.pop")

# Initial values
# Function to create a matrix of initial values for latent state z
ch.init <- function(ch, f){
  for (i in 1:dim(ch)[1]){ch[i,1:f[i]] <- NA}
  return(ch)
}

inits.telemetry <- function(){list(z = ch.init(y.telemetry, f.telemetry),
                                   #mean.phi = matrix(runif(4, 0.5, 0.999),nrow=2), ### this is only valid for model 7
                                   mean.phi = runif(2, 0.5, 0.999), ### this is only valid for models 1-6
					                         base.obs = rnorm(1,0, 0.001))} 

# MCMC settings
ni <- 10
nt <- 5
nb <- 1000
nc <- 4

# # Call JAGS from R (took 70 min)
EVsurv1 <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
			"C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study\\EGVU_telemetry_CJS.jags",
			n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T)#, n.iter = ni)


EVsurv2 <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
                   "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study\\EGVU_telemetry_multistate_tagfail_phi_lp2.jags",
                   n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T)#, n.iter = ni)

EVsurv3 <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
                   "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study\\EGVU_telemetry_multistate_tagfail_phi_lp3.jags",
                   n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T)#, n.iter = ni)

EVsurv4 <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
                   "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study\\EGVU_telemetry_multistate_tagfail_phi_lp4.jags",
                   n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T)#, n.iter = ni)

EVsurv5 <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
                    "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study\\EGVU_telemetry_multistate_tagfail_phi_lp5.jags",
                    n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T)#, n.iter = ni)

EVsurv6 <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
                    "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study\\EGVU_telemetry_multistate_tagfail_phi_lp6.jags",
                    n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T)#, n.iter = ni)

EVsurv7 <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
                    "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study\\EGVU_telemetry_multistate_tagfail_phi_lp7.jags",
                    n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T)#, n.iter = ni)

EVsurv8 <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
                    "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Survival\\EV.TV.Survival.Study\\EGVU_telemetry_multistate_tagfail_phi_FINAL.jags",
                    n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T)#, n.iter = ni)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXPORT THE OUTPUT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load("EGVU_survival_CJS_output.RData")
out1<-as.data.frame(EVsurv1$summary)
out1$parameter<-row.names(EVsurv1$summary)
out1$model<-"m1"
write.table(out1,"EGVU_telemetry_survival_estimates_m1.csv", sep=",", row.names=F)

out2<-as.data.frame(EVsurv2$summary)
out2$parameter<-row.names(EVsurv2$summary)
out2$model<-"m2"
write.table(out2,"EGVU_telemetry_survival_estimates_m2.csv", sep=",", row.names=F)

out3<-as.data.frame(EVsurv3$summary)
out3$parameter<-row.names(EVsurv3$summary)
out3$model<-"m3"
write.table(out3,"EGVU_telemetry_survival_estimates_m3.csv", sep=",", row.names=F)

out4<-as.data.frame(EVsurv4$summary)
out4$parameter<-row.names(EVsurv4$summary)
out4$model<-"m4"
write.table(out4,"EGVU_telemetry_survival_estimates_m4.csv", sep=",", row.names=F)

out5<-as.data.frame(EVsurv5$summary)
out5$parameter<-row.names(EVsurv5$summary)
out5$model<-"m5"
write.table(out5,"EGVU_telemetry_survival_estimates_m5.csv", sep=",", row.names=F)

out6<-as.data.frame(EVsurv6$summary)
out6$parameter<-row.names(EVsurv6$summary)
out6$model<-"m6"
write.table(out6,"EGVU_telemetry_survival_estimates_m6.csv", sep=",", row.names=F)

out7<-as.data.frame(EVsurv7$summary)
out7$parameter<-row.names(EVsurv7$summary)
out7$model<-"m7"
write.table(out7,"EGVU_telemetry_survival_estimates_m7.csv", sep=",", row.names=F)

out8<-as.data.frame(EVsurv8$summary)
out8$parameter<-row.names(EVsurv8$summary)
out8$model<-"m8"
write.table(out8,"EGVU_telemetry_survival_estimates_m8.csv", sep=",", row.names=F)

save.image("EGVU_survival_CJS_output.RData")







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPECIFY JAGS MODEL [NEEDS TO BE RUN FIRST - ONLY ONE MODEL SHOWN HERE]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Specify model in BUGS language
sink("EGVU_telemetry_CJS.jags")
cat("
    model {
    
    # -------------------------------------------------
    # Parameters:
    # phi: monthly survival probability intercept

    # p.obs: probability to be tracked with functioning tag (=1)
    # -------------------------------------------------

    
    # Priors and constraints
    
    
    # MONTHLY SURVIVAL PROBABILITY
  for (i in 1:nind){
    for (t in f[i]:(n.occasions)){
      logit(phi[i,t]) <- lp.mean[mig[i,t]] + b.phi.age*(age[i,t])  +   ### age and migratory stage category-specific intercept and slope for non-adult bird to increase survival with age
                            b.phi.capt*(capt[i])*(adult[i,t])   +     ### survival dependent on captive-release and time since the captive bird was released as long as captive-released bird is not an adult
                            b.phi.lat*(lat[i,t]) +                      #### probability of monthly survival dependent on latitude
                            b.phi.pop[pop[i]]                             #### probability of survival varies by population
      } #t
    } #i
    
    
    #### CATEGORICAL INTERCEPTS FOR SURVIVAL PROBABILITY based on age (adult/sub-adult) and migratory stage (stationary/migratory)
    for(stagecat in 1:3) {
      mean.phi[stagecat] ~ dunif(0.5, 0.999999)   # uninformative prior for all MONTHLY survival probabilities
      lp.mean[stagecat] <- log(mean.phi[stagecat]/(1 - mean.phi[stagecat]))    # logit transformed survival intercept
    }
    
    #### SLOPE PARAMETERS FOR SURVIVAL PROBABILITY
    b.phi.age ~ dnorm(0, 0.001)                # Prior for slope of age on survival probability on logit scale
    b.phi.capt ~ dnorm(0, 0.001)         # Prior for slope of time since release on survival probability on logit scale
    b.phi.lat ~ dnorm(0, 0.001)         # Prior for slope of latitude on survival probability on logit scale

    #### OFFSET FOR POPULATIONS
    for(p in 1:4){
      b.phi.pop[p] ~ dnorm(0, 0.001)         # Prior for slope of longitude on survival probability on logit scale
    }

    # TAG FAILURE AND LOSS PROBABILITY
    for (i in 1:nind){
      for (t in f[i]:(n.occasions)){
        logit(p.obs[i,t]) <- mean.p + obs.error[i]   #### probability of observation
      } #t
    } #i
    
    for (i in 1:nind){
      obs.error[i] ~ dnorm(0, tau) ### RANDOM INDIVIDUAL EFFECT
    }
    
    # SLOPE PARAMETERS FOR OBSERVATION PROBABILITY
    mean.p ~ dnorm(0, 0.001)                # Prior for intercept of observation probability on logit scale
    sigma ~ dunif(0, 10)                     # Prior on standard deviation for random error term
    tau <- pow(sigma, -2)
    


# Likelihood 
  for (i in 1:nind){
    # Define latent state at first capture
    z[i,f[i]] <- 1

    for (t in (f[i]+1):n.occasions){

      # State process
      z[i,t] ~ dbern(mu1[i,t])
      mu1[i,t] <- phi[i,t-1] * z[i,t-1]
      
      # Observation process
      y[i,t] ~ dbern(mu2[i,t])
      mu2[i,t] <- p.obs[i,t-1] * z[i,t]
    } #t
  } #i
}
    ",fill = TRUE)
sink()




