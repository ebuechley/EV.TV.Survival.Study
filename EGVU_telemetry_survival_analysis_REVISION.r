##########################################################################
#
# EGYPTIAN VULTURE MONTHLY SURVIVAL ANALYSIS FROM TELEMETRY
#
##########################################################################
# written by Steffen Oppel, July 2019
# data preparation by Evan Buechley, Ron Efrat, Louis Phipps, and Evan Buechley
# branched from "EGVU_telemetry_survival_analysis.r' on 9 Jan 2020
# removed all exploratory analyses, retained streamlined code for final model for inference (previously called m31)

## MAJOR CHANGES ON 7 APRIL 2020
## removed all residents
## removed age effect and simply retained 2 age groups (<18 months and >18 months)
## test 3 models with breed,mig, and winter stage and west, Balkan and Caucasus populations, either additive or interactive

## INCLUDED NEW DATA on 14 APRIL 2020 and re-ran models

## REVISION RECEIVED ON 08 OCT 2020:
## requires incorporation of random year effect
## editor wants a quadratic age effect
## potential reduction of survival interval to 1 week

## ADDED REVISED DATA ON 10 NOV 2020
## many more individuals added, plus 1.5 years worth of data added
## amended the start and end date conversion on 11 Nov 2020

## REVISED MODEL FORMULATION ON 17 NOV 2020
## remove 'sea-crossing' parameter
## include 3-level population parameter
## consider interaction between population and migration and age and migration

## ADDED INTERACTION MODELS ON 19 NOV 2020
## interaction between population and migration and age and migration
## need to remove fixed effect of population, but inserted 3-level intercept
## finalised 4 candidate models on 20 Nov 2020

## AFTER SUMMARISING RESULTS THE 3-pop INTERCEPT MAKES NO SENSE AS THE SURVIVAL ESTIMATES ARE TOO LOW
## MODIFIED ORIGINAL MODEL TO INCLUDE 2-3 MIG PARAMETERS
## incorporated revised data on 24 NOV 2020 and re-ran two simple models: mig const and 3 pop

## 25 Nov 2020: explored effect of re-classifying drowning in Med as 'likely dead' (rather than 'confirmed dead') - no problem at all!
## FINAL DISCUSSION WITH RON AND EVAN - decided to use 2pop model and examine continuous age, quadratic age, and effect of removing the rehab/recap birds


library(jagsUI)
library(tidyverse)
library(data.table)
library(lubridate)
filter<-dplyr::filter
select<-dplyr::select


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DATA FROM SPREADSHEETS AND ASSIGN FINAL STATES OF EACH ANIMAL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

try(setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study"), silent=T) ## changed after re-cloning remote
#EV<-fread("ev.tv.summary.proofed_RE4_migrantsonly.csv")   ## updated on 9 April 2020
EV<-fread("ev.summary.final.Rev1.survival.prepared.csv")   ## REVISED DATA 10 NOV 2020
EV<-EV %>% filter(id.tag!="R2_190604")
#EV<-fread("ev.summary.final.csv")   ## new file provided by Evan on 27 May 2020
#EV<-fread("FINAL_ANALYSIS_DATA.csv")   ## new file reated by Steffen on 28 May 2020 - just changed from RE4 by switching 'Apollo' from unknown to confirmed dead
EVcovar<-fread("ev.final.Rev1.survival.prepared.csv")
EVcovar %>% filter(id.tag=="R2_190604")
EV<-EV %>% filter(id.tag!="R2_190604")   ## remove one individual that is the same of a failed tag
#names(EV)[1]<-'species'
head(EV)
dim(EV)
EV$id.tag = as.character(EV$id.tag)
EVcovar$id.tag = as.character(EVcovar$id.tag)
EV$id.tag[EV$id.tag=="Clara_IHC_NA"] <- "Clara_IHC_B05AF11F" ### changed in input file of mig matrix, so we need to change that here as well
EVcovar$id.tag[EVcovar$id.tag=="Clara_IHC_NA"] <- "Clara_IHC_B05AF11F" ### changed in input file of mig matrix, so we need to change that here as well

unique(EV$fate)

#EV<-EV %>% mutate(start=mdy_hm(start.date), end= mdy_hm(end.date)) %>%
EV<-EV %>% #mutate(start=parse_date_time(start.date, c("mdy", "mdy HM")), end= parse_date_time(end.date, c("mdy", "mdy HM"))) %>%
  #mutate(start=parse_date_time(start.date, c("mdy", "mdy HM")), end= as.POSIXct(as.Date(as.numeric(end.date), origin="1970-01-01"))) %>% ### revised file has awkward date for end.date
  #mutate(start=ymd_hms(start.date), end= as.POSIXct(as.Date(as.numeric(end.date), origin="1970-01-01"))) %>% ### revised file has awkward date for end.date
  #mutate(start=ymd_hms(start.date), end= min(as.POSIXct(ymd(end.date)), ymd_hms(mortality.date))) %>% ### revised file has awkward date for end.date
  mutate(start=ymd_hms(start.date), end= if_else(ymd_hms(end.date)<ymd_hms(mortality.date),ymd_hms(end.date),ymd_hms(mortality.date))) %>% ### USE THE EARLIER OF END / MORTALITY DATE
  filter(!is.na(start)) %>%
  filter(species=="Neophron percnopterus") %>%
  filter(start<ymd_hm("2020-09-01 12:00")) %>%  ## remove birds only alive for a few months in 2020 (removes 1 bird: Baronnies_2019_Imm_wild_OR181635_5T_181635)
  #filter(rehabilitated=="N") %>%  ## remove rehabilitated adult birds
  #filter(!(fate %in% c("returned to captivity","confirmed dead") & how.fate.determined %in% c("recaptured","resighted / recaptured"))) %>%  ## remove imprinted juveniles that were recaptured
  mutate(fate=if_else(fate=="returned to captivity","confirmed dead",fate)) %>%  ## set recaptured individuals to 'confirmed dead'
  select(species,population,id.tag,sex,age.at.deployment.months,captive.raised,rehabilitated, start, end, fate, how.fate.determined,mean.GPS.dist.last10fixes, cause.of.death)  ## , mean.GPS.dist.last10fixes.degrees
head(EV)
dim(EV)


### ADD DATA NOT IN 28 MAY 2020 version of final summary
#EV$mean.GPS.dist.last10fixes.degrees<-EVtagfail$mean.GPS.dist.last10fixes.degrees[match(EV$id.tag,EVtagfail$id.tag)]

### CHANGE FATE OF APOLLO - inserted after Guido Ceccolini's comments
EV %>% filter(grepl("Apollo",id.tag))
EV$fate[EV$id.tag=="Apollo_16093"]<-"confirmed dead"
EV$how.fate.determined[EV$id.tag=="Apollo_16093"]<-"carcass found"

### CHANGE END DATE OF ANDI, HEDHJET, AND R2
EV$end[EV$id.tag=="Andi_182261"]<-ymd_hms("2020-10-30 12:00:00")
EV$end[EV$id.tag=="R2_16015"]<-ymd_hms("2020-10-30 12:00:00")
EV$end[EV$id.tag=="Hedjet_171349"]<-ymd_hms("2020-10-30 12:00:00")

### CHANGE FATE OF GURMAN (Resighted by Igor)
EV$how.fate.determined[EV$id.tag =="Gurman_114"]<-"resighted / recaptured"
EV$fate[EV$id.tag =="Gurman_114"]<-"confirmed transmitter failure"
  
### SHOW END DATES OF BIRDS ALIVE
set.to.unknown<-EV %>% filter(fate=="alive") %>% filter(end<ymd_hms("2020-08-10 23:59:59"))
update.end.date<-EV %>% filter(fate=="alive") %>% filter(end<ymd_hms("2020-09-30 23:59:59"))

### FIX END DATES AND FATE
#EV$end[EV$id.tag %in% update.end.date$id.tag]<-ymd_hms("2020-10-30 23:59:59")       ## temporary hack until Evan has sorted out final dates
#fwrite(update.end.date, "EV.end.date_update_needed.csv")

### SUM TOTAL OF TRACKING EFFORT
EV %>% mutate(end=if_else(is.na(end), ymd_hms("2020-10-30 00:00:00"), end)) %>%
  mutate(tracklength=difftime(end,start, unit="days")) %>% summarise(TOTAL=sum(tracklength)/30)

### REDUCE COVARIATES TO THOSE TAGS IN EV DATA
EVcovar<-EVcovar %>% filter(id.tag %in% EV$id.tag)


####### ASSIGNMENT OF STATES ########
unique(EV$how.fate.determined)
unique(EV$age.at.deployment)
unique(EV$fate)
unique(EV$cause.of.death)

#quest.fates<-EV %>% filter(fate!=fate.final) %>% select(id.tag,population,age.at.deployment,start,end,fate,fate.final,how.fate.determined.clean)

#### revert 3 fates to 'unknown'
#EV <- EV %>%
 # mutate(fate=ifelse(id.tag %in% quest.fates$id.tag[c(1,2,12)],"unknown",fate))


#### SELECT THE BIRDS THAT TERMINATED OVER WATER AND ARE 'CONFIRMED' DEAD
change.fate<-EV %>% filter(cause.of.death %in% c("drowned","drop in the sea","Natural barrier","Drop in the sea")) %>%
  filter(how.fate.determined  %in% c("terminated over water","inferred from transmissions")) %>%
  filter(fate=="confirmed dead") %>%
  select(population,id.tag,fate, how.fate.determined,cause.of.death)
EV$fate[EV$id.tag %in% change.fate$id.tag]<-"likely dead"


### SHOW INVENTORY OF POSSIBLE COMBINATIONS OF STATES
as.data.frame(table(EV$how.fate.determined,EV$fate)) %>%
	filter(Freq!=0) %>%
	rename(fate=Var2, how.fate.det=Var1)
EV %>% filter(fate=="confirmed dead" & how.fate.determined=="resighted / recaptured")  ### WHY CAN A CONFIRMED DEAD BIRD BE RECAPTURED?
EV %>% filter(fate=="returned to captivity")  ### check the two birds returned to captivity - now set to 'confirmed dead'
EV %>% filter(fate=="unknown" & how.fate.determined=="active")  ### WHY CAN AN ACTIVE BIRD BE UNKNOWN?



### STATE ASSIGNMENT

EV<-EV %>%

# True States (S) - these are often unknown and cannot be observed, we just need them to initialise the model (best guess)
# 1 dead
# 2 alive with functioning tag
# 3 alive with defunct tag OR without tag (when tag was lost)

  mutate(TS= ifelse(tolower(fate)=="alive",2,
                    ifelse(tolower(fate) %in% c("confirmed dead","likely dead","unknown"),1,3))) %>%
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
                    ifelse(fate %in% c("unknown","likely transmitter failure"),5,
                           ifelse(fate=="confirmed transmitter failure",3,
                                  ifelse(how.fate.determined %in% c("carcass found","resighted / recaptured","transmitter recovered"),4,2))))) %>%
  arrange(id.tag)

head(EV)

### CHECK WHETHER STATE ASSIGNMENT IS PLAUSIBLE ###
table(EV$TS,EV$OS)

EV %>% filter(OS==3 & TS==3) %>% select(id.tag,population,age.at.deployment.months,start,end,fate,fate,how.fate.determined) ## the birds observed after having lost transmitter
EV %>% filter(OS==2 & TS==3) %>% select(id.tag,population,age.at.deployment.months,start,end,fate,fate,how.fate.determined) ## birds with failed tag but not moving


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# REASSIGN POPULATIONS FOR BIRDS MIGRATING OUT OF ETHIOPIA AND REMOVE RESIDENT BIRDS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
EV$population[EV$id.tag=="Ardi_182265"]<-"middle east"
EV$population[EV$id.tag=="Awash_182257"]<-"middle east"
EV$population[EV$id.tag=="Dalol_182250"]<-"middle east"
EV$population[EV$id.tag=="Chuupa_182258"]<-"caucasus"
EV$population[EV$id.tag=="Loma_182260"]<-"caucasus"
EV$population[EV$id.tag=="Wesal_182264"]<-"caucasus"

unique(EV$population)
EV<-EV %>% filter(!(population %in% c("unknown","oman","horn of africa")))
dim(EV)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE CAPTURE HISTORY FOR SURVIVAL ESTIMATIONS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### CREATE A TIME SERIES DATA FRAME ###
mindate<-min(EV$start)
maxdate<-max(EV$end)
timeseries<-data.frame(date=seq(mindate, maxdate, "1 month")) %>%
  mutate(month=month(date),year=year(date)) %>%
  mutate(date=format(date, format="%m-%Y")) %>%
  mutate(season=ifelse(month %in% c(2,3,4,9,10), 'migration',ifelse(month %in% c(11,12,1),"winter","summer"))) %>%
  mutate(col=seq_along(date)+1)
dim(timeseries)

### CREATE YEAR AND MONTH VECTORS TO USE FOR RANDOM TIME EFFECT
years<-timeseries$year-2006
months<-timeseries$month

### CREATE BLANK MATRICES TO HOLD INFORMATION ABOUT TRUE AND OBSERVED STATES ###

EV.obs.matrix<-EV %>% select(id.tag) %>%
  arrange(id.tag)
EV.obs.matrix[,2:max(timeseries$col)]<-NA									

EV.state.matrix<-EV %>% select(id.tag) %>%
  arrange(id.tag)
EV.state.matrix[,2:max(timeseries$col)]<-NA								

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
  EV.obs.matrix[EV.obs.matrix$id.tag==n,stopcol:max(timeseries$col)]<-xl$OS   ## this assumes that state never changes - some birds may have gone off air and then been found dead - this needs manual adjustment!  
  
  ## ASSIGN INITIAL TRUE STATE (to initialise z-matrix of model)
  EV.state.matrix[EV.state.matrix$id.tag==n,(startcol+1):(stopcol-1)]<-2      ## state at first capture is known, hence must be NA in z-matrix
  EV.state.matrix[EV.state.matrix$id.tag==n,stopcol:max(timeseries$col)]<-xl$TS   ## this assumes that state never changes - some birds may have gone off air and then been found dead - this needs manual adjustment!
  EV.state.matrix[EV.state.matrix$id.tag==n,2:startcol]<-NA ## error occurs if z at first occ is not NA, so we need to specify that for birds alive for <1 month because stopcol-1 = startcol
  
}


# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # CREATE MATRIX OF SURVIVAL PARAMETERS BASED ON AGE AND SEASON
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0	No data
# 1	Stationary
# 3	Migratory - set this to 3 so we have states 1 and 2 for stationary, and 3,4,5 for migratory

EV.phi.states<-fread("Mig_stage_matrix.Rev1.csv")
head(EV.phi.states)
dim(EV.phi.states)

### arrange into full matrix ###
EV.phi.matrix<-EV.phi.states %>%
  #mutate(id.tag=substr(id.year,1,nchar(id.year)-9)) %>%
  mutate(id.tag=substr(id.tag.year,1,nchar(id.tag.year)-5)) %>%
  mutate(year=as.numeric(substr(id.tag.year,nchar(id.tag.year)-3,nchar(id.tag.year)))) %>%  
  select(-id.tag.year) %>%
  gather(key="month",value="state",-id.tag,-year) %>%
  mutate(month=as.numeric(str_replace(month,"X",""))) %>%
  mutate(date=ymd(paste(year,month,"01",sep="-"))) %>%
  mutate(date=format(date, format="%m-%Y")) %>%
  mutate(col=timeseries$col[match(date,timeseries$date)]) %>%
  filter(!is.na(col)) %>%
  mutate(state=ifelse(state==2,3,1)) %>%  ## replace 0 as there will be no parameter with index 0
  #mutate(state=ifelse(state==1,1,ifelse(month<7,2,3))) %>%  ## state==2 for spring migration and state==3 for fall migration - removed on 8 Jan 2020 to increase precision in estimates
  select(-year,-month,-date) %>%
  spread(key=col,value=state, fill=1) %>%
  arrange(id.tag)
dim(EV.phi.matrix)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIX ID TAG VALUES
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## fix provided by Evan Buechley on 3 Dec 2019
## Mig.matrix csv file amended by Ron Efrat and Steffen Oppel on 13 Nov 2020
## none of these manual re-labels should be necessary after the correction of input files

EV.phi.matrix$id.tag = as.character(EV.phi.matrix$id.tag)
#EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="1_1"] <- "52027_1"
#EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="93_14"] <- "81_14"
#EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="AF5AF11F_NA"] <-"Bianca_IHB_AF5AF11F"
#EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="B05AF11F_NA"] <- "Clara_IHC_NA" ### changed in input file to "Clara_IHC_B05AF11F"
#EV.phi.matrix$id.tag[EV.phi.matrix$id.tag=="Enciña-9FJ_5783"] <- "Encina-9FJ_5783"

EV.phi.matrix<-EV.phi.matrix %>% filter(id.tag %in% EV$id.tag) %>%
  arrange(id.tag)
dim(EV.phi.matrix)



#### FIND MISMATCH BETWEEN EV STATE MATRIX AND MIG MATRIX
unique(EV$id.tag)
EV$id.tag[which((EV$id.tag %in% EV.phi.matrix$id.tag)==FALSE)]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# USE NEW COVARIATE DATA FRAME TO SPECIFY THREE COVARIATE MATRICES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## for each occasion one value per individual that specifies age, dist moved, and lat
head(EVcovar)
dim(EVcovar)

## CREATE AGE MATRIx
age.matrix<-EVcovar %>% filter(id.tag %in% EV.obs.matrix$id.tag) %>%
  mutate(timestamp=ymd_hms(timestamp)) %>%
  #mutate(timestamp=dmy_hm(timestamp)) %>% ## necessary because Excel automatically converted date format on 13 Nov 2020
  mutate(yr.mo=format(timestamp, format="%m-%Y")) %>%
  mutate(col=timeseries$col[match(yr.mo,timeseries$date)]) %>%
  group_by(id.tag,col) %>%
  summarise(age=mean(age.in.months)) %>%   ### changed on 20 Nov 2020 from age.in.months.capped
  spread(key=col,value=age) %>%
  arrange(id.tag)

## CREATE LATITUDE MATRIX
lat.matrix<-EVcovar %>% filter(id.tag %in% EV.obs.matrix$id.tag) %>%
  mutate(timestamp=ymd_hms(timestamp)) %>%
  #mutate(timestamp=dmy_hm(timestamp)) %>% ## necessary because Excel automatically converted date format on 13 Nov 2020
  mutate(yr.mo=format(timestamp, format="%m-%Y")) %>%
  mutate(col=timeseries$col[match(yr.mo,timeseries$date)]) %>%
  group_by(id.tag,col) %>%
  summarise(lat=mean(mean.monthly.lat)) %>%
  spread(key=col,value=lat) %>%
  arrange(id.tag)

## CREATE LONGITUDE MATRIX AT ORIGIN (FIRST LONGITUDE VALUE FOR EACH BIRD)
long.orig<-EVcovar %>% filter(id.tag %in% EV.obs.matrix$id.tag) %>%
  mutate(timestamp=ymd_hms(timestamp)) %>%
  #mutate(timestamp=dmy_hm(timestamp)) %>% ## necessary because Excel automatically converted date format on 13 Nov 2020
  arrange(id.tag,timestamp) %>%
  group_by(id.tag) %>%
  summarise(long=first(mean.monthly.long)) %>%
  arrange(id.tag)

### THE ABOVE MATRICES HAVE GAPS
### FILL GAPS IN MATRICES BY LOOPING ###

for(n in EV.obs.matrix$id.tag){
  xl<-EV %>% filter(id.tag==n)
  xage<-age.matrix %>% dplyr::filter(id.tag==n)
  mindate<-format(xl$start, format="%m-%Y")
  maxdate<-format(xl$end, format="%m-%Y")
  startcol<-timeseries$col[timeseries$date==mindate]
  endcol<-timeseries$col[timeseries$date==maxdate]
  xage[startcol:endcol]

  ### ASSIGN VALUES TO ALL SUBSEQUENT INTERVALS
  for (col in min(max(timeseries$col),(endcol+1)):max(timeseries$col)){
    #age.matrix[age.matrix$id.tag==n,col]<-min((age.matrix[age.matrix$id.tag==n,(col-1)]+1),54)
    age.matrix[age.matrix$id.tag==n,col]<-(age.matrix[age.matrix$id.tag==n,(col-1)]+1) ### continuous age for sensecence analysis
    lat.matrix[lat.matrix$id.tag==n,col]<-lat.matrix[lat.matrix$id.tag==n,(col-1)]

  } ## end loop over each occasion
  
  
  misscol<-which(is.na(age.matrix[age.matrix$id.tag==n,]))
  misscol<-misscol[misscol>startcol]
  
  ### INTERPOLATE VALUES IF NA IN COLUMNS THAT SHOULD HAVE VALUES
  for (col in misscol){
    #age.matrix[age.matrix$id.tag==n,col]<-min((age.matrix[age.matrix$id.tag==n,(col-1)]+1),54)
    age.matrix[age.matrix$id.tag==n,col]<-(age.matrix[age.matrix$id.tag==n,(col-1)]+1) ### continuous age for sensecence analysis
    lat.matrix[lat.matrix$id.tag==n,col]<-lat.matrix[lat.matrix$id.tag==n,(col-1)]

  } ## end loop over each occasion

} ## end loop over each animal







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ADJUST PHI MATRIX TO REFLECT THE STAGES WE WANT TO TEST
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Option 1: stationary for summer and winter, migration for 3 geographic populations
# Option 2: stationary for summer and winter, migration

### CREATE A VULNERABLE MATRIX WITH 1/0 and 1 for juveniles from Italy/Balkans on migration 
vul.mat<-EV.phi.matrix


dim(lat.matrix)

for(row in 1:nrow(EV.phi.matrix)) {
  id<-EV.phi.matrix$id.tag[row]
  pop<-EV$population[match(id,EV$id.tag)]
  
  for(col in 2:ncol(EV.phi.matrix)) {
    age<-age.matrix[match(id,age.matrix$id.tag),col]
    age<-ifelse(is.na(age),1,age)
    if(EV.phi.matrix[row,col]==1){
      vul.mat[row,col]<-0
      EV.phi.matrix[row,col]<-ifelse(lat.matrix[row,col]>30,1,2)
      EV.phi.matrix[row,col]<-ifelse(is.na(lat.matrix[row,col]),1,EV.phi.matrix[row,col])
      EV.phi.matrix[row,col]<-ifelse(age<10,2,EV.phi.matrix[row,col])     ### b.phi.mig[1] never converges for juveniles because there are too few data, so we set this to 2 for the month after fledging.
    }
    if(EV.phi.matrix[row,col]==3){
      EV.phi.matrix[row,col]<-ifelse(pop=="western europe",3,ifelse(pop %in% c("italy","balkans"),4,5))
      vul.mat[row,col]<-ifelse(age<19,ifelse(pop %in% c("italy","balkans"),1,0),0)
    }
  }
}

dim(EV.phi.matrix)
dim(EV.obs.matrix)
dim(lat.mat)
dim(age.mat)

EV$id.tag[which((EV$id.tag %in% age.matrix$id.tag)==FALSE)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE INPUT DATA FOR JAGS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Convert to numeric matrices that JAGS can loop over
y.telemetry<-as.matrix(EV.obs.matrix[,2:max(timeseries$col)])
z.telemetry<-as.matrix(EV.state.matrix[,2:max(timeseries$col)])
age.mat<-as.matrix(age.matrix[,2:max(timeseries$col)])
lat.mat<-as.matrix(lat.matrix[,2:max(timeseries$col)])
#mig.mat<- as.matrix(EV.phi.matrix[,2:max(timeseries$col)])

#y.telemetry[74,157:159]<-
#z.telemetry[74,159]<-1   ###  bird lost at sea, but erroneously classified as transmitter failure because of 'Confirmed dead' misspelling

range(age.mat, na.rm=T)
table(age.mat)
range(lat.mat, na.rm=T)
#range(mig.mat)


#### create vector of first marking and of last alive record
get.first.telemetry<-function(x)min(which(!is.na(x)))
get.last.telemetry<-function(x)max(which(!is.na(x) & x==1))
f.telemetry<-apply(y.telemetry,1,get.first.telemetry)
l.telemetry<-apply(y.telemetry,1,get.last.telemetry)
l.telemetry[3]<-85

#### extract and standardise covariates
tag.fail.indicator<-EV$mean.GPS.dist.last10fixes

#### BUNDLE DATA INTO A LIST
INPUT.telemetry <- list(y = y.telemetry,
                        f = f.telemetry,
                        l = l.telemetry,
                        age = age.mat,
                        adult = ifelse(age.mat>18,0,1), ### provide a simple classification for adults and non-adults
                        mig = as.matrix(EV.phi.matrix[,2:max(timeseries$col)]), ### provide a simple binary classification for stationary and migratory periods
                        lat = lat.mat,
                        pop = ifelse(EV$pop %in% c("western europe"),1,0),  ##"italy",
                        pop.num = ifelse(EV$pop %in% c("western europe"),1,ifelse(EV$pop %in% c("italy","balkans"),2,3)),  ##numeric three level pop index
                        pop1 = ifelse(EV$pop %in% c("western europe"),1,0),  ##pop indicator 1 for 3pop model
                        pop2 = ifelse(EV$pop %in% c("italy","balkans"),1,0),  ##pop indicator 2 for 3pop model
                        vul = as.matrix(vul.mat[,2:max(timeseries$col)]),
                        #long = long.orig$long, ##long.mat,      ### if we want this as a continuous pop definition we would need to use just one value per bird, not a monthly value
                        capt = ifelse(EV$captive.raised=="N",0,1),
                        #resid = ifelse(EV$population %in% c("unknown","oman","horn of africa"),0,1),
				                tfail = as.numeric(tag.fail.indicator),
				                tag.age = as.matrix(tag.age[,2:max(timeseries$col)]),
                        nind = dim(y.telemetry)[1],
                        n.occasions = dim(y.telemetry)[2],
				                year=years,
				                nyears=length(unique(years)))
INPUT.telemetry$mig<-ifelse(INPUT.telemetry$mig>2,1,0) 

# EV %>% filter(population %in% c("italy","balkans")) %>% filter(age.at.deployment=="juv") %>%
#   group_by(population, fate) %>%
#   summarise(n=length(id.tag))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPECIFY AND SET UP MODEL RUNS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Parameters monitored
parameters.telemetry <- c("p.seen.alive","base.obs","base.fail","base.recover","beta1","beta2","beta3","beta4",
                          "mean.phi","lp.mean","b.phi.mig","b.phi.capt","b.phi.pop","b.phi.age","b.phi.vul","b.phi.age2","b.phi.lat",
                          "b.phi.pop1","b.phi.pop2")

# MCMC settings
ni <- 25000
nt <- 4
nb <- 1000
nc <- 3




#### PREPARE INITS ##########

inits.telemetry <- function(){list(z = z.telemetry,
                                   mean.phi = runif(1, 0.9, 1), ### two intercepts for juvenile and adults
                                   base.obs = rnorm(1,0, 0.001),                # Prior for intercept of observation probability on logit scale
                                   base.fail = rnorm(1,0, 0.001),               # Prior for intercept of tag failure probability on logit scale
                                   beta2 = rnorm(1,0, 0.001),         # Prior for slope parameter for 
                                   beta3 = rnorm(1,0, 0.001))} 


#### FINAL MODELS FOR REVISION

REV1_2pop_cat_age <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
                      "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVU_binary_additive_const_mig.jags",
                      n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni


# for continuous age model we need to curtail age at 54 months to avoid perpetual increase
INPUT.telemetry$age <- ifelse(INPUT.telemetry$age>54,54,INPUT.telemetry$age)
REV1_2pop_cont_age <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
                      "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVU_cont_age_const_mig.jags",
                      n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni

## for quadratic model we need to scale the age because it would cause very large numbers (>10,000)
agescale<-scale(1:max(age.mat, na.rm=T))
INPUT.telemetry$age <- matrix(agescale[age.mat], ncol=ncol(age.mat), nrow=nrow(age.mat))
REV1_2pop_quad_age <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
                               "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVU_quad_age_const_mig.jags",
                               n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni




# #### THE FOLLOWING MODELS WERE DISCARDED ON 25 NOVEMBER

# # REGION CONSTANT MODEL - migration cost is similar across ages and geographic regions, but survival in general varies by geographic region and age
# REV1_3pop <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                            "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVU_3pop_additive_random_year.jags",
#                            n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni)
# 
# 
# ## THE ORIGINAL MODEL FROM THE FIRST SUBMISSION PLUS RANDOM YEAR EFFECT MANDATED BY REVIEWER
# REV1_sea_cross <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                                          "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVU_binary_additive_random_year.jags",
#                                          n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni




# ## MODIFICATIONS OF THE ORIGINAL MODEL
# # 2 migration cost parameters - one for juveniles crossing the sea, 1 for everything else
# REV1_EGVU_mig2 <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                            "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVU_binary_additive_mig2.jags",
#                            n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni
# 
# # 3 migration cost parameters - one for west, one for adults east, 1 for juveniles east
# INPUT.telemetry$mig.group = ifelse(age.mat>18,3,2)
# INPUT.telemetry$mig.group = ifelse(EV$pop=="western europe" & INPUT.telemetry$mig.group>0,1,INPUT.telemetry$mig.group)  ## for three levels of migration cost
# REV1_EGVU_mig3 <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                            "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVU_binary_additive_mig3.jags",
#                            n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni
# 
# REV1_EGVU_mig_constant <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                                    "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVU_binary_additive_const_mig.jags",
#                                    n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni
# 
# # REGION INTERACTION MODEL - migration cost and survival in general vary by geographic region
# inits.telemetry <- function(){list(z = z.telemetry,
#                                    mean.phi = runif(3, 0.9, 1), ### two intercepts for juvenile and adults
#                                    base.obs = rnorm(1,0, 0.001),                # Prior for intercept of observation probability on logit scale
#                                    base.fail = rnorm(1,0, 0.001),               # Prior for intercept of tag failure probability on logit scale
#                                    beta2 = rnorm(1,0, 0.001),         # Prior for slope parameter for 
#                                    beta3 = rnorm(1,0, 0.001))} 
# REV1_EGVU_mig_by_pop <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                                         "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVU_mig_by_pop.jags",
#                                         n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni
# 
# # AGE INTERACTION MODEL - migration cost varies by age, and survival in general varies by geographic region
# agescale<-scale(1:max(age.mat, na.rm=T))
# INPUT.telemetry$age <- matrix(agescale[age.mat], ncol=ncol(age.mat), nrow=nrow(age.mat))
# REV1_EGVU_mig_by_age <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                                  "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVU_mig_by_age.jags",
#                                  n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni
# 
# # FULL INTERACTION MODEL - migration cost varies by age and geographic region
# INPUT.telemetry$pop = ifelse(EV$pop %in% c("western europe","italy"),1,0)  ## for full interaction model try to lump italy and iberia
# REV1_EGVU_mig_by_age_pop <- autojags(INPUT.telemetry, inits.telemetry, parameters.telemetry,
#                                      "C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study\\EGVU_mig_by_age_pop.jags",
#                                      n.chains = nc, n.thin = nt, n.burnin = nb, n.cores=nc, parallel=T) #, n.iter = ni







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXPORT THE OUTPUT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
try(setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study"), silent=T)
save.image("EGVU_survival_output_REV1_reduced_sample_size.RData")

load("EGVU_survival_output_REV1_FINAL.RData")






#### QUERY RAW DATA TO INFORM MODEL SPECIFICATION

EV %>% filter (population=="western europe") %>% filter(age.at.deployment.months>30) 





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPECIFY JAGS MODELS [NEEDS TO BE RUN FIRST]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Specify model for categorical age effect
sink("EGVU_binary_additive_const_mig.jags")
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
    #b.phi.lat*(lat[i,t]) +       ### survival dependent on migratory stage of the month (stationary or migratory)
    b.phi.capt*(capt[i]) +     ### survival dependent on captive-release (captive-raised or other)
    b.phi.age*(adult[i,t]) +     ### survival dependent on age (juvenile or other)
    b.phi.pop*(pop[i])  +    ### survival dependent on population (western Europe or other)
    surv.raneff[year[t]]
    } #t
    } #i
    
    #### BASELINE FOR SURVIVAL PROBABILITY (wild adult stationary from east)
    mean.phi ~ dunif(0.9, 1)   # uninformative prior for all MONTHLY survival probabilities
    lp.mean <- log(mean.phi/(1 - mean.phi))    # logit transformed survival intercept
    
    #### SLOPE PARAMETERS FOR SURVIVAL PROBABILITY
    b.phi.capt ~ dnorm(0, 0.01)         # Prior for captive effect on survival probability on logit scale
    b.phi.mig ~ dnorm(0, 0.01)          # Prior for migration effect on survival probability on logit scale
    #b.phi.lat ~ dnorm(0, 0.01)          # Prior for migration effect on survival probability on logit scale
    b.phi.age ~ dnorm(0, 0.01)            # Prior for age effect on survival probability on logit scale
    b.phi.pop ~ dunif(0, 4)         # Prior for population effect on survival probability on logit scale
    
    
    #### TAG FAILURE AND LOSS PROBABILITY
    for (i in 1:nind){
    for (t in f[i]:(n.occasions)){
    logit(p.obs[i,t]) <- base.obs ##+ beta1*(t-l[i]) #### probability of observation GIVEN THAT TAG IS WORKING is reciprocal to time since last good record
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




# Specify model for continuous age effect
sink("EGVU_cont_age_const_mig.jags")
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
    b.phi.age*(age[i,t]) +     ### survival dependent on age (juvenile or other)
    b.phi.pop*(pop[i])  +    ### survival dependent on population (western Europe or other)
    surv.raneff[year[t]]
    } #t
    } #i
    
    #### BASELINE FOR SURVIVAL PROBABILITY (wild adult stationary from east)
    mean.phi ~ dunif(0.9, 1)   # uninformative prior for all MONTHLY survival probabilities
    lp.mean <- log(mean.phi/(1 - mean.phi))    # logit transformed survival intercept
    
    #### SLOPE PARAMETERS FOR SURVIVAL PROBABILITY
    b.phi.capt ~ dnorm(0, 0.01)         # Prior for captive effect on survival probability on logit scale
    b.phi.mig ~ dnorm(0, 0.01)          # Prior for migration effect on survival probability on logit scale
    b.phi.age ~ dnorm(0, 0.01)            # Prior for age effect on survival probability on logit scale
    b.phi.pop ~ dunif(0,4)         # Prior for population effect on survival probability on logit scale
    
    
    #### TAG FAILURE AND LOSS PROBABILITY
    for (i in 1:nind){
    for (t in f[i]:(n.occasions)){
    logit(p.obs[i,t]) <- base.obs ##+ beta1*(t-l[i]) #### probability of observation GIVEN THAT TAG IS WORKING is reciprocal to time since last good record
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




# Specify model for quadratic age effect
sink("EGVU_quad_age_const_mig.jags")
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
    b.phi.age*(age[i,t]) +  b.phi.age2*(pow(age[i,t],2)) +    ### survival dependent on age (juvenile or other)
    b.phi.pop*(pop[i])  +    ### survival dependent on population (western Europe or other)
    surv.raneff[year[t]]
    } #t
    } #i
    
    #### BASELINE FOR SURVIVAL PROBABILITY (wild adult stationary from east)
    mean.phi ~ dunif(0.9, 1)   # uninformative prior for all MONTHLY survival probabilities
    lp.mean <- log(mean.phi/(1 - mean.phi))    # logit transformed survival intercept
    
    #### SLOPE PARAMETERS FOR SURVIVAL PROBABILITY
    b.phi.capt ~ dnorm(0, 0.01)         # Prior for captive effect on survival probability on logit scale
    b.phi.mig ~ dnorm(0, 0.01)          # Prior for migration effect on survival probability on logit scale
    b.phi.age ~ dnorm(0, 0.01)            # Prior for age effect on survival probability on logit scale
    b.phi.age2 ~ dnorm(0, 0.01)            # Prior for quadratic age effect on survival probability on logit scale
    b.phi.pop ~ dunif(0,4)         # Prior for population effect on survival probability on logit scale
    
    
    #### TAG FAILURE AND LOSS PROBABILITY
    for (i in 1:nind){
    for (t in f[i]:(n.occasions)){
    logit(p.obs[i,t]) <- base.obs ##+ beta1*(t-l[i]) #### probability of observation GIVEN THAT TAG IS WORKING is reciprocal to time since last good record
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