###Load relevant libraries###
#install.packages("devtools")
library(devtools)
#install_github("dbspitz/migrateR/migrateR", build_vignettes = T)
library(adehabitatLT)
library(adehabitatHR)
library(plotrix)
library(lattice)
library(gdata)
library(nlme)
library(maps)
library(lubridate)
library(plyr)
library(data.table)
library(amt)
library(argosfilter)
library(ggplot2)
require(sp)
require(maps)
require(stringr)
require(reshape2)
require(ggthemes)
require(pander)

#Clear workspace
rm(list = ls())

#set wd
setwd("~/Google Drive/Research Projects/EV-TV Survival Study/Dataset/Final/Rev1/")

################################################################
# cleaning data
################################################################
#filter data to remove bad fixes
d = read.csv("./Data/ev.1ptperday.csv")
head(d)
unique(d$id) #231
unique(d$study.name) #15
unique(d$species) #1
unique(d$population) #5

#lubridate
summary(d$timestamp)
d$timestamp = ymd_hms(d$timestamp)
summary(d$timestamp)

#remove fixes with lat & long = 0
d<-d[!(d$long==0 & d$lat==0),]
d<-d[!(d$long<=-25 & d$species=="Neophron percnopterus"),]
d<-d[!(d$long>=70 & d$species=="Neophron percnopterus"),]

#remove height above elipsoid < -100 and > 30,000
summary(d$height.above.ellipsoid)
which(d$height.above.ellipsoid < -100)
which(d$height.above.ellipsoid > 30000)
d = d[-c(which(d$height.above.ellipsoid < -100)),]
d = d[-c(which(d$height.above.ellipsoid > 30000)),]
summary(d$height.above.ellipsoid)

#remove any rows that don't have date, lat or long
summary(d[1:3])
#d = d[complete.cases(d[,1:3]),] 

#quick plot of data
library(ggplot2)
ggplot() + annotation_map(map_data("world"), fill = 'grey')  + coord_quickmap() + theme_bw() +
  geom_path(data = d, aes(long,lat, group = id, colour = population)) + labs(x = "longitude", y = "latitude") + 
  theme(legend.title = element_blank())  #notice bad fixes in dataset

#speed filter from 'trip' package
names(d)
#d = d[,c(1,2,3,31,4:30,32:135)] # need to reorder columns to x,y,time,id
names(d)
library(trip)

#convert to 'trip'
tr = trip(d)

#run a speed filter and add a column to the data, max speed in km/hr
tr$spd = speedfilter(tr, max.speed = 25)
mean(tr$spd) #what % are not filtered out
summary(tr$spd) # number filtered

#convert to spdf for plotting
b = as(tr, "SpatialPointsDataFrame")
b2 = subset(b, b$spd == "TRUE")
plot(b2)
b1 = subset(b, b$spd == "FALSE")
plot(b1, col = "red", add = T)

#save as df
d.filtered = as.data.frame(b2)
head(d.filtered)

#quick plot of data
ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white")  + coord_quickmap() + theme_bw() +
 geom_path(data = d.filtered, aes(long,lat, group = id, colour = population), alpha = .5) + labs(x = "longitude", y = "latitude") +
 theme(legend.title = element_blank()) 

#write
write.csv(d.filtered, "./Data/ev.filtered.csv", row.names=FALSE)

#####################################################
#compute movement stats in adeHabitatLT
#####################################################
d = read.csv("./Data/ev.filtered.csv")
head(d)
names(d)
unique(d$population)
unique(d$id) #note 231 unique id

#lubridate
summary(d$timestamp)
d$timestamp = ymd_hms(d$timestamp)

#convert to ltraj
#use UTM so that distance values are calculated in meters
names(d)
b = as.ltraj(xy = d[, c("long", "lat")], date = d$timestamp, id = d$id)
b

#save ltraj as dataframe
d1 <- do.call(rbind, b)
head(d1)

#merge ltraj calcs to full dataset
d$NSD <- d1$R2n
d$ND <- sqrt(d$NSD)
d$dist <- d1$dist
d$dt.days <- d1$dt/3600/24
head(d)
names(d)

#write
write.csv(d, "./Data/ev.filtered.csv", row.names = FALSE)

########################################
#data summary 
########################################
#convert to data.table to summarize
d.dt = setDT(d)

#remove rows that have NA for columns we are summarizing here 
d.dt = d.dt[!is.na(d.dt$dist),]
d.dt = d.dt[!is.na(d.dt$dt.days),]

#check data
head(d.dt)
summary(d.dt)
names(d.dt)

#summarize
ev.tv.summary = d.dt[,.(unique(species), unique(study.name), unique(tag), 
                        min(timestamp), max(timestamp), head(lat,1), head(long,1), 
                        tail(lat,1), tail(long,1)),by = .(id)]

#add headers
head(ev.tv.summary)
names(ev.tv.summary) = c("id", "species", "study.name", "tag",  "start.date", 
                         "end.date", "start.lat", "start.long", "end.lat", "end.long")
summary(ev.tv.summary)

#add deployment duration
ev.tv.summary$deployment.duration = as.numeric(difftime(ev.tv.summary$end.date, ev.tv.summary$start.date, units = "days"))
head(ev.tv.summary)

#add fate = alive if still transmitting Oct 15, 2020
ev.tv.summary = cbind(ev.tv.summary, ifelse(ev.tv.summary$end.date > ymd(20201015), "alive", 'NA'))
names(ev.tv.summary)
names(ev.tv.summary)[12] = 'fate'
ev.tv.summary$fate = as.factor(ev.tv.summary$fate)
head(ev.tv.summary)

#add other columns
ev.tv.summary$age.at.deployment = NA
ev.tv.summary$sex = NA
ev.tv.summary$captive.raised = NA
ev.tv.summary$rehabilitated = NA
ev.tv.summary$how.fate.determined = ifelse(ev.tv.summary$fate == "alive", "still transmitting", "NA")
ev.tv.summary$cause.of.death = NA
ev.tv.summary$how.fate.determined = as.factor(ev.tv.summary$how.fate.determined)
ev.tv.summary$death.or.failure.date = NA
summary(ev.tv.summary$how.fate.determined)
ev.tv.summary$comments = NA

#write
write.csv(ev.tv.summary, "./Summary/ev.summary.quant.csv", row.names = FALSE)

#####################################################################
#merging quantitative data summary with coauthor info input in summary sheet
#####################################################################
#set wd
setwd("~/Google Drive/Research Projects/EV-TV Survival Study/Dataset/Final/Rev1/")

#Clear workspace
rm(list = ls())

#read final summaries
ev.gs = read.csv("./Summary/ev.summary.rev1.merged.csv")
ev.tv.summary = read.csv("./Summary/ev.summary.quant.csv")

#correct id
ev.tv.summary$id[ev.tv.summary$id == "Provence_2016_Ad_wild_EO5018_Salomé_8P"] <- "Provence_2016_Ad_wild_EO5018_Salome_8P"

#check for duplicates
unique(ev.gs$id)
which(duplicated(ev.gs$id))
which(duplicated(ev.tv.summary$id))

#adding blank columns that we want to match to coauthor input sheet
names(ev.gs)
names(ev.tv.summary)
ev.tv.summary$migrant = NA 
ev.tv.summary$population = NA
ev.tv.summary$transmitter.make.model = NA
ev.tv.summary$transmitter.attachment.method = NA
ev.tv.summary$transmitter.mass.grams = NA
ev.tv.summary$mortality.date = NA
ev.tv.summary$start.country = NA
ev.tv.summary$end.country = NA
ev.tv.summary$age.at.deployment.months = NA
names(ev.tv.summary)

#insert coauthor input into quantitative summary
for (i in unique(ev.gs$id)) { 
  ev.tv.summary$population[which(ev.tv.summary$id == i)] = ev.gs$population[which(ev.gs$id == i)]
  ev.tv.summary$migrant[which(ev.tv.summary$id == i)] = ev.gs$migrant[which(ev.gs$id == i)]
  ev.tv.summary$transmitter.make.model[which(ev.tv.summary$id == i)] = ev.gs$transmitter.make.model[which(ev.gs$id == i)]
  ev.tv.summary$transmitter.attachment.method[which(ev.tv.summary$id == i)] = ev.gs$transmitter.attachment.method[which(ev.gs$id == i)]
  ev.tv.summary$transmitter.mass.grams[which(ev.tv.summary$id == i)] = ev.gs$transmitter.mass.grams[which(ev.gs$id == i)]
  ev.tv.summary$fate[which(ev.tv.summary$id == i)] = ev.gs$fate[which(ev.gs$id == i)]
  ev.tv.summary$how.fate.determined[which(ev.tv.summary$id == i)] = ev.gs$how.fate.determined[which(ev.gs$id == i)]
  ev.tv.summary$cause.of.death[which(ev.tv.summary$id == i)] = ev.gs$cause.of.death[which(ev.gs$id == i)]
  ev.tv.summary$mortality.date[which(ev.tv.summary$id == i)] = ev.gs$mortality.date[which(ev.gs$id == i)]
  ev.tv.summary$start.country[which(ev.tv.summary$id == i)] = ev.gs$start.country[which(ev.gs$id == i)]
  ev.tv.summary$end.country[which(ev.tv.summary$id == i)] = ev.gs$end.country[which(ev.gs$id == i)]
  ev.tv.summary$sex[which(ev.tv.summary$id == i)] = ev.gs$sex[which(ev.gs$id == i)]
  ev.tv.summary$age.at.deployment.months[which(ev.tv.summary$id == i)] = ev.gs$age.at.deployment.months[which(ev.gs$id == i)]
  ev.tv.summary$captive.raised[which(ev.tv.summary$id == i)] = ev.gs$captive.raised[which(ev.gs$id == i)]
  ev.tv.summary$rehabilitated[which(ev.tv.summary$id == i)] = ev.gs$rehabilitated[which(ev.gs$id == i)]
  ev.tv.summary$comments[which(ev.tv.summary$id == i)] = ev.gs$comments[which(ev.gs$id == i)]
}

#drop unecessary columns
ev.tv.summary$age.at.deployment = NULL #redundant
ev.tv.summary$death.or.failure.date = NULL

#reorder columns
names(ev.tv.summary)
ev.tv.summary = ev.tv.summary[,c(2,3,1,4,5:17,19:27,18)] 

#remove some individuals that were either tagged in Africa and residents
#or that had VERY limited data, rendering it useless
unique(ev.tv.summary$id)
ev.tv.summary<-ev.tv.summary[!(ev.tv.summary$id=="Alimerah"),]
ev.tv.summary<-ev.tv.summary[!(ev.tv.summary$id=="Atena"),]
ev.tv.summary<-ev.tv.summary[!(ev.tv.summary$id=="Djibouti"),]
ev.tv.summary<-ev.tv.summary[!(ev.tv.summary$id=="Era"),]
ev.tv.summary<-ev.tv.summary[!(ev.tv.summary$id=="Fentale"),]
ev.tv.summary<-ev.tv.summary[!(ev.tv.summary$id=="Milli"),]
ev.tv.summary<-ev.tv.summary[!(ev.tv.summary$id=="Qero"),]
ev.tv.summary<-ev.tv.summary[!(ev.tv.summary$id=="Segev-Shalom 2019 I00"),]
ev.tv.summary<-ev.tv.summary[!(ev.tv.summary$id=="Teti"),]
ev.tv.summary<-ev.tv.summary[!(ev.tv.summary$id=="BatuecasP"),]
ev.tv.summary<-ev.tv.summary[!(ev.tv.summary$id=="Alolobad"),]
ev.tv.summary<-ev.tv.summary[!(ev.tv.summary$id=="Basaka"),]
ev.tv.summary<-ev.tv.summary[!(ev.tv.summary$id=="Lubo"),]
ev.tv.summary<-ev.tv.summary[!(ev.tv.summary$id=="Lucy"),]
ev.tv.summary<-ev.tv.summary[!(ev.tv.summary$id=="Semera"),]
ev.tv.summary<-ev.tv.summary[!(ev.tv.summary$id=="Mille"),]
unique(ev.tv.summary$id) #215

#remove these ids from mig_stage_matrix, as well
#did this manually because it is classified by id.tag.year

#review
head(ev.tv.summary)
summary(ev.tv.summary)
write.csv(ev.tv.summary, "./Summary/ev.summary.quant.coauthor.merged.csv", row.names = FALSE)

###################################################################
#AT THIS POINT "ev.summary.quant.coauthor.merged.csv" WAS MANUALLY REVIEW BY COAUTHORS AGAIN
#AND FATES WERE REVIEWED AND INPUT MANUALLY
#THIS MANUALLY EDITED FILE WAS NAMED "ev.summary.final.Rev1.csv"
###################################################################

###################################################################
#Final structuring of data for survival analyses
###################################################################
#load packages
library(lubridate)
library(ggplot2)
library(data.table)

#Clear workspace
rm(list = ls())

#set wd
setwd("~/Google Drive/Research Projects/EV-TV Survival Study/Dataset/Final/Rev1/")

#read data
d = read.csv("./Data/ev.filtered.csv")

#lubridate
d$timestamp = ymd_hms(d$timestamp)
summary(d)

#remove some individuals that were either tagged in Africa and residents
#or that had VERY limited data, rendering it useless
d<-d[!(d$id=="Alimerah"),]
d<-d[!(d$id=="Atena"),]
d<-d[!(d$id=="Djibouti"),]
d<-d[!(d$id=="Era"),]
d<-d[!(d$id=="Fentale"),]
d<-d[!(d$id=="Milli"),]
d<-d[!(d$id=="Qero"),]
d<-d[!(d$id=="Segev-Shalom 2019 I00"),]
d<-d[!(d$id=="Teti"),]
d<-d[!(d$id=="BatuecasP"),]
d<-d[!(d$id=="Alolobad"),]
d<-d[!(d$id=="Basaka"),]
d<-d[!(d$id=="Lubo"),]
d<-d[!(d$id=="Lucy"),]
d<-d[!(d$id=="Semera"),]
d<-d[!(d$id=="Mille"),]
unique(d$id) #215
#clean up this id
d$id[d$id=="Enciña-9FJ"] <- "Encina-9FJ"

#clean up data to relevent columns
names(d)
d = d[,c("species","study.name","population","id","tag","id.tag","timestamp","lat",
         "long","year","month","day","NSD","ND","dist","dt.days")]
d$tag = as.factor(d$tag)
d$id.tag = as.factor((d$id.tag))
d$species = as.factor(d$species)
d$study.name = as.factor(d$study.name)
d$population = as.factor(d$population)
d$id = as.factor(d$id)
summary(d)
unique(d$id.tag) #216

#age in months
#append age.at.deployment.month to each id
ev.summary = read.csv("./Summary/ev.summary.final.Rev1.csv")
names(ev.summary)
ev.summary = ev.summary[,c("species","study.name", "population", "id","tag", "start.date", "end.date",
                           "mortality.date", "start.lat", "start.long", "end.lat", "end.long", 
                           "start.country", "end.country",
                           "deployment.duration", "fate", "age.at.deployment.months", "sex",
                           "migrant", "captive.raised", "rehabilitated", "how.fate.determined", "cause.of.death",
                           "transmitter.make.model", "transmitter.attachment.method", "transmitter.mass.grams",
                           "comments")]
#clean up this id
unique(ev.summary$id)
ev.summary$id[ev.summary$id=="Enci√±a-9FJ"] <- "Encina-9FJ"
unique(ev.summary$id)

#sort data by id
d = d[order(d$id),] 
ev.summary = ev.summary[order(ev.summary$id),] 

#check if data and summary have same id
unique(ev.summary$id)== unique(d$id)
unique(ev.summary$id)[166] 
unique(d$id)[166] 
d$id = as.character(d$id)
ev.summary$id = as.character(ev.summary$id)
d$id[d$id=="Provence_2016_Ad_wild_EO5018_Salomé_8P"] <- "Provence_2016_Ad_wild_EO5018_Salome_8P"
unique(ev.summary$id) == unique(d$id) #all good

#add id.tag variable
ev.summary$id.tag = c(paste(ev.summary$id,ev.summary$tag,sep="_")) 
d$id.tag = c(paste(d$id,d$tag,sep="_")) 

#sort data by id.tag
d = d[order(d$id.tag),] 
ev.summary = ev.summary[order(ev.summary$id.tag),] 

#check if id.tag match
unique(ev.summary$id.tag) == unique(d$id.tag) #all good

#add data subset categories
#need: id, yr.mo, age.in.months, mean.monthly.nd
d$yr.mo = format(as.Date(d$timestamp), "%Y-%m")
d$id.tag.yr.mo = c(paste(d$id.tag,d$yr.mo,sep=" - ")) 
head(d)

# replace cell values for matching ids
d$age.at.deployment.month = NA
head(d)
head(ev.summary)
for (i in unique(ev.summary$id.tag)) { 
  d$age.at.deployment.month[which(d$id.tag == i)] = ev.summary$age.at.deployment.month[which(ev.summary$id.tag == i)]
}
summary(d)

#add column with date of tagging for each id.tag
d$deployment.date = d$timestamp
head(ev.summary$start.date)
ev.summary$start.date = mdy_hm(ev.summary$start.date)
summary(ev.summary$start.date)

for (i in unique(ev.summary$id.tag)) { 
  d$deployment.date[which(d$id.tag == i)] = ev.summary$start.date[which(ev.summary$id.tag == i)]
}
summary(d)

#add column with captive/wild for each individual
d$captive.raised = NA
summary(ev.summary$captive.raised)

for (i in unique(ev.summary$id.tag)) { 
  d$captive.raised[which(d$id.tag == i)] = ev.summary$captive.raised[which(ev.summary$id.tag == i)]
}
d$captive.raised = as.factor(d$captive.raised)
d$captive.raised[d$captive.raised=="y"] <- "Y"
summary(d)

#add column with mortality date for each individual
d$mortality.date = d$timestamp
head(ev.summary$mortality.date)
ev.summary$mortality.date = mdy_hm(ev.summary$mortality.date)

#if mortality date = NA, then assign to last day of study (Oct 31, 2020)
summary(ev.summary$mortality.date)
ev.summary$mortality.date[is.na(ev.summary$mortality.date)] = ymd_hms("2020-10-31 00:00:00")

for (i in unique(ev.summary$id.tag)) { 
  d$mortality.date[which(d$id.tag == i)] = ev.summary$mortality.date[which(ev.summary$id.tag == i)]
}

summary(d$mortality.date)

#censor data to mortality date
summary(d$mortality.date)
d = subset(d, d$timestamp <= d$mortality.date)
summary(d$mortality.date)
unique(d$id.tag) #214 -- THIS REMOVED 2 ID

#find and remove id.tags
unique(ev.summary$id.tag) == unique(d$id.tag)
unique(d$id.tag)[171]
unique(ev.summary$id.tag)[171]
#IDs REMOVED Were "R2_190604", REMOVE ALSO FROM EV.SUMMARY
ev.summary<-ev.summary[!(ev.summary$id.tag=="R2_190604"),]
unique(ev.summary$id.tag) == unique(d$id.tag)
unique(d$id.tag)[209]
unique(ev.summary$id.tag)[209]
#IDs REMOVED Were "Zaror I30 Red_200659", REMOVE ALSO FROM EV.SUMMARY
ev.summary<-ev.summary[!(ev.summary$id.tag=="Zaror I30 Red_200659"),]
unique(ev.summary$id.tag) == unique(d$id.tag) #all good

#add column for human imprinted
d$human.imprinted = "N"
d$human.imprinted[d$id == "Francesca"] <- "Y"
d$human.imprinted[d$id == "Zikmund"] <- "Y"
d$human.imprinted = as.factor(d$human.imprinted)
summary(d$human.imprinted)

#review
summary(d)
summary(ev.summary)

###########################################################################
#after censoring data to mortality date, need to recalculate summary stats!
#and insert into summary sheet
###########################################################################
#convert to data.table to summarize
d.dt = setDT(d)

#remove any rows with NA for dist to calculate summary stats
#THIS REMOVES THE FIRST LOCATION FROM EACH ID
d.dt = d.dt[complete.cases(d.dt[,"dist"]),] 
summary(d.dt)

#check data
head(d.dt)
summary(d.dt)
names(d.dt)

#summarize
ev.summary.mort = d.dt[,.(unique(species), unique(study.name),
                          min(timestamp), max(timestamp), head(lat,1), head(long,1), 
                          tail(lat,1), tail(long,1), mean(tail(dist,10)), mean(tail(dt.days,10))),by = .(id.tag)]
head(ev.summary.mort)

#add headers
names(ev.summary.mort) = c("id.tag", "species", "study.name", "start.date", 
                           "end.date", "start.lat", "start.long", "end.lat", "end.long", 
                           "mean.GPS.dist.last10fixes", "mean.GPS.fixrate.last10fixes")
head(ev.summary.mort)
summary(ev.summary.mort)

#add deployment duration
ev.summary.mort$deployment.duration = as.numeric(difftime(ev.summary.mort$end.date, ev.summary.mort$start.date, units = "days"))
head(ev.summary.mort)

#insert new summary calculations into the full summary sheet
unique(ev.summary.mort$id.tag) == unique(ev.summary$id.tag)
names(ev.summary.mort)
names(ev.summary)
names(ev.summary.mort)
ev.summary$mean.GPS.dist.last10fixes = NA
ev.summary$mean.GPS.fixrate.last10fixes = NA

for (i in unique(ev.summary.mort$id.tag)) { 
  ev.summary$end.date[which(ev.summary$id.tag == i)] = as.Date(ev.summary.mort$end.date[which(ev.summary.mort$id.tag == i)])
}
for (i in unique(ev.summary.mort$id.tag)) { 
  ev.summary$end.lat[which(ev.summary$id.tag == i)] = ev.summary.mort$end.lat[which(ev.summary.mort$id.tag == i)]
}
for (i in unique(ev.summary.mort$id.tag)) { 
  ev.summary$end.long[which(ev.summary$id.tag == i)] = ev.summary.mort$end.long[which(ev.summary.mort$id.tag == i)]
}
for (i in unique(ev.summary.mort$id.tag)) { 
  ev.summary$mean.GPS.dist.last10fixes[which(ev.summary$id.tag == i)] = ev.summary.mort$mean.GPS.dist.last10fixes[which(ev.summary.mort$id.tag == i)]
}
for (i in unique(ev.summary.mort$id.tag)) { 
  ev.summary$mean.GPS.fixrate.last10fixes[which(ev.summary$id.tag == i)] = ev.summary.mort$mean.GPS.fixrate.last10fixes[which(ev.summary.mort$id.tag == i)]
}
head(ev.summary)

########################################
#add start / end country
########################################
require(rgdal)
world = readOGR(dsn = "./Data/TM_WORLD_BORDERS_SIMPL-0.3/", layer = "TM_WORLD_BORDERS_SIMPL-0.3")

#convert summary to spdf
names(ev.summary)
xy.start <- ev.summary[,c("start.long","start.lat")]
spdf.start <- SpatialPointsDataFrame(coords = xy.start, data = ev.summary,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#check that the projections match
proj4string(spdf.start) == proj4string(world)
plot(spdf.start)
plot(world, add = T)

#sample and append country to summary
start.country = data.frame(over(spdf.start, world[,5]))
names(start.country) = c("start.country.quant")
start.country
ev.summary.country = cbind(ev.summary, start.country)
summary(ev.summary.country)

#end country
names(ev.summary.country)
xy.end = ev.summary.country[,c("end.long","end.lat")]
spdf.end <- SpatialPointsDataFrame(coords = xy.end, data = ev.summary.country,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#check that the prjections match
proj4string(spdf.end) == proj4string(world)
plot(spdf.end)
plot(world, add = T)

#sample and append country to summary
end.country = data.frame(over(spdf.end, world[,5]))
names(end.country) = c("end.country.quant")
end.country
ev.summary.country = cbind(ev.summary.country, end.country)
summary(ev.summary.country)
unique(ev.summary.country$id.tag) 
ev.summary = ev.summary.country

#add number of locations column
ev.summary = as.data.frame(ev.summary)
ev.summary
#library(plyr)
#library(dplyr)
n.locs = count(d.dt$id.tag)
n.locs$id.tag = n.locs$x
n.locs$n.locs = n.locs$freq
n.locs$freq = NULL
n.locs$x = NULL
head(n.locs)
ev.summary = merge(ev.summary, n.locs, by = "id.tag", all = T)
summary(ev.summary)

# Order the data frame by study
ev.summary = ev.summary[order(ev.summary$study.name),]

##############################################################
#calculate # months from tagging (pulled this function from: https://stackoverflow.com/questions/1995933/number-of-months-between-two-dates/1996404)
# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) {  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); lt$year*12 + lt$mon } 
# compute a month difference as a difference between two month's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
# calculate on dataset
d$months.from.tagging = mondf(d$deployment.date,d$timestamp)
d[,c("timestamp","deployment.date", "months.from.tagging")] #checked and looks good
summary(d)
head(d)

#remove any rows with NA for dist to calculate summary stats
#THIS REMOVES THE FIRST LOCATION FROM EACH ID
d = d[complete.cases(d[,"dist"]),] 
summary(d)

#add age.at.deployment.month + months.from.tagging to get age.in.months
d$age.in.months = d$age.at.deployment.month + d$months.from.tagging
summary(d$age.in.months)
d[,c("age.in.months","age.at.deployment.month", "months.from.tagging")]

d$age.in.months.capped = d$age.in.months
d$age.in.months.capped[d$age.in.months.capped>=54] <- 54
summary(d)

#mean monthly ND
d$mean.monthly.ND = ave(d$ND, d$id.tag.yr.mo)

#mean.monthly dist
d$mean.monthly.dist = ave(d$dist, d$id.tag.yr.mo, FUN = mean)
d$sum.monthly.dist = ave(d$dist, d$id.tag.yr.mo, FUN = sum)
head(d)
summary(d)

#mean.monthly.latitude
d$mean.monthly.lat = ave(d$lat, d$id.tag.yr.mo, FUN = mean)
d$mean.monthly.long = ave(d$long, d$id.tag.yr.mo, FUN = mean)
summary(d)

#explore data structure
d$month = as.factor(d$month)
ggplot(d, aes(month,mean.monthly.ND)) + geom_boxplot()
ggplot(d, aes(month,mean.monthly.dist)) + geom_boxplot()
ggplot(d, aes(month,sum.monthly.dist)) + geom_boxplot()
ggplot(d, aes(month,mean.monthly.lat)) + geom_boxplot()
ggplot(d, aes(month,mean.monthly.long)) + geom_boxplot()
hist(d$sum.monthly.dist)
hist(d$mean.monthly.long)
hist(d$age.at.deployment.month)
hist(d$age.in.months.capped)
hist(d$age.in.months)

#quick plot
ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white")  + 
  coord_quickmap() + theme_bw() + #geom_path(data = d, aes(long,lat, group = id, color = population)) + 
  geom_point(data = d, aes(long,lat, group = id, color = population)) +
  labs(x = "longitude", y = "latitude") +
  theme(legend.title = element_blank()) 

#check final files
summary(d)
summary(ev.summary)
head(ev.summary)
ev.summary$start.country == ev.summary$start.country.quant
ev.summary$end.country == ev.summary$end.country.quant
unique(ev.summary$id.tag)

#write edited files
#set GitHub wd to write final files
setwd("~/Google Drive/GitHub/EV.TV.Survival.Study/")
write.csv(d,"./ev.final.Rev1.survival.prepared.csv", row.names=FALSE)
write.csv(ev.summary,"./ev.summary.final.Rev1.survival.prepared.csv", row.names=FALSE)
