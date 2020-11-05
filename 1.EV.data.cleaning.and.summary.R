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
ev.tv.summary = ev.tv.summary[,c(2,3,1,4,5:18,20:28,19)] 

#review
head(ev.tv.summary)
summary(ev.tv.summary)
write.csv(ev.tv.summary, "./Summary/ev.summary.quant.coauthor.merged.csv", row.names = FALSE)

####################################################################################
#standardizing column values 
####################################################################################
# did some manual editing of final summary here, incorporating coauthor input

