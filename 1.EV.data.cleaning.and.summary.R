###Load relevant libraries###
##install MigrateR
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
setwd("~/Documents/GitHub/EV - TV Survival Study/")

################################################################
# cleaning data
################################################################

#filter data to remove bad fixes
d = read.csv("ev.tv.1ptperday.csv")
unique(d$id.tag)
unique(d$study.name)
unique(d$species)

#first remove fixes with lat & long = 0
d<-d[!(d$long==0 & d$lat==0),]
d<-d[!(d$long<=-25 & d$species=="Neophron percnopterus"),]
d<-d[!(d$long>=70 & d$species=="Neophron percnopterus"),]
d<-d[!(d$long<5 & d$study.name=="Neophron percnopterus Bulgaria/Greece"),]
d<-d[!(d$long<19 & d$lat>35 & d$study.name=="Neophron percnopterus Bulgaria/Greece"),]
d<-d[!(d$lat>35 & d$id=="Mille"),]
d<-d[!(d$long<=-1 & d$study.name=="Released Egyptian Vultures in Italy"),]
d<-d[!(d$long>=-40 & d$species=="Cathartes aura"),]
d<-d[!(d$lat==0 & d$species=="Cathartes aura"),]

#remove height above elipsoid < -100 and > 30,000
summary(d$height.above.ellipsoid)
which(d$height.above.ellipsoid < -100)
which(d$height.above.ellipsoid > 30000)
d = d[-c(which(d$height.above.ellipsoid < -100)),]
d = d[-c(which(d$height.above.ellipsoid > 30000)),]
summary(d$height.above.ellipsoid)

#remove any rows that don't have date, lat or long
summary(d[1:4])
#d = d[complete.cases(d[,1:4]),] 

#speed filter from 'trip' package
library(trip)

#convert to 'trip'
tr = trip(d)

#plot trip
#plot(tr)
##lines(tr)
#maps::map("world", add = TRUE)

#run a speed filter and add a column to the data, max speed in km/hr
#?speedfilter
#?sda
tr$spd = speedfilter(tr, max.speed = 17)

#what % are not filtered out? (not clear how this works...)
mean(tr$spd)
summary(tr$spd)

#plot with censored
plot(tr)
#plot(tr[tr$spd,], col = 'green', add = T)
#lines(tr[tr$spd,])
#maps::map("world", add = TRUE)
#axis(1)
#axis(2)

#convert to spdf for plotting
b = as(tr, "SpatialPointsDataFrame")
b2 = subset(b, b$spd == "TRUE")
plot(b2)
b1 = subset(b, b$spd == "FALSE")
plot(b1, color = "red", add = T)

#save as df
d.filtered = as.data.frame(b2)
head(d.filtered)

#quick plot of data
map.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white")  + coord_quickmap() + theme_bw() 
map.plot = map.plot + geom_path(data = d.filtered, aes(long,lat, group = id.tag), alpha = .5) + labs(x = "longitude", y = "latitude")
map.plot = map.plot + theme(legend.title = element_blank()) 
map.plot

#write
write.csv(d.filtered, "ev.tv.filtered.csv", row.names=FALSE)

#####################################################
#compute movement stats in adeHabitatLT
#####################################################
d = read.csv("ev.tv.filtered.csv")
head(d)
names(d)
unique(d$id.tag) #note 260 unique id.tag

#lubridate
summary(d$timestamp)
d$timestamp = ymd_hms(d$timestamp)

#convert to ltraj
#use UTM so that distance values are calculated in meters
b = as.ltraj(xy = d[, c("long", "lat")], date = d$timestamp, id = d$id.tag)
b

#index
#dx, dy, dt == describe the distance of the x and y directions and duration in time between the relocations i and i + 1
#dist == the distance between successive relocations
#R2n == the squared distance between the first relocation of the trajectory and the current relocation (net squared displacement)

#plot ltraj
plot.ltraj(b[4])

#to look at time interval between locations plot 
#dt is measrured in second. to conver to days == dt / 3600 / 24 
plotltr(b[1], "dt/3600/24")
#dist is measured in meters. convert to km by == / 1000
plotltr(b[1], which = "dist/1000")
#plot net [squared] displacement
plotltr(b[1], which = 'sqrt(R2n)')

#save ltraj as dataframe
d1 <- do.call(rbind, b)
head(d1)

#merge ltraj calcs to full dataset
d$NSD <- d1$R2n
d$ND <- sqrt(d$NSD)
d$dist <- d1$dist
d$dt.days <- d1$dt/3600/24
head(d)

#write
write.csv(d, "ev.tv.filtered.csv")

########################################
#data summary
########################################
#convert to data.table to summarize
d.dt = setDT(d)
head(d.dt)
unique(d.dt$id.tag) #note 260 unique id.tag

#create summary
ev.tv.summary = d.dt[,.(unique(species), unique(study.name), unique(id),unique(tag), min(timestamp), max(timestamp), head(lat,1), head(long,1), tail(lat,1), tail(long,1)), by = .(id.tag)]
names(ev.tv.summary) = c("id.tag", "species", "study.name", "id", "tag",  "start.date", "end.date", "start.lat", "start.long", "end.lat", "end.long")
head(ev.tv.summary)
summary(ev.tv.summary)

#add deployment duration
ev.tv.summary$deployment.duration = as.numeric(difftime(ev.tv.summary$end.date, ev.tv.summary$start.date, units = "days"))
head(ev.tv.summary)

#add fate = alive if still transmitting May 1
ev.tv.summary = cbind(ev.tv.summary, ifelse(ev.tv.summary$end.date > ymd(20190501), "alive", 'NA'))
names(ev.tv.summary)[13] = 'fate'
ev.tv.summary$fate = as.factor(ev.tv.summary$fate)
head(ev.tv.summary)

#add other columns, and input data from Google Sheet
ev.tv.summary$age.at.deployment = NA

#
ev.tv.summary$sex = NA
#ev.tv.summary$sex[which(ev.tv.summary$id == "Agata")]= 'F'
#ev.tv.summary$sex[which(ev.tv.summary$id == "Aneta")]= 'F'
#ev.tv.summary$sex[which(ev.tv.summary$id == "Arianna")]= 'F'
#ev.tv.summary$sex[which(ev.tv.summary$id == "Barabara")]= 'F'
#ev.tv.summary$sex[which(ev.tv.summary$id == "BatuecasP")]= 'F'
#ev.tv.summary$sex[which(ev.tv.summary$id == "Bianca")]= 'F'
#ev.tv.summary$sex[which(ev.tv.summary$id == "Camaces")]= 'F'

#
ev.tv.summary$captive.raised = NA
ev.tv.summary$rehabilitated = NA
ev.tv.summary$how.fate.determined = ifelse(ev.tv.summary$fate == "alive", "still transmitting", "NA")
ev.tv.summary$cause.of.death = NA
ev.tv.summary$how.fate.determined = as.factor(ev.tv.summary$how.fate.determined)
ev.tv.summary$death.or.failure.date = NA
summary(ev.tv.summary$how.fate.determined)

#add comments column
ev.tv.summary$comments = NA

#comment from observations above
ev.tv.summary$comments[which(ev.tv.summary$id == "2HP")]= 'highly intermittent fixes'
ev.tv.summary$comments[which(ev.tv.summary$id == "9FC")]= 'very few fixes'
ev.tv.summary$comments[which(ev.tv.summary$id == "18 White")]= 'very few fixes'
ev.tv.summary$comments[which(ev.tv.summary$id == "93")]= 'possible mortality / dropped tx'
ev.tv.summary$comments[which(ev.tv.summary$id == "A17 Green")]= 'errant last point?'
ev.tv.summary$comments[which(ev.tv.summary$id == "A25 Green")]= 'errant last point?'
ev.tv.summary$comments[which(ev.tv.summary$id == "Cabuk")]= 'errant last point?'
ev.tv.summary$comments[which(ev.tv.summary$id == "Iliaz")]= 'amazing figure of ontogenic migration development'
ev.tv.summary$comments[which(ev.tv.summary$id == "Levkipos")]= 'possible mortality or dropped tx in early 2015?'
ev.tv.summary$comments[which(ev.tv.summary$id == "Mille")]= 'errant last point?'
ev.tv.summary$comments[which(ev.tv.summary$id == "NeoPer_Poiares")]= 'errant last point? has a straight shot back from Africa to Spain'
ev.tv.summary$comments[which(ev.tv.summary$id == "Sarygush")]= 'possible mortality or dropped tx?'
ev.tv.summary$comments[which(ev.tv.summary$id == "Sharka")]= 'only 3 points!'
ev.tv.summary$comments[which(ev.tv.summary$id == "Carmen")]= 'looks to have died at see and tx floated long after'
ev.tv.summary$comments[which(ev.tv.summary$id == "Provence_2016_Ad_wild_EO5018_Salomé_8P")]= 'errant last point? has a straight shot from Africa back to France'
ev.tv.summary$comments[which(ev.tv.summary$id == "White 08")]= 'has a random point in the Med'
ev.tv.summary$comments[which(ev.tv.summary$id == "Yellow 04")]= 'looks like erroneous points in the Med'

#add start / end country
require(rgdal)
world = readOGR(dsn = "./TM_WORLD_BORDERS_SIMPL-0.3/", layer = "TM_WORLD_BORDERS_SIMPL-0.3")

#convert summary to spdf
names(ev.tv.summary)
xy.start <- ev.tv.summary[,c(9,8)]
spdf.start <- SpatialPointsDataFrame(coords = xy.start, data = ev.tv.summary,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#check that the prjections match
proj4string(spdf.start) == proj4string(world)
plot(spdf.start)
plot(world, add = T)

#sample and append country to summary
start.country = data.frame(over(spdf.start, world[,5]))
names(start.country) = c("start.country")
start.country
ev.tv.summary.country = cbind(ev.tv.summary, start.country)
summary(ev.tv.summary.country)

#
names(ev.tv.summary.country)
xy.end = ev.tv.summary.country[,c(11,10)]
spdf.end <- SpatialPointsDataFrame(coords = xy.end, data = ev.tv.summary.country,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#check that the prjections match
proj4string(spdf.end) == proj4string(world)
plot(spdf.end)
plot(world, add = T)

#sample and append country to summary
end.country = data.frame(over(spdf.end, world[,5]))
names(end.country) = c("end.country")
end.country
ev.tv.summary.country = cbind(ev.tv.summary.country, end.country)
summary(ev.tv.summary.country)
unique(ev.tv.summary.country$id.tag) #131 unique
ev.tv.summary = ev.tv.summary.country

#add number of locations column
ev.tv.summary = as.data.frame(ev.tv.summary)
ev.tv.summary
n.locs = count(d.dt$id.tag)
n.locs$id.tag = n.locs$x
n.locs$n.locs = n.locs$freq
n.locs$freq = NULL
n.locs$x = NULL
ev.tv.summary = merge(ev.tv.summary, n.locs, by = "id.tag", all = T)

#write data summary
head(ev.tv.summary)
summary(ev.tv.summary)
names(ev.tv.summary)
ev.tv.summary = ev.tv.summary[,c(3,2,5,4,1,15,14,16,17,6:11,22,23,12,24,13,18,19,20,21)]
names(ev.tv.summary)

# Order the data frame by study
ev.tv.summary = ev.tv.summary[order(ev.tv.summary$study.name),]

#write 
head(ev.tv.summary)
write.csv(ev.tv.summary, "ev.tv.summary.csv", row.names = FALSE)

#quick plot of data
map.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white")  + coord_quickmap() + theme_bw() 
map.plot = map.plot + geom_path(data = d, aes(long,lat, group = id.tag), alpha = .5) + labs(x = "longitude", y = "latitude")
map.plot = map.plot + theme(legend.title = element_blank()) 
map.plot

#####################################################################
#merging data summary with coauthor info input in Google Sheet
#set wd
setwd("~/Documents/GitHub/EV - TV Survival Study/")

#Clear workspace
rm(list = ls())

#read final summaries
ev.gs = read.csv("./Google Sheets/Egyptian Vulture tracking summary - EV summary.csv", colClasses = "character")
tv.gs = read.csv("./Google Sheets/Turkey Vulture tracking summary - TV summary.csv", colClasses = "character")
ev.tv.summary = read.csv("ev.tv.summary.csv", colClasses = "character")

#check id's match, manually correct
unique(ev.gs$id) #Provence_2016_Ad_wild_EO5018_Salom√©_8P
unique(ev.tv.summary$id) # Provence_2016_Ad_wild_EO5018_Salomé_8P

#check for duplicates
summary(ev.gs$id)
which(duplicated(ev.gs$id))

#remove rows that have NA for study. These are id that Guido put in the Sheet, 
#but which we don't have any data for
ev.gs =  ev.gs[!is.na(ev.gs$study.name),]

# replace cell values for matching ids
for (i in unique(ev.gs$id.tag)) { 
  
  ev.tv.summary$start.date.adjusted[which(ev.tv.summary$id.tag == i)] = ev.gs$start.date.adjusted[which(ev.gs$id.tag == i)]
  ev.tv.summary$end.date.adjusted[which(ev.tv.summary$id.tag == i)] = ev.gs$end.date.adjusted[which(ev.gs$id.tag == i)]
  ev.tv.summary$sex[which(ev.tv.summary$id.tag == i)] = ev.gs$sex[which(ev.gs$id.tag == i)]
  ev.tv.summary$age.at.deployment[which(ev.tv.summary$id.tag == i)] = ev.gs$age.at.deployment[which(ev.gs$id.tag == i)]
  ev.tv.summary$captive.raised[which(ev.tv.summary$id.tag == i)] = ev.gs$captive.raised[which(ev.gs$id.tag == i)]
  ev.tv.summary$rehabilitated[which(ev.tv.summary$id.tag == i)] = ev.gs$rehabilitated[which(ev.gs$id.tag == i)]
  ev.tv.summary$fate[which(ev.tv.summary$id.tag == i)] = ev.gs$fate[which(ev.gs$id.tag == i)]
  ev.tv.summary$how.fate.determined[which(ev.tv.summary$id.tag == i)] = ev.gs$how.fate.determined[which(ev.gs$id.tag == i)]
  ev.tv.summary$cause.of.death[which(ev.tv.summary$id.tag == i)] = ev.gs$cause.of.death[which(ev.gs$id.tag == i)]
  ev.tv.summary$comments[which(ev.tv.summary$id.tag == i)] = ev.gs$comments[which(ev.gs$id.tag == i)]
  
}

for (i in unique(tv.gs$id.tag)) { 
  
  ev.tv.summary$sex[which(ev.tv.summary$id.tag == i)] = tv.gs$sex[which(tv.gs$id.tag == i)]
  ev.tv.summary$age.at.deployment[which(ev.tv.summary$id.tag == i)] = tv.gs$age.at.deployment[which(tv.gs$id.tag == i)]
  ev.tv.summary$captive.raised[which(ev.tv.summary$id.tag == i)] = tv.gs$captive.raised[which(tv.gs$id.tag == i)]
  ev.tv.summary$rehabilitated[which(ev.tv.summary$id.tag == i)] = tv.gs$rehabilitated[which(tv.gs$id.tag == i)]
  ev.tv.summary$fate[which(ev.tv.summary$id.tag == i)] = tv.gs$fate[which(tv.gs$id.tag == i)]
  ev.tv.summary$how.fate.determined[which(ev.tv.summary$id.tag == i)] = tv.gs$how.fate.determined[which(tv.gs$id.tag == i)]
  ev.tv.summary$cause.of.death[which(ev.tv.summary$id.tag == i)] = tv.gs$cause.of.death[which(tv.gs$id.tag == i)]
  ev.tv.summary$comments[which(ev.tv.summary$id.tag == i)] = tv.gs$comments[which(tv.gs$id.tag == i)]
  
}

write.csv(ev.tv.summary, "ev.tv.summary.merged.csv", row.names = FALSE)

##################################################################
#merge with Blakans sheet

#read data
ev.tv.summary = read.csv("ev.tv.summary.merged.csv", colClasses = "character")
balkans = read.csv("./Fate summaries/EGVU_fate_summary_Balkans.csv", colClasses = "character")

#check for duplicates
which(duplicated(balkans$id))

# replace cell values for matching ids
head(ev.tv.summary)
head(balkans)
for (i in unique(balkans$id.tag)) { 
  
  ev.tv.summary$start.date.adjusted[which(ev.tv.summary$id.tag == i)] = balkans$start.date.adjusted[which(balkans$id.tag == i)]
  ev.tv.summary$end.date.adjusted[which(ev.tv.summary$id.tag == i)] = balkans$end.date.adjusted[which(balkans$id.tag == i)]
  ev.tv.summary$sex[which(ev.tv.summary$id.tag == i)] = balkans$sex[which(balkans$id.tag == i)]
  ev.tv.summary$age.at.deployment[which(ev.tv.summary$id.tag == i)] = balkans$age.at.deployment[which(balkans$id.tag == i)]
  ev.tv.summary$captive.raised[which(ev.tv.summary$id.tag == i)] = balkans$captive.raised[which(balkans$id.tag == i)]
  ev.tv.summary$rehabilitated[which(ev.tv.summary$id.tag == i)] = balkans$rehabilitated[which(balkans$id.tag == i)]
  ev.tv.summary$fate[which(ev.tv.summary$id.tag == i)] = balkans$fate[which(balkans$id.tag == i)]
  ev.tv.summary$how.fate.determined[which(ev.tv.summary$id.tag == i)] = balkans$how.fate.determined[which(balkans$id.tag == i)]
  ev.tv.summary$cause.of.death[which(ev.tv.summary$id.tag == i)] = balkans$cause.of.death[which(balkans$id.tag == i)]
  ev.tv.summary$comments[which(ev.tv.summary$id.tag == i)] = balkans$comments[which(balkans$id.tag == i)]
  
}

write.csv(ev.tv.summary, "ev.tv.summary.merged.csv", row.names = FALSE)


#################################################
#merge data from McGrady csv

#Clear workspace
rm(list = ls())

ev.tv.summary.merged = read.csv("ev.tv.summary.merged.csv", colClasses = "character")
mcgrady.summary = read.csv("./Fate summaries/McGrady summaries.csv", colClasses = "character")

#rename columns to match
#mcgrady.summary$start.date.adjusted = dmy(mcgrady.summary$Date.ringed)
mcgrady.summary$age.at.deployment = mcgrady.summary$Age
mcgrady.summary$comments = mcgrady.summary$Fate
mcgrady.summary$id = mcgrady.summary$PTT.ID
mcgrady.summary$start.date.adjusted = mcgrady.summary$Date.ringed

#check for duplicates
which(duplicated(mcgrady.summary$id))

# replace cell values for matching ids
head(ev.tv.summary.merged)
head(mcgrady.summary)

for (i in unique(mcgrady.summary$id)) { 
  
  ev.tv.summary.merged$start.date.adjusted[which(ev.tv.summary.merged$id == i)] = mcgrady.summary$start.date.adjusted[which(mcgrady.summary$id == i)]
  ev.tv.summary.merged$age.at.deployment[which(ev.tv.summary.merged$id == i)] = mcgrady.summary$age.at.deployment[which(mcgrady.summary$id == i)]
  ev.tv.summary.merged$comments[which(ev.tv.summary.merged$id == i)] = mcgrady.summary$comments[which(mcgrady.summary$id == i)]
  
}

#write
write.csv(ev.tv.summary.merged, "ev.tv.summary.merged.csv", row.names = F)

####################################################################################
#standardizing column values

d = read.csv("ev.tv.summary.merged.csv")
summary(d)

#sex
summary(d$sex)
d$sex[d$sex == "female"] <- "F"
d$sex[d$sex == "male"] <- "M"
d$sex[d$sex == "unknown"] <- NA

#age at deployment
summary(d$age.at.deployment)
d$age.at.deployment = as.character(d$age.at.deployment)
d$age.at.deployment[d$age.at.deployment == "<1"] <- "1"
d$age.at.deployment[d$age.at.deployment == ">3 adult"] <- "6"
d$age.at.deployment[d$age.at.deployment == "2cal_year"] <- "2"
d$age.at.deployment[d$age.at.deployment == "2nd year"] <- "2"
d$age.at.deployment[d$age.at.deployment == "2nd Year"] <- "2"
d$age.at.deployment[d$age.at.deployment == "3cal_year"] <- "3"
d$age.at.deployment[d$age.at.deployment == "3rd year"] <- "3"
d$age.at.deployment[d$age.at.deployment == "3RD YEAR"] <- "3"
d$age.at.deployment[d$age.at.deployment == "4cal_year"] <- "4"
d$age.at.deployment[d$age.at.deployment == "4th winter"] <- "4"
d$age.at.deployment[d$age.at.deployment == "5th Year"] <- "5"
d$age.at.deployment[d$age.at.deployment == "A"] <- "6"
d$age.at.deployment[d$age.at.deployment == "ad"] <- "6"
d$age.at.deployment[d$age.at.deployment == "adult"] <- "6"
d$age.at.deployment[d$age.at.deployment == "Adult"] <- "6"
d$age.at.deployment[d$age.at.deployment == "Adult (5+ years)"] <- "6"
d$age.at.deployment[d$age.at.deployment == "ADULT (5+ years)"] <- "6"
d$age.at.deployment[d$age.at.deployment == "Adult 5+"] <- "6"
d$age.at.deployment[d$age.at.deployment == "AHY"] <- "2"
d$age.at.deployment[d$age.at.deployment == "ASY"] <- "3"
d$age.at.deployment[d$age.at.deployment == "ATY"] <- "4"
d$age.at.deployment[d$age.at.deployment == "chick"] <- "1"
d$age.at.deployment[d$age.at.deployment == "imm, 1.5 yrt"] <- "2"
d$age.at.deployment[d$age.at.deployment == "juv"] <- "1"
d$age.at.deployment[d$age.at.deployment == "juvenile"] <- "1"
d$age.at.deployment[d$age.at.deployment == "SA"] <- "3"
d$age.at.deployment[d$age.at.deployment == "SA (2-3 years)"] <- "3"
d$age.at.deployment[d$age.at.deployment == "SUBADULT (~2.5 YEARS)"] <- "3"
d$age.at.deployment[d$age.at.deployment == "SY"] <- "2"
d$age.at.deployment[d$age.at.deployment == "TY"] <- "3"
d$age.at.deployment[d$age.at.deployment == "unknown"] <- NA
d$age.at.deployment = as.numeric(d$age.at.deployment)
summary(d$age.at.deployment)
d$age.at.deployment[d$age.at.deployment >=365] <- 2
d$age.at.deployment[d$age.at.deployment >=7] <- 1
summary(d$age.at.deployment)
d$age.at.deployment = as.factor(d$age.at.deployment)
summary(d$age.at.deployment)

#captive.raised
summary(d$captive.raised)
d$captive.raised[d$captive.raised == "captive"] <- "Y"
d$captive.raised[d$captive.raised == "N "] <- "N"
d$captive.raised[d$captive.raised == "wild"] <- "N"
summary(d$captive.raised)

#rehabilitated
summary(d$rehabilitated)
d$rehabilitated[d$rehabilitated == "N "] <- "N"
d$rehabilitated[d$rehabilitated == "no"] <- "N"
d$rehabilitated[d$rehabilitated == "yes"] <- "Y"
d$rehabilitated[d$rehabilitated == ""] <- NA
summary(d$rehabilitated)

#fate: alive, confirmed dead, likely dead, confirmed transmitter failure, likely transmitter failure
summary(d$fate)
d$fate = as.character(d$fate)
d$fate[d$fate == ""] <- NA
d$fate[d$fate == "Alive"] <- "alive"
d$fate[d$fate == "Almost certainly dead"] <- "likely dead"
d$fate[d$fate == "Dead"] <- "confirmed dead"
d$fate[d$fate == "dead"] <- "confirmed dead"
d$fate[d$fate == "Probably dead"] <- "likely dead"
d$fate[d$fate == "suspected dead"] <- "likely dead"
d$fate[d$fate == "suspected mortality"] <- "likely dead"
d$fate[d$fate == "suspected transmitter failure"] <- "likely transmitter failure"
d$fate[d$fate == "transmitter failure"] <- "confirmed transmitter failure"
d$fate[d$fate == "unknown"] <- NA
d$fate[d$fate == "verified mortality"] <- "confirmed dead"
d$fate[d$fate == "verified transmitter failure"] <- "confirmed transmitter failure"
d$fate = as.factor(d$fate)
summary(d$fate)

#write
write.csv(d, "ev.tv.summary.merged.csv", row.names = F)


#############################################################
#Figure
d = read.csv("ev.tv.filtered.csv")
ev.tv.summary.merged = read.csv("ev.tv.summary.merged.csv")
ev.tv.summary.merged.alive.removed = ev.tv.summary.merged[!(ev.tv.summary.merged$fate == "alive"),]
names(d)

#ggplot map
colourpalette<-c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999','#000120')
colourpalette
tiff("tracking.overview.tiff", units="cm", width=35, height=19, res=300)
map.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white") + coord_quickmap() + theme_bw() 
map.plot = map.plot + geom_path(data = d, aes(long,lat, group = id.tag, color = species), alpha = .5) 
map.plot = map.plot + geom_point(data = ev.tv.summary.merged, aes(start.long, start.lat, color = "tag deployment"))  
map.plot = map.plot + geom_point(data = ev.tv.summary.merged.alive.removed, aes(end.long, end.lat, color = "tag termination")) 
#map.plot = map.plot + geom_segment(data = ev.tv.summary.merge.alive.removed, aes(x = start.long, y = start.lat, xend = end.long, yend = end.lat)) 
map.plot = map.plot + scale_color_manual(values=c('#4daf4a','#377eb8','#ffff33','#e41a1c')) + labs(x = "longitude", y = "latitude")
map.plot = map.plot + theme(legend.title = element_blank()) 
map.plot = map.plot + ggtitle("vulture tracking, deployment, and termination overview") + theme(plot.title = element_text(hjust = 0.5))
map.plot
dev.off()
