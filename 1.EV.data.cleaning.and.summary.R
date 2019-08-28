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
summary(d$population)

#lubridate
summary(d$timestamp)
d$timestamp = ymd_hms(d$timestamp)
summary(d$timestamp)

#remove data from before official start date that was provided by coauthors for each tag


ev.tv <- subset(ev.tv, timestamp <= as.POSIXct('2019-05-31'))

#remove fixes with lat & long = 0
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
plot(tr[tr$spd,], col = 'green', add = T)
lines(tr[tr$spd,])
maps::map("world", add = TRUE)
axis(1)
axis(2)

#convert to spdf for plotting
b = as(tr, "SpatialPointsDataFrame")
b2 = subset(b, b$spd == "TRUE")
#plot(b2)
b1 = subset(b, b$spd == "FALSE")
#plot(b1, color = "red", add = T)

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
unique(d$population)
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
#plot.ltraj(b[4])

#to look at time interval between locations plot 
#dt is measrured in second. to conver to days == dt / 3600 / 24 
#plotltr(b[1], "dt/3600/24")
#dist is measured in meters. convert to km by == / 1000
#plotltr(b[1], which = "dist/1000")
#plot net [squared] displacement
#plotltr(b[1], which = 'sqrt(R2n)')

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
write.csv(d, "ev.tv.filtered.csv", row.names = FALSE)

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
ev.tv.summary = d.dt[,.(unique(species), unique(study.name), unique(id),unique(tag), 
                        min(timestamp), max(timestamp), head(lat,1), head(long,1), 
                        tail(lat,1), tail(long,1), mean(tail(dist,10)), mean(tail(dt.days,10)),
                        mean(tail(battery.charge.percent,10))),by = .(id.tag)]

#add headers
names(ev.tv.summary) = c("id.tag", "species", "study.name", "id", "tag",  "start.date", 
                         "end.date", "start.lat", "start.long", "end.lat", "end.long", 
                         "mean.GPS.dist.last10fixes", "mean.GPS.fixrate.last10fixes",
                         "mean.battery.charge.percent.last10fixes")
head(ev.tv.summary)
summary(ev.tv.summary)

#add deployment duration
ev.tv.summary$deployment.duration = as.numeric(difftime(ev.tv.summary$end.date, ev.tv.summary$start.date, units = "days"))
head(ev.tv.summary)

#add fate = alive if still transmitting May 1
ev.tv.summary = cbind(ev.tv.summary, ifelse(ev.tv.summary$end.date > ymd(20190501), "alive", 'NA'))
names(ev.tv.summary)
names(ev.tv.summary)[16] = 'fate'
ev.tv.summary$fate = as.factor(ev.tv.summary$fate)
head(ev.tv.summary)

#add other columns, and input data from Google Sheet
ev.tv.summary$age.at.deployment = NA

#
ev.tv.summary$sex = NA

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
#ev.tv.summary = ev.tv.summary[,c(3,2,5,4,1,18,17,16,17,6:11,22,23,12,24,13,18,19,20,21)]
names(ev.tv.summary)

# Order the data frame by study
ev.tv.summary = ev.tv.summary[order(ev.tv.summary$study.name),]

#write 
head(ev.tv.summary)
summary(ev.tv.summary$start.date)
summary(ev.tv.summary$end.date)
write.csv(ev.tv.summary, "ev.tv.summary.csv", row.names = FALSE)

#quick plot of data
map.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white")  + coord_quickmap() + theme_bw() 
map.plot = map.plot + geom_path(data = d, aes(long,lat, group = id.tag), alpha = .5) + labs(x = "longitude", y = "latitude")
map.plot = map.plot + theme(legend.title = element_blank()) 
map.plot

#####################################################################
#fix date times in all files
#set wd
setwd("~/Documents/GitHub/EV - TV Survival Study/")

#Clear workspace
rm(list = ls())

#load files
ev.gs = read.csv("./Google Sheets/Egyptian Vulture tracking summary - EV summary.csv")
tv.gs = read.csv("./Google Sheets/Turkey Vulture tracking summary - TV summary.csv")
ev.tv.summary = read.csv("ev.tv.summary.csv")
balkans = read.csv("./Fate summaries/EGVU_fate_summary_Balkans.csv")
mcgrady.summary = read.csv("./Fate summaries/McGrady summaries.csv")

#check date formats
#
summary(ev.gs$start.date.adjusted)
ev.gs$start.date.adjusted = ymd(ev.gs$start.date.adjusted)
hour(ev.gs$start.date.adjusted) = 12:00
summary(ev.gs$start.date.adjusted)
#
summary(ev.gs$end.date.adjusted)
head(ev.gs)
#
names(tv.gs)
tv.gs$start.date.adjusted = NA
tv.gs$end.date.adjusted = NA
summary(tv.gs$start.date.adjusted)
summary(tv.gs$end.date.adjusted)
#
ev.tv.summary$start.date.adjusted = NA
ev.tv.summary$end.date.adjusted = NA
summary(ev.tv.summary$end.date.adjusted)
summary(ev.tv.summary$start.date.adjusted)
#
summary(balkans$start.date.adjusted)
balkans$start.date.adjusted = dmy_hm(balkans$start.date.adjusted)
summary(balkans$start.date.adjusted)
#
summary(balkans$end.date.adjusted)
balkans$end.date.adjusted = dmy_hm(balkans$end.date.adjusted)
summary(balkans$end.date.adjusted)
#
summary(mcgrady.summary$Date.ringed)
mcgrady.summary$start.date.adjusted = mdy_hm(mcgrady.summary$Date.ringed)
summary(mcgrady.summary$start.date.adjusted)

#write files with fixed date structure
write.csv(ev.gs, "./Google Sheets/Egyptian Vulture tracking summary - EV summary.csv", row.names = FALSE)
write.csv(tv.gs, "./Google Sheets/Turkey Vulture tracking summary - TV summary.csv", row.names = FALSE)
write.csv(balkans, "./Fate summaries/EGVU_fate_summary_Balkans_FINAL.csv", row.names = FALSE)
write.csv(mcgrady.summary, "./Fate summaries/McGrady summaries.csv", row.names = FALSE)
write.csv(ev.tv.summary, "ev.tv.summary.csv", row.names = FALSE)

#####################################################################
#merging data summary with coauthor info input in Google Sheet
#set wd
setwd("~/Documents/GitHub/EV - TV Survival Study/")

#Clear workspace
rm(list = ls())

#read final summaries
ev.gs = read.csv("./Google Sheets/Egyptian Vulture tracking summary - EV summary.csv", colClasses = "character")
unique(ev.gs$start.date.adjusted)
unique(ev.gs$end.date.adjusted)
ev.gs$start.date.adjusted = ymd_hms(ev.gs$start.date.adjusted)
summary(ev.gs$start.date.adjusted)
ev.gs$start.date.adjusted = as.character(ev.gs$start.date.adjusted)
unique(ev.gs$start.date.adjusted)
tv.gs = read.csv("./Google Sheets/Turkey Vulture tracking summary - TV summary.csv", colClasses = "character")
unique(tv.gs$start.date.adjusted)
unique(tv.gs$end.date.adjusted)
ev.tv.summary = read.csv("ev.tv.summary.csv", colClasses = "character")
unique(ev.tv.summary$end.date.adjusted)
unique(ev.tv.summary$start.date.adjusted)

#check id's match
unique(ev.gs$id) #Provence_2016_Ad_wild_EO5018_Salome_8P
unique(ev.tv.summary$id) # Provence_2016_Ad_wild_EO5018_Salomé_8P
ev.tv.summary$id[ev.tv.summary$id == "Provence_2016_Ad_wild_EO5018_Salomé_8P"] <- "Provence_2016_Ad_wild_EO5018_Salome_8P"
unique(ev.tv.summary$id)

#
ev.tv.summary$start

#check for duplicates
unique(ev.gs$id)
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

head(ev.tv.summary)

write.csv(ev.tv.summary, "ev.tv.summary.merged.csv", row.names = FALSE)

#read data
ev.tv.summary = read.csv("ev.tv.summary.merged.csv")
summary(ev.tv.summary)

##################################################################
#merge with Blakans sheet

#read data
ev.tv.summary = read.csv("ev.tv.summary.merged.csv", colClasses = "character")
unique(ev.tv.summary$start.date.adjusted)
balkans = read.csv("./Fate summaries/EGVU_fate_summary_Balkans.csv", colClasses = "character")
unique(balkans$start.date.adjusted)
unique(balkans$end.date.adjusted)
balkans$start.date.adjusted = dmy_hm(balkans$start.date.adjusted)
balkans$end.date.adjusted = dmy_hm(balkans$end.date.adjusted)
balkans$start.date.adjusted = as.character(balkans$start.date.adjusted)
balkans$end.date.adjusted = as.character(balkans$end.date.adjusted)
head(balkans)

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

head(ev.tv.summary)

write.csv(ev.tv.summary, "ev.tv.summary.merged.csv", row.names = FALSE)

#################################################
#merge data from McGrady csv

#Clear workspace
rm(list = ls())

ev.tv.summary.merged = read.csv("ev.tv.summary.merged.csv", colClasses = "character")
unique(ev.tv.summary.merged$start.date.adjusted)
unique(ev.tv.summary.merged$end.date.adjusted)
mcgrady.summary = read.csv("./Fate summaries/McGrady summaries.csv", colClasses = "character")
unique(mcgrady.summary$start.date.adjusted)
mcgrady.summary$start.date.adjusted = ymd(mcgrady.summary$start.date.adjusted)
hour(mcgrady.summary$start.date.adjusted) = 12:00
summary(mcgrady.summary$start.date.adjusted)
mcgrady.summary$start.date.adjusted = as.character(mcgrady.summary$start.date.adjusted)
unique(mcgrady.summary$end.date.adjusted)
mcgrady.summary$end.date.adjusted = NA
mcgrady.summary$end.date.adjusted = as.character(mcgrady.summary$end.date.adjusted)

#rename columns to match
#mcgrady.summary$start.date.adjusted = dmy(mcgrady.summary$Date.ringed)
mcgrady.summary$age.at.deployment = mcgrady.summary$Age
mcgrady.summary$comments = mcgrady.summary$Fate
mcgrady.summary$id = mcgrady.summary$PTT.ID

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
#check dataset and dates

d = read.csv("ev.tv.summary.merged.csv")
summary(d$start.date.adjusted)
d$start.date.adjusted = ymd_hms(d$start.date.adjusted)
d$end.date.adjusted = ymd_hms(d$end.date.adjusted)
summary(d)

#standardizing column values
d = read.csv("ev.tv.summary.merged.csv")
summary(d)

#sex
summary(d$sex)
d$sex[d$sex == "female"] <- "F"
d$sex[d$sex == "male"] <- "M"
d$sex[is.na(d$sex)] <- "unknown"
summary(d$sex)

#lubridate
summary(d$start.date)
d$start.date = ymd_hms(d$start.date)
d$deployment.month = month(d$start.date)
d$deployment.month = as.factor(d$deployment.month)
summary(d$deployment.month)

#set age at deployment by year
summary(d$age.at.deployment)
d$age.at.deployment.original = d$age.at.deployment
d$age.at.deployment = as.character(d$age.at.deployment)
d$age.at.deployment[d$age.at.deployment == "1"] <- "juv"
d$age.at.deployment[d$age.at.deployment == "<1"] <- "juv"
d$age.at.deployment[d$age.at.deployment == ">3 adult"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "2cal_year"] <- "2nd yr"
d$age.at.deployment[d$age.at.deployment == "2nd year"] <- "2nd yr"
d$age.at.deployment[d$age.at.deployment == "2nd Year"] <- "2nd yr"
d$age.at.deployment[d$age.at.deployment == "3cal_year"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "3rd year"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "3RD YEAR"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "4cal_year"] <- "4th yr"
d$age.at.deployment[d$age.at.deployment == "4th winter"] <- "4th yr"
d$age.at.deployment[d$age.at.deployment == "5th Year"] <- "5th yr"
d$age.at.deployment[d$age.at.deployment == "A"] <- "ad"
d$age.at.deployment[d$age.at.deployment == "ad"] <- "ad"
d$age.at.deployment[d$age.at.deployment == "adult"] <- "ad"
d$age.at.deployment[d$age.at.deployment == "Adult"] <- "ad"
d$age.at.deployment[d$age.at.deployment == "Adult (5+ years)"] <- "ad"
d$age.at.deployment[d$age.at.deployment == "ADULT (5+ years)"] <- "ad"
d$age.at.deployment[d$age.at.deployment == "Adult 5+"] <- "ad"
d$age.at.deployment[d$age.at.deployment == "AHY"] <- "2nd yr"
d$age.at.deployment[d$age.at.deployment == "ASY"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "ATY"] <- "4th yr"
d$age.at.deployment[d$age.at.deployment == "chick"] <- "juv"
d$age.at.deployment[d$age.at.deployment == "Chick"] <- "juv"
d$age.at.deployment[d$age.at.deployment == "imm, 1.5 yrt"] <- "2nd yr"
d$age.at.deployment[d$age.at.deployment == "juv"] <- "juv"
d$age.at.deployment[d$age.at.deployment == "juvenile"] <- "juv"
d$age.at.deployment[d$age.at.deployment == "SA"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (2-3 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SUBADULT (~2.5 YEARS)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SY"] <- "2nd yr"
d$age.at.deployment[d$age.at.deployment == "TY"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "imm, 1.5 yr"] <- "2nd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[d$age.at.deployment == "SA (3-4 years)"] <- "3rd yr"
d$age.at.deployment[is.na(d$age.at.deployment)] <- "unknown"
d$age.at.deployment = as.factor(d$age.at.deployment)
summary(d$age.at.deployment)
#d$age.at.deployment[d$age.at.deployment >=365] <- 2 # this number is in days. the max value is <700, so less than 2 yrs. So greater than 365 = 2nd year, less than = 1st year.
#d$age.at.deployment[d$age.at.deployment >=7] <- 1 #greatern than 7 = 1st year (there were no tagged birds <7 days old)

#set age at deployment by month, assuming birth in June #NOT DONE WITH THIS YET
d$age.at.deployment.month = paste(d$age.at.deployment, "-", d$deployment.month)
d$age.at.deployment.month = as.factor(d$age.at.deployment.month)
summary(d$age.at.deployment.month)
d$age.at.deployment.month = as.character(d$age.at.deployment.month)
d$age.at.deployment.month[d$age.at.deployment.month == "155 - 10"] <- "6" #155/30 = 5.2, i.e. more than 5 months
d$age.at.deployment.month[d$age.at.deployment.month == "171 - 12"] <- "6" #171/30 = 5.7
d$age.at.deployment.month[d$age.at.deployment.month == "182 - 12"] <- "7"
d$age.at.deployment.month[d$age.at.deployment.month == "207 - 12"] <- "7"
d$age.at.deployment.month[d$age.at.deployment.month == "208 - 12"] <- "7"
d$age.at.deployment.month[d$age.at.deployment.month == "219 - 12"] <- "8"
d$age.at.deployment.month[d$age.at.deployment.month == "223 - 12"] <- "8"
d$age.at.deployment.month[d$age.at.deployment.month == "224 - 7"] <- "8"
d$age.at.deployment.month[d$age.at.deployment.month == "292 - 4"] <- "10"
d$age.at.deployment.month[d$age.at.deployment.month == "293 - 4"] <- "10"
d$age.at.deployment.month[d$age.at.deployment.month == "297 - 11"] <- "10"
d$age.at.deployment.month[d$age.at.deployment.month == "297 - 4"] <- "10"
d$age.at.deployment.month[d$age.at.deployment.month == "298 - 4"] <- "10"
d$age.at.deployment.month[d$age.at.deployment.month == "299 - 4"] <- "10"
d$age.at.deployment.month[d$age.at.deployment.month == "2nd yr - 1"] <- "7"
d$age.at.deployment.month[d$age.at.deployment.month == "2nd yr - 11"] <- "17"
d$age.at.deployment.month[d$age.at.deployment.month == "2nd yr - 12"] <- "18"
d$age.at.deployment.month[d$age.at.deployment.month == "2nd yr - 4"] <- "10"
d$age.at.deployment.month[d$age.at.deployment.month == "2nd yr - 5"] <- "11"
d$age.at.deployment.month[d$age.at.deployment.month == "2nd yr - 7"] <- "13"
d$age.at.deployment.month[d$age.at.deployment.month == "302 - 4"] <- "11"
d$age.at.deployment.month[d$age.at.deployment.month == "305 - 4"] <- "11"
d$age.at.deployment.month[d$age.at.deployment.month == "312 - 4"] <- "11"
d$age.at.deployment.month[d$age.at.deployment.month == "324 - 4"] <- "11"
d$age.at.deployment.month[d$age.at.deployment.month == "370 - 3"] <- "13"
d$age.at.deployment.month[d$age.at.deployment.month == "392 - 3"] <- "14"
d$age.at.deployment.month[d$age.at.deployment.month == "3rd yr - 1"] <- "19"
d$age.at.deployment.month[d$age.at.deployment.month == "3rd yr - 10"] <- "28"
d$age.at.deployment.month[d$age.at.deployment.month == "3rd yr - 11"] <- "29"
d$age.at.deployment.month[d$age.at.deployment.month == "3rd yr - 12"] <- "30"
d$age.at.deployment.month[d$age.at.deployment.month == "3rd yr - 2"] <- "20"
d$age.at.deployment.month[d$age.at.deployment.month == "3rd yr - 4"] <- "22"
d$age.at.deployment.month[d$age.at.deployment.month == "3rd yr - 6"] <- "24"
d$age.at.deployment.month[d$age.at.deployment.month == "3rd yr - 7"] <- "25"
d$age.at.deployment.month[d$age.at.deployment.month == "3rd yr - 8"] <- "26"
d$age.at.deployment.month[d$age.at.deployment.month == "3rd yr - 9"] <- "27"
d$age.at.deployment.month[d$age.at.deployment.month == "4th yr - 1"] <- "31"
d$age.at.deployment.month[d$age.at.deployment.month == "4th yr - 4"] <- "34"
d$age.at.deployment.month[d$age.at.deployment.month == "4th yr - 7"] <- "37"
d$age.at.deployment.month[d$age.at.deployment.month == "543 - 12"] <- "19"
d$age.at.deployment.month[d$age.at.deployment.month == "545 - 12"] <- "19"
d$age.at.deployment.month[d$age.at.deployment.month == "549 - 12"] <- "19"
d$age.at.deployment.month[d$age.at.deployment.month == "550 - 12"] <- "19"
d$age.at.deployment.month[d$age.at.deployment.month == "551 - 12"] <- "20"
d$age.at.deployment.month[d$age.at.deployment.month == "570 - 12"] <- "20"
d$age.at.deployment.month[d$age.at.deployment.month == "577 - 12"] <- "20"
d$age.at.deployment.month[d$age.at.deployment.month == "582 - 12"] <- "21"
d$age.at.deployment.month[d$age.at.deployment.month == "5th yr - 6"] <- "54" # maxing out month count as 5th year adult at 54 months
d$age.at.deployment.month[d$age.at.deployment.month == "660 - 4"] <- "22"
d$age.at.deployment.month[d$age.at.deployment.month == "663 - 4"] <- "23"
d$age.at.deployment.month[d$age.at.deployment.month == "691 - 4"] <- "24"
d$age.at.deployment.month[d$age.at.deployment.month == "7 - 3"] <- "54"  # not sure what 7 means here -- need to check with data owner, assuming Adult for now
d$age.at.deployment.month[d$age.at.deployment.month == "701 - 4"] <- "24"
d$age.at.deployment.month[d$age.at.deployment.month == "ad - 1"] <- "54" # maxing out month count as 5th year adult at 54 months
d$age.at.deployment.month[d$age.at.deployment.month == "ad - 10"] <- "54" # maxing out month count as 5th year adult at 54 months
d$age.at.deployment.month[d$age.at.deployment.month == "ad - 11"] <- "54" # maxing out month count as 5th year adult at 54 months
d$age.at.deployment.month[d$age.at.deployment.month == "ad - 12"] <- "54" # maxing out month count as 5th year adult at 54 months
d$age.at.deployment.month[d$age.at.deployment.month == "ad - 2"] <- "54" # maxing out month count as 5th year adult at 54 months
d$age.at.deployment.month[d$age.at.deployment.month == "ad - 3"] <- "54" # maxing out month count as 5th year adult at 54 months
d$age.at.deployment.month[d$age.at.deployment.month == "ad - 4"] <- "54" # maxing out month count as 5th year adult at 54 months
d$age.at.deployment.month[d$age.at.deployment.month == "ad - 5"] <- "54" # maxing out month count as 5th year adult at 54 months
d$age.at.deployment.month[d$age.at.deployment.month == "ad - 6"] <- "54" # maxing out month count as 5th year adult at 54 months
d$age.at.deployment.month[d$age.at.deployment.month == "ad - 7"] <- "54" # maxing out month count as 5th year adult at 54 months
d$age.at.deployment.month[d$age.at.deployment.month == "ad - 8"] <- "54" # maxing out month count as 5th year adult at 54 months
d$age.at.deployment.month[d$age.at.deployment.month == "ad - 9"] <- "54" # maxing out month count as 5th year adult at 54 months
d$age.at.deployment.month[d$age.at.deployment.month == "juv - 4"] <- "10"
d$age.at.deployment.month[d$age.at.deployment.month == "juv - 6"] <- "1"
d$age.at.deployment.month[d$age.at.deployment.month == "juv - 7"] <- "1"
d$age.at.deployment.month[d$age.at.deployment.month == "juv - 8"] <- "2"
d$age.at.deployment.month[d$age.at.deployment.month == "juv - 9"] <- "3"
d$age.at.deployment.month[d$age.at.deployment.month == "unknown - 1"] <- NA #need to see if i can get info on these from data owner
d$age.at.deployment.month[d$age.at.deployment.month == "unknown - 7"] <- NA
d$age.at.deployment.month[d$age.at.deployment.month == "unknown - 8"] <- NA
d$age.at.deployment.month[d$age.at.deployment.month == "unknown - 9"] <- NA
d$age.at.deployment.month = as.factor(d$age.at.deployment.month)
summary(d$age.at.deployment.month)

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
d$fate[is.na(d$fate)] <- "unknown"
d$fate = as.factor(d$fate)
summary(d$fate)

#write
write.csv(d, "ev.tv.summary.merged.csv", row.names = F)

#clean up "how.fate.determined"
d = read.csv("ev.tv.summary.merged.csv")
d$how.fate.determined = as.character(d$how.fate.determined)
d$how.fate.determined.clean = d$how.fate.determined
unique(d$how.fate.determined.clean)
d$how.fate.determined.clean[is.na(d$how.fate.determined.clean)] <- "undetermined"
d$how.fate.determined.clean[d$how.fate.determined.clean == "Transmission ceased at a point where the bird was clearly moving along a trajectory. There was no collection of stationary locations to suggest a specific drop point, thus we suspect transmitter failure."] = "abrupt termination" 
d$how.fate.determined.clean[d$how.fate.determined.clean == "bird and tag photographed on roadside"] = "resighted" 
d$how.fate.determined.clean[d$how.fate.determined.clean == "last location in middle of Hwy 331"] = "undetermined"
d$how.fate.determined.clean[d$how.fate.determined.clean == "last location at Pigeon Point roost"] = "undetermined"
d$how.fate.determined.clean[d$how.fate.determined.clean == "last location in Key West neighborhood"] = "undetermined"
d$how.fate.determined.clean[d$how.fate.determined.clean == "last location at Boca Grande Key off Key West, FL"] = "undetermined"
d$how.fate.determined.clean[d$how.fate.determined.clean == "transmitter recovered from beach"] = "transmitter recovered"
d$how.fate.determined.clean[d$how.fate.determined.clean == "last location at water surface off FL Keys" ] = "terminated over water"
d$how.fate.determined.clean[d$how.fate.determined.clean == "transmitter recovered from dead bird"  ] = "corpse recovered"
d$how.fate.determined.clean[d$how.fate.determined.clean == "transmission from same location over 1month before ceasing"] = "static transmission"
d$how.fate.determined.clean[d$how.fate.determined.clean == "sudden transmission stop" ] = "abrupt termination"
d$how.fate.determined.clean[d$how.fate.determined.clean == "transmission ceased abruptly while bird was moving and transmitter was acting erratic"  ] = "abrupt termination"
d$how.fate.determined.clean[d$how.fate.determined.clean == "transmission from same location over several months before ceasing"] = "static transmission"
d$how.fate.determined.clean[d$how.fate.determined.clean == "transmission from same location over 30 days before ceasing"] = "static transmission"
d$how.fate.determined.clean[d$how.fate.determined.clean == "bird observed with unit on but missing antenna"] = "resighted"
d$how.fate.determined.clean[d$how.fate.determined.clean == "failed transmitter recovered from live bird"] = "resighted"
d$how.fate.determined.clean[d$how.fate.determined.clean == "transmission from same location over 3 months before ceasing"] = "static transmission"
d$how.fate.determined.clean[d$how.fate.determined.clean == "transmission ceased abruptly while bird was moving and transmitter was 11 years old"] = "abrupt termination"
d$how.fate.determined.clean[d$how.fate.determined.clean == "transmission from same location over 5 days before ceasing, last location was on island on migration"  ] = "static transmission"
d$how.fate.determined.clean[d$how.fate.determined.clean == "transmission from same location over 5 days before ceasing"] = "static transmission"
d$how.fate.determined.clean[d$how.fate.determined.clean == "transmission from same location over 3 days before ceasing"] = "static transmission"
d$how.fate.determined.clean[d$how.fate.determined.clean == "transmission ceased abruptly while bird was moving" ] = "abrupt termination"
d$how.fate.determined.clean[d$how.fate.determined.clean == "bird recaptured and transmitter removed"] = "resighted"
d$how.fate.determined.clean[d$how.fate.determined.clean == "transmission from same location over 1 month period"] = "static transmission"
d$how.fate.determined.clean[d$how.fate.determined.clean == "transmission from same location over 2 month period"] = "static transmission"
d$how.fate.determined.clean[d$how.fate.determined.clean == "transmisson from same location over a period of time" ] = "static transmission"
d$how.fate.determined.clean[d$how.fate.determined.clean == "" ] = "undetermined"
d$how.fate.determined.clean[d$how.fate.determined.clean == "tag retrieved, asked locals" ] = "transmitter recovered"
d$how.fate.determined.clean[d$how.fate.determined.clean == "carcass retrieved" ] = "carcass found"
d$how.fate.determined.clean[d$how.fate.determined.clean == "found feathers" ] = "carcass found"
d$how.fate.determined.clean[d$how.fate.determined.clean == "found carcass" ] = "carcass found"
d$how.fate.determined.clean[d$how.fate.determined.clean == "transmission ended"] = "undetermined"
d$how.fate.determined.clean[d$how.fate.determined.clean == "retrieved transmitter and carcass" ] = "carcass found"
d$how.fate.determined.clean[d$how.fate.determined.clean == "retrieved transmitter and asked locals"] = "transmitter recovered"
d$how.fate.determined.clean[d$how.fate.determined.clean == "two isolated GPS locations from April 2018 indciated the bird was close to the 2017 nest site after several months of no data_Bird never resighted so fate is unknown"] = "inferred from transmissions"
d$how.fate.determined.clean[d$how.fate.determined.clean == "resighted"] = "resighted / recaptured"
d$how.fate.determined.clean[d$how.fate.determined.clean == "recaptured"] = "resighted / recaptured"
d$how.fate.determined.clean[d$how.fate.determined.clean == "Transmitter detached due to harness failure_Bird was seen incubating at nest several days later identified by colour ring"] = "resighted / recaptured"
d$how.fate.determined.clean[d$how.fate.determined.clean == "Normal movements" ] = "inferred from transmissions"
d$how.fate.determined.clean[d$how.fate.determined.clean == "Same location for a long period, was searched for 4 months after disapearnace with nothing found"  ] = "static transmission"
d$how.fate.determined.clean[d$how.fate.determined.clean == "Last location in a house"] = "inferred from transmissions"
d$how.fate.determined.clean[d$how.fate.determined.clean == "Transmitted from same location in SA-Iraq border for a few weeks, after roosting on what seems like a military antena"] = "static transmission"
d$how.fate.determined.clean[d$how.fate.determined.clean == "GPS on roads after a few days on the ground" ] = "inferred from transmissions"
d$how.fate.determined.clean[d$how.fate.determined.clean == "corpse recovered"] = "carcass found"
d$how.fate.determined.clean[d$how.fate.determined.clean == "Seen"] = "resighted / recaptured"
d$how.fate.determined.clean[d$how.fate.determined.clean == "Found"] = "transmitter recovered" 
d$how.fate.determined.clean[d$how.fate.determined.clean == "Last location very hostile, no data for a long period"] = "inferred from transmissions"
d$how.fate.determined.clean[d$how.fate.determined.clean == "Same location for 4 months"] = "inferred from transmissions"
d$how.fate.determined.clean[d$how.fate.determined.clean == "Last location hostile and with good GSM cover, no data for a long period"] = "inferred from transmissions"
d$how.fate.determined.clean[d$how.fate.determined.clean == "still transmitting"] = "active"
d$how.fate.determined.clean[d$how.fate.determined.clean == "telemetry data: still moving"] = "active"
d$how.fate.determined.clean[d$how.fate.determined.clean == "bird / tag recovery"] = "transmitter recovered"
d$how.fate.determined.clean[d$how.fate.determined.clean == "Transmitter active and moving by \"end date\""] = "active"
unique(d$how.fate.determined.clean)

#reorder columns
names(d)
d = d[,c(2,3,4,5,1,6:11,12:31)]
names(d)
d = d[,c(1:5,17:20,6:16,21:31)]
names(d)
d = d[,c(1:9,27,10:26,28:31)]
names(d)
d = d[,c(1:12,28:29,13:16,20:24,17:19,25:27,30:31)]
names(d)
summary(d)
write.csv(d, "ev.tv.summary.merged.csv", row.names = F)

############################################################
d = read.csv("ev.tv.summary.merged.csv")
summary(d)
names(d)
summary(d$fate)
summary(d$how.fate.determined.clean)

#assign age.at.deployment by month
summary(d$age.at.deployment.original)
summary(d$age.at.deployment)

