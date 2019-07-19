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

#################################################################################
#plot net displacement
#tiff("ev.tv.tracking.nd.overview.tiff", units="cm", width=50, height=40, res=300)
#plot = ggplot(d, aes(timestamp, ND)) + geom_line() + facet_wrap(~ id)
#plot = plot + labs(x = "date", y = "net displacement (degrees)") + theme_bw() 
#plot 
#dev.off()

#plot ND for each id
# create for loop to produce ggplot2 graphs 
library(gridExtra)

for (i in unique(d$id)) { 
  
  #net displacement
  plot1 <- 
    ggplot(aes(timestamp, ND), data = subset(d, id ==  i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "net displacement (degrees)") 
  
  #fix rate
  plot2 <- 
    ggplot(aes(timestamp, dt.days), data = subset(d, id == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "time between fixes (days)") 
  
  #tracks
  plot3 <- 
    ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white")  + coord_quickmap() + theme_bw() +
    geom_path(data = subset(d, id ==  i), aes(long,lat)) + labs(x = "longitude", y = "latitude") + 
    theme(legend.title = element_blank()) +
    ggtitle(paste(i)) + theme(plot.title = element_text(hjust = 0.5))
  
  #arrange
  plot4 = grid.arrange(plot3, plot1, plot2, ncol = 2, nrow = 2, 
                       widths = c(1,1), layout_matrix = rbind(c(1, 2), c(1,3)))
  
  ggsave(filename = sprintf('./overview.plots/%s.png', i), plot = plot4, width = 30, height = 20, units = c("cm"),dpi = 300)
}

#check battery charge fields
#head(d)
#summary(d$battery.charge.percent)
#summary(d$battery.charging.current)
#summary(d$tag.voltage)
#summary(d$eobs.battery.voltage)
#summary(d$eobs.fix.battery.voltage)
#summary(d$U_bat_mV)
#summary(d$bat_soc_pct)
#summary(d$Battery.voltage)
#summary(d$Solar.voltage)

#battery charge
for (i in unique(d$id)) { 
  
  b1 <- 
    ggplot(aes(timestamp, battery.charge.percent), data = subset(d, id ==  i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "battery.charge.percent") 
  b2 <- 
    ggplot(aes(timestamp, battery.charging.current), data = subset(d, id == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "battery.charging.current") +
    ggtitle(paste(i)) + theme(plot.title = element_text(hjust = 0.5))
  b3 <- 
    ggplot(aes(timestamp, tag.voltage), data = subset(d, id == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "tag.voltage") 
  b5 <- 
    ggplot(aes(timestamp, eobs.battery.voltage), data = subset(d, id == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "eobs.battery.voltage") 
  b6 <- 
    ggplot(aes(timestamp, eobs.fix.battery.voltage), data = subset(d, id == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "eobs.fix.battery.voltage") 
  b7 <- 
    ggplot(aes(timestamp, U_bat_mV), data = subset(d, id == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "U_bat_mV")
  b8 <- 
    ggplot(aes(timestamp, bat_soc_pct), data = subset(d, id == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "bat_soc_pct")
  b9 <- 
    ggplot(aes(timestamp, Battery.voltage), data = subset(d, id == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "Battery.voltage")
  b10 <- 
    ggplot(aes(timestamp, Solar.voltage), data = subset(d, id == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "Solar.voltage")
  
  #arrange
  b11 = grid.arrange(b1,b2,b3,b5,b6,b7,b8,b9,b10, ncol = 3)
  
  ggsave(filename = sprintf('./battery.plots/%s.png', i), plot = b11, width = 20, height = 30, units = c("cm"),dpi = 300)
}

#2HP has highly intermittent fixes
#9FC has very few fixes
#18 White has very few fixes
#93 - possible mortality / dropped tx
#A17 Green - last point far from others
#A25 Green - last point far from others
#Cabuk - errant last point?
#Iliaz ND is such a cool figure of individual migration development
#Levkipos -- possible mortality or dropped tx in early 2015?
#Mille - errant last point?
#NeoPer_Poiares - errant last point?
#Sarygush - possible mortality or dropped tx?
#Sharka - only 3 points!

#Carmen looks to have died at see and tx floated long after
#NeoPer_Poiares has a straight shot back from Africa to Spain
#Provence_2016_Ad_wild has a straight shot from Africa back to France
#White 08 has a random point in the Med
#Yellow 04 has what looks like erroneous points in the Med

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

#####################################################################
#merging data summary with coauthor info input in Google Sheet

#Clear workspace
rm(list = ls())

#read final summaries
gs = read.csv("./Google Sheet/Egyptian Vulture tracking summary - EV summary.csv")
ev.tv.summary = read.csv("./Outputs/ev.tv.summary.csv")

#check id's match
unique(gs$id) #Provence_2016_Ad_wild_EO5018_Salom√©_8P
unique(ev.tv.summary$id) # Provence_2016_Ad_wild_EO5018_Salomé_8P

# merge data frames
ev.tv.summary.merge = merge(ev.tv.summary, gs, by = "id.tag", all = T)
summary(ev.tv.summary.merge[1])
summary(ev.tv.summary.merge[1])

#select, reorganize and rename columns
names(ev.tv.summary.merge)
ev.tv.summary.merge = ev.tv.summary.merge[c(2:5,1,29:32,10,11,35:36,12:19,44:47)]
names(ev.tv.summary.merge)
names(ev.tv.summary.merge) = c("study.name","species","tag","id","id.tag","sex","age.at.deployment",
                            "captive.raised","rehabilitated","start.date","end.date","start.date.adjusted", "end.date.adjusted",
                            "start.lat","start.long","end.lat","end.long","start.country", "end.country",        
                            "deployment.duration", "n.locs","fate","how.fate.determined", "cause.of.death","comments")
head(ev.tv.summary.merge)
ev.tv.summary.merge$end.date.adjusted = as.factor(ev.tv.summary.merge$end.date.adjusted)
summary(ev.tv.summary.merge)


#remove rows that have NA for study. These are id that Guido put in the Sheet, 
#but which we don't have any data for
ev.tv.summary.merge =  ev.tv.summary.merge[!is.na(ev.tv.summary.merge$study.name),]

# Order the data frame by study
ev.tv.summary.merge = ev.tv.summary.merge[order(ev.tv.summary.merge$study.name),]

#check data
any(duplicated(ev.tv.summary.merge$id))
unique(ev.tv.summary$id) == unique(ev.tv.summary.merge$id)

#write
write.csv(ev.tv.summary.merge, "./Outputs/ev.tv.summary.merge.csv", row.names = F)

#################################################
#merge data from Balkans sent as csv

#Clear workspace
rm(list = ls())

ev.tv.summary.merge = read.csv("./Outputs/ev.tv.summary.merge.csv")
balkans.summary = read.csv("./Fate summaries/EGVU_fate_summary_Balkans.csv")
head(ev.tv.summary.merge)

#subset
unique(balkans.summary$study.name)
unique(ev.tv.summary.merge$study.name)

#lubridate
ev.tv.summary.merge$start.date = ymd_hms(ev.tv.summary.merge$start.date)
ev.tv.summary.merge$end.date = ymd_hms(ev.tv.summary.merge$end.date)
summary(ev.tv.summary.merge$start.date.adjusted)
#ev.tv.summary.merge$start.date.adjusted = mdy_hm(ev.tv.summary.merge$start.date.adjusted)
summary(balkans.summary$end.date.adjusted)
#balkans.summary$start.date.adjusted = mdy_hm(balkans.summary$start.date.adjusted)
#balkans.summary$end.date.adjusted = mdy_hm(balkans.summary$end.date.adjusted)
summary(balkans.summary)

#remove balkans data from summary, so that we can append their updated info
ev.tv.summary.merge.balkans <-ev.tv.summary.merge[!(ev.tv.summary.merge$study.name=="Neophron percnopterus Bulgaria/Greece"),]
summary(ev.tv.summary.merge.balkans)
ev.tv.summary.merge.balkans$end.date.adjusted = as.character(ev.tv.summary.merge.balkans$end.date.adjusted)
ev.tv.summary.merge.balkans$start.date.adjusted = as.character(ev.tv.summary.merge.balkans$start.date.adjusted)

#merge (vertically)
ev.tv.summary.merge.balkans = rbind.fill(ev.tv.summary.merge.balkans, balkans.summary)
summary(ev.tv.summary.merge.balkans)
head(ev.tv.summary.merge.balkans$start.date.adjusted)
#ev.tv.summary.merge.balkans$end.date.adjusted = ymd_hms(ev.tv.summary.merge.balkans$end.date.adjusted)
#ev.tv.summary.merge.balkans$start.date.adjusted = ymd(ev.tv.summary.merge.balkans$start.date.adjusted)

# Order the data frame by id.tag
ev.tv.summary.merge.balkans = ev.tv.summary.merge.balkans[order(ev.tv.summary.merge.balkans$id.tag),]
ev.tv.summary.merge = ev.tv.summary.merge[order(ev.tv.summary.merge$id.tag),]
ev.tv.summary.merge$id.tag == ev.tv.summary.merge.balkans$id.tag

#add n.locs
names(ev.tv.summary.merge)
ev.tv.summary.merge.balkans$n.locs = ev.tv.summary.merge$n.locs
ev.tv.summary.merge.balkans$start.date = ev.tv.summary.merge$start.date
ev.tv.summary.merge.balkans$end.date = ev.tv.summary.merge$end.date

#check data
ev.tv.summary.merge.balkans = ev.tv.summary.merge.balkans[order(ev.tv.summary.merge.balkans$id.tag),]
ev.tv.summary.merge = ev.tv.summary.merge[order(ev.tv.summary.merge$id.tag),]
any(duplicated(ev.tv.summary.merge.balkans$id.tag))
unique(ev.tv.summary.merge$id) == unique(ev.tv.summary.merge.balkans$id)
ev.tv.summary.merge.balkans = ev.tv.summary.merge.balkans[order(ev.tv.summary.merge.balkans$study.name),]
names(ev.tv.summary.merge.balkans)
summary(ev.tv.summary.merge.balkans)
#ev.tv.summary.merge.balkans = ev.tv.summary.merge.balkans[c(1:11,24:25,12:23)]

#write
write.csv(ev.tv.summary.merge.balkans, "./Outputs/ev.tv.summary.merge.csv", row.names = F)

#################################################
#merge data from McGrady csv

#Clear workspace
rm(list = ls())

ev.tv.summary.merge = read.csv("./Outputs/ev.tv.summary.merge.csv")
mcgrady.summary = read.csv("./Fate summaries/McGrady summaries.csv")

#rename columns to match
#mcgrady.summary$start.date.adjusted = dmy(mcgrady.summary$Date.ringed)
mcgrady.summary$age.at.deployment = mcgrady.summary$Age
mcgrady.summary$comments = mcgrady.summary$Fate
mcgrady.summary$id = mcgrady.summary$PTT.ID
mcgrady.summary$id = as.character(mcgrady.summary$id)
ev.tv.summary.merge$age.at.deployment = as.character(ev.tv.summary.merge$age.at.deployment)
mcgrady.summary$comments = as.character(mcgrady.summary$comments)
ev.tv.summary.merge$comments = as.character(ev.tv.summary.merge$comments)
#mcgrady.summary$start.date.adjusted = as.character(mcgrady.summary$start.date.adjusted)
#ev.tv.summary.merge$start.date.adjusted = as.character(ev.tv.summary.merge$start.date.adjusted)

# replace cell values for matching ids
for (i in unique(mcgrady.summary$id)) { 
  
  ev.tv.summary.merge$start.date.adjusted[which(ev.tv.summary.merge$id == i)] = mcgrady.summary$start.date.adjusted[which(mcgrady.summary$id == i)]
  ev.tv.summary.merge$age.at.deployment[which(ev.tv.summary.merge$id == i)] = mcgrady.summary$age.at.deployment[which(mcgrady.summary$id == i)]
  ev.tv.summary.merge$comments[which(ev.tv.summary.merge$id == i)] = mcgrady.summary$comments[which(mcgrady.summary$id == i)]
  
}

#write
write.csv(ev.tv.summary.merge, "./Outputs/ev.tv.summary.merge.csv", row.names = F)

####################################################################################
#standardizing row values

#did some manual cleaning of the dataset and columns here in excel

#############################################################
#Figure
d = read.csv("./Outputs/ev.tv.filtered.csv")
ev.tv.summary.merge = read.csv("./Outputs/ev.tv.summary.merge.manualcleaning.csv")
summary(ev.tv.summary.merge)
summary(d$argos.lat1)
summary(d$argos.lat2)
summary(ev.tv.summary.merge$fate)
summary(ev.tv.summary.merge$captive.raised)
ev.tv.summary.merge.alive.removed = ev.tv.summary.merge[!ev.tv.summary.merge$fate == "alive", ]
summary(ev.tv.summary.merge.alive.removed$fate)

#ggplot map
colourpalette<-c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999','#000120')
colourpalette
tiff("./Outputs/ev.tracking.overview.alive.removed.tiff", units="cm", width=25, height=20, res=300)
map.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white") + coord_quickmap() + theme_bw() 
map.plot = map.plot + geom_path(data = d, aes(long,lat, group = id), color = '#4daf4a') 
map.plot = map.plot + geom_point(data = ev.tv.summary.merge, aes(start.long, start.lat, color = "deployment"))  
map.plot = map.plot + geom_point(data = ev.tv.summary.merge.alive.removed, aes(end.long, end.lat, color = "termination")) + labs(x = "longitude", y = "latitude")
#map.plot = map.plot + geom_segment(data = ev.tv.summary.merge.alive.removed, aes(x = start.long, y = start.lat, xend = end.long, yend = end.lat)) 
map.plot = map.plot + scale_color_manual(values=c('#377eb8','#e41a1c'))  
map.plot = map.plot + theme(legend.title = element_blank()) + theme(legend.position="bottom") 
map.plot = map.plot + ggtitle("Egyptian Vulture Tracking Overview") + theme(plot.title = element_text(hjust = 0.5))
map.plot
dev.off()
