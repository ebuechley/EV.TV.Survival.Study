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
#read in merged dataset
ev.all = read.csv("ev.all.csv")

#rename/simplify column headers
colnames(ev.all)[colnames(ev.all)=="location.long"] <- "long"
colnames(ev.all)[colnames(ev.all)=="location.lat"] <- "lat"
colnames(ev.all)[colnames(ev.all)=="tag.local.identifier"] <- "tag"
colnames(ev.all)[colnames(ev.all)=="individual.local.identifier"] <- "id"
colnames(ev.all)[colnames(ev.all)=="individual.taxon.canonical.name"] <- "species"
ev.all$id = as.factor(ev.all$id)
summary(ev.all$id)
head(ev.all)

###Create burst by ID and tag
ev.all$id.tag <- c(paste(ev.all$id,ev.all$tag,sep="_")) 
ev.all$id.tag <- as.factor(ev.all$id.tag) 
unique(ev.all$id.tag) #note 156 unique id.tag

#lubridate
summary(ev.all$timestamp)
ev.all$timestamp = ymd_hms(ev.all$timestamp)
summary(ev.all$timestamp)

#limit to one point per day
ev.all$year <- year(ev.all$timestamp)
ev.all$month <- month(ev.all$timestamp)
ev.all$day = day(ev.all$timestamp)
ev.all$hour <- hour(ev.all$timestamp)
ev.all = ev.all[!duplicated(ev.all[,c('id', 'year', 'month', 'day')]),]
unique(ev.all$id.tag) #note 156 unique id.tag

#remove any rows that don't have date, lat or long
names(ev.all)
ev.all = ev.all[complete.cases(ev.all[,3:5]),] 
unique(ev.all$id.tag) #note 156 unique id.tag

#remove data from after May 31, 2019 (setting a cutoff point for the study data)
summary(ev.all$timestamp)
which(ev.all$timestamp >= as.POSIXct('2019-05-31'))
ev.all <- subset(ev.all, timestamp <= as.POSIXct('2019-05-31'))
summary(ev.all$timestamp)
unique(ev.all$id.tag) #THIS PROCESS REMOVED 1 ID.TAG

#remove species other than ev
ev.all$species = as.factor(ev.all$species)
summary(ev.all$species)
ev.all$species = "Neophron percnopterus"

#reorder dataframe to have x,y,date,id.tag as first four columns
names(ev.all)
ev.all = ev.all[,c(4,5,3,163,1:2,6:162,164:167)]
names(ev.all)

# Order the data frame by date
ev.all = ev.all[order(ev.all$timestamp),]
unique(ev.all$id.tag) #note 155 unique id.tag

#quick plot of data
map.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey')  + coord_quickmap() + theme_bw() 
map.plot = map.plot + geom_path(data = ev.all, aes(long,lat, group = id.tag)) + labs(x = "longitude", y = "latitude")
map.plot = map.plot + theme(legend.title = element_blank()) 
map.plot #notice bad fixes in dataset

#write 1ptperday dataset
write.csv(ev.all, "./Outputs/ev.all.1ptperday.csv", row.names=FALSE)

########################################
#filter data to remove bad fixes
########################################
setwd("~/Documents/GitHub/EV - TV Survival Study/")
ev.all = read.csv("./Outputs/ev.all.1ptperday.csv")
unique(ev.all$id.tag) #Note 155 unique id.tag
unique(ev.all$study.name)

#first remove fixes with lat & long = 0
ev.all<-ev.all[!(ev.all$long==0 & ev.all$lat==0),]
ev.all<-ev.all[!(ev.all$long<=-25),]
ev.all<-ev.all[!(ev.all$long>=70),]
ev.all<-ev.all[!(ev.all$long<5 & ev.all$study.name=="Neophron percnopterus Bulgaria/Greece"),]
ev.all<-ev.all[!(ev.all$long<19 & ev.all$lat>35 & ev.all$study.name=="Neophron percnopterus Bulgaria/Greece"),]
ev.all<-ev.all[!(ev.all$lat>35 & ev.all$id=="Mille"),]
ev.all<-ev.all[!(ev.all$long<=-1 & ev.all$study.name=="Released Egyptian Vultures in Italy"),]

#remove height above elipsoid < -100 and > 30,000
summary(ev.all$height.above.ellipsoid)
which(ev.all$height.above.ellipsoid < -100)
which(ev.all$height.above.ellipsoid > 30000)
ev.all = ev.all[-c(which(ev.all$height.above.ellipsoid < -100)),]
ev.all = ev.all[-c(which(ev.all$height.above.ellipsoid > 30000)),]
summary(ev.all$height.above.ellipsoid)

#remove any rows that don't have date, lat or long
summary(ev.all[1:4])
ev.all = ev.all[complete.cases(ev.all[,1:4]),] 

#speed filter from 'trip' package
library(trip)

#convert to 'trip'
tr = trip(ev.all)

#plot trip
#plot(tr)
##lines(tr)
#maps::map("world", add = TRUE)

#run a speed filter and add a column to the data, max speed in km/hr
#?speedfilter
#?sda
tr$spd = speedfilter(tr, max.speed = 15)

#what % are not filtered out? (not clear how this works...)
mean(tr$spd)
summary(tr$spd)

#plot with censored
#plot(tr)
#plot(tr[tr$spd,], col = 'green', add = T)
#lines(tr[tr$spd,])
#maps::map("world", add = TRUE)
#axis(1)
#axis(2)

#convert to spdf for plotting
d = as(tr, "SpatialPointsDataFrame")
d1 = subset(d, d$spd == "FALSE")
plot(d1)
d2 = subset(d, d$spd == "TRUE")
plot(d2)

#save as df
ev.all.filtered = as.data.frame(d2)
head(ev.all.filtered)

#quick plot of data
map.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white")  + coord_quickmap() + theme_bw() 
map.plot = map.plot + geom_path(data = ev.all.filtered, aes(long,lat, group = id.tag), alpha = .5) + labs(x = "longitude", y = "latitude")
map.plot = map.plot + theme(legend.title = element_blank()) 
map.plot #notice bad fixes in dataset

#write
write.csv(ev.all.filtered, "./Outputs/ev.all.filtered.csv", row.names=FALSE)

#####################################################
#compute movement stats in adeHabitatLT
#####################################################
setwd("~/Documents/GitHub/EV - TV Survival Study/")
ev.all.filtered = read.csv("./ev.all.filtered.csv")
head(ev.all.filtered)
names(ev.all.filtered)
unique(ev.all$id.tag) #note 155 unique id.tag

#lubridate
ev.all.filtered$timestamp = ymd_hms(ev.all.filtered$timestamp)

#convert to ltraj
#use UTM so that distance values are calculated in meters
d = as.ltraj(xy = ev.all.filtered[, c("long", "lat")], date = ev.all.filtered$timestamp, id = ev.all.filtered$id.tag)
d

#index
#dx, dy, dt == describe the distance of the x and y directions and duration in time between the relocations i and i + 1
#dist == the distance between successive relocations
#R2n == the squared distance between the first relocation of the trajectory and the current relocation (net squared displacement)

#plot ltraj
plot.ltraj(d[4])

#to look at time interval between locations plot 
#dt is measrured in second. to conver to days == dt / 3600 / 24 
plotltr(d[1], "dt/3600/24")
#dist is measured in meters. convert to km by == / 1000
plotltr(d[1], which = "dist/1000")
#plot net [squared] displacement
plotltr(d[1], which = 'sqrt(R2n)')

#save ltraj as dataframe
d

d1 <- do.call(rbind, d)
head(d1)

#merge ltraj calcs to full dataset
ev.all.filtered$NSD <- d1$R2n
ev.all.filtered$ND <- sqrt(ev.all.filtered$NSD)
ev.all.filtered$dist <- d1$dist
ev.all.filtered$dt.days <- d1$dt/3600/24
head(ev.all.filtered)

#write filtered dataset
write.csv(ev.all.filtered, "./ev.all.filtered.csv", row.names = FALSE)

#read
ev.all.filtered = read.csv("./ev.all.filtered.csv")
head(ev.all.filtered)

#lubridate
ev.all.filtered$timestamp = ymd_hms(ev.all.filtered$timestamp)

#plot net displacement
tiff("./Outputs/ev.tracking.nd.overview.tiff", units="cm", width=50, height=40, res=300)
plot = ggplot(ev.all.filtered, aes(timestamp, ND)) + geom_line() + facet_wrap(~ id)
plot = plot + labs(x = "date", y = "net displacement (degrees)") + theme_bw() 
plot 
dev.off()

#plot ND for each id
# create for loop to produce ggplot2 graphs 
library(gridExtra)

for (i in unique(ev.all.filtered$id)) { 
  
  #net displacement
  plot1 <- 
    ggplot(aes(timestamp, ND), data = subset(ev.all.filtered, id ==  i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "net displacement (degrees)") 
  
  #fix rate
  plot2 <- 
    ggplot(aes(timestamp, dt.days), data = subset(ev.all.filtered, id == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "time between fixes (days)") 
  
  #tracks
  plot3 <- 
    ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white")  + coord_quickmap() + theme_bw() +
    geom_path(data = subset(ev.all.filtered, id ==  i), aes(long,lat)) + labs(x = "longitude", y = "latitude") + 
    theme(legend.title = element_blank()) +
    ggtitle(paste(i)) + theme(plot.title = element_text(hjust = 0.5))
  
  #arrange
  plot4 = grid.arrange(plot3, plot1, plot2, ncol = 2, nrow = 2, 
                       widths = c(1,1), layout_matrix = rbind(c(1, 2), c(1,3)))
  
  ggsave(filename = sprintf('./ev.id.tag.plots/Combined/%s.png', i), plot = plot4, width = 30, height = 20, units = c("cm"),dpi = 300)
}

#check battery charge fields
#head(ev.all.filtered)
#summary(ev.all.filtered$battery.charge.percent)
#summary(ev.all.filtered$battery.charging.current)
#summary(ev.all.filtered$tag.voltage)
#summary(ev.all.filtered$eobs.battery.voltage)
#summary(ev.all.filtered$eobs.fix.battery.voltage)
#summary(ev.all.filtered$U_bat_mV)
#summary(ev.all.filtered$bat_soc_pct)
#summary(ev.all.filtered$Battery.voltage)
#summary(ev.all.filtered$Solar.voltage)

#battery charge
for (i in unique(ev.all.filtered$id)) { 
  
  b1 <- 
    ggplot(aes(timestamp, battery.charge.percent), data = subset(ev.all.filtered, id ==  i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "battery.charge.percent") 
  b2 <- 
    ggplot(aes(timestamp, battery.charging.current), data = subset(ev.all.filtered, id == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "battery.charging.current") +
    ggtitle(paste(i)) + theme(plot.title = element_text(hjust = 0.5))
  b3 <- 
    ggplot(aes(timestamp, tag.voltage), data = subset(ev.all.filtered, id == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "tag.voltage") 
  b5 <- 
    ggplot(aes(timestamp, eobs.battery.voltage), data = subset(ev.all.filtered, id == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "eobs.battery.voltage") 
  b6 <- 
    ggplot(aes(timestamp, eobs.fix.battery.voltage), data = subset(ev.all.filtered, id == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "eobs.fix.battery.voltage") 
  b7 <- 
    ggplot(aes(timestamp, U_bat_mV), data = subset(ev.all.filtered, id == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "U_bat_mV")
  b8 <- 
    ggplot(aes(timestamp, bat_soc_pct), data = subset(ev.all.filtered, id == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "bat_soc_pct")
  b9 <- 
    ggplot(aes(timestamp, Battery.voltage), data = subset(ev.all.filtered, id == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "Battery.voltage")
  b10 <- 
    ggplot(aes(timestamp, Solar.voltage), data = subset(ev.all.filtered, id == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "Solar.voltage")
  
  #arrange
  b11 = grid.arrange(b1,b2,b3,b5,b6,b7,b8,b9,b10, ncol = 3)
  
  ggsave(filename = sprintf('./ev.id.tag.plots/Battery/%s.png', i), plot = b11, width = 20, height = 30, units = c("cm"),dpi = 300)
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
ev.all.dt = setDT(ev.all.filtered)
head(ev.all.dt)
unique(ev.all.dt$id.tag) #note 155 unique id.tag

#create summary
ev.summary = ev.all.dt[,.(unique(species), unique(study.name), unique(id),unique(tag), min(timestamp), max(timestamp), head(lat,1), head(long,1), tail(lat,1), tail(long,1)), by = .(id.tag)]
names(ev.summary) = c("id.tag", "species", "study.name", "id", "tag",  "start.date", "end.date", "start.lat", "start.long", "end.lat", "end.long")
head(ev.summary)
summary(ev.summary)

#add deployment duration
ev.summary$deployment.duration = as.numeric(difftime(ev.summary$end.date, ev.summary$start.date, units = "days"))
head(ev.summary)

#add fate = alive if still transmitting May 1
ev.summary = cbind(ev.summary, ifelse(ev.summary$end.date > ymd(20190501), "alive", 'NA'))
names(ev.summary)[13] = 'fate'
ev.summary$fate = as.factor(ev.summary$fate)
head(ev.summary)

#add other columns, and input data from Google Sheet
ev.summary$age.at.deployment = NA

#
ev.summary$sex = NA
#ev.summary$sex[which(ev.summary$id == "Agata")]= 'F'
#ev.summary$sex[which(ev.summary$id == "Aneta")]= 'F'
#ev.summary$sex[which(ev.summary$id == "Arianna")]= 'F'
#ev.summary$sex[which(ev.summary$id == "Barabara")]= 'F'
#ev.summary$sex[which(ev.summary$id == "BatuecasP")]= 'F'
#ev.summary$sex[which(ev.summary$id == "Bianca")]= 'F'
#ev.summary$sex[which(ev.summary$id == "Camaces")]= 'F'

#
ev.summary$captive.raised = NA
ev.summary$rehabilitated = NA
ev.summary$how.fate.determined = ifelse(ev.summary$fate == "alive", "still transmitting", "NA")
ev.summary$cause.of.death = NA
ev.summary$how.fate.determined = as.factor(ev.summary$how.fate.determined)
ev.summary$death.or.failure.date = NA
summary(ev.summary$how.fate.determined)

#add comments column
ev.summary$comments = NA

#comment from observations above
ev.summary$comments[which(ev.summary$id == "2HP")]= 'highly intermittent fixes'
ev.summary$comments[which(ev.summary$id == "9FC")]= 'very few fixes'
ev.summary$comments[which(ev.summary$id == "18 White")]= 'very few fixes'
ev.summary$comments[which(ev.summary$id == "93")]= 'possible mortality / dropped tx'
ev.summary$comments[which(ev.summary$id == "A17 Green")]= 'errant last point?'
ev.summary$comments[which(ev.summary$id == "A25 Green")]= 'errant last point?'
ev.summary$comments[which(ev.summary$id == "Cabuk")]= 'errant last point?'
ev.summary$comments[which(ev.summary$id == "Iliaz")]= 'amazing figure of ontogenic migration development'
ev.summary$comments[which(ev.summary$id == "Levkipos")]= 'possible mortality or dropped tx in early 2015?'
ev.summary$comments[which(ev.summary$id == "Mille")]= 'errant last point?'
ev.summary$comments[which(ev.summary$id == "NeoPer_Poiares")]= 'errant last point? has a straight shot back from Africa to Spain'
ev.summary$comments[which(ev.summary$id == "Sarygush")]= 'possible mortality or dropped tx?'
ev.summary$comments[which(ev.summary$id == "Sharka")]= 'only 3 points!'
ev.summary$comments[which(ev.summary$id == "Carmen")]= 'looks to have died at see and tx floated long after'
ev.summary$comments[which(ev.summary$id == "Provence_2016_Ad_wild_EO5018_Salomé_8P")]= 'errant last point? has a straight shot from Africa back to France'
ev.summary$comments[which(ev.summary$id == "White 08")]= 'has a random point in the Med'
ev.summary$comments[which(ev.summary$id == "Yellow 04")]= 'looks like erroneous points in the Med'

#add start / end country
require(rgdal)
world = readOGR(dsn = "./TM_WORLD_BORDERS_SIMPL-0.3/", layer = "TM_WORLD_BORDERS_SIMPL-0.3")

#convert summary to spdf
names(ev.summary)
xy.start <- ev.summary[,c(9,8)]
spdf.start <- SpatialPointsDataFrame(coords = xy.start, data = ev.summary,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#check that the prjections match
proj4string(spdf.start) == proj4string(world)
plot(spdf.start)
plot(world, add = T)

#sample and append country to summary
start.country = data.frame(over(spdf.start, world[,5]))
names(start.country) = c("start.country")
start.country
ev.summary.country = cbind(ev.summary, start.country)
summary(ev.summary.country)

#
names(ev.summary.country)
xy.end = ev.summary.country[,c(11,10)]
spdf.end <- SpatialPointsDataFrame(coords = xy.end, data = ev.summary.country,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#check that the prjections match
proj4string(spdf.end) == proj4string(world)
plot(spdf.end)
plot(world, add = T)

#sample and append country to summary
end.country = data.frame(over(spdf.end, world[,5]))
names(end.country) = c("end.country")
end.country
ev.summary.country = cbind(ev.summary.country, end.country)
summary(ev.summary.country)
unique(ev.summary.country$id.tag) #131 unique
ev.summary = ev.summary.country

#add number of locations column
ev.summary = as.data.frame(ev.summary)
ev.summary
n.locs = count(ev.all.dt$id.tag)
n.locs$id.tag = n.locs$x
n.locs$n.locs = n.locs$freq
n.locs$freq = NULL
n.locs$x = NULL
ev.summary = merge(ev.summary, n.locs, by = "id.tag", all = T)

#write data summary
head(ev.summary)
summary(ev.summary)
names(ev.summary)
ev.summary = ev.summary[,c(3,2,5,4,1,15,14,16,17,6:11,22,23,12,24,13,18,19,20,21)]
names(ev.summary)

# Order the data frame by study
ev.summary = ev.summary[order(ev.summary$study.name),]

#write 
head(ev.summary)
write.csv(ev.summary, "./Outputs/ev.summary.csv", row.names = FALSE)

#####################################################################
#merging data summary with coauthor info input in Google Sheet

#Clear workspace
rm(list = ls())

#read final summaries
gs = read.csv("./Google Sheet/Egyptian Vulture tracking summary - EV summary.csv")
ev.summary = read.csv("./Outputs/ev.summary.csv")

#check id's match
unique(gs$id) #Provence_2016_Ad_wild_EO5018_Salom√©_8P
unique(ev.summary$id) # Provence_2016_Ad_wild_EO5018_Salomé_8P

# merge data frames
ev.summary.merge = merge(ev.summary, gs, by = "id.tag", all = T)
summary(ev.summary.merge[1])
summary(ev.summary.merge[1])

#select, reorganize and rename columns
names(ev.summary.merge)
ev.summary.merge = ev.summary.merge[c(2:5,1,29:32,10,11,35:36,12:19,44:47)]
names(ev.summary.merge)
names(ev.summary.merge) = c("study.name","species","tag","id","id.tag","sex","age.at.deployment",
                            "captive.raised","rehabilitated","start.date","end.date","start.date.adjusted", "end.date.adjusted",
                            "start.lat","start.long","end.lat","end.long","start.country", "end.country",        
                            "deployment.duration", "n.locs","fate","how.fate.determined", "cause.of.death","comments")
head(ev.summary.merge)
ev.summary.merge$end.date.adjusted = as.factor(ev.summary.merge$end.date.adjusted)
summary(ev.summary.merge)


#remove rows that have NA for study. These are id that Guido put in the Sheet, 
#but which we don't have any data for
ev.summary.merge =  ev.summary.merge[!is.na(ev.summary.merge$study.name),]

# Order the data frame by study
ev.summary.merge = ev.summary.merge[order(ev.summary.merge$study.name),]

#check data
any(duplicated(ev.summary.merge$id))
unique(ev.summary$id) == unique(ev.summary.merge$id)

#write
write.csv(ev.summary.merge, "./Outputs/ev.summary.merge.csv", row.names = F)

#################################################
#merge data from Balkans sent as csv

#Clear workspace
rm(list = ls())

ev.summary.merge = read.csv("./Outputs/ev.summary.merge.csv")
balkans.summary = read.csv("./Fate summaries/EGVU_fate_summary_Balkans.csv")
head(ev.summary.merge)

#subset
unique(balkans.summary$study.name)
unique(ev.summary.merge$study.name)

#lubridate
ev.summary.merge$start.date = ymd_hms(ev.summary.merge$start.date)
ev.summary.merge$end.date = ymd_hms(ev.summary.merge$end.date)
summary(ev.summary.merge$start.date.adjusted)
#ev.summary.merge$start.date.adjusted = mdy_hm(ev.summary.merge$start.date.adjusted)
summary(balkans.summary$end.date.adjusted)
#balkans.summary$start.date.adjusted = mdy_hm(balkans.summary$start.date.adjusted)
#balkans.summary$end.date.adjusted = mdy_hm(balkans.summary$end.date.adjusted)
summary(balkans.summary)

#remove balkans data from summary, so that we can append their updated info
ev.summary.merge.balkans <-ev.summary.merge[!(ev.summary.merge$study.name=="Neophron percnopterus Bulgaria/Greece"),]
summary(ev.summary.merge.balkans)
ev.summary.merge.balkans$end.date.adjusted = as.character(ev.summary.merge.balkans$end.date.adjusted)
ev.summary.merge.balkans$start.date.adjusted = as.character(ev.summary.merge.balkans$start.date.adjusted)

#merge (vertically)
ev.summary.merge.balkans = rbind.fill(ev.summary.merge.balkans, balkans.summary)
summary(ev.summary.merge.balkans)
head(ev.summary.merge.balkans$start.date.adjusted)
#ev.summary.merge.balkans$end.date.adjusted = ymd_hms(ev.summary.merge.balkans$end.date.adjusted)
#ev.summary.merge.balkans$start.date.adjusted = ymd(ev.summary.merge.balkans$start.date.adjusted)

# Order the data frame by id.tag
ev.summary.merge.balkans = ev.summary.merge.balkans[order(ev.summary.merge.balkans$id.tag),]
ev.summary.merge = ev.summary.merge[order(ev.summary.merge$id.tag),]
ev.summary.merge$id.tag == ev.summary.merge.balkans$id.tag

#add n.locs
names(ev.summary.merge)
ev.summary.merge.balkans$n.locs = ev.summary.merge$n.locs
ev.summary.merge.balkans$start.date = ev.summary.merge$start.date
ev.summary.merge.balkans$end.date = ev.summary.merge$end.date

#check data
ev.summary.merge.balkans = ev.summary.merge.balkans[order(ev.summary.merge.balkans$id.tag),]
ev.summary.merge = ev.summary.merge[order(ev.summary.merge$id.tag),]
any(duplicated(ev.summary.merge.balkans$id.tag))
unique(ev.summary.merge$id) == unique(ev.summary.merge.balkans$id)
ev.summary.merge.balkans = ev.summary.merge.balkans[order(ev.summary.merge.balkans$study.name),]
names(ev.summary.merge.balkans)
summary(ev.summary.merge.balkans)
#ev.summary.merge.balkans = ev.summary.merge.balkans[c(1:11,24:25,12:23)]

#write
write.csv(ev.summary.merge.balkans, "./Outputs/ev.summary.merge.csv", row.names = F)

#################################################
#merge data from McGrady csv

#Clear workspace
rm(list = ls())

ev.summary.merge = read.csv("./Outputs/ev.summary.merge.csv")
mcgrady.summary = read.csv("./Fate summaries/McGrady summaries.csv")

#rename columns to match
#mcgrady.summary$start.date.adjusted = dmy(mcgrady.summary$Date.ringed)
mcgrady.summary$age.at.deployment = mcgrady.summary$Age
mcgrady.summary$comments = mcgrady.summary$Fate
mcgrady.summary$id = mcgrady.summary$PTT.ID
mcgrady.summary$id = as.character(mcgrady.summary$id)
ev.summary.merge$age.at.deployment = as.character(ev.summary.merge$age.at.deployment)
mcgrady.summary$comments = as.character(mcgrady.summary$comments)
ev.summary.merge$comments = as.character(ev.summary.merge$comments)
#mcgrady.summary$start.date.adjusted = as.character(mcgrady.summary$start.date.adjusted)
#ev.summary.merge$start.date.adjusted = as.character(ev.summary.merge$start.date.adjusted)

# replace cell values for matching ids
for (i in unique(mcgrady.summary$id)) { 
  
  ev.summary.merge$start.date.adjusted[which(ev.summary.merge$id == i)] = mcgrady.summary$start.date.adjusted[which(mcgrady.summary$id == i)]
  ev.summary.merge$age.at.deployment[which(ev.summary.merge$id == i)] = mcgrady.summary$age.at.deployment[which(mcgrady.summary$id == i)]
  ev.summary.merge$comments[which(ev.summary.merge$id == i)] = mcgrady.summary$comments[which(mcgrady.summary$id == i)]
  
}

#write
write.csv(ev.summary.merge, "./Outputs/ev.summary.merge.csv", row.names = F)

####################################################################################
#standardizing row values

#did some manual cleaning of the dataset and columns here in excel

#############################################################
#Figure
ev.all.filtered = read.csv("./Outputs/ev.all.filtered.csv")
ev.summary.merge = read.csv("./Outputs/ev.summary.merge.manualcleaning.csv")
summary(ev.summary.merge)
summary(ev.all.filtered$argos.lat1)
summary(ev.all.filtered$argos.lat2)
summary(ev.summary.merge$fate)
summary(ev.summary.merge$captive.raised)
ev.summary.merge.alive.removed = ev.summary.merge[!ev.summary.merge$fate == "alive", ]
summary(ev.summary.merge.alive.removed$fate)

#ggplot map
colourpalette<-c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999','#000120')
colourpalette
tiff("./Outputs/ev.tracking.overview.alive.removed.tiff", units="cm", width=25, height=20, res=300)
map.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white") + coord_quickmap() + theme_bw() 
map.plot = map.plot + geom_path(data = ev.all.filtered, aes(long,lat, group = id), color = '#4daf4a') 
map.plot = map.plot + geom_point(data = ev.summary.merge, aes(start.long, start.lat, color = "deployment"))  
map.plot = map.plot + geom_point(data = ev.summary.merge.alive.removed, aes(end.long, end.lat, color = "termination")) + labs(x = "longitude", y = "latitude")
#map.plot = map.plot + geom_segment(data = ev.summary.merge.alive.removed, aes(x = start.long, y = start.lat, xend = end.long, yend = end.lat)) 
map.plot = map.plot + scale_color_manual(values=c('#377eb8','#e41a1c'))  
map.plot = map.plot + theme(legend.title = element_blank()) + theme(legend.position="bottom") 
map.plot = map.plot + ggtitle("Egyptian Vulture Tracking Overview") + theme(plot.title = element_text(hjust = 0.5))
map.plot
dev.off()
