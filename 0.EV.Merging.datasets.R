#Set WD
setwd("~/Google Drive/Research Projects/EV-TV Survival Study/Dataset/Final/Rev1")

###Load relevant libraries###
##install MigrateR
#install_github("dbspitz/migrateR/migrateR", build_vignettes = T)
library(devtools)
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
library(plyr)
library(lubridate)

##Clear workspace
rm(list = ls())

########################################################
#EV
########################################################
# read data
ev1 = read.csv("./Raw data/Egyptian vulture (Neophron percnopterus) in Arribes del Duero (Salamanca) - SALORO.csv")
ev1$population = "western europe"
ev2 = read.csv("./Raw data/Egyptian vultures in the Middle East and East Africa.csv")
ev2$population = "caucasus"
ev3 = read.csv("./Raw data/Egyptian vulture in France (grands Causses-Baronnies) ID_PROG 961.csv")
ev3$population = "western europe"
ev4 = read.csv("./Raw data/Egyptian vulture Kobierzycki Gardon ID_PROG 457.csv")
ev4$population = "western europe"
ev5 = read.csv("./Raw data/Egyptian vulture Kobierzycki Pyrenees ID_PROG 457.csv")
ev5$population = "western europe"
ev6 = read.csv("./Raw data/Egyptian vulture Kobierzycki Vaucluse ID_PROG 457.csv")
ev6$population = "western europe"
ev7 = read.csv("./Raw data/Egyptian Vulture wild-birds Israel.csv")
ev7$population = "middle east"
ev8 = read.csv("./Raw data/Egyptian vultures Dagestan2.csv")
ev8$population = "caucasus"
#ev9 = read.csv("./Raw data/Egyptian vultures in Djibouti.csv")
#ev9$population = "horn of africa"
ev10 = read.csv("./Raw data/Egyptian_Vulture_Reintroduction_Israel.csv")
ev10$population = "middle east"
ev11 = read.csv("./Raw data/LIFE_Rupis_EgyptianVultures.csv")
ev11$population = "western europe"
ev12 = read.csv("./Raw data/Neophron percnopterus Bulgaria_Greece.csv")
ev12$population = "balkans"
ev13 = read.csv("./Raw data/Neophron percnopterus. GREFA. Spain.csv")
ev13$population = "western europe"
#ev14 = read.csv("./Raw data/Omanvulture.csv")
#ev14$population = "oman"
ev15 = read.csv("./Raw data/Released Egyptian Vultures in Italy.csv")
ev15$population = "italy"
ev32 = read.csv("./Raw data/Egyptian vulture EB Terra Natura UA Spain.csv")
ev32$population = "western europe"
ev33 = read.csv("./Raw data/_Egyptian Vulture in Spain - Migra Program in Spain.csv")
ev33$population = "western europe"

#merge (vertically) the data, keeping all unique columns
ev.movebank = rbind.fill(ev1,ev2,ev3,ev4,ev5,ev6,ev7,ev8,ev10,ev11,ev12,ev13,ev15,ev32,ev33) #removed ev9,ev14
ev.movebank$timestamp = ymd_hms(ev.movebank$timestamp)
summary(ev.movebank$timestamp)

############################################################
# read and process additional raw (non Movebank) data
#ev16 = read.csv("./Original Data/McGrady.Meyburg/139 AquilaSystem_GPSData_2018_Jan_2019_.csv")
#head(ev16)
#ev16$timestamp = dmy_hm(ev16$timestamp)
#head(ev16)
#summary(ev16)
#
#ev17 = read.csv("./Original Data/McGrady.Meyburg/2015_GeotrakPTTs_locations.csv")
#head(ev17)
#ev17$timestamp = dmy_hms(ev17$timestamp)
#head(ev17)
#summary(ev17)
#
#ev18 = read.csv("./Original Data/McGrady.Meyburg/47638 GPS_to nov2018_final.csv")
#head(ev18)
#names(ev18)[1]<-"individual.local.identifier"
#names(ev18)[4]<-"location.lat"
#names(ev18)[5]<-"location.long"
#ev18$timestamp = paste(ev18$Date, ev18$Time, sep = " ")
#head(ev18)
#ev18$timestamp = dmy_hm(ev18$timestamp)
#head(ev18)
#summary(ev18)
#
#ev19 = read.csv("./Original Data/McGrady.Meyburg/52027 GPS_jan-sept 2018_clean.csv")
#head(ev19)
#ev19$timestamp = paste(ev19$Date, ev19$Time, sep = " ")
#head(ev19)
#ev19$timestamp = dmy_hm(ev19$timestamp)
#head(ev19)
#ev19$individual.local.identifier = "52027"
#head(ev19)
#names(ev19)[3]<- "location.lat"
#names(ev19)[4]<- "location.long"
#head(ev19)
#ev19$individual.local.identifier = as.factor(ev19$individual.local.identifier)
#summary(ev19)
#
#ev20 = read.csv("./Original Data/McGrady.Meyburg/70107 GPS_Jan_2018_May_2019nozeros.csv")
#head(ev20)
#names(ev20)[1] = "individual.local.identifier"
#names(ev20)[4] = "location.lat"
#names(ev20)[5] = "location.long"
#ev20$timestamp = paste(ev20$Date, ev20$Time, sep = " ")
#ev20$timestamp = dmy_hm(ev20$timestamp)
#head(ev20)
#summary(ev20)
#
#ev21 = read.csv("./Original Data/McGrady.Meyburg/95784 GPS_jan_2018_may_2019_all_nozeros.csv")
#head(ev21)
#names(ev21)[1] = "individual.local.identifier"
#names(ev21)[4] = "location.lat"
#names(ev21)[5] = "location.long"
#ev21$timestamp = paste(ev21$Date, ev21$Time, sep = " ")
#ev21$timestamp = dmy_hm(ev21$timestamp)
#head(ev21)
#ev21$location.lat = as.numeric(ev21$location.lat)
#ev21$location.long = as.numeric(ev21$location.long)
#ev21$individual.local.identifier = "95784"
#summary(ev21)
#
#ev22 = read.csv("./Original Data/McGrady.Meyburg/171325_2018_jan_2019_may_all.csv")
#head(ev22)
#summary(ev22)
#names(ev22)[1] = "individual.local.identifier"
#names(ev22)[2] = "timestamp"
#names(ev22)[11] = "location.lat"
#names(ev22)[12] = "location.long"
#ev22$timestamp = ymd_hms(ev22$timestamp)
#head(ev22)
#summary(ev22)
#
#ev23 = read.csv("./Original Data/McGrady.Meyburg/171326_2018_jan_2019_may_all.csv")
#head(ev23)
#names(ev23)[1] = "individual.local.identifier"
#names(ev23)[2] = "timestamp"
#names(ev23)[11] = "location.lat"
#names(ev23)[12] = "location.long"
#ev23$timestamp = ymd_hms(ev23$timestamp)
#head(ev23)
#summary(ev23)
#
#ev24 = read.csv("./Original Data/McGrady.Meyburg/171327_2018_jan_2018_17_06_all_final.csv")
#head(ev24)
#summary(ev24)
#names(ev24)[1] = "individual.local.identifier"
#names(ev24)[2] = "timestamp"
#names(ev24)[11] = "location.lat"
#names(ev24)[12] = "location.long"
#ev24$timestamp = ymd_hms(ev24$timestamp)
#head(ev24)
#summary(ev24)
#
#ev25 = read.csv("./Original Data/McGrady.Meyburg/171328_2018_Jan_2019_may_all.csv")
#head(ev25)
#names(ev25)[1] = "individual.local.identifier"
#names(ev25)[2] = "timestamp"
#names(ev25)[11] = "location.lat"
#names(ev25)[12] = "location.long"
#ev25$timestamp = ymd_hms(ev25$timestamp)
#head(ev25)
#summary(ev25)
#
#ev26 = read.csv("./Original Data/McGrady.Meyburg/171329_2018_Jan_2019_may_all.csv")
#head(ev26)
#names(ev26)[1] = "individual.local.identifier"
#names(ev26)[2] = "timestamp"
#names(ev26)[11] = "location.lat"
#names(ev26)[12] = "location.long"
#ev26$timestamp = ymd_hms(ev26$timestamp)
#head(ev26)
#summary(ev26)
#
#ev27 = read.csv("./Original Data/McGrady.Meyburg/171330_2018_Jan_2019_may_all.csv")
#head(ev27)
#names(ev27)[1] = "individual.local.identifier"
#names(ev27)[2] = "timestamp"
#names(ev27)[11] = "location.lat"
#names(ev27)[12] = "location.long"
#ev27$timestamp = ymd_hms(ev27$timestamp)
#head(ev27)
#summary(ev27)
#
#ev28 = read.csv("./Original Data/McGrady.Meyburg/AquilaSystem_GPSData_ID080_all data.csv")
#head(ev28)
#names(ev28)[1] = "individual.local.identifier"
#names(ev28)[2] = "timestamp"
#names(ev28)[5] = "location.lat"
#names(ev28)[6] = "location.long"
#ev28$timestamp = dmy_hm(ev28$timestamp)
#head(ev28)
#summary(ev28)
#
#ev29 = read.csv("./Original Data/McGrady.Meyburg/AquilaSystem_GPSData_ID093_2016_10_06_.csv")
#head(ev29)
#names(ev29)[1] = "individual.local.identifier"
#names(ev29)[2] = "timestamp"
#names(ev29)[5] = "location.lat"
#names(ev29)[6] = "location.long"
#ev29$timestamp = dmy_hm(ev29$timestamp)
#head(ev29)

#merge (vertically) the data, keeping all unique columns
#ev.mcgrady = rbind.fill(ev16,ev17,ev18,ev19,ev20,ev21,ev22,ev23,ev24,ev25,ev26,ev27,ev28,ev29)
#ev.mcgrady$population = "oman"
#head(ev.mcgrady)
#summary(ev.mcgrady$timestamp)
#ev.mcgrady$individual.local.identifier = as.factor(ev.mcgrady$individual.local.identifier)
#summary(ev.mcgrady$individual.local.identifier)
#ev.mcgrady$tag.local.identifier = ev.mcgrady$individual.local.identifier
#names(ev.mcgrady)
#ev.mcgrady$study.name = "McGrady.Meyburg"

#write 
#write.csv(ev.mcgrady, "./Original Data/ev.mcgrady.meyburg.all.csv")

###################################################################
# read and process additional raw (non Movebank) data
ev30 = read.csv("./Raw data/ISPRA.Italy.CaptiveRaised/AF5AF11F_2018_EV_Italy.csv")
names(ev30)
names(ev30)[1] = "individual.local.identifier"
names(ev30)[34] = "timestamp"
names(ev30)[30] = "location.lat"
names(ev30)[31] = "location.long"
ev30$timestamp = ymd_hms(ev30$timestamp)
head(ev30)
summary(ev30)
#
ev31 = read.csv("./Raw data/ISPRA.Italy.CaptiveRaised/B05AF11F_2018_EV_Italy.csv")
head(ev31)
names(ev31)
names(ev31)[1] = "individual.local.identifier"
names(ev31)[34] = "timestamp"
names(ev31)[30] = "location.lat"
names(ev31)[31] = "location.long"
ev31$timestamp = ymd_hms(ev31$timestamp)
head(ev31)
summary(ev31)

#merge (vertically) the data, keeping all unique columns
ev.ISPRA = rbind.fill(ev30,ev31)
ev.ISPRA$population = "italy"
ev.ISPRA$study.name = "ISPRA"
head(ev.ISPRA)
names(ev.ISPRA)
summary(ev.ISPRA)

#write 
write.csv(ev.ISPRA, "./Raw data/ev.ISPRA.all.csv")

########################################################
#Merge
########################################################
#check that timestamp is clean
summary(ev.movebank$timestamp)
#summary(ev.mcgrady$timestamp)
summary(ev.ISPRA$timestamp)

#merge (vertically) the data, keeping all unique columns
ev.all = rbind.fill(ev.movebank,ev.ISPRA) #removed ev.mcgrady
head(ev.all)
names(ev.all)
unique(ev.all$individual.local.identifier) #note 229 unique id.tag
ev.all$timestamp = ymd_hms(ev.all$timestamp)
summary(ev.all$timestamp)

#remove any other species
ev.all$individual.taxon.canonical.name = as.factor(ev.all$individual.taxon.canonical.name)
summary(ev.all$individual.taxon.canonical.name) #ut oh, some eagles in here!
ev.all = ev.all[!(ev.all$individual.taxon.canonical.name== "Aquila chrysaetos"),]
summary(ev.all$individual.taxon.canonical.name)
ev.all$individual.taxon.canonical.name = "Neophron percnopterus" #standardize the species name for all remaining which were confirmed EV

########################################################
#TV - skip for now
########################################################
# read data
tv1 = read.csv("./Original Data/Turkey Vulture Acopian Center USA GPS.csv")
tv1$population = NA
tv2 = read.csv("./Original Data/Black Vultures and Turkey Vultures Southeastern USA.csv")
tv2$population = "southeast"
tv3 = read.csv("./Original Data/Vulture Movements.csv")
tv3$population = "southeast"

#check and set timestamp
summary(tv1$timestamp)
tv1$timestamp = ymd_hms(tv1$timestamp)
summary(tv2$timestamp)
tv2$timestamp = ymd_hms(tv2$timestamp)
summary(tv3$timestamp)
tv3$timestamp = ymd_hms(tv3$timestamp)

#merge (vertically) the data, keeping all unique columns
tv.all = rbind.fill(tv1, tv2, tv3)
head(tv.all)
tv.all$timestamp = ymd_hms(tv.all$timestamp)
summary(tv.all$timestamp)

#remove other species
summary(tv.all$individual.taxon.canonical.name)
tv.all = tv.all[!(tv.all$individual.taxon.canonical.name == "Coragyps atratus"),] #some black vultures were included in some of the data
tv.all = tv.all[!(tv.all$individual.taxon.canonical.name == ""),] # the blank species are black vultures in this data
summary(tv.all$individual.taxon.canonical.name)
tv.all$individual.taxon.canonical.name = "Cathartes aura" #standardize the species name for all
unique(tv.all$individual.local.identifier) #note 104 unique id

########################################################
#Merge EV & TV
########################################################

#merge (vertically) the data, keeping all unique columns
summary(ev.all$timestamp)
summary(tv.all$timestamp)
#ev.tv = rbind.fill(ev.all,tv.all)
ev.tv = ev.all
summary(ev.tv$timestamp)
head(ev.tv)
names(ev.tv)
unique(ev.tv$individual.local.identifier) #note 227 unique id

#rename/simplify column headers
colnames(ev.tv)[colnames(ev.tv)=="location.long"] <- "long"
colnames(ev.tv)[colnames(ev.tv)=="location.lat"] <- "lat"
colnames(ev.tv)[colnames(ev.tv)=="tag.local.identifier"] <- "tag"
colnames(ev.tv)[colnames(ev.tv)=="individual.local.identifier"] <- "id"
colnames(ev.tv)[colnames(ev.tv)=="individual.taxon.canonical.name"] <- "species"

###Create burst by ID and tag
ev.tv$id.tag <- c(paste(ev.tv$id,ev.tv$tag,sep="_")) 
ev.tv$id.tag <- as.factor(ev.tv$id.tag) 

#lubridate
ev.tv$timestamp = ymd_hms(ev.tv$timestamp)

#remove data from after Oct 1, 2020 (setting a cutoff point for the study data)
ev.tv <- subset(ev.tv, timestamp <= as.POSIXct('2020-10-01'))
summary(ev.tv$timestamp)
unique(ev.tv$id.tag) #227 individuals

#remove any rows that don't have date, lat or long
names(ev.tv)
ev.tv = ev.tv[complete.cases(ev.tv[,3:5]),] 

#reorder dataframe to have x,y,date,id.tag as first four columns
names(ev.tv)
ev.tv = ev.tv[,c(4,5,3,132,1:2,6:131)]
names(ev.tv)

#add ymdh
ev.tv$year <- year(ev.tv$timestamp)
ev.tv$month <- month(ev.tv$timestamp)
ev.tv$day = day(ev.tv$timestamp)
ev.tv$hour <- hour(ev.tv$timestamp)

#set wd
#setwd("~/Documents/GitHub/EV - TV Survival Study/")

#write complete dataset --- this is a 5GB file!!!
#write.csv(ev.tv, "ev.tv.all.merged.csv", row.names=FALSE)

#censor to one point per day 
#(at least to start, to have a workable dataset, as well as to standardize across transmitter types)
ev.tv.1ptperday = ev.tv[!duplicated(ev.tv[,c('id', 'year', 'month', 'day')]),]

#quick plot of data
library(ggplot2)
map.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey')  + coord_quickmap() + theme_bw() 
map.plot = map.plot + geom_path(data = ev.tv.1ptperday, aes(long,lat, group = id)) + labs(x = "longitude", y = "latitude")
map.plot = map.plot + theme(legend.title = element_blank()) 
map.plot #notice bad fixes in dataset

#set wd
setwd("~/Google Drive/Research Projects/EV-TV Survival Study/Dataset/Final/Rev1/")

#write 1ptperday dataset
write.csv(ev.tv.1ptperday, "./Data/ev.1ptperday.csv", row.names=FALSE)
