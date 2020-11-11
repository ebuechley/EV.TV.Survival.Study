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
ev10 = read.csv("./Raw data/Egyptian Vulture Reintroduction Israel.csv")
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

#add ymdh
ev.movebank$year <- year(ev.movebank$timestamp)
ev.movebank$month <- month(ev.movebank$timestamp)
ev.movebank$day = day(ev.movebank$timestamp)
ev.movebank$hour <- hour(ev.movebank$timestamp)

#censor to one point per day 
ev.movebank = ev.movebank[!duplicated(ev.movebank[,c('individual.local.identifier', 'year', 'month', 'day')]),]
summary(ev.movebank)
names(ev.movebank)
unique(ev.movebank$individual.local.identifier) #232
unique(ev.movebank$population)

###################################################################
# read and process additional raw (non Movebank) data
###################################################################
library(stringi)
library(stringr)
#
ev38 = read.csv("./Raw data/ISPRA.Italy.CaptiveRaised/AC5AF11F_Noe.csv", header = TRUE, sep = ";")
head(ev38)
names(ev38)
ev38$individual.local.identifier = "Noe"
names(ev38)[1] = "tag.local.identifier"
names(ev38)[34] = "timestamp"
names(ev38)[30] = "location.lat"
names(ev38)[31] = "location.long"
ev38$timestamp = ymd_hms(ev38$timestamp)
head(ev38)
ev38 = ev38[!is.na(ev38$location.lat),] #remove NAs
summary(ev38)
#
test = ev38
test = dplyr::arrange(test, timestamp) #order by date
#remove lat/long with 0
summary(test)
#test$location.lat = gsub("[a-zA-Z ]", "", test$location.lat) #remove characters from numeric string
#test$location.long = gsub("[a-zA-Z ]", "", test$location.long) #remove characters from numeric string
#test$location.lat = as.numeric(test$location.lat)
#test$location.long = as.numeric(test$location.long)
summary(test$location.lat)
summary(test)
#test<-test[!(test$location.long==0 & test$location.lat==0),]
#fix lat
head(test$location.lat)
test$location.lat = str_remove(test$location.lat, "[.]") # remove the decimal which is wrongly placed
test$location.lat = str_remove(test$location.lat, "[.]")
test$location.lat = substr(test$location.lat, 1,6) #extract the first n characters from the string to standardize the string length
stri_sub(test$location.lat, 3, 2) ="." # add a . between 2nd and 3rd characters in string
head(test$location.lat) 
tail(test$location.lat) #looks good
test$location.lat = as.numeric(test$location.lat)
test = test[!is.na(test$location.lat),] #remove NAs
summary(test$location.lat)
#fix long
head(test$location.long)
test$location.long = str_remove(test$location.long, "[.]") # remove the decimal which is wrongly placed
test$location.long = substr(test$location.long, 1,6) #extract the first n characters from the string to standardize the string length
test$location.long = as.numeric(test$location.long)
summary(test$location.long)
test$location.long = as.character(test$location.long)
test$location.long = ifelse(startsWith(test$location.long, "1"), (as.numeric(test$location.long)/10000), (as.numeric(test$location.long)/100000))
head(test$location.long)
tail(test$location.long)
test<-test[!(test$location.long<5),] #removing outliers
test<-test[!(test$location.lat>42),] #removing outliers
summary(test$location.long)
head(test$timestamp)
tail(test$timestamp)
#quick plot of data
ggplot() + annotation_map(map_data("world"), fill = 'grey')  + coord_quickmap() + theme_bw() +
  geom_path(data = test, aes(location.long,location.lat)) + labs(x = "longitude", y = "latitude") +
  theme(legend.title = element_blank()) 
ev38 = test

#
ev39 = read.csv("./Raw data/ISPRA.Italy.CaptiveRaised/AF5AF11F_Beatrice.csv", header = TRUE, sep = ";")
head(ev39)
names(ev39)
ev39$individual.local.identifier = "Beatrice"
names(ev39)[1] = "tag.local.identifier"
names(ev39)[34] = "timestamp"
names(ev39)[30] = "location.lat"
names(ev39)[31] = "location.long"
ev39$timestamp = ymd_hms(ev39$timestamp)
head(ev39)
ev39 = ev39[!is.na(ev39$location.lat),] #remove NAs
summary(ev39)
#
test = ev39
test = dplyr::arrange(test, timestamp) #order by date
#remove lat/long with 0
test<-test[!(test$location.long==0 & test$location.lat==0),]
test$location.lat = as.numeric(test$location.lat)
test$location.long = as.numeric(test$location.long)
head(test)
summary(test)
#fix lat
head(test$location.lat)
#test$location.lat = str_remove(test$location.lat, "[.]") # remove the decimal which is wrongly placed
#head(test$location.lat)
test$location.lat = substr(test$location.lat, 1,6) #extract the first n characters from the string to standardize the string length
stri_sub(test$location.lat, 3, 2) ="." # add a . between 2nd and 3rd characters in string
head(test$location.lat) 
tail(test$location.lat) #looks good
test$location.lat = as.numeric(test$location.lat)
summary(test$location.lat)
#fix long
head(test$location.long)
#test$location.long = str_remove(test$location.long, "[.]") # remove the decimal which is wrongly placed
test$location.long = substr(test$location.long, 1,6) #extract the first n characters from the string to standardize the string length
test$location.long = as.numeric(test$location.long)
summary(test$location.long)
test$location.long = as.character(test$location.long)
test$location.long = ifelse(startsWith(test$location.long, "1"), (as.numeric(test$location.long)/10000), (as.numeric(test$location.long)/100000))
head(test$location.long)
tail(test$location.long)
test<-test[!(test$location.long<5),] #removing outliers
summary(test$location.long)
#quick plot of data
ggplot() + annotation_map(map_data("world"), fill = 'grey')  + coord_quickmap() + theme_bw() +
  geom_path(data = test, aes(location.long,location.lat)) + labs(x = "longitude", y = "latitude") +
  theme(legend.title = element_blank()) 
ev39 = test

#
ev40 = read.csv("./Raw data/ISPRA.Italy.CaptiveRaised/B05AF11F_Lucrezia.csv", header = TRUE, sep = ";")
head(ev40)
names(ev40)
ev40$individual.local.identifier = "Lucrezia"
names(ev40)[1] = "tag.local.identifier"
names(ev40)[34] = "timestamp"
names(ev40)[30] = "location.lat"
names(ev40)[31] = "location.long"
ev40$timestamp = ymd_hms(ev40$timestamp)
head(ev40)
summary(ev40)
#
test = ev40
test = dplyr::arrange(test, timestamp) #order by date
#remove lat/long with 0
summary(test)
#test$location.lat = gsub("[a-zA-Z ]", "", test$location.lat) #remove characters from numeric string
#test$location.long = gsub("[a-zA-Z ]", "", test$location.long) #remove characters from numeric string
#test$location.lat = as.numeric(test$location.lat)
#test$location.long = as.numeric(test$location.long)
summary(test$location.lat)
summary(test)
#test<-test[!(test$location.long==0 & test$location.lat==0),]
#fix lat
head(test$location.lat)
test$location.lat = str_remove(test$location.lat, "[.]") # remove the decimal which is wrongly placed
test$location.lat = str_remove(test$location.lat, "[.]")
test$location.lat = substr(test$location.lat, 1,6) #extract the first n characters from the string to standardize the string length
stri_sub(test$location.lat, 3, 2) ="." # add a . between 2nd and 3rd characters in string
head(test$location.lat) 
tail(test$location.lat) #looks good
test$location.lat = as.numeric(test$location.lat)
test = test[!is.na(test$location.lat),] #remove NAs
summary(test$location.lat)
#fix long
head(test$location.long)
test$location.long = str_remove(test$location.long, "[.]") # remove the decimal which is wrongly placed
test$location.long = substr(test$location.long, 1,6) #extract the first n characters from the string to standardize the string length
test$location.long = as.numeric(test$location.long)
summary(test$location.long)
test$location.long = as.character(test$location.long)
test$location.long = ifelse(startsWith(test$location.long, "1"), (as.numeric(test$location.long)/10000), (as.numeric(test$location.long)/100000))
head(test$location.long)
tail(test$location.long)
test<-test[!(test$location.long<5),] #removing outliers
summary(test$location.long)
#quick plot of data
ggplot() + annotation_map(map_data("world"), fill = 'grey')  + coord_quickmap() + theme_bw() +
  geom_path(data = test, aes(location.long,location.lat)) + labs(x = "longitude", y = "latitude") +
  theme(legend.title = element_blank()) 
ev40 = test

#
ev41 = read.csv("./Raw data/ISPRA.Italy.CaptiveRaised/B15AF11F_Leonardo.csv", header = TRUE, sep = ";")
head(ev41)
names(ev41)
ev41$individual.local.identifier = "Leonardo"
names(ev41)[1] = "tag.local.identifier"
names(ev41)[34] = "timestamp"
names(ev41)[30] = "location.lat"
names(ev41)[31] = "location.long"
ev41$timestamp = ymd_hms(ev41$timestamp)
head(ev41)
summary(ev41)
#
test = ev41
head(test)
test = dplyr::arrange(test, timestamp) #order by date
#remove lat/long with 0
summary(test)
#test$location.lat = gsub("[a-zA-Z ]", "", test$location.lat) #remove characters from numeric string
#test$location.long = gsub("[a-zA-Z ]", "", test$location.long) #remove characters from numeric string
#test$location.lat = as.numeric(test$location.lat)
#test$location.long = as.numeric(test$location.long)
summary(test$location.lat)
summary(test)
#test<-test[!(test$location.long==0 & test$location.lat==0),]
#fix lat
head(test$location.lat)
test$location.lat = str_remove(test$location.lat, "[.]") # remove the decimal which is wrongly placed
test$location.lat = str_remove(test$location.lat, "[.]")
test$location.lat = substr(test$location.lat, 1,6) #extract the first n characters from the string to standardize the string length
stri_sub(test$location.lat, 3, 2) ="." # add a . between 2nd and 3rd characters in string
head(test$location.lat) 
tail(test$location.lat) #looks good
test$location.lat = as.numeric(test$location.lat)
test = test[!is.na(test$location.lat),] #remove NAs
summary(test$location.lat)
#fix long
head(test$location.long)
test$location.long = str_remove(test$location.long, "[.]") # remove the decimal which is wrongly placed
test$location.long = substr(test$location.long, 1,6) #extract the first n characters from the string to standardize the string length
test$location.long = as.numeric(test$location.long)
summary(test$location.long)
test$location.long = as.character(test$location.long)
test$location.long = ifelse(startsWith(test$location.long, "1"), (as.numeric(test$location.long)/10000), (as.numeric(test$location.long)/100000))
head(test$location.long)
tail(test$location.long)
test<-test[!(test$location.long<5),] #removing outliers
test<-test[!(test$location.lat>42),] #removing outliers
head(test$timestamp)
tail(test$timestamp)
summary(test$location.long)
#quick plot of data
ggplot() + annotation_map(map_data("world"), fill = 'grey')  + coord_quickmap() + theme_bw() +
  geom_path(data = test, aes(location.long,location.lat)) + labs(x = "longitude", y = "latitude") +
  theme(legend.title = element_blank()) 
ev41 = test
#
test = ev41
head(test)
test = dplyr::arrange(test, timestamp) #order by date
#remove lat/long with 0
summary(test)
#test$location.lat = gsub("[a-zA-Z ]", "", test$location.lat) #remove characters from numeric string
#test$location.long = gsub("[a-zA-Z ]", "", test$location.long) #remove characters from numeric string
#test$location.lat = as.numeric(test$location.lat)
#test$location.long = as.numeric(test$location.long)
summary(test$location.lat)
summary(test)
#test<-test[!(test$location.long==0 & test$location.lat==0),]
#fix lat
head(test$location.lat)
test$location.lat = str_remove(test$location.lat, "[.]") # remove the decimal which is wrongly placed
test$location.lat = str_remove(test$location.lat, "[.]")
test$location.lat = substr(test$location.lat, 1,6) #extract the first n characters from the string to standardize the string length
stri_sub(test$location.lat, 3, 2) ="." # add a . between 2nd and 3rd characters in string
head(test$location.lat) 
tail(test$location.lat) #looks good
test$location.lat = as.numeric(test$location.lat)
test = test[!is.na(test$location.lat),] #remove NAs
summary(test$location.lat)
#fix long
head(test$location.long)
test$location.long = str_remove(test$location.long, "[.]") # remove the decimal which is wrongly placed
test$location.long = substr(test$location.long, 1,6) #extract the first n characters from the string to standardize the string length
test$location.long = as.numeric(test$location.long)
summary(test$location.long)
test$location.long = as.character(test$location.long)
test$location.long = ifelse(startsWith(test$location.long, "1"), (as.numeric(test$location.long)/10000), (as.numeric(test$location.long)/100000))
head(test$location.long)
tail(test$location.long)
test<-test[!(test$location.long<5),] #removing outliers
test<-test[!(test$location.lat>42),] #removing outliers
head(test$timestamp)
tail(test$timestamp)
summary(test$location.long)
#quick plot of data
ggplot() + annotation_map(map_data("world"), fill = 'grey')  + coord_quickmap() + theme_bw() +
  geom_path(data = test, aes(location.long,location.lat)) + labs(x = "longitude", y = "latitude") +
  theme(legend.title = element_blank()) 
ev41 = test

#
ev42 = read.csv("./Raw data/ISPRA.Italy.CaptiveRaised/AF5AF11F_2018_EV_Italy.csv", header = TRUE)
head(ev42)
names(ev42)
ev42$individual.local.identifier = "Bianca_IHB"
names(ev42)[1] = "tag.local.identifier"
names(ev42)[34] = "timestamp"
names(ev42)[30] = "location.lat"
names(ev42)[31] = "location.long"
ev42$individual.local.identifier = as.factor(ev42$individual.local.identifier)
names(ev42)
ev42$timestamp = ymd_hms(ev42$timestamp)
head(ev42)
summary(ev42)
#
test = ev42
head(test)
test = dplyr::arrange(test, timestamp) #order by date
summary(test$location.lat)
summary(test)
names(test)
test = test[complete.cases(test[ ,30]),]
#quick plot of data
ggplot() + annotation_map(map_data("world"), fill = 'grey')  + coord_quickmap() + theme_bw() +
  geom_path(data = test, aes(location.long,location.lat)) + labs(x = "longitude", y = "latitude") +
  theme(legend.title = element_blank()) 
ev42 = test

#
ev43 = read.csv("./Raw data/ISPRA.Italy.CaptiveRaised/B05AF11F_2018_EV_Italy.csv", header = TRUE)
head(ev43)
names(ev43)
ev43$individual.local.identifier = "Clara_IHC"
names(ev42)[1] = "tag.local.identifier"
names(ev43)[34] = "timestamp"
names(ev43)[30] = "location.lat"
names(ev43)[31] = "location.long"
ev43$timestamp = ymd_hms(ev43$timestamp)
head(ev43)
summary(ev43)
#
test = ev43
head(test)
test = dplyr::arrange(test, timestamp) #order by date
summary(test$location.lat)
summary(test)
names(test)
test = test[complete.cases(test[ ,30]),]
#quick plot of data
ggplot() + annotation_map(map_data("world"), fill = 'grey')  + coord_quickmap() + theme_bw() +
  geom_path(data = test, aes(location.long,location.lat)) + labs(x = "longitude", y = "latitude") +
  theme(legend.title = element_blank()) 
ev43 = test

#merge (vertically) the data, keeping all unique columns
ev.ISPRA = rbind.fill(ev38,ev39,ev40,ev41,ev42,ev43)
ev.ISPRA$population = "italy"
ev.ISPRA$study.name = "ISPRA"
head(ev.ISPRA)
names(ev.ISPRA)
summary(ev.ISPRA)
ev.ISPRA <- ev.ISPRA[order(ev.ISPRA$individual.local.identifier),] 
unique(ev.ISPRA$individual.local.identifier)

#write 
write.csv(ev.ISPRA, "./Raw data/ev.ISPRA.all.csv")

########################################################
#Merge
########################################################
#check that timestamp is clean
summary(ev.movebank$timestamp)
summary(ev.ISPRA$timestamp)

#check lat / long
summary(ev.movebank$location.lat)
summary(ev.movebank$location.long)
summary(ev.ISPRA$location.long)

#check unique IDs
ev.ISPRA <- ev.ISPRA[order(ev.ISPRA$individual.local.identifier),] 
unique(ev.ISPRA$individual.local.identifier) #5
ev.movebank <- ev.movebank[order(ev.movebank$individual.local.identifier),] 
unique(ev.movebank$individual.local.identifier) #232

#remove eagles from movebank data
ev.movebank$individual.taxon.canonical.name = as.factor(ev.movebank$individual.taxon.canonical.name)
summary(ev.movebank$individual.taxon.canonical.name) #ut oh, some eagles in here!
ev.movebank = ev.movebank[!(ev.movebank$individual.taxon.canonical.name== "Aquila chrysaetos"),]
summary(ev.movebank$individual.taxon.canonical.name) #now good
unique(ev.movebank$individual.local.identifier) #231 -- dropped 1 tag

#merge (vertically) the data, keeping all unique columns
ev.all = rbind.fill(ev.movebank,ev.ISPRA)
head(ev.all)
summary(ev.all$location.lat)
names(ev.all)
ev.all$timestamp = ymd_hms(ev.all$timestamp)
summary(ev.all$timestamp)
ev.all <- ev.all[order(ev.all$individual.local.identifier),] 
unique(ev.all$individual.local.identifier) #237

#remove any other species
ev.all$individual.taxon.canonical.name = as.factor(ev.all$individual.taxon.canonical.name)
summary(ev.all$individual.taxon.canonical.name)
ev.all$individual.taxon.canonical.name = "Neophron percnopterus" #standardize the species name for all remaining which were the ISPRA EV
ev.all <- ev.all[order(ev.all$individual.local.identifier),] 
unique(ev.all$individual.local.identifier) #237

########################################################
#Final clean up
########################################################
ev.tv = ev.all #JUST ASSIGN EV DATA AS EV.TV HERE SO AS TO NOT CHANGE CODE HEREAFTER
summary(ev.tv$timestamp)
names(ev.tv)
unique(ev.tv$individual.local.identifier) #note 237 unique id

#rename/simplify column headers
colnames(ev.tv)[colnames(ev.tv)=="location.long"] <- "long"
colnames(ev.tv)[colnames(ev.tv)=="location.lat"] <- "lat"
colnames(ev.tv)[colnames(ev.tv)=="tag.local.identifier"] <- "tag"
colnames(ev.tv)[colnames(ev.tv)=="individual.local.identifier"] <- "id"
colnames(ev.tv)[colnames(ev.tv)=="individual.taxon.canonical.name"] <- "species"
summary(ev.tv)

###Create burst by ID and tag
ev.tv$id.tag <- c(paste(ev.tv$id,ev.tv$tag,sep="_")) 
ev.tv$id.tag <- as.factor(ev.tv$id.tag) 

#lubridate
summary(ev.tv$timestamp)
ev.tv$timestamp = ymd_hms(ev.tv$timestamp)

#remove data from after Oct 31, 2020 (setting a cutoff point for the study data)
ev.tv <- subset(ev.tv, timestamp <= as.POSIXct('2020-10-31'))
summary(ev.tv$timestamp)
unique(ev.tv$id.tag) #238 id.tag -- note this added one as 1 bird has 2 tag deployments

#remove any rows that don't have date, lat or long
names(ev.tv)
nrow(ev.tv)
ev.tv = ev.tv[complete.cases(ev.tv[,c("timestamp","long","lat")]),] 
unique(ev.tv$id.tag) #238

#reorder dataframe to have x,y,date,id.tag as first four columns
names(ev.tv)
ev.tv = ev.tv[,c(4,5,3,102,1:2,6:101)]
names(ev.tv)

#add ymdh
ev.tv$year <- year(ev.tv$timestamp)
ev.tv$month <- month(ev.tv$timestamp)
ev.tv$day = day(ev.tv$timestamp)
ev.tv$hour <- hour(ev.tv$timestamp)

#censor to one point per day 
#(at least to start, to have a workable dataset, as well as to standardize across transmitter types)
ev.tv.1ptperday = ev.tv[!duplicated(ev.tv[,c('id', 'year', 'month', 'day')]),]
summary(ev.tv.1ptperday)
unique(ev.tv.1ptperday$id.tag) #238
unique(ev.tv.1ptperday$population)

#quick plot of data
library(ggplot2)
ggplot() + annotation_map(map_data("world"), fill = 'grey')  + coord_quickmap() + theme_bw() + 
  geom_path(data = ev.tv.1ptperday, aes(long,lat, group = id)) + labs(x = "longitude", y = "latitude") + 
  theme(legend.title = element_blank())  #notice bad fixes in dataset

#set wd
setwd("~/Google Drive/Research Projects/EV-TV Survival Study/Dataset/Final/Rev1/")

#write 1ptperday dataset
write.csv(ev.tv.1ptperday, "./Data/ev.1ptperday.csv", row.names=FALSE)
