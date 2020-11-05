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
unique(ev.movebank$individual.local.identifier) #232
unique(ev.movebank$population)

###################################################################
# read and process additional raw (non Movebank) data
###################################################################
library(stringi)
library(stringr)
ev30 = read.csv("./Raw data/ISPRA.Italy.CaptiveRaised/190554_Clint.csv", header = TRUE, sep = ";")
head(ev30)
names(ev30)
names(ev30)[1] = "individual.local.identifier"
names(ev30)[2] = "timestamp"
names(ev30)[11] = "location.lat"
names(ev30)[12] = "location.long"
head(ev30$timestamp)
ev30$timestamp = dmy_hm(ev30$timestamp)
head(ev30)
summary(ev30)
#
test = ev30
test = dplyr::arrange(test, timestamp) #order by date
#remove lat/long with 0
test<-test[!(test$location.long==0 & test$location.lat==0),]
test$location.lat = as.numeric(test$location.lat)
test$location.long = as.numeric(test$location.long)
test$X = NULL
head(test)
summary(test)
#fix lat
head(test$location.lat)
test$location.lat = str_remove(test$location.lat, "[.]") # remove the decimal which is wrongly placed
head(test$location.lat)
test$location.lat = substr(test$location.lat, 1,6) #extract the first n characters from the string to standardize the string length
stri_sub(test$location.lat, 3, 2) ="." # add a . between 2nd and 3rd characters in string
head(test$location.lat) 
tail(test$location.lat) #looks good
test$location.lat = as.numeric(test$location.lat)
summary(test$location.lat)
#fix long
head(test$location.long)
test$location.long = str_remove(test$location.long, "[.]") # remove the decimal which is wrongly placed
test$location.long = substr(test$location.long, 1,6) #extract the first n characters from the string to standardize the string length
test$location.long = as.numeric(test$location.long)
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
ev30 = test
#
ev31 = read.csv("./Raw data/ISPRA.Italy.CaptiveRaised/190554_Francesca.csv", header = TRUE, sep = ";")
head(ev31)
names(ev31)
names(ev31)[1] = "individual.local.identifier"
names(ev31)[2] = "timestamp"
names(ev31)[11] = "location.lat"
names(ev31)[12] = "location.long"
ev31$timestamp = dmy_hm(ev31$timestamp)
head(ev31)
summary(ev31)
#
test = ev31
test = dplyr::arrange(test, timestamp) #order by date
#remove lat/long with 0
test<-test[!(test$location.long==0 & test$location.lat==0),]
test$location.lat = as.numeric(test$location.lat)
test$location.long = as.numeric(test$location.long)
head(test)
summary(test)
#fix lat
head(test$location.lat)
test$location.lat = str_remove(test$location.lat, "[.]") # remove the decimal which is wrongly placed
head(test$location.lat)
test$location.lat = substr(test$location.lat, 1,6) #extract the first n characters from the string to standardize the string length
stri_sub(test$location.lat, 3, 2) ="." # add a . between 2nd and 3rd characters in string
head(test$location.lat) 
tail(test$location.lat) #looks good
test$location.lat = as.numeric(test$location.lat)
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
ev31 = test
#
ev32 = read.csv("./Raw data/ISPRA.Italy.CaptiveRaised/190555_Diego.csv", header = TRUE, sep = ";")
head(ev32)
names(ev32)
names(ev32)[1] = "individual.local.identifier"
names(ev32)[2] = "timestamp"
names(ev32)[11] = "location.lat"
names(ev32)[12] = "location.long"
ev32$timestamp = dmy_hm(ev32$timestamp)
head(ev32)
summary(ev32)
#
test = ev32
test = dplyr::arrange(test, timestamp) #order by date
#remove lat/long with 0
test<-test[!(test$location.long==0 & test$location.lat==0),]
test$location.lat = as.numeric(test$location.lat)
test$location.long = as.numeric(test$location.long)
head(test)
summary(test)
#fix lat
head(test$location.lat)
test$location.lat = str_remove(test$location.lat, "[.]") # remove the decimal which is wrongly placed
head(test$location.lat)
test$location.lat = substr(test$location.lat, 1,6) #extract the first n characters from the string to standardize the string length
stri_sub(test$location.lat, 3, 2) ="." # add a . between 2nd and 3rd characters in string
head(test$location.lat) 
tail(test$location.lat) #looks good
test$location.lat = as.numeric(test$location.lat)
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
ev32 = test
#
ev33 = read.csv("./Raw data/ISPRA.Italy.CaptiveRaised/190556_Jane.csv", header = TRUE, sep = ";")
head(ev33)
names(ev33)
names(ev33)[1] = "individual.local.identifier"
names(ev33)[2] = "timestamp"
names(ev33)[11] = "location.lat"
names(ev33)[12] = "location.long"
ev33$timestamp = dmy_hm(ev33$timestamp)
head(ev33)
summary(ev33)
#
test = ev33
test = dplyr::arrange(test, timestamp) #order by date
#remove lat/long with 0
test<-test[!(test$location.long==0 & test$location.lat==0),]
test$location.lat = as.numeric(test$location.lat)
test$location.long = as.numeric(test$location.long)
head(test)
summary(test)
#fix lat
head(test$location.lat)
test$location.lat = str_remove(test$location.lat, "[.]") # remove the decimal which is wrongly placed
head(test$location.lat)
test$location.lat = substr(test$location.lat, 1,6) #extract the first n characters from the string to standardize the string length
stri_sub(test$location.lat, 3, 2) ="." # add a . between 2nd and 3rd characters in string
head(test$location.lat) 
tail(test$location.lat) #looks good
test$location.lat = as.numeric(test$location.lat)
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
test<-test[!(test$location.long>10 & test$location.lat<20),]
summary(test$location.long)
#quick plot of data
ggplot() + annotation_map(map_data("world"), fill = 'grey')  + coord_quickmap() + theme_bw() +
  geom_path(data = test, aes(location.long,location.lat)) + labs(x = "longitude", y = "latitude") +
  theme(legend.title = element_blank()) 
ev33 = test
#
ev34 = read.csv("./Raw data/ISPRA.Italy.CaptiveRaised/190557_Fabio.csv", header = TRUE, sep = ";")
head(ev34)
names(ev34)
names(ev34)[1] = "individual.local.identifier"
names(ev34)[2] = "timestamp"
names(ev34)[11] = "location.lat"
names(ev34)[12] = "location.long"
ev34$timestamp = dmy_hm(ev34$timestamp)
head(ev34)
summary(ev34)
#
test = ev34
test = dplyr::arrange(test, timestamp) #order by date
#remove lat/long with 0
test<-test[!(test$location.long==0 & test$location.lat==0),]
test$location.lat = as.numeric(test$location.lat)
test$location.long = as.numeric(test$location.long)
head(test)
summary(test)
#fix lat
head(test$location.lat)
test$location.lat = str_remove(test$location.lat, "[.]") # remove the decimal which is wrongly placed
head(test$location.lat)
test$location.lat = substr(test$location.lat, 1,6) #extract the first n characters from the string to standardize the string length
stri_sub(test$location.lat, 3, 2) ="." # add a . between 2nd and 3rd characters in string
head(test$location.lat) 
tail(test$location.lat) #looks good
test$location.lat = as.numeric(test$location.lat)
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
ev34 = test
#
ev35 = read.csv("./Raw data/ISPRA.Italy.CaptiveRaised/190557_Kate.csv", header = TRUE, sep = ";")
head(ev35)
names(ev35)
names(ev35)[1] = "individual.local.identifier"
names(ev35)[2] = "timestamp"
names(ev35)[11] = "location.lat"
names(ev35)[12] = "location.long"
ev35$timestamp = dmy_hm(ev35$timestamp)
head(ev35)
summary(ev35)
#
test = ev35
test = dplyr::arrange(test, timestamp) #order by date
#remove lat/long with 0
test<-test[!(test$location.long==0 & test$location.lat==0),]
test$location.lat = as.numeric(test$location.lat)
test$location.long = as.numeric(test$location.long)
head(test)
summary(test)
#fix lat
head(test$location.lat)
test$location.lat = str_remove(test$location.lat, "[.]") # remove the decimal which is wrongly placed
head(test$location.lat)
test$location.lat = substr(test$location.lat, 1,6) #extract the first n characters from the string to standardize the string length
stri_sub(test$location.lat, 3, 2) ="." # add a . between 2nd and 3rd characters in string
head(test$location.lat) 
tail(test$location.lat) #looks good
test$location.lat = as.numeric(test$location.lat)
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
ev35 = test

#
ev36 = read.csv("./Raw data/ISPRA.Italy.CaptiveRaised/200140_Birba.csv", header = TRUE, sep = ";")
head(ev36)
names(ev36)
names(ev36)[1] = "individual.local.identifier"
names(ev36)[2] = "timestamp"
names(ev36)[11] = "location.lat"
names(ev36)[12] = "location.long"
ev36$timestamp = dmy_hm(ev36$timestamp)
head(ev36)
summary(ev36)
#
test = ev36
test = dplyr::arrange(test, timestamp) #order by date
#remove lat/long with 0
test<-test[!(test$location.long==0 & test$location.lat==0),]
test$location.lat = as.numeric(test$location.lat)
test$location.long = as.numeric(test$location.long)
head(test)
summary(test)
#fix lat
head(test$location.lat)
test$location.lat = str_remove(test$location.lat, "[.]") # remove the decimal which is wrongly placed
head(test$location.lat)
test$location.lat = substr(test$location.lat, 1,6) #extract the first n characters from the string to standardize the string length
stri_sub(test$location.lat, 3, 2) ="." # add a . between 2nd and 3rd characters in string
head(test$location.lat) 
tail(test$location.lat) #looks good
test$location.lat = as.numeric(test$location.lat)
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
summary(test$timestamp)
#quick plot of data
ggplot() + annotation_map(map_data("world"), fill = 'grey')  + coord_quickmap() + theme_bw() +
  geom_path(data = test, aes(location.long,location.lat)) + labs(x = "longitude", y = "latitude") +
  theme(legend.title = element_blank()) 
ev36 = test
#
ev37 = read.csv("./Raw data/ISPRA.Italy.CaptiveRaised/202342_Zoe.csv", header = TRUE, sep = ";")
head(ev37)
names(ev37)
names(ev37)[1] = "individual.local.identifier"
names(ev37)[2] = "timestamp"
names(ev37)[11] = "location.lat"
names(ev37)[12] = "location.long"
ev37$timestamp = dmy_hm(ev37$timestamp)
head(ev37)
summary(ev37)
#
test = ev37
test = dplyr::arrange(test, timestamp) #order by date
#remove lat/long with 0
test<-test[!(test$location.long==0 & test$location.lat==0),]
test$location.lat = as.numeric(test$location.lat)
test$location.long = as.numeric(test$location.long)
head(test)
summary(test)
#fix lat
head(test$location.lat)
test$location.lat = str_remove(test$location.lat, "[.]") # remove the decimal which is wrongly placed
head(test$location.lat)
test$location.lat = substr(test$location.lat, 1,6) #extract the first n characters from the string to standardize the string length
stri_sub(test$location.lat, 3, 2) ="." # add a . between 2nd and 3rd characters in string
head(test$location.lat) 
tail(test$location.lat) #looks good
test$location.lat = as.numeric(test$location.lat)
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
ev37 = test
#
ev38 = read.csv("./Raw data/ISPRA.Italy.CaptiveRaised/AC5AF11F_Noe.csv", header = TRUE, sep = ";")
head(ev38)
names(ev38)
names(ev38)[1] = "individual.local.identifier"
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
names(ev39)[1] = "individual.local.identifier"
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
names(ev40)[1] = "individual.local.identifier"
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
names(ev41)[1] = "individual.local.identifier"
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

#merge (vertically) the data, keeping all unique columns
ev.ISPRA = rbind.fill(ev30,ev31,ev32,ev33,ev34,ev35,ev36,ev37,ev38,ev39,ev40,ev41)
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
summary(ev.ISPRA$timestamp)

#merge (vertically) the data, keeping all unique columns
ev.all = rbind.fill(ev.movebank,ev.ISPRA)
head(ev.all)
names(ev.all)
unique(ev.all$individual.local.identifier) #note 242 unique id.tag
ev.all$timestamp = ymd_hms(ev.all$timestamp)
summary(ev.all$timestamp)

#remove any other species
ev.all$individual.taxon.canonical.name = as.factor(ev.all$individual.taxon.canonical.name)
summary(ev.all$individual.taxon.canonical.name) #ut oh, some eagles in here!
ev.all = ev.all[!(ev.all$individual.taxon.canonical.name== "Aquila chrysaetos"),]
summary(ev.all$individual.taxon.canonical.name)
ev.all$individual.taxon.canonical.name = "Neophron percnopterus" #standardize the species name for all remaining which were the ISPRA EV
unique(ev.all$individual.local.identifier)

########################################################
#Final clean up
########################################################
ev.tv = ev.all #JUST ASSIGN EV DATA AS EV.TV HERE SO AS TO NOT CHANGE CODE HEREAFTER
summary(ev.tv$timestamp)
names(ev.tv)
unique(ev.tv$individual.local.identifier) #note 232 unique id

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
summary(ev.tv$timestamp)
ev.tv$timestamp = ymd_hms(ev.tv$timestamp)

#remove data from after Oct 31, 2020 (setting a cutoff point for the study data)
ev.tv <- subset(ev.tv, timestamp <= as.POSIXct('2020-10-31'))
summary(ev.tv$timestamp)
unique(ev.tv$id.tag) #232 individuals

#remove any rows that don't have date, lat or long
names(ev.tv)
ev.tv = ev.tv[complete.cases(ev.tv[,c("timestamp","long","lat")]),] 
unique(ev.tv$id.tag) #232 individuals

#reorder dataframe to have x,y,date,id.tag as first four columns
names(ev.tv)
ev.tv = ev.tv[,c(4,5,3,122,1:2,6:121)]
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
unique(ev.tv.1ptperday$id) #231
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
