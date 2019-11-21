#Structuring data for survival analyses
#load packages
library(lubridate)
library(ggplot2)
library(data.table)

#Clear workspace
rm(list = ls())

#set wd
setwd("~/Documents/GitHub/EV - TV Survival Study/")

#read data
d = read.csv("Final Cleaned Data/ev.tv.filtered.csv")

#limit to EV
d = subset(d, species == "Neophron percnopterus")
d$timestamp = ymd_hms(d$timestamp)
summary(d)

#clean up data to relevent columns
#index
#dt.days == duration in time (in days) between the relocations i and i + 1
#dist == the distance between successive relocations
#NSD/ND == distance between the first relocation of the trajectory and 
  #the current relocation (net [squared] displacement)
names(d)
d = d[,c("species","study.name","population","id.tag","timestamp","lat",
         "long","year","month","day","NSD","ND","dist","dt.days")]
summary(d)

#remove any rows with NA
d = d[complete.cases(d[,"dist"]),] 
summary(d)

#add data subset categories
#need: id.tag, yr.mo, age.in.months, mean.monthly.nd
d$yr.mo = format(as.Date(d$timestamp), "%Y-%m")
d$id.tag.yr.mo = c(paste(d$id.tag,d$yr.mo,sep=" - ")) 
head(d)

#age in months
#append age.at.deployment.month to each id
head(d)
ev.summary = read.csv("ev.tv.summary.proofed.csv")
names(ev.summary)
ev.summary = subset(ev.summary, species == "Neophron percnopterus")
d <- d[order(d$id.tag),] 
ev.summary <- ev.summary[order(ev.summary$id.tag),] 
unique(d$id.tag)
unique(ev.summary$id.tag)

# fix id.tag values
#these were edits made to the summary table by data owners that I am reincorprating back to the original data
d$id.tag = as.character(d$id.tag)
ev.summary$id.tag = as.character(ev.summary$id.tag)
d$id.tag[d$id.tag=="1_1"] <- "52027_1"
d$id.tag[d$id.tag=="93_14"] <- "81_14"
d$id.tag[d$id.tag=="AF5AF11F_NA"] <- "Bianca_IHB_AF5AF11F"
d$id.tag[d$id.tag=="B05AF11F_NA"] <- "Clara_IHC_B05AF11F"
d$id.tag[d$id.tag=="Provence_2016_Ad_wild_EO5018_Salomé_8P_5018"] <- "Provence_2016_Ad_wild_EO5018_Salome_8P_5018"
ev.summary$id.tag[ev.summary$id.tag=="Provence_2016_Ad_wild_EO5018_Salom√©_8P_5018"] <- "Provence_2016_Ad_wild_EO5018_Salome_8P_5018"
d<-d[!(d$id.tag=="Djibouti_127589"),]

# replace cell values for matching ids
d$age.at.deployment.month = NA
for (i in unique(ev.summary$id.tag)) { 
  d$age.at.deployment.month[which(d$id.tag == i)] = ev.summary$age.at.deployment.month[which(ev.summary$id.tag == i)]
}
summary(d)

#add column with date of tagging for each individual
d$deployment.date = d$timestamp
ev.summary$start.date = mdy_hm(ev.summary$start.date)
summary(ev.summary$start.date)

for (i in unique(ev.summary$id.tag)) { 
  d$deployment.date[which(d$id.tag == i)] = ev.summary$start.date[which(ev.summary$id.tag == i)]
}
summary(d)
head(d)

#add column with mortality date for each individual
d$mortality.date = d$timestamp
ev.summary$mortality.date = mdy_hm(ev.summary$mortality.date)
summary(ev.summary$mortality.date)

for (i in unique(ev.summary$id.tag)) { 
  d$mortality.date[which(d$id.tag == i)] = ev.summary$mortality.date[which(ev.summary$id.tag == i)]
}
summary(d)
head(d)

#censor data to mortality date
d = subset(d, d$timestamp <= d$mortality.date)
summary(d)

#calculate # months from tagging (pulled this function from: https://stackoverflow.com/questions/1995933/number-of-months-between-two-dates/1996404)
# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) {  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); lt$year*12 + lt$mon } 
# compute a month difference as a difference between two month's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
# calcvulate on dataset
d$months.from.tagging = mondf(d$deployment.date,d$timestamp)
d[,c("timestamp","deployment.date", "months.from.tagging")] #checked and looks good
summary(d)
head(d)

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
d$mean.monthly.lomg = ave(d$long, d$id.tag.yr.mo, FUN = mean)
summary(d)

#explore data structure
d$month = as.factor(d$month)
ggplot(d, aes(month,mean.monthly.ND)) + geom_boxplot()
ggplot(d, aes(month,mean.monthly.dist)) + geom_boxplot()
ggplot(d, aes(month,sum.monthly.dist)) + geom_boxplot()
ggplot(d, aes(month,mean.monthly.lat)) + geom_boxplot()
hist(d$sum.monthly.dist)
hist(d$age.at.deployment.month)
hist(d$age.in.months.capped)
hist(d$age.in.months)

#quick plot
map.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey')  + 
  coord_quickmap() + theme_bw() + geom_path(data = d, aes(long,lat, group = id.tag)) + 
  labs(x = "longitude", y = "latitude") +
  theme(legend.title = element_blank()) 
map.plot

write.csv(d,"Final Cleaned Data/ev.survival.prepared.csv", row.names=FALSE)
