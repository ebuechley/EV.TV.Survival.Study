#Structuring data for survival analyses
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

#limit to EV
#d = subset(d, species == "Neophron percnopterus")
d$timestamp = ymd_hms(d$timestamp)
summary(d)

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
unique(d$id) #231

#remove any rows with NA for dist -- THIS WOULD REMOVE THE FIRST LOCATION FROM EACH ID
#d = d[complete.cases(d[,"dist"]),] 
#summary(d)
#unique(d$id)

#add data subset categories
#need: id, yr.mo, age.in.months, mean.monthly.nd
d$yr.mo = format(as.Date(d$timestamp), "%Y-%m")
d$id.tag.yr.mo = c(paste(d$id.tag,d$yr.mo,sep=" - ")) 
head(d)

#age in months
#append age.at.deployment.month to each id
ev.summary = read.csv("./Summary/ev.summary.quant.coauthor.merged.csv")
names(ev.summary)
ev.summary = ev.summary[,c("species","study.name", "population", "id","tag", "start.date", "end.date",
                           "mortality.date", "start.lat", "start.long", "end.lat", "end.long", 
                           "start.country", "end.country",
                           "deployment.duration", "fate", "age.at.deployment.months", "sex",
                           "migrant", "captive.raised", "rehabilitated", "how.fate.determined", "cause.of.death",
                           "transmitter.make.model", "transmitter.attachment.method", "transmitter.mass.grams",
                           "comments"
                           )]
#add id.tag variable
ev.summary$id.tag = c(paste(ev.summary$id,ev.summary$tag,sep="_")) 
summary(ev.summary)
head(ev.summary)

#review if summary and full data
#note there are 26 ids without age.at.deployment.month in the summary sheet -- these are non-migrants 
#or birds otherwise not to include in our dataset, so ok to now delete them
ev.summary = ev.summary[complete.cases(ev.summary[ ,"age.at.deployment.months"]),]
summary(ev.summary)

d <- d[order(d$id),] 
ev.summary <- ev.summary[order(ev.summary$id),] 
unique(d$id) #231
unique(ev.summary$id) #205
unique(ev.summary$start.country)
summary(ev.summary)

# fix id values
#these were edits made to the summary table by data owners that I am reincorprating back to the original data
#d$id = as.character(d$id)
#ev.summary$id = as.character(ev.summary$id)
#d$id[d$id=="1_1"] <- "52027_1"
#d$id[d$id=="93_14"] <- "81_14"
#d$id[d$id=="AF5AF11F_NA"] <- "Bianca_IHB_AF5AF11F"
#d$id[d$id=="B05AF11F_NA"] <- "Clara_IHC_B05AF11F"
#d$id[d$id=="Provence_2016_Ad_wild_EO5018_Salomé_8P_5018"] <- "Provence_2016_Ad_wild_EO5018_Salome_8P_5018"
#ev.summary$id[ev.summary$id=="Provence_2016_Ad_wild_EO5018_Salom√©_8P_5018"] <- "Provence_2016_Ad_wild_EO5018_Salome_8P_5018"
#d<-d[!(d$id=="Djibouti_127589"),]

# replace cell values for matching ids
d$age.at.deployment.month = NA
head(d)
head(ev.summary)
for (i in unique(ev.summary$id)) { 
  d$age.at.deployment.month[which(d$id == i)] = ev.summary$age.at.deployment.month[which(ev.summary$id == i)]
}
summary(d)

#note there are 26 ids without age.at.deployment.month in the summary sheet -- these are non-migrants 
#or birds otherwise not to include in our dataset, so ok to now delete them from d
names(d)
d = d[complete.cases(d[ ,17]),]
summary(d)
unique(d$id) #now down to 205 unique id
unique(ev.summary$id)

#add column with date of tagging for each individual
d$deployment.date = d$timestamp
head(ev.summary)
ev.summary$start.date = ymd_hms(ev.summary$start.date)
summary(ev.summary$start.date)

for (i in unique(ev.summary$id)) { 
  d$deployment.date[which(d$id == i)] = ev.summary$start.date[which(ev.summary$id == i)]
}
summary(d)
head(d)

#add column with captive/wild for each individual
d$captive.raised = NA
summary(ev.summary$captive.raised)

for (i in unique(ev.summary$id)) { 
  d$captive.raised[which(d$id == i)] = ev.summary$captive.raised[which(ev.summary$id == i)]
}

d$captive.raised = as.vector(d$captive.raised)

#add column with mortality date for each individual
d$mortality.date = d$timestamp
head(ev.summary$mortality.date)
ev.summary$mortality.date = mdy_hm(ev.summary$mortality.date)

#if mortality date = NA, then assign to last day of study (Oct 31, 2020)
summary(ev.summary$mortality.date)
ev.summary$mortality.date[is.na(ev.summary$mortality.date)] = ymd_hms("2020-10-31 00:00:00")

for (i in unique(ev.summary$id)) { 
  d$mortality.date[which(d$id == i)] = ev.summary$mortality.date[which(ev.summary$id == i)]
}

summary(d$mortality.date)

#censor data to mortality date
summary(d$mortality.date)
d = subset(d, d$timestamp <= d$mortality.date)
summary(d$mortality.date)
unique(d$id) #204 -- THIS REMOVED 1 ID
unique(ev.summary$id)
unique(ev.summary$id) #ID REMOVED WAS "Zaror I30 Red", REMOVE ALSO FROM EV.SUMMARY
ev.summary<-ev.summary[!(ev.summary$id=="Zaror I30 Red"),]
unique(ev.summary$id) == unique(d$id)
head(ev.summary$end.date)

###########################################################################
#after censoring data to mortality date, need to recalculate summary stats!
#and insert into summary sheet
###########################################################################
summary(d)

#convert to data.table to summarize
d.dt = setDT(d)

#check data
head(d.dt)
summary(d.dt)
names(d.dt)

#summarize
ev.summary.mort = d.dt[,.(unique(species), unique(study.name),
                        min(timestamp), max(timestamp), head(lat,1), head(long,1), 
                        tail(lat,1), tail(long,1), mean(tail(dist,10)), mean(tail(dt.days,10))),by = .(id)]
head(ev.summary.mort)

#add headers
names(ev.summary.mort) = c("id", "species", "study.name", "start.date", 
                         "end.date", "start.lat", "start.long", "end.lat", "end.long", 
                         "mean.GPS.dist.last10fixes", "mean.GPS.fixrate.last10fixes")
head(ev.summary.mort)
summary(ev.summary.mort)

#add deployment duration
ev.summary.mort$deployment.duration = as.numeric(difftime(ev.summary.mort$end.date, ev.summary.mort$start.date, units = "days"))
head(ev.summary.mort)

#insert new summary calculations into the full summary sheet
unique(ev.summary.mort$id) == unique(ev.summary$id)
names(ev.summary.mort)
names(ev.summary)

for (i in unique(ev.summary.mort$id)) { 
  ev.summary$end.date[which(ev.summary$id == i)] = as.Date(ev.summary.mort$end.date[which(ev.summary.mort$id == i)])
}
for (i in unique(ev.summary.mort$id)) { 
  ev.summary$end.lat[which(ev.summary$id == i)] = ev.summary.mort$end.lat[which(ev.summary.mort$id == i)]
}
for (i in unique(ev.summary.mort$id)) { 
  ev.summary$end.long[which(ev.summary$id == i)] = ev.summary.mort$end.long[which(ev.summary.mort$id == i)]
}
head(ev.summary)

########################################
#add start / end country
########################################
require(rgdal)
world = readOGR(dsn = "./Data/TM_WORLD_BORDERS_SIMPL-0.3/", layer = "TM_WORLD_BORDERS_SIMPL-0.3")

#convert summary to spdf
names(ev.summary)
xy.start <- ev.summary[,c(10,9)]
spdf.start <- SpatialPointsDataFrame(coords = xy.start, data = ev.summary,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#check that the projections match
proj4string(spdf.start) == proj4string(world)
plot(spdf.start)
plot(world, add = T)

#sample and append country to summary
start.country = data.frame(over(spdf.start, world[,5]))
names(start.country) = c("start.country.new")
start.country
ev.summary.country = cbind(ev.summary, start.country)
summary(ev.summary.country)

#
names(ev.summary.country)
xy.end = ev.summary.country[,c(12,11)]
spdf.end <- SpatialPointsDataFrame(coords = xy.end, data = ev.summary.country,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#check that the prjections match
proj4string(spdf.end) == proj4string(world)
plot(spdf.end)
plot(world, add = T)

#sample and append country to summary
end.country = data.frame(over(spdf.end, world[,5]))
names(end.country) = c("end.country.new")
end.country
ev.summary.country = cbind(ev.summary.country, end.country)
summary(ev.summary.country)
unique(ev.summary.country$id.tag) 
ev.summary = ev.summary.country

#add number of locations column
ev.summary = as.data.frame(ev.summary)
ev.summary
n.locs = count(d.dt$id)
n.locs$id = n.locs$x
n.locs$n.locs = n.locs$freq
n.locs$freq = NULL
n.locs$x = NULL
head(n.locs)
ev.summary = merge(ev.summary, n.locs, by = "id", all = T)
head(ev.summary)

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

#add age.at.deployment.month + months.from.tagging to get age.in.months
d$age.in.months = d$age.at.deployment.month + d$months.from.tagging
summary(d$age.in.months)
d[,c("age.in.months","age.at.deployment.month", "months.from.tagging")]

d$age.in.months.capped = d$age.in.months
d$age.in.months.capped[d$age.in.months.capped>=54] <- 54
summary(d)

#mean monthly ND
d$mean.monthly.ND = ave(d$ND, d$id.yr.mo)

#mean.monthly dist
d$mean.monthly.dist = ave(d$dist, d$id.yr.mo, FUN = mean)
d$sum.monthly.dist = ave(d$dist, d$id.yr.mo, FUN = sum)
head(d)
summary(d)

#mean.monthly.latitude
d$mean.monthly.lat = ave(d$lat, d$id.yr.mo, FUN = mean)
d$mean.monthly.long = ave(d$long, d$id.yr.mo, FUN = mean)
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
  coord_quickmap() + theme_bw() + geom_path(data = d, aes(long,lat, group = id, color = population)) + 
  labs(x = "longitude", y = "latitude") +
  theme(legend.title = element_blank()) 

#write edited files
summary(d)
summary(ev.summary)
write.csv(d,"./Data/ev.survival.prepared.Rev1.csv", row.names=FALSE)
write.csv(ev.summary,"./Summary/ev.summary.final.Rev1.csv", row.names=FALSE)
