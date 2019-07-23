# decision tree model to predict mortality from telemetry data

#Set WD
setwd("~/Documents/GitHub/EV - TV Survival Study")

##Clear workspace
rm(list = ls())

#read in data
d = read.csv("ev.tv.filtered.csv", colClasses = "character")
summ = read.csv("ev.tv.summary.merged.csv", colClasses = "character")
summary(d)
summary(summ$fate)

#need to create a data table that summarizes variables for the last 10 days...
#of transmission for each id (per Sergio et al. 2019)


#add column of fate to full dataset
# replace cell values for matching ids
d$fate = NA

for (i in unique(summ$id.tag)) { 
  d$fate[which(d$id.tag == i)] = summ$fate[which(summ$id.tag == i)]
}

d$fate = as.factor(d$fate)
summary(d$fate)

#build decision trees to predict fate of tracked individuals
#Potential Variables (per Sergio et al. 2019): 
#GPS dist --- the distance traveled between succesive fixes
#GPS dt.days --- the time (in days) between fixes (i.e. fix rate)
#Argos dist --- the Argos distance traveled between succesive fixes
#Argos dt.days --- the Argos time (in days) between fixes (i.e. fix rate)
#battery charge --- either increasing or decreasing battery charge (only available for some units)
#temperature --- either increasing or decreasing temp (only available for some units)
#activity counter --- will turn to stable number in absence of activity
#GPS spatial error --- low spatial error may indicate lack of movement, and thus mortality (??? not sure I buy this)
#Argos location classes 


library(rpart)
names(d)
d.tree = rpart(fate ~ dist + dt.days + NSD, data = d, method = "class")
