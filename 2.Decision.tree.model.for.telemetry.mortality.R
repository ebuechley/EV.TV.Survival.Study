# decision tree model to predict mortality from telemetry data

#Set WD
setwd("~/Documents/GitHub/EV - TV Survival Study")

##Clear workspace
rm(list = ls())

#read in data
d = read.csv("ev.all.1ptperday.csv")
summ = read.csv("ev.summary.merge.csv")
summary(d)
summary(summ)

#add column of fate to full dataset
