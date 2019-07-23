# decision tree model to predict mortality from telemetry data
library(party)

#Set WD
setwd("~/Documents/GitHub/EV - TV Survival Study")

##Clear workspace
rm(list = ls())

#read in data
d = read.csv("ev.tv.summary.merged.csv")
summary(d)

unique(d$fate)
d<-d[!(d$fate=="likely transmitter failure"),]
d<-d[!(d$fate=="likely dead"),]
d<-d[!(d$fate=="unknown"),]
d<-d[!(d$fate=="captive"),]
d<-d[!(d$fate=="confirmed transmitter failure"),]
write.csv(d, "test.csv")
d = read.csv("test.csv")
summary(d$fate)
head(d)
d

#need to create a data table that summarizes status or change in variables ...
#for the last 10 days of transmission for each id (per Sergio et al. 2019)
#BOTE --- have started to do this in the previous script

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

names(d)
#rpart
d.tree = rpart(fate ~ mean.GPS.fixrate.last10fixes + mean.GPS.dist.last10fixes + mean.battery.charge.percent.last10fixes, data = d, method = "class")
print(d.tree)
plot(d.tree, uniform=TRUE, main="Vulture Mortality Tree")
text(d.tree, use.n=TRUE, all=TRUE, cex=.8)

#party
fit <- ctree(fate ~ mean.GPS.fixrate.last10fixes + mean.GPS.dist.last10fixes + mean.battery.charge.percent.last10fixes, data = d)
plot(fit, main="Vulture Fates")

###############################################
#add column of fate to full dataset
# replace cell values for matching ids
d$fate = NA

for (i in unique(d$id.tag)) { 
  d$fate[which(d$id.tag == i)] = d$fate[which(d$id.tag == i)]
}

d$fate = as.factor(d$fate)
summary(d$fate)


