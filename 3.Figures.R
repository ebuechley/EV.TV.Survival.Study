#############################################################
#Figures and summary stats
#############################################################
library(ggplot2)
library("gridExtra")
library(lubridate)
library(ggmap)
library(data.table)
library(ggpubr)
library(ggnewscale)
library(plyr)

#Clear workspace
rm(list = ls())

#set wd
setwd("~/Google Drive/GitHub/EV.TV.Survival.Study/")

#read data
d = read.csv("ev.final.Rev1.survival.prepared.csv", stringsAsFactors=TRUE)
d.summ= read.csv("ev.summary.final.Rev1.survival.prepared.AA.csv", stringsAsFactors=TRUE)
d.summ$start.date
d.summ$start.date = mdy_hm(d.summ$start.date)
d.summ$end.date
d.summ$end.date = mdy(d.summ$end.date)
summary(d.summ$end.date)
summary(d.summ$start.date)
d.summ$mortality.date
d.summ$mortality.date = mdy_hm(d.summ$mortality.date)

#add population.binned
unique(d.summ$population.binned)
d.summ$population.binned = as.character(d.summ$population.binned)
d.summ$population.binned[d.summ$population.binned == "central & eastern"] <-  "eastern"
unique(d$population)
d$population.binned = NA
d$population.binned[d$population == "middle east"] <-  "eastern"
d$population.binned[d$population == "caucasus"] <-  "eastern"
d$population.binned[d$population == "western europe"] <-  "western"
d$population.binned[d$population == "italy"] <-  "eastern"
d$population.binned[d$population == "balkans"] <-  "eastern"
unique(d$population.binned)
d$color = d$population.binned
d$color[d$color == "eastern"] <-  "#800000"
#d$color[d$color == "central & eastern"] <-  "#8B4513"
d$color[d$color == "western"] <-  "#F4A460"
head(d)
head(d.summ)

#reclassify age
unique(d.summ$age.at.fate.simple)
d.summ$age.at.fate.simple = as.character(d.summ$age.at.fate.simple)
d.summ$age.at.fate.simple[d.summ$age.at.fate.simple == "adult"] <-  "immature/adult"

#remove alive for plotting end fates
d.summ2<-d.summ[!(d.summ$fate == "alive"),] 

#remove birds from unknown pop
#d.summ2<-d.summ2[!(d.summ2$population == "unknown"),] 

#remove rehabbed birds 
#d.summ2<-d.summ2[!(d.summ2$rehabilitated == "Y"),] 

#remove imprinted birds 
#d.summ2<-d.summ2[!(d.summ2$fate == "returned to captivity"),] 

#rename origin variables
d.summ2$origin = as.character(d.summ2$origin)
d.summ2$origin[d.summ2$origin == "N"] <-  "wild"
d.summ2$origin[d.summ2$origin == "Y"] <-  "captive-raised"

#rename fate at annual stage variables
d.summ2$fate.at.annual.stage = as.character(d.summ2$fate.at.annual.stage)
unique(d.summ2$fate.at.annual.stage)
d.summ2$fate.at.annual.stage[d.summ2$fate.at.annual.stage == "non-breeding"] <-  "non-breed."
d.summ2$fate.at.annual.stage[d.summ2$fate.at.annual.stage == "fall migration"] <-  "fall mig."
d.summ2$fate.at.annual.stage[d.summ2$fate.at.annual.stage == "breeding"] <-  "breed."
d.summ2$fate.at.annual.stage[d.summ2$fate.at.annual.stage == "spring migration"] <-  "spring mig."
unique(d.summ2$fate.at.annual.stage)
d.summ2$fate.at.annual.stage = as.factor(d.summ2$fate.at.annual.stage)
summary(d.summ2$fate.at.annual.stage)
summary(d.summ2)

#rename fate variables
d.summ2$fate = as.character(d.summ2$fate)
unique(d.summ2$fate)
d.summ2$fate.binned = NA
d.summ2$fate.binned[d.summ2$fate == "confirmed dead"] <-  "dead"
d.summ2$fate.binned[d.summ2$fate == "likely transmitter failure"] <-  "unknown"
d.summ2$fate.binned[d.summ2$fate == "unknown"] <-  "unknown"
d.summ2$fate.binned[d.summ2$fate == "confirmed transmitter failure"] <-  "tx failure"
d.summ2$fate.binned[d.summ2$fate == "likely dead"] <-  "dead"
d.summ2$fate.binned[d.summ2$fate == "Confirmed dead"] <-  "dead"
d.summ2$fate.binned[d.summ2$fate == "returned to captivity"] <-  "dead"
unique(d.summ2$fate.binned)
d.summ2$fate.binned = as.factor(d.summ2$fate.binned)
summary(d.summ2$fate.binned)
summary(d.summ2)

#plot specific id's
unique(d$id)
unique(d$year)
test = subset(d, d$study.name == "Egyptian vulture Kobierzycki Vaucluse ID_PROG 457")
test = subset(d, d$id == "Provence_2016_Ad_wild_EO5018_Salome_8P")
d = d[!(d$id =="Provence_2016_Ad_wild_EO5018_Salome_8P" & d$year == "2019"),] #remove data from long data gaps that produce straight lines over Med
test = subset(d, d$study.name == "Neophron percnopterus Bulgaria/Greece")
test = subset(d, d$id == "Aoos")
d = d[!(d$id =="Aoos" & d$year == "2020"),] #remove data from long data gaps that produce straight lines over Med
test = subset(d, d$study.name == "LIFE_Rupis_EgyptianVultures")
test = subset(d, d$id == "Rupis")
d = d[!(d$id =="Rupis" & d$year == "2020"),] #remove data from long data gaps that produce straight lines over Med

ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white")  + coord_quickmap() + theme_bw() + 
  geom_path(data = d, aes(long,lat, group = id, color = year)) + labs(x = "longitude", y = "latitude") 

#plot
setwd("~/Google Drive/Research Projects/EV-TV Survival Study/Manuscript/Latest/Rev1/")

pmap = ggplot() + annotation_map(map_data("world"), fill = 'light grey', color = "white") + coord_quickmap() + theme_bw() +
  geom_path(data = d, aes(long,lat, group = id), color = d$color, alpha = .5) +
  scale_color_manual(values= c( "#F4A460","#8B4513"), name = "subpopulation") + #"#800000"
  #scale_color_viridis_d(begin = .2, end = .8, direction = 1, name = "subpopulation:") +
  new_scale_color() + 
  geom_point(data = d.summ2, aes(end.long, end.lat, color = fate.binned),  size = 3) +
  scale_color_viridis_d(option = "D", begin = 0.4, direction = -1, name = "fate:") + 
  theme(plot.title = element_text(hjust = 0.5)) + labs(x = "longitude", y = "latitude") 
pmap

#geom_bar(aes(y = (..count..)/sum(..count..)), position = position_dodge()) + scale_y_continuous(labels=scales::percent_format(accuracy=1)) + 
d.summ2$population.binned
p1 = ggplot(d.summ2, aes(population.binned, fill = fate.binned)) + theme_bw() + 
  geom_bar(position = position_dodge()) + ylab("# fates") + xlab("subpopulation") + 
  scale_fill_viridis_d(begin = .4,  direction = -1,  name = "fate:") + theme(legend.position = "none")
  p1 

# fate by age
p2 = ggplot(d.summ2, aes(age.at.fate.simple, fill = fate.binned)) + theme_bw() + 
  geom_bar(position = position_dodge()) + ylab("# fates") + xlab("age") + 
  scale_fill_viridis_d(begin = 0.4, direction = -1,  name = "fate:") + theme(legend.position = "none")
p2

# fate by annual stage
p3 = ggplot(d.summ2, aes(fate.at.annual.stage, fill = fate.binned)) + theme_bw() + 
  geom_bar(position = position_dodge()) + ylab("# fates") + xlab("annual stage") + 
  scale_fill_viridis_d(begin = 0.4, direction = -1,  name = "fate:") + theme(legend.position = "none")
p3

# fate by origin
p4 = ggplot(d.summ2, aes(origin, fill = fate.binned)) + theme_bw() + 
  geom_bar(position = position_dodge()) + ylab("# fates") + xlab("origin") + 
  scale_fill_viridis_d(begin = .4, direction = -1,  name = "fate:") + theme(legend.position = "none")
p4

#combine plots
p5 = ggarrange(pmap, 
               ggarrange(p1, p2, p3, p4,  ncol =1, nrow = 4),
               widths = c(3,1.2), legend = "bottom")
p5

#print
jpeg("./Figures/fate.summary.plot.Rev1.AA.jpg", units="cm", width=28, height=15, res=300)
p5
dev.off()

#############################################################################
# summary stats on data summary
#year, location, age and origin by bird months
#############################################################################
library(data.table)
setwd("~/Google Drive/GitHub/EV.TV.Survival.Study/")
ev.summ= read.csv("ev.summary.final.Rev1.survival.prepared.csv", stringsAsFactors=TRUE)
d = read.csv("ev.final.Rev1.survival.prepared.csv", stringsAsFactors=TRUE)
summary(ev.summ)
unique(ev.summ$id)
summary(d)

#lubridate
head(ev.summ)
ev.summ$start.date
ev.summ$start.date = mdy_hm(ev.summ$start.date)

#create population.origin.age.year
#reclassify origin
summary(ev.summ$origin)
ev.summ$origin = as.character(ev.summ$origin)
ev.summ$origin[ev.summ$origin=="N"] <- "wild" 
ev.summ$origin[ev.summ$origin=="Y"] <- "captive"
ev.summ$population.origin = c(paste(ev.summ$population.binned,ev.summ$origin,sep=" - "))
head(ev.summ$population.origin)

#create age simple 
summary(ev.summ$age.at.deployment.months)
ev.summ$age.at.deployment.simple  = ifelse(ev.summ$age.at.deployment.months >=18, "adult", "juvenile")
ev.summ$age.at.deployment.simple = as.factor(ev.summ$age.at.deployment.simple)
summary(ev.summ$age.at.deployment.simple)
ev.summ$population.origin.age = c(paste(ev.summ$population.origin,ev.summ$age.at.deployment.simple,sep=" - "))
ev.summ$population.origin.age = as.factor(ev.summ$population.origin.age)
summary(ev.summ$population.origin.age)

#create "year.tagged"
ev.summ$year.tagged = year(ev.summ$start.date)
ev.summ$population.origin.age.year = c(paste(ev.summ$population.origin.age,ev.summ$year.tagged,sep=" - "))
ev.summ$population.origin.age.year = as.factor(ev.summ$population.origin.age.year)
summary(ev.summ$population.origin.age.year)

#process full data to get bird months
#extract unique bird months by id
d = d[!duplicated(d[,c('id.tag.yr.mo')]),]

#sort data by id and check if match
ev.summ = ev.summ[order(ev.summ$id.tag),] 
d = d[order(d$id.tag),] 
ev.summ$id.tag = as.character(ev.summ$id.tag)
d$id.tag = as.character(d$id.tag)
unique(ev.summ$id.tag) == unique(d$id.tag) #don't match
unique(ev.summ$id.tag)[177] 
unique(d$id.tag)[177] 
ev.summ<-ev.summ[!(ev.summ$id.tag=="R2_190604"),]
unique(ev.summ$id.tag) == unique(d$id.tag) #don't match

#add columns from summary to full data that we want to summarize
ev.summ$age.at.deployment.simple = as.character(ev.summ$age.at.deployment.simple)
d$population.binned = NA
d$origin = NA
d$age.at.deployment.simple = NA
d$year.tagged = NA
d$transmitter.make.model = NA
d$transmitter.mass.grams.binned = NA
d$transmitter.attachment.method = NA
summary(d)
summary(ev.summ)
ev.summ$population.binned = as.character(ev.summ$population.binned)
ev.summ$transmitter.attachment.method = as.character(ev.summ$transmitter.attachment.method)
ev.summ$transmitter.mass.grams.binned = as.character(ev.summ$transmitter.mass.grams.binned)
for (i in unique(ev.summ$id.tag)) { 
  d$population.binned[which(d$id.tag == i)] = ev.summ$population.binned[which(ev.summ$id.tag == i)]
}
for (i in unique(ev.summ$id.tag)) { 
  d$origin[which(d$id.tag == i)] = ev.summ$origin[which(ev.summ$id.tag == i)]
}
for (i in unique(ev.summ$id.tag)) { 
  d$age.at.deployment.simple[which(d$id.tag == i)] = ev.summ$age.at.deployment.simple[which(ev.summ$id.tag == i)]
}
for (i in unique(ev.summ$id.tag)) { 
  d$year.tagged[which(d$id.tag == i)] = ev.summ$year.tagged[which(ev.summ$id.tag == i)]
}
for (i in unique(ev.summ$id.tag)) { 
  d$transmitter.make.model[which(d$id.tag == i)] = ev.summ$transmitter.make.model[which(ev.summ$id.tag == i)]
}
for (i in unique(ev.summ$id.tag)) { 
  d$transmitter.mass.grams.binned[which(d$id.tag == i)] = ev.summ$transmitter.mass.grams.binned[which(ev.summ$id.tag == i)]
}
for (i in unique(ev.summ$id.tag)) { 
  d$transmitter.attachment.method[which(d$id.tag == i)] = ev.summ$transmitter.attachment.method[which(ev.summ$id.tag == i)]
}
head(d)
d$age.in.months.capped.simple = ifelse(d$age.in.months.capped >=18, "adult", "juvenile")
d = data.table(d)
d$population.binned = as.factor(d$population.binned)
head(d)

# create and write supplemental tables
setwd("~/Google Drive/Research Projects/EV-TV Survival Study/Manuscript/Latest/Rev1/Tables/")
table1 <- d[, .(.N), by = .(population.binned,origin,age.in.months.capped.simple)]
names(table1) = c("population","origin", "age","bird months")
table1 = table1[order(table1$population, table1$origin, table1$age),] 
table1
write.csv(table1, "pop.origin.age.csv", row.names = F)

#year
table2 <- d[, .(.N), by = .(year)]
names(table2) = c("year","bird months")
table2 = table2[order(table2$year),] 
table2
write.csv(table2, "year.csv", row.names = F)

#tags
table3 <- d[, .(.N), by = .(population.binned,age.in.months.capped.simple,transmitter.attachment.method,transmitter.mass.grams.binned)]
table3 = table3[order(population.binned,age.in.months.capped.simple,transmitter.attachment.method,transmitter.mass.grams.binned,N),] 
names(table3) = c("population", "age","tx attachment method", "tx mass (grams)",   "bird months")
table3
write.csv(table3, "transmitter.method.mass.age.pop.csv", row.names = F)

########################################################
# summarize deaths
#########################################################
setwd("~/Google Drive/GitHub/EV.TV.Survival.Study/")
ev.summ= read.csv("ev.summary.final.Rev1.survival.prepared.csv", stringsAsFactors=TRUE)
summary(ev.summ$fate)
dead = subset(ev.summ, ev.summ$fate == "confirmed dead")
likely.dead = subset(ev.summ, ev.summ$fate == "likely dead")
dead = rbind(dead, likely.dead)
summary(dead$cause.of.death)
nrow(dead)
setwd("~/Google Drive/Research Projects/EV-TV Survival Study/Manuscript/Latest/Rev1/")
#write.csv(dead, "dead.Rev1.3.csv", row.names = F)

##edited this sheet to simplify cause of death
dead = read.csv("dead.Rev1.3.csv")
head(dead)

#reclassify origin
dead$origin = as.character(dead$origin)
dead$origin[dead$origin=="N"] <- "wild" 
dead$origin[dead$origin=="Y"] <- "captive"
dead$origin

#rename macedonia
dead$start.country = as.character(dead$start.country)
dead$end.country = as.character(dead$end.country)
dead$start.country[dead$start.country=="The former Yugoslav Republic of Macedonia"] <- "North Macedonia" 
dead$end.country[dead$end.country=="The former Yugoslav Republic of Macedonia"] <- "North Macedonia"

#age simple
dead$age.at.deployment.simple  = ifelse(dead$age.at.deployment.months >=18, "immature/adult", "juvenile")
dead$age.at.fate.simple  = ifelse(dead$age.at.fate.months >=18, "immature/adult", "juvenile")
head(dead$age.at.deployment.simple)
head(dead$age.at.fate.simple)
head(dead$cause.of.death.binned)
head(dead$death.anthro)
head(dead$origin)

#convert to factor
dead$start.country = as.factor(dead$start.country)
dead$end.country = as.factor(dead$end.country)
dead$origin = as.factor(dead$origin)
dead$age.at.deployment.simple = as.factor(dead$age.at.deployment.simple)
dead$age.at.fate.simple = as.factor(dead$age.at.fate.simple)
dead$cause.of.death.binned = as.factor(dead$cause.of.death.binned)
dead$death.anthro = as.factor(dead$death.anthro)
summary(dead)

#convert to data table to summarize
d = data.table(dead)
names(d)
dead <- d[, .(.N), by = .(id,start.country,end.country,origin,age.at.deployment.simple, age.at.fate.simple,cause.of.death.binned,death.anthro,how.fate.determined)]
dead = dead[order(id,start.country,end.country,origin,age.at.deployment.simple, age.at.fate.simple,cause.of.death.binned, death.anthro,how.fate.determined),] 
dead$N = NULL
dead$id = NULL
names(dead) = c("country tagged","location died","origin","age at tagging", "age at fate", "cause of death", "anthropogenic death?", "how fate determined")
dead
summary(dead)
setwd("~/Google Drive/Research Projects/EV-TV Survival Study/Manuscript/Latest/Rev1/Tables/")
write.csv(dead, "mortalities.csv", row.names = F)

############################################################
#data summary table
############################################################
setwd("~/Google Drive/GitHub/EV.TV.Survival.Study/")
d = read.csv("ev.summary.final.Rev1.survival.prepared.csv", stringsAsFactors=TRUE)
d = data.table(d)
names(d)
d1 <- d[, .(.N), by = .(study.name,start.country)]
d1
setwd("~/Google Drive/Research Projects/EV-TV Survival Study/Manuscript/Latest/Rev1/Tables/")
#write.csv(d1, "data.origin.csv", row.names = F)

############################################################
#summary stats for Mss
############################################################
setwd("~/Google Drive/GitHub/EV.TV.Survival.Study/")
d = read.csv("ev.final.Rev1.survival.prepared.csv", stringsAsFactors=TRUE)
d.summ= read.csv("ev.summary.final.Rev1.survival.prepared.csv", stringsAsFactors=TRUE)
d1 = data.table(d.summ)

#n birds
unique(d1$id)

#n birds by pop and origin
d1[, .(.N), by = .(population.binned)]
d1[, .(.N), by = .(population.binned,origin)]

#duration tracked
summary(d1$deployment.duration.months)

#fate
summary(d1$fate)

############################################################
#Plots by EV populations
############################################################
d1 = subset(d, d$species == "Neophron percnopterus")
names(d1)
unique(d1$population)
we = subset(d1, d1$population == "western europe")
it = subset(d1, d1$population == "italy")
ba = subset(d1, d1$population == "balkans")
ca = subset(d1, d1$population == "caucasus")
me = subset(d1, d1$population == "middle east")

#all
ggplot() + theme_bw() + coord_quickmap() + 
  geom_path(data = d1, aes(long,lat, group = id.tag, color = id.tag), alpha = .5) +
  scale_fill_viridis_d(begin = .4, direction = -1) +
  theme(legend.position = "none",  plot.background = element_blank(), 
                            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.border = element_blank(), axis.ticks = element_blank(), 
                            axis.text=element_blank(), axis.title = element_blank())
                            
#western europe
summary(we)
we.plot = ggplot() + theme_bw() + coord_quickmap()
we.plot = we.plot + geom_path(data = we, aes(long,lat, group = id.tag, color = id.tag), alpha = .5) 
we.plot = we.plot + theme(legend.position = "none",  plot.background = element_blank(), 
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.border = element_blank(), axis.ticks = element_blank(), 
                          axis.text=element_blank(), axis.title = element_blank())
we.plot

#italy
it.plot = ggplot() + theme_bw() + coord_quickmap()
it.plot = it.plot + geom_path(data = it, aes(long,lat, group = id.tag, color = id.tag), alpha = .5) 
it.plot = it.plot + theme(legend.position = "none",  plot.background = element_blank(), 
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.border = element_blank(), axis.ticks = element_blank(), 
                          axis.text=element_blank(), axis.title = element_blank())
it.plot

#balkans
ba.plot = ggplot() + theme_bw() + coord_quickmap()
ba.plot = ba.plot + geom_path(data = ba, aes(long,lat, group = id.tag, color = id.tag), alpha = .5) 
ba.plot = ba.plot + theme(legend.position = "none",  plot.background = element_blank(), 
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.border = element_blank(), axis.ticks = element_blank(), 
                          axis.text=element_blank(), axis.title = element_blank())
ba.plot


#caucasus
ca.plot = ggplot() + theme_bw() + coord_quickmap()
ca.plot = ca.plot + geom_path(data = ca, aes(long,lat, group = id.tag, color = id.tag), alpha = .5) 
ca.plot = ca.plot + theme(legend.position = "none",  plot.background = element_blank(), 
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.border = element_blank(), axis.ticks = element_blank(), 
                          axis.text=element_blank(), axis.title = element_blank())
ca.plot

#middle east
me.plot = ggplot() + theme_bw() + coord_quickmap() + 
  geom_path(data = me, aes(long,lat, group = id.tag, color = id.tag), alpha = .5) + 
  theme(legend.position = "none",  plot.background = element_blank(), 
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.border = element_blank(), axis.ticks = element_blank(), 
                          axis.text=element_blank(), axis.title = element_blank())
me.plot

#combine separate plots
grid_plot = ggarrange(we.plot, it.plot, ba.plot, ca.plot, me.plot, nrow = 2, ncol=3)
grid_plot

tiff("subpopulation.path.plot.tiff", units="cm", width=20, height=15, res=300)
grid_plot
dev.off()

#subpopulation
summary(ev.summ$population.binned)
#it.ba = subset(ev.summ, ev.summ$population.binned == "Italy / Balkans")
#we.eu = subset(ev.summ, ev.summ$population.binned == "W. Europe")
#me = subset(ev.summ, ev.summ$population.binned == "Middle East")
#summary(it.ba$origin)
#summary(we.eu$origin)
#summary(me$origin)