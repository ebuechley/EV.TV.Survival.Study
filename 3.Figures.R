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
d.summ= read.csv("ev.summary.final.Rev1.survival.prepared.csv", stringsAsFactors=TRUE)
d.summ$start.date = ymd_hms(d.summ$start.date)
d.summ$end.date = ymd(d.summ$end.date)
summary(d.summ$end.date)
summary(d.summ$start.date)
d.summ$mortality.date = ymd_hms(d.summ$mortality.date)

#add population.binned
unique(d$population)
d$population.binned = NA
d$population.binned[d$population == "middle east"] <-  "Middle East"
d$population.binned[d$population == "caucasus"] <-  "Middle East"
d$population.binned[d$population == "western europe"] <-  "Western Europe"
d$population.binned[d$population == "italy"] <-  "Balkans & Italy"
d$population.binned[d$population == "balkans"] <-  "Balkans & Italy"
unique(d$population.binned)
d$color = d$population.binned
d$color[d$color == "Balkans & Italy"] <-  "#800000"
d$color[d$color == "Western Europe"] <-  "#8B4513"
d$color[d$color == "Middle East"] <-  "#F4A460"
head(d)
head(d.summ)

#remove alive for plotting end fates
d.summ2<-d.summ[!(d.summ$fate == "alive"),] 

#remove birds from unknown pop
d.summ2<-d.summ2[!(d.summ2$population == "unknown"),] 

#remove rehabbed birds 
d.summ2<-d.summ2[!(d.summ2$rehabilitated == "Y"),] 

#remove imprinted birds 
d.summ2<-d.summ2[!(d.summ2$fate == "returned to captivity"),] 

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

#plot
setwd("~/Google Drive/Research Projects/EV-TV Survival Study/Figures/")

pmap = ggplot() + annotation_map(map_data("world"), fill = 'light grey', color = "white") + coord_quickmap() + theme_bw() +
  geom_path(data = d, aes(long,lat, group = id), color = d$color, alpha = .5) +
  scale_color_manual(values= c( "#800000","#F4A460","#8B4513"), name = "subpopulation") + 
  #scale_color_viridis_d(begin = .2, end = .8, direction = 1, name = "subpopulation:") +
  new_scale_color() + 
  geom_point(data = d.summ2, aes(end.long, end.lat, color = fate.binned),  size = 3) +
  scale_color_viridis_d(option = "D", begin = 0.4, direction = -1, name = "fate:") + 
  theme(plot.title = element_text(hjust = 0.5)) + labs(x = "longitude", y = "latitude") 
pmap

#geom_bar(aes(y = (..count..)/sum(..count..)), position = position_dodge()) + scale_y_continuous(labels=scales::percent_format(accuracy=1)) + 
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
tiff("fate.summary.plot.Rev1.path.tiff", units="cm", width=28, height=15, res=300)
p5
dev.off()

#############################################################################
# summary stats on data summary
library(data.table)
setwd("~/Documents/GitHub/EV - TV Survival Study/")
ev.summ= read.csv("Final Cleaned Data/ev.summary.final.csv")

summary(ev.summ$species)

#subpopulation
summary(ev.summ$population.binned)
it.ba = subset(ev.summ, ev.summ$population.binned == "Italy / Balkans")
we.eu = subset(ev.summ, ev.summ$population.binned == "W. Europe")
me = subset(ev.summ, ev.summ$population.binned == "Middle East")
summary(it.ba$origin)
summary(we.eu$origin)
summary(me$origin)

#how many studies
head(ev.summ)
names(ev.summ)
unique(ev.summ$id)

#study
unique(ev.summ$study.name)
d <- data.table(ev.summ)
names(d)
projs <- d[, .(.N), by = .(study.name)]
projs

#age
age = d[, .(.N), by = .(age.at.deployment.clean)]
age

#origin
orig = d[, .(.N), by = .(origin)]
orig

#deployment duration
summary(d$deployment.duration)

#final fate
nrow(d)
summary(d$fate.final)

unique(ev.summ$start.country)
summary(ev.summ$fate)
summary(ev.summ$migrant)
summary(ev.summ$age.at.deployment.clean)
summary(ev.summ$captive.raised)
summary(ev.summ$rehabilitated)
summary(ev.summ$sex)
summary(ev.summ$deployment.duration)

#fates
summary(ev.summ$how.fate.determined.clean)
summary(ev.summ$cause.of.death)

#mortality summary
death = subset(ev.summ, ev.summ$fate.final == "confirmed dead")
death <- droplevels(death)
names(death)
names(death)
summary(death$cause.of.death.simple)
summary(death$death.anthro)
summary(death$end.country)

names(death)
head(death)
death = death[,c("start.country", "end.country", "origin", "age.at.deployment.month", "age.at.fate.month", 
                 "deployment.duration", "cause.of.death.simple", "death.anthro")]
head(death)

#write.csv(death, "Final Cleaned Data/ev.mortality.summary.csv", row.names = F)

#plot anthropogenic mortalities
d1 = read.csv("Final Cleaned Data/ev.survival.prepared.csv")


death.anthro = subset(death, death$death.anthro == "y")
summary(death.anthro$end.country)
summary(death.anthro$population.binned)
nrow(death.anthro)

ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white") + coord_quickmap() + theme_bw() +
  geom_path(data = d1, aes(long,lat, group = id.tag), alpha = .5, show.legend = FALSE) + 
  geom_point(data = death.anthro, aes(long.mort, lat.mort, color = death.anthro),  size = 3) +
  scale_color_viridis_d(begin = .4, direction = -1, name = "fate") + 
  theme(plot.title = element_text(hjust = 0.5)) + labs(x = "longitude", y = "latitude")

#write.csv(death, "death.csv", row.names = F)

summary(death$how.fate.determined.clean)
nrow(death)

#tiff("all.overview.tiff", units="cm", width=35, height=20, res=300)
#combined <- grid.arrange(tv.plot, ev.plot, nrow = 1, ncol=2)
#dev.off()

#map.plot = map.plot + geom_point(data = d.summ, aes(start.long, start.lat, color = "tag deployment"))  
#map.plot = map.plot + theme(legend.title = element_blank()) 
#map.plot = map.plot + geom_segment(data = ev.tv.summary.merged.alive.removed, aes(x = start.long, y = start.lat, xend = end.long, yend = end.lat)) 


#by EV populations
d1 = subset(d, d$species == "Neophron percnopterus")
names(d1)
d1 = d1[,c(2:5,39)]
head(d1)
we = subset(d1, d1$population == "western europe")
it = subset(d1, d1$population == "italy")
ba = subset(d1, d1$population == "balkans")
ca = subset(d1, d1$population == "caucasus")
d2 = rbind(we,it,ba,ca)
head(d2)
#
#write.csv(d2, "ev.all.csv")
#write.csv(we, "ev.western.europe.csv")
#summary(we$id.tag)
#write.csv(it, "ev.italy.csv")
#write.csv(ba, "ev.balkans.csv")
#write.csv(ca, "ev.caucasus.csv")

#all
#svg("ev.all.tracking.overview.svg")
map.plot = ggplot() + theme_bw() + coord_quickmap()
map.plot = map.plot + geom_path(data = d2, aes(long,lat, group = id.tag, color = id.tag), alpha = .5) 
map.plot = map.plot + theme(legend.position = "none",  plot.background = element_blank(), 
                            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.border = element_blank(), axis.ticks = element_blank(), 
                            axis.text=element_blank(), axis.title = element_blank())
map.plot
#dev.off()

#western europe
summary(we)
#svg("ev.westerneurope.tracking.overview.svg")
we.plot = ggplot() + theme_bw() + coord_quickmap()
we.plot = we.plot + geom_path(data = we, aes(long,lat, group = id.tag, color = id.tag), alpha = .5) 
we.plot = we.plot + theme(legend.position = "none",  plot.background = element_blank(), 
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.border = element_blank(), axis.ticks = element_blank(), 
                          axis.text=element_blank(), axis.title = element_blank())
we.plot
#dev.off()

#italy
summary(it)
#svg("ev.italy.tracking.overview.svg")
it.plot = ggplot() + theme_bw() + coord_quickmap()
it.plot = it.plot + geom_path(data = it, aes(long,lat, group = id.tag, color = id.tag), alpha = .5) 
it.plot = it.plot + theme(legend.position = "none",  plot.background = element_blank(), 
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.border = element_blank(), axis.ticks = element_blank(), 
                          axis.text=element_blank(), axis.title = element_blank())
it.plot
#dev.off()

#balkans
summary(ba)
#svg("ev.balkans.tracking.overview.svg")
ba.plot = ggplot() + theme_bw() + coord_quickmap()
ba.plot = ba.plot + geom_path(data = ba, aes(long,lat, group = id.tag, color = id.tag), alpha = .5) 
ba.plot = ba.plot + theme(legend.position = "none",  plot.background = element_blank(), 
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.border = element_blank(), axis.ticks = element_blank(), 
                          axis.text=element_blank(), axis.title = element_blank())
ba.plot
#dev.off()

#caucasus
summary(ca)
#svg("ev.caucasus.tracking.overview.svg")
ca.plot = ggplot() + theme_bw() + coord_quickmap()
ca.plot = ca.plot + geom_path(data = ca, aes(long,lat, group = id.tag, color = id.tag), alpha = .5) 
ca.plot = ca.plot + theme(legend.position = "none",  plot.background = element_blank(), 
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.border = element_blank(), axis.ticks = element_blank(), 
                          axis.text=element_blank(), axis.title = element_blank())
ca.plot
#dev.off()

#combine seperate plots
grid_plot <- grid.arrange(we.plot, it.plot, ba.plot, ca.plot, nrow = 2, ncol=2)















#############################################################
#Old Figure
library(ggplot2)
library("gridExtra")
library(lubridate)
library(ggmap)

#Clear workspace
rm(list = ls())

#set wd
setwd("~/Google Drive/Research Projects/EV-TV Survival Study/Dataset/Final/Rev1/")

#read data
d = read.csv("./Data/ev.survival.prepared.Rev1.csv")
d.summ= read.csv("./Summary/ev.summary.final.Rev1.csv")
head(d.summ)
head(d.summ$start.date)
d.summ$start.date = ymd_hms(d.summ$start.date)

#summary stats for study
summary(d.summ)
unique(d.summ$id) 
unique(d$id) 
unique(d.summ$study.name)
summary(d.summ$fate)
summary(d.summ$population)
summary(d.summ$sex)
head(d.summ)
names(d)
head(d)
#tv.summ = subset(d.summ, d.summ$species == "Cathartes aura")
#ev.summ = subset(d.summ, d.summ$species == "Neophron percnopterus")
ev.summ = d.summ
#summary(tv.summ$sex)
#summary(ev.summ$sex)
#summary(tv.summ$age.at.deployment.clean)
#summary(ev.summ$age.at.deployment.clean)
#summary(ev.summ$migrant)
#summary(tv.summ$migrant)
#summary(ev.summ$captive.raised)
#summary(tv.summ$captive.raised)
#summary(ev.summ$rehabilitated)
#summary(tv.summ$rehabilitated)
#ev.summ.wild = subset(ev.summ, ev.summ$captive.raised == "N")
#summary(ev.summ.wild$age.at.deployment.clean)

#tv = subset(d, d$species == "Cathartes aura")
#ev = subset(d, d$species == "Neophron percnopterus")

#colourpalette<-c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999','#000120')
#colourpalette

#tv figure
#tv.summ$fate <- factor(tv.summ$fate, levels = c("alive","confirmed dead","likely dead","confirmed transmitter failure","likely transmitter failure","unknown"))
ev.summ$fate = as.factor(ev.summ$fate)
summary(ev.summ$fate)
ev.summ$fate[ev.summ$fate == "Confirmed dead"] = "confirmed dead"
summary(ev.summ$fate)

#remove fate = returned to captivity
ev.summ<-ev.summ[!(ev.summ$fate == "returned to captivity"),]
ev.summ$fate <- factor(ev.summ$fate, levels = c("alive","confirmed dead","likely dead","confirmed transmitter failure","likely transmitter failure","unknown"))
summary(ev.summ$fate)

#tiff("tv.overview.tiff", units="cm", width=20, height=20, res=300)
#tv.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white") + coord_quickmap() + theme_bw() +
#  geom_path(data = tv, aes(long,lat, group = id.tag), alpha = .5, show.legend = FALSE) + 
#  geom_point(data = tv.summ, aes(end.long, end.lat, color = fate)) + 
#  scale_color_manual(values=c('#4daf4a','#e41a1c','#ff7f00','#377eb8','#984ea3','#ffff33'), name = "fate") + 
#  ggtitle("turkey vulture") + theme(plot.title = element_text(hjust = 0.5)) + labs(x = "longitude", y = "latitude") 
#tv.plot
#dev.off()

summary(ev.summ)
ev.summ2 = ev.summ[!(ev.summ$fate== "alive"),]
summary(ev.summ2$fate)
ev.summ2$fate = as.character(ev.summ2$fate)
ev.summ2$fate[which(ev.summ2$fate == "likely dead")] = "dead"
ev.summ2$fate[which(ev.summ2$fate == "confirmed dead")] = "dead"
ev.summ2$fate[which(ev.summ2$fate == "confirmed transmitter failure")] = "transmitter failure"
ev.summ2$fate[which(ev.summ2$fate == "likely transmitter failure")] = "transmitter failure"
ev.summ2$fate[which(ev.summ2$fate == "transmitter failure")] = "tx failure"
summary(ev.summ2)
head(ev.summ2)
ev=d
head(ev)
summary(ev$age.in.months)
#unique(ev$age.in.months)
#ev$age.simple[which(ev$age.in.months < 8)] = "juvenile"
#ev$age.simple[which(ev$age.in.months > 8)] = "adult"
#ev$age.simple = as.factor(ev$age.simple)
#summary(ev$age.simple)

#register_google(key = "xxxx")
#overview.map = get_map(location = c(20,25), maptype = "terrain", source = "google", zoom = 4)
head(ev.summ2)
tiff("./Figures/ev.overview.simple.tiff", units="cm", width=18, height=11, res=300)
  #ggmap(overview.map) + coord_quickmap() + theme_bw() +
  ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white") + coord_quickmap() + theme_bw() +
  geom_path(data = ev, aes(long,lat, group = id), alpha = .5, show.legend = FALSE) + 
  geom_point(data = ev.summ2, aes(end.long, end.lat, color = fate)) + 
  #scale_color_manual(values=c('#e41a1c',"#4daf4a",'#ffff33'), name = "fate") + 
  scale_color_viridis_d(begin =.5, direction = -1) +
  #ggtitle("Egyptian vulture") + 
  theme(plot.title = element_text(hjust = 0.5)) + labs(x = "longitude", y = "latitude")
dev.off()

# summary stats on data summary
ev.summ = read.csv("./Summary/ev.summary.final.Rev1.csv")
#ev.summ = subset(ev.summ, ev.summ$species == "Neophron percnopterus", drop = TRUE)
#ev.summ <- droplevels(ev.summ)
#summary(ev.summ$species)
head(ev.summ)
names(ev.summ)
ev.summ$start.country == ev.summ$start.country.new
ev.summ$end.country == ev.summ$end.country.new #A LOT OF DISCREPENCIES HERE!

#basic stats
unique(ev.summ$id)
unique(ev.summ$study.name)
unique(ev.summ$start.country)
unique(ev.summ$end.country)
summary(ev.summ$fate)
summary(ev.summ$migrant)
summary(ev.summ$age.at.deployment.clean)
summary(ev.summ$captive.raised)
summary(ev.summ$rehabilitated)
summary(ev.summ$sex)
summary(ev.summ$deployment.duration)
#fates
summary(ev.summ$how.fate.determined.clean)
summary(ev.summ$cause.of.death)

#mortality summary
death = read.csv("Final Cleaned Data/ev.mortality.summary.csv")
summary(death$cause.of.death)

#tiff("all.overview.tiff", units="cm", width=35, height=20, res=300)
#combined <- grid.arrange(tv.plot, ev.plot, nrow = 1, ncol=2)
#dev.off()

#map.plot = map.plot + geom_point(data = d.summ, aes(start.long, start.lat, color = "tag deployment"))  
#map.plot = map.plot + theme(legend.title = element_blank()) 
#map.plot = map.plot + geom_segment(data = ev.tv.summary.merged.alive.removed, aes(x = start.long, y = start.lat, xend = end.long, yend = end.lat)) 


#by EV populations
d1 = subset(d, d$species == "Neophron percnopterus")
names(d1)
d1 = d1[,c(2:5,39)]
head(d1)
we = subset(d1, d1$population == "western europe")
it = subset(d1, d1$population == "italy")
ba = subset(d1, d1$population == "balkans")
ca = subset(d1, d1$population == "caucasus")
d2 = rbind(we,it,ba,ca)
head(d2)
#
#write.csv(d2, "ev.all.csv")
#write.csv(we, "ev.western.europe.csv")
#summary(we$id.tag)
#write.csv(it, "ev.italy.csv")
#write.csv(ba, "ev.balkans.csv")
#write.csv(ca, "ev.caucasus.csv")

#all
#svg("ev.all.tracking.overview.svg")
map.plot = ggplot() + theme_bw() + coord_quickmap()
map.plot = map.plot + geom_path(data = d2, aes(long,lat, group = id.tag, color = id.tag), alpha = .5) 
map.plot = map.plot + theme(legend.position = "none",  plot.background = element_blank(), 
                            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.border = element_blank(), axis.ticks = element_blank(), 
                            axis.text=element_blank(), axis.title = element_blank())
map.plot
#dev.off()

#western europe
summary(we)
#svg("ev.westerneurope.tracking.overview.svg")
we.plot = ggplot() + theme_bw() + coord_quickmap()
we.plot = we.plot + geom_path(data = we, aes(long,lat, group = id.tag, color = id.tag), alpha = .5) 
we.plot = we.plot + theme(legend.position = "none",  plot.background = element_blank(), 
                            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.border = element_blank(), axis.ticks = element_blank(), 
                            axis.text=element_blank(), axis.title = element_blank())
we.plot
#dev.off()

#italy
summary(it)
#svg("ev.italy.tracking.overview.svg")
it.plot = ggplot() + theme_bw() + coord_quickmap()
it.plot = it.plot + geom_path(data = it, aes(long,lat, group = id.tag, color = id.tag), alpha = .5) 
it.plot = it.plot + theme(legend.position = "none",  plot.background = element_blank(), 
                            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.border = element_blank(), axis.ticks = element_blank(), 
                            axis.text=element_blank(), axis.title = element_blank())
it.plot
#dev.off()

#balkans
summary(ba)
#svg("ev.balkans.tracking.overview.svg")
ba.plot = ggplot() + theme_bw() + coord_quickmap()
ba.plot = ba.plot + geom_path(data = ba, aes(long,lat, group = id.tag, color = id.tag), alpha = .5) 
ba.plot = ba.plot + theme(legend.position = "none",  plot.background = element_blank(), 
                            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.border = element_blank(), axis.ticks = element_blank(), 
                            axis.text=element_blank(), axis.title = element_blank())
ba.plot
#dev.off()

#caucasus
summary(ca)
#svg("ev.caucasus.tracking.overview.svg")
ca.plot = ggplot() + theme_bw() + coord_quickmap()
ca.plot = ca.plot + geom_path(data = ca, aes(long,lat, group = id.tag, color = id.tag), alpha = .5) 
ca.plot = ca.plot + theme(legend.position = "none",  plot.background = element_blank(), 
                            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.border = element_blank(), axis.ticks = element_blank(), 
                            axis.text=element_blank(), axis.title = element_blank())
ca.plot
#dev.off()

#combine seperate plots
grid_plot <- grid.arrange(we.plot, it.plot, ba.plot, ca.plot, nrow = 2, ncol=2)
