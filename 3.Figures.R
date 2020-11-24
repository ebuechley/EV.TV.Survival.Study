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

