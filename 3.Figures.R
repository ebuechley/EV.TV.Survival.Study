#############################################################
#Figure
library(ggplot2)
library("gridExtra")
library(lubridate)

#Clear workspace
rm(list = ls())

#set wd
setwd("~/Documents/GitHub/EV - TV Survival Study/")

#read data
#d = read.csv("ev.tv.filtered.csv")
d.summ= read.csv("ev.tv.summary.proofed.csv")
d.summ$start.date = mdy_hm(d.summ$start.date)

#summary stats for study
summary(d.summ)
unique(d.summ$study.name)
summary(d.summ$fate)
summary(d.summ$population)
summary(d.summ$sex)

names(d)
head(d)
tv.summ = subset(d.summ, d.summ$species == "Cathartes aura")
ev.summ = subset(d.summ, d.summ$species == "Neophron percnopterus")
summary(tv.summ$sex)
summary(ev.summ$sex)
summary(tv.summ$age.at.deployment.clean)
summary(ev.summ$age.at.deployment.clean)
summary(ev.summ$migrant)
summary(tv.summ$migrant)
summary(ev.summ$captive.raised)
summary(tv.summ$captive.raised)
summary(ev.summ$rehabilitated)
summary(tv.summ$rehabilitated)
ev.summ.wild = subset(ev.summ, ev.summ$captive.raised == "N")
summary(ev.summ.wild$age.at.deployment.clean)

tv = subset(d, d$species == "Cathartes aura")
ev = subset(d, d$species == "Neophron percnopterus")

colourpalette<-c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999','#000120')
colourpalette

#tv figure
tv.summ$fate <- factor(tv.summ$fate, levels = c("alive","confirmed dead","likely dead","confirmed transmitter failure","likely transmitter failure","unknown"))
ev.summ$fate <- factor(ev.summ$fate, levels = c("alive","confirmed dead","likely dead","confirmed transmitter failure","likely transmitter failure","unknown"))

tiff("tv.overview.tiff", units="cm", width=20, height=20, res=300)
tv.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white") + coord_quickmap() + theme_bw() +
  geom_path(data = tv, aes(long,lat, group = id.tag), alpha = .5, show.legend = FALSE) + 
  geom_point(data = tv.summ, aes(end.long, end.lat, color = fate)) + 
  scale_color_manual(values=c('#4daf4a','#e41a1c','#ff7f00','#377eb8','#984ea3','#ffff33'), name = "fate") + 
  ggtitle("turkey vulture") + theme(plot.title = element_text(hjust = 0.5)) + labs(x = "longitude", y = "latitude") 
tv.plot
dev.off()

tiff("ev.overview.tiff", units="cm", width=35, height=20, res=300)
ev.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white") + coord_quickmap() + theme_bw() +
  geom_path(data = ev, aes(long,lat, group = id.tag), alpha = .5, show.legend = FALSE) + 
  geom_point(data = ev.summ, aes(end.long, end.lat, color = fate)) + 
  scale_color_manual(values=c('#4daf4a','#e41a1c','#ff7f00','#377eb8','#984ea3','#ffff33'), name = "fate") + 
  ggtitle("egyptian vulture") + theme(plot.title = element_text(hjust = 0.5)) + labs(x = "longitude", y = "latitude")
ev.plot
dev.off()

tiff("all.overview.tiff", units="cm", width=35, height=20, res=300)
combined <- grid.arrange(tv.plot, ev.plot, nrow = 1, ncol=2)
dev.off()

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
