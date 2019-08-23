#############################################################
#Figure
library(ggplot2)
library("gridExtra")

#Clear workspace
rm(list = ls())

#set wd
setwd("~/Documents/GitHub/EV - TV Survival Study/")

#read data
d = read.csv("ev.tv.filtered.csv")
ev.tv.summary.merged = read.csv("ev.tv.summary.merged.csv")
ev.tv.summary.merged.alive.removed = ev.tv.summary.merged[!(ev.tv.summary.merged$fate == "alive"),]
names(d)
head(d)

#ggplot map
colourpalette<-c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999','#000120')
colourpalette
tiff("tracking.overview.tiff", units="cm", width=35, height=19, res=300)
map.plot = ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white") + coord_quickmap() + theme_bw() 
map.plot = map.plot + geom_path(data = d, aes(long,lat, group = id.tag, color = species), alpha = .5) 
map.plot = map.plot + geom_point(data = ev.tv.summary.merged, aes(start.long, start.lat, color = "tag deployment"))  
map.plot = map.plot + geom_point(data = ev.tv.summary.merged.alive.removed, aes(end.long, end.lat, color = "tag termination")) 
#map.plot = map.plot + geom_segment(data = ev.tv.summary.merged.alive.removed, aes(x = start.long, y = start.lat, xend = end.long, yend = end.lat)) 
map.plot = map.plot + scale_color_manual(values=c('#4daf4a','#377eb8','#ffff33','#e41a1c')) + labs(x = "longitude", y = "latitude")
map.plot = map.plot + theme(legend.title = element_blank()) 
map.plot = map.plot + ggtitle("vulture tracking, deployment, and termination overview") + theme(plot.title = element_text(hjust = 0.5))
map.plot
dev.off()

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
