#################################################################################
#Individual-based summary plots
#################################################################################
#Clear workspace
rm(list = ls())

#set wd
setwd("~/Documents/GitHub/EV - TV Survival Study/")

#read data
d = read.csv("./Final Cleaned Data/ev.tv.filtered.csv")
summary(d)
names(d)
summary(d$sensor.type)
summary(d$tag)
d$timestamp = ymd_hms(d$timestamp)
summary(d$ND)
unique(d$id.tag)
names(d)

#plot net displacement
#tiff("./overview.plots/ev.tv.tracking.nd.overview.tiff", units="cm", width=70, height=40, res=300)
#plot = ggplot(d, aes(timestamp, ND)) + geom_line() + facet_wrap(~ id.tag)
#plot = plot + labs(x = "date", y = "net displacement (degrees)") + theme_bw() 
#plot 
#dev.off()

#plots for each id
# create for loop to produce ggplot2 graphs 
library(gridExtra)

names(d)
unique(d$species)
d = subset(d, species == "Neophron percnopterus")
summary(d$species)
d$id.tag.yr <- c(paste(d$id.tag,d$year,sep="_")) 
unique(d$id.tag.yr)

d$timestamp = as.Date(d$timestamp)
summary(d$timestamp)


for (i in unique(d$id.tag.yr)) { 
  #net displacement
  plot1 <- 
    ggplot(aes(timestamp, ND), data = subset(d, id.tag.yr == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "net displacement (degrees)") + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 month") 
  
  #fix rate
  plot2 <- 
    ggplot(aes(timestamp, dt.days), data = subset(d, id.tag.yr == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "time between fixes (days)") + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 month")
  
  #GPS tracks
  plot3 <- 
    ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white")  + coord_quickmap() + theme_bw() +
    geom_path(data = subset(d, id.tag.yr ==  i), aes(long,lat)) + labs(x = "longitude", y = "latitude") + 
    theme(legend.title = element_blank()) +
    ggtitle(paste(i)) + theme(plot.title = element_text(hjust = 0.5)) 
  
  #distance
  plot4 <- 
    ggplot(aes(timestamp, dist), data = subset(d, id.tag.yr ==  i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "distance (degrees)") + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 month")
  
  #arrange
  plot5 = grid.arrange(plot3, plot1, plot4, plot2, ncol = 2, nrow = 2, 
                       widths = c(1,1), layout_matrix = rbind(c(1, 2), c(3,4)))
  
  ggsave(filename = sprintf('./overview.plots/%s.png', i), plot = plot5, width = 30, height = 20, units = c("cm"),dpi = 300)
}


#Argos tracks
summary(d$argos.lat1)
d1 = d[!is.na(d$argos.lat1),]
summary(d1$argos.lat1)
summary(d1$argos.lon1)
summary(d1)
unique(d1$id.tag)
write.csv(d1, "gps.argos.comp.csv")
d1 = read.csv("gps.argos.comp.csv")
unique(d1$id.tag)
head(d1)

plot1 <- 
  ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white")  + coord_quickmap() + theme_bw() +
  geom_path(data = subset(d1, id.tag == "White 12_119245"), aes(long,lat)) + labs(x = "longitude", y = "latitude") + 
  theme(legend.title = element_blank()) +
  ggtitle(paste(i)) + theme(plot.title = element_text(hjust = 0.5))
plot1

for (i in unique(d1$id.tag)) { 
  
  #GPS tracks
  plot1 <- 
    ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white")  + coord_quickmap() + theme_bw() +
    geom_path(data = subset(d1, id.tag ==  i), aes(long,lat)) + labs(x = "longitude", y = "latitude") + 
    theme(legend.title = element_blank()) +
    ggtitle(paste(i)) + theme(plot.title = element_text(hjust = 0.5))
  
  #Argos tracks
  plot2 <- 
  ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white")  + coord_quickmap() + theme_bw() +
  geom_path(data = subset(d1, id.tag ==  i), aes(argos.lon1,argos.lat1)) + labs(x = "longitude", y = "latitude") + 
  theme(legend.title = element_blank()) +
  ggtitle(paste(i)) + theme(plot.title = element_text(hjust = 0.5))

  #arrange
  plot3 = grid.arrange(plot1, plot2, ncol = 2, nrow = 1, 
                       widths = c(1,1), layout_matrix = rbind(c(1, 2)))
  
ggsave(filename = sprintf('./argos.plots/%s.png', i), plot = plot4, width = 40, height = 20, units = c("cm"),dpi = 300)
}


#check battery charge fields
#head(d)
#summary(d$battery.charge.percent)
#summary(d$battery.charging.current)
#summary(d$tag.voltage)
#summary(d$eobs.battery.voltage)
#summary(d$eobs.fix.battery.voltage)
#summary(d$U_bat_mV)
#summary(d$bat_soc_pct)
#summary(d$Battery.voltage)
#summary(d$Solar.voltage)

names(d)


#battery charge
for (i in unique(d$id.tag)) { 
  
  b1 <- 
    ggplot(aes(timestamp, battery.charge.percent), data = subset(d, id.tag ==  i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "battery.charge.percent") 
  b2 <- 
    ggplot(aes(timestamp, battery.charging.current), data = subset(d, id.tag == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "battery.charging.current") +
    ggtitle(paste(i)) + theme(plot.title = element_text(hjust = 0.5))
  b3 <- 
    ggplot(aes(timestamp, tag.voltage), data = subset(d, id.tag == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "tag.voltage") 
  b5 <- 
    ggplot(aes(timestamp, eobs.battery.voltage), data = subset(d, id.tag == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "eobs.battery.voltage") 
  b6 <- 
    ggplot(aes(timestamp, eobs.fix.battery.voltage), data = subset(d, id.tag == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "eobs.fix.battery.voltage") 
  b7 <- 
    ggplot(aes(timestamp, U_bat_mV), data = subset(d, id.tag == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "U_bat_mV")
  b8 <- 
    ggplot(aes(timestamp, bat_soc_pct), data = subset(d, id.tag == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "bat_soc_pct")
  b9 <- 
    ggplot(aes(timestamp, Battery.voltage), data = subset(d, id.tag == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "Battery.voltage")
  b10 <- 
    ggplot(aes(timestamp, Solar.voltage), data = subset(d, id.tag == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "Solar.voltage")
  
  #arrange
  b11 = grid.arrange(b1,b2,b3,b5,b6,b7,b8,b9,b10, ncol = 3)
  
  ggsave(filename = sprintf('./battery.plots/%s.png', i), plot = b11, width = 20, height = 30, units = c("cm"),dpi = 300)
}

#temperature
for (i in unique(d$id.tag)) { 
  
  b1 <- 
    ggplot(aes(timestamp, external.temperature), data = subset(d, id.tag ==  i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "external.temperature") 
  b2 <- 
    ggplot(aes(timestamp, internal.temperature), data = subset(d, id.tag == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "internal.temperature") +
    ggtitle(paste(i)) + theme(plot.title = element_text(hjust = 0.5))
  b3 <- 
    ggplot(aes(timestamp, eobs.temperature), data = subset(d, id.tag == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "eobs.temperature") 
  b5 <- 
    ggplot(aes(timestamp, temperature_C), data = subset(d, id.tag == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "temperature_C") 
  b6 <- 
    ggplot(aes(timestamp, CPU.temperature.C.), data = subset(d, id.tag == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "CPU.temperature.C.") 
  b7 <- 
    ggplot(aes(timestamp, Temperature.sensor.C.), data = subset(d, id.tag == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "Temperature.sensor.C.")
  
  #arrange
  b11 = grid.arrange(b1,b2,b3,b5,b6,b7, ncol = 3)
  
  ggsave(filename = sprintf('./temperature.plots/%s.png', i), plot = b11, width = 20, height = 30, units = c("cm"),dpi = 300)
}


#2HP has highly intermittent fixes
#9FC has very few fixes
#18 White has very few fixes
#93 - possible mortality / dropped tx
#A17 Green - last point far from others
#A25 Green - last point far from others
#Cabuk - errant last point?
#Iliaz ND is such a cool figure of individual migration development
#Levkipos -- possible mortality or dropped tx in early 2015?
#Mille - errant last point?
#NeoPer_Poiares - errant last point?
#Sarygush - possible mortality or dropped tx?
#Sharka - only 3 points!

#Carmen looks to have died at sea and tx floated long after
#NeoPer_Poiares has a straight shot back from Africa to Spain
#Provence_2016_Ad_wild has a straight shot from Africa back to France
#White 08 has a random point in the Med
#Yellow 04 has what looks like erroneous points in the Med

#summarize the summary
d = read.csv("ev.tv.summary.proofed.csv")

summary(d)
