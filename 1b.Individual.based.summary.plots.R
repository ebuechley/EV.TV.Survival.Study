#################################################################################
#Individual-based summary plots
#################################################################################
#Clear workspace
rm(list = ls())

#read data
setwd("~/Google Drive/GitHub/EV.TV.Survival.Study/")
d = read.csv("./ev.final.Rev1.survival.prepared.csv")
d$timestamp = ymd_hms(d$timestamp)
summary(d$ND)
unique(d$id) #220
names(d)

#set wd
setwd("~/Google Drive/Research Projects/EV-TV Survival Study/Dataset/Final/Rev1/")

#plots for each id
# create for loop to produce ggplot2 graphs 
library(gridExtra)
names(d)
d$id.tag.year <- c(paste(d$id.tag,d$year,sep="_")) 
unique(d$id.tag.year)
d$timestamp = as.Date(d$timestamp)
summary(d$timestamp)

#updated Mig_stage_matrix doc, i.e. manual summary of whether each individual migration in each month
#mig.stage = read.csv("./Data/Mig_stage_matrix.reviewed.csv")
#summary(mig.stage)
#unique(mig.stage$id.year)
#head(mig.stage)
#mig.stage$id.year = substr(mig.stage$id.year,1,nchar(mig.stage$id.year)-4)  #drop ".png"
#unique(mig.stage$id.year)
#need to create id_tag_yr for d
#d$id.tag <- c(paste(d$id,d$tag,sep="_")) 
#d$id.tag.year = c(paste(d$id.tag,d$year,sep="_")) 
#head(d)
#mig.stage.new = as.data.frame(unique(d$id.tag.year))
#names(mig.stage.new)[1] <- "id.tag.year"
#unique(mig.stage.new$id.tag.year)
#mig.stage.new$X1 = NA
#mig.stage.new$X2 = NA
#mig.stage.new$X3 = NA
#mig.stage.new$X4 = NA
#mig.stage.new$X5 = NA
#mig.stage.new$X6 = NA
#mig.stage.new$X7 = NA
#mig.stage.new$X8 = NA
#mig.stage.new$X9 = NA
#mig.stage.new$X10 = NA
#mig.stage.new$X11 = NA
#mig.stage.new$X12 = NA
#head(mig.stage.new)

#assign old values to new mig.stage
#head(mig.stage)
#names(mig.stage)[names(mig.stage) == "id.year"] <- "id.tag.year"
#head(mig.stage.new)
#mig.stage.new = merge(mig.stage,mig.stage.new, by.x = "id.tag.year",by.y = "id.tag.year", all = T)
#head(mig.stage.new)
#summary(mig.stage.new)
#clean up 
#names(mig.stage.new)
#mig.stage.new = mig.stage.new[,c("id.tag.year", "X1.x", "X2.x", "X3.x", "X4.x", "X5.x", "X6.x", 
#                                 "X7.x", "X8.x", "X9.x", "X10.x", "X11.x", "X12.x","coments.1","coments.2","coments.3")]
#names(mig.stage.new) <- c("id.tag.year","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","comments.1","comments.2","comments.3")
#summary(mig.stage.new)
#write.csv(mig.stage.new,"./Data/Mig_stage_matrix.Rev1.csv", row.names=FALSE)

#export individual id.yr summary plots
names(d)
for (i in unique(d$id.tag.year)) { 
  #net displacement
  plot1 <- 
    ggplot(aes(timestamp, ND), data = subset(d, id.tag.year == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "net displacement (degrees)") + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 month") 
  
  #fix rate
  plot2 <- 
    ggplot(aes(timestamp, dt.days), data = subset(d, id.tag.year == i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "time between fixes (days)") + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 month")
  
  #GPS tracks
  plot3 <- 
    ggplot() + annotation_map(map_data("world"), fill = 'grey', color = "white")  + coord_quickmap() + theme_bw() +
    geom_path(data = subset(d, id.tag.year ==  i), aes(long,lat)) + labs(x = "longitude", y = "latitude") + 
    theme(legend.title = element_blank()) +
    ggtitle(paste(i)) + theme(plot.title = element_text(hjust = 0.5)) 
  
  #distance
  plot4 <- 
    ggplot(aes(timestamp, dist), data = subset(d, id.tag.year ==  i))  + 
    theme_bw() + geom_line() + labs(x = "date", y = "distance (degrees)") + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 month")
  
  #arrange
  plot5 = grid.arrange(plot3, plot1, plot4, plot2, ncol = 2, nrow = 2, 
                       widths = c(1,1), layout_matrix = rbind(c(1, 2), c(3,4)))
  
  ggsave(filename = sprintf('./overview.plots/%s.png', i), plot = plot5, width = 30, height = 20, units = c("cm"),dpi = 300)
}

#plot net displacement
#tiff("./overview.plots/ev.tv.tracking.nd.overview.tiff", units="cm", width=70, height=40, res=300)
#ggplot(d, aes(timestamp, ND)) + geom_line() + facet_wrap(~ id) + 
#  labs(x = "date", y = "net displacement (degrees)") + theme_bw() 
#dev.off()