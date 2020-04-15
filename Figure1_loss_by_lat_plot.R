#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOTTING RAW DATA - LOSSES OF BIRDS PER LATITUDINAL BAND
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(data.table)
library(lubridate)
filter<-dplyr::filter
select<-dplyr::select


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DATA FROM MATRICES FOR MODEL INPUT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

try(setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study"), silent=T) 
load("EGVU_survival_output_full_additive.RData")


dim(EV.phi.matrix) ### this is the matrix that specifies the migration stage per animal per month
dim(lat.matrix)    ### this is the matrix that specifies the mean latitude per animal per month
dim(age.matrix) 	 ### this is the matrix that specifies the age per animal per month
dim(EV.obs.matrix) ### this is the matrix that specifies the observed state of each animal per month (1=Tag ok, bird moving, 2=Tag ok, bird not moving (dead), 3=Tag failed, bird observed alive, 4=Dead bird recovered 5=No signal)
timeseries


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### REMOVE THE IMPUTED (UNSAMPLED) MONTHS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get.last.loc<-function(x)min(which(!is.na(x) & x!=1))
last.telemetry<-apply(y.telemetry,1,get.last.loc)
for (li in 1:dim(EV.obs.matrix)[1]){
	if(EV.obs.matrix[li,dim(EV.obs.matrix)[2]]>1){
	 if((last.telemetry[li]+2)<=dim(EV.obs.matrix)[2]){
		EV.obs.matrix[li,((last.telemetry[li]+2):dim(EV.obs.matrix)[2])]<-NA
		}
	}
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### MELT THE MATRICES AND JOIN THEM
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

LatMonths<-lat.matrix %>% gather(key="col",value="Lat",-id.tag) %>%
	filter(!is.na(Lat))

TrackMonths<-EV.obs.matrix %>% gather(key="col",value="State",-id.tag) %>%
	filter(!is.na(State)) %>%
	mutate(col=gsub(pattern="V",replacement="",x=col)) %>%
	left_join(LatMonths, by=c("id.tag","col"))
head(TrackMonths)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### SUMMARISE THE NUMBER OF TRACKING MONTHS AND LOSSES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



SUMMARY <- TrackMonths %>%
	mutate(LatBand=ifelse(Lat<20,"< 20 N",ifelse(Lat>30,"> 30 N","20-30 N"))) %>%
	mutate(Dead=ifelse(State %in% c(2,4),1,0)) %>%
	mutate(Failure=ifelse(State==3,1,0)) %>%
	mutate(Unknown=ifelse(State==5,1,0)) %>%
	group_by(LatBand) %>%
	summarise(n_months=length(State),n_dead=sum(Dead),n_failed=sum(Failure),n_unknown=sum(Unknown)) %>%
	mutate(dead_rate=n_dead/n_months, fail_rate=n_failed/n_months, unknown_rate=n_unknown/n_months) %>%
	select(LatBand,n_months,dead_rate,fail_rate,unknown_rate) %>%
	gather(key=Loss_type,value=rate,-LatBand,-n_months) %>%
	mutate(rate=rate*12) %>% 		# scale to yearly rate
	mutate(Fate=ifelse(Loss_type=="dead_rate","dead",ifelse(Loss_type=="fail_rate","transmitter failure","unknown")))
SUMMARY



### CREATE A SIMPLE BAR PLOT SIMILAR TO EVAN's COLOURS
SUMMARY$LatBand <- factor(SUMMARY$LatBand, levels = c("< 20 N","20-30 N","> 30 N"))  ## this ensures correct presentation on the plot
ggplot(SUMMARY, aes(x=LatBand,y=rate, fill = Fate)) +
	theme_bw() +
  	coord_flip()+
	geom_col(position = position_dodge()) +
	ylab("n losses / tracking year") + xlab("geographic region") +
	guides(fill=guide_legend(title="fate")) +
	scale_fill_viridis_d(begin = .4, direction = -1) +
	  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.x=element_text(size=16, color="black"),
        axis.text.y=element_text(size=16, color="black", angle=90,hjust=0.5),
	legend.text=element_text(size=14, color="black"),
	legend.title=element_text(size=16, color="black"),  
        axis.title=element_text(size=18), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("Fig1_lat_barplot.jpg", width=8, height=7)


