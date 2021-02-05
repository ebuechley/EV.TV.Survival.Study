##########################################################################
#
# EGYPTIAN VULTURE MONTHLY MIGRATION STATE FROM TELEMETRY
#
##########################################################################
library(zoo)
library(tidyverse)
library(data.table)
filter<-dplyr::filter
select<-dplyr::select


# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # REVIEWER 2 WANTS TO KNOW HOW OFTEN MIGRATION INCLUDES 2 MONTHS
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0	No data
# 1	Stationary
# 2 migratory

try(setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\EV.TV.Survival.Study"), silent=T) ## changed after re-cloning remote
EV.phi.states<-fread("Mig_stage_matrix.Rev1.csv")

## MELT DATA FRAME ##
mig.out<-EV.phi.states %>% gather(key=month, value=mig,-id.tag.year) %>%
	mutate(month.num=str_replace(month,"X","")) %>%
	mutate(MONTH=month.abb[as.numeric(month.num)]) %>%
	mutate(mig=ifelse(mig==2,1,0)) %>%
	arrange(id.tag.year,as.numeric(month.num)) %>%
	mutate(next.mig=dplyr::lag(mig), prev.mig=dplyr::lead(mig)) %>%
	dplyr::select(id.tag.year,month.num,MONTH,mig,prev.mig,next.mig)
dim(mig.out)
head(mig.out)

## CREATE UNIQUE MIGRATION BOUTS
mig.out$BOUT<-1
for(l in 2:dim(mig.out)[1]){
	mig.out$BOUT[l]<-ifelse(mig.out$mig[l]==mig.out$next.mig[l],mig.out$BOUT[l-1],mig.out$BOUT[l-1]+1)  
    }
head(mig.out)
tail(mig.out)


### SUMMARISE MIGRATION BOUTS
mig.sum<- mig.out %>% group_by(BOUT) %>%
	summarise(months=sum(mig)) %>%
	dplyr::filter(months>0)
table(mig.sum$months)/sum(table(mig.sum$months))
length(unique(mig.sum$BOUT))

### TROUBLESHOOT LONG MIGRATIONS - correct immature wanderings!
mig.sum %>% filter(months>3) %>% left_join(mig.out, by="BOUT")


