#CUT SCRIPT

########################################################################################################################
#remove data from before official start date that was provided by coauthors for each tag
d = read.csv("ev.tv.1ptperday.csv")
summary(d$timestamp)
d$timestamp = ymd_hms(d$timestamp)
summ = read.csv("ev.tv.summary.merged.csv")
summ$end.date.adjusted = ymd_hms(summ$end.date.adjusted)
summ$start.date.adjusted = ymd_hms(summ$start.date.adjusted)

#replace NAs in start date adjusted with start date if NA
summ$start.date.adjusted[is.na(summ$start.date.adjusted)] <- summ$start.date[is.na(summ$start.date.adjusted)]
summ$end.date.adjusted[is.na(summ$end.date.adjusted)] <- summ$end.date[is.na(summ$end.date.adjusted)]
summ$start.date = ymd_hms(summ$start.date)
summ$end.date = ymd_hms(summ$end.date)
summ$start.date.adjusted = ymd_hms(summ$start.date.adjusted)
summ$end.date.adjusted = ymd_hms(summ$end.date.adjusted)
summ$start.diff = (summ$start.date.adjusted) - (summ$start.date)
summ$end.diff = (summ$end.date.adjusted) - (summ$end.date)
# this looks like garbage -- i don't trust the start and end dates provided by coauthors 
# and prefer to use the calculations from the raw data

#append star.date.adjusted and end.date.adjusted to respective id within d, then cut
d = merge(d,summ, by = "id", all = T)
summary(d)
head(d)
summary(d$start.date.adjusted)
d1 = d[((d$timestamp > d$start.date.adjusted) & (d$timestamp < d$end.date.adjusted)),]
summary(d1)

#subset data by date and id
test = subset(d, id == "Spartacus" & timestamp >= as.POSIXct('2010-09-19 4:00'))
summary(test$timestamp)

for (i in unique(summ$id)) { 
  subset(d, id == i)
  ev.tv.summary$start.date.adjusted[which(ev.tv.summary$id.tag == i)] = balkans$start.date.adjusted[which(balkans$id.tag == i)]
  
}

