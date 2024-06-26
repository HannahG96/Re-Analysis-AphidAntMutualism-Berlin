###GENERAL_PLOT###
#What is relevant?
#-->includes summary of fieldwork

###Remove empty columns:
General_plot<-General_plot[,-8]

###Rename a column:
colnames(General_plot)[6]<-"sampleSize"

###Add coordinates and sealing info:
General_plot<-merge(General_plot,MyGivenMeta_plots[,c("Long","Lat","Seal_500","plot.simple")],by="plot.simple",all=TRUE)

###Add extinction events:


###Calculate Summary table of fieldwork:
field.summary<-summaryBy(formula=visits+sampleSize+N_plant~plot.simple+Long.y+Lat.y+Seal_500, data=General_plot,
                         FUN=sum, na.rm=T)

###Rename columns:
colnames(field.summary)<-c("plot.simple","Long","Lat","Seal_500","N_visits","N_sampling_session","N_plant")

###Store field summary as xlsx file
write.xlsx(field.summary, file="results/field_summary.xlsx")
