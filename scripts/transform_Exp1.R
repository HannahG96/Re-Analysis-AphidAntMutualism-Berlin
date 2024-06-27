###EXP1###
#What is relevant?
#-->Behaviour data, ant category info (caretaker/scout/transporter)

###Remove empty columns:
Exp1<-Exp1[,-24]

###Add ant category: 
#caretaker=length of ant action ("L_act") area equal/inferior to focal zone length+moved around colony ("act_c"=1)
#scouts=length of ant action area ("L_act") exceeds focal zone length+moved not around colony ("act_c"=0)
#transporter=ant left the plant
Exp1<-unique(merge(Exp1,Exp2[,c("plot.simple","plot","plant","date","l")], all=TRUE))#add length of focal zone info
Exp1$indv_cat<-NA
Exp1$l<-Exp1$l / 10 #change mm in cm

for(i in 1:nrow(Exp1)){
  num<-Exp1[i,"l"]- Exp1[i,"L_act"]
  if(is.na(num)==T && Exp1[i,"left"]==0) next  
  if(Exp1[i,"act_c"]==1 && num > 0 && Exp1[i,"left"]==0) {Exp1[i,"indv_cat"]<-"caretaker"}
  if(Exp1[i,"act_c"]==0 && num > 0 && Exp1[i,"left"]==0){Exp1[i,"indv_cat"]<-"scouts"}
  if(num < 0 && Exp1[i,"left"]==0){Exp1[i,"indv_cat"]<-"scouts"}
  if(Exp1[i,"left"]==1){Exp1[i,"indv_cat"]<-"transporter"}}

###Make date information understandable for R:
Exp1$date <- as.Date (Exp1$date , format = "%d.%m.%Y")

###Calculate the cumulated record period of ant behaviour per ant category*date*plant*plot
colnames(Exp1)[which(colnames(Exp1)=="rec")]<-"record.name"
record.period<-merge(Exp1[,c("plot.simple","plant", "date","indv_cat", "record.name")], 
                         activ_seq_raw[,c("record.name", "time.length.s.")], by="record.name", all=TRUE)
cum.record.period<-summaryBy(formula=time.length.s.~ plot.simple+plant+date+indv_cat, 
                             data=record.period, FUN=sum, na.rm=F)

#at level of plot (to visualize task allocation at plot level):
record_plot<-summaryBy(formula=time.length.s.~ plot.simple+indv_cat, 
                        data=record.period, FUN=sum, na.rm=F)

colnames(cum.record.period)[5]<-"cum.record.period" #change this complicated column name
colnames(record_plot)[3]<-"record.plot"

Exp1<-merge(Exp1, cum.record.period, all=TRUE)#add this cum. record period to Exp1
Exp1<-merge(Exp1, record_plot, all=TRUE)#sum of all record periods at plot level

###Format behaviour data to obtain time proportions spent in each behaviour 
source("scripts/import activity sequence.R")
Tasks<-merge(Exp1, activ_sum.plot, by="record.name", all=T)
Exp1<-merge(Exp1, activ_sum, by="record.name", all=T)

###Calculate rate of behavioural transitions
#=number of behavioural transitions ("nb.switch") / cumulated record period*ant category*date*plot
Exp1$unit_switch <- Exp1$nb.switch / Exp1$cum.record.period

###Produce a summary table indicating the cumulated proportion of time spent in each behaviour at 

##ant category*date*plot-level:
cum.task.allocation<-summaryBy(formula=record.period+unit_switch+aphid_IA+move+
                                 stand+ant_IA+other~plot.simple+plant+date+indv_cat,
                               data=Exp1, FUN=sum, na.rm=F)
cum.task.allocation$SUM<-apply(cum.task.allocation[,c(7:11)],1,FUN=sum)#CONTROL

##and at plant*plot-level
Tasks<-Tasks[which(Tasks[,"indv_cat"]=="caretaker"),]
Tasks.plot<-summaryBy(formula=record.period+aphid_IA+move+
                        stand+ant_IA+other~plot.simple+record.plot,
                 data=Tasks, FUN=sum, na.rm=F)
Tasks.plot$SUM<-apply(Tasks.plot[,c(4:8)],1,FUN=sum)#CONTROL