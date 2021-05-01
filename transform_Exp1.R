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
#+at level of plant (to visualize task allocation at plant level):
record_plant<-summaryBy(formula=time.length.s.~ plot.simple+plant+indv_cat, 
                        data=record.period, FUN=sum, na.rm=F)

colnames(cum.record.period)[5]<-"cum.record.period" #change this complicated column name
colnames(record_plant)[4]<-"record.plant"

Exp1<-merge(Exp1, cum.record.period, all=TRUE)#add this cum. record period to Exp1
Exp1<-merge(Exp1, record_plant, all=TRUE)#+sum of all record periods at plant level

###Format behaviour data to obtain time proportions spent in each behaviour 
source("import activity sequence.R")
Tasks<-merge(Exp1, activ_sum.plant, by="record.name", all=T)
Exp1<-merge(Exp1, activ_sum, by="record.name", all=T)

###Calculate rate of behavioural transitions
#=number of behavioural transitions ("nb.switch") / cumulated record period*ant category*date*plot
Exp1$unit_switch <- Exp1$nb.switch / Exp1$cum.record.period

###Produce a summary table indicating the cumulated proportion of time spent in each behaviour at 
#ant category*date*plot-level:

cum.task.allocation<-summaryBy(formula=record.period+unit_switch+aphid_IA+inactive+move+
                                 walk.flower+walk.leave+selfclean+eatpollen+stretching+
                                 ant_IA+uncategorized~plot.simple+plant+date+indv_cat,
                               data=Exp1, FUN=sum, na.rm=F)
cum.task.allocation$SUM<-apply(cum.task.allocation[,c(7:16)],1,FUN=sum)#CONTROL

##and at plant*plot-level
Tasks<-Tasks[which(Tasks[,"indv_cat"]=="caretaker"),]
Tasks.plant<-summaryBy(formula=record.period+aphid_IA+inactive+move+
                   walk.flower+walk.leave+selfclean+eatpollen+stretching+
                   ant_IA+uncategorized~plot.simple+plant+record.plant,
                 data=Tasks, FUN=sum, na.rm=F)
Tasks.plant$SUM<-apply(Tasks.plant[,c(5:14)],1,FUN=sum)#CONTROL

#Explore SUM of task time proportions~0.96 for Nl-200
#=16.08-->summed task proportions for caretaker+transporter+scouts do not reach 1 but [0.72-0.92]
#Records:D1,D2,D3,D4,D5,D6
#->This was the day where another ant colony "invaded" the plant and I observed a fight between colonies
#=ants showed many behaviours that I did not predefine eg. biting gaster of other ant...
#These behaviours where classified in the category "ow"-->undefined behaviour
#Example activity sequence:
activ_seq_raw[which(activ_seq_raw[,"record.name"]=="D1"), "states"]
#-->ERROR now corrected: I added a "uncategorized behaviour" column
#=SUM of all task time proportions should yield zero

#+Ol-55 record E59 (25.08): typing error-->"c" instead of "cl"
#+Ol-55 record E7 (21.08): "xl" instead of "cl"
#+3 other typing errors
#   =corrected in raw data

