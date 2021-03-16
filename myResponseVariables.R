###MY RESPONSE VARIABLES###
#-->create tables for each of the modelled response variables that join together all explanatory variables 
   #needed to build the model

###EXPLANATORY VARIABLES
expVar<-Exp2_plot_date[,c("plot.simple", "plant", "date")]
expVar$plantPop<-paste(expVar[,"plot.simple"], expVar[,"plant"], sep=" ") #random effect=plant population
expVar<-merge(expVar, Exp2_plot_date[,c("plot.simple", "plant", "date", "meanAnt.mean", "N_aphid")], all=T)#ant+aphid number
expVar<-merge(expVar, Met_plant[,c("plot.simple", "plant", "date", "plantStade")], all=T) #+plant stade
expVar<-merge(expVar, Met_plot_date[,c("plot.simple", "date","mean.temp")], all=T) #+temperature
expVar<-merge(expVar, field.summary[,c("plot.simple","Seal_500")], all=T)#+sealing
glimpse(expVar)#check class of each variable
expVar$date<-as.Date(expVar$date, format = "%d.%m.%Y")#make date column understandable for R
expVar$Seal_500<-as.numeric(expVar$Seal_500)#make sealing numeric
expVar$plantStade<-as.factor(expVar$plantStade)#factorize phenological stade
expVar$plantPop<-as.factor(expVar$plantPop)#factorize plant population category
expVar$plot.simple<-as.factor(expVar$plot.simple)#factorize plot category

######################
#Check for correlation of predictors:
ggpairs(expVar[,c("date","mean.temp","N_aphid","meanAnt.mean","Seal_500","plantStade")])
#continuous/continuous variables-->Pearson´s correlation coefficient
cor(expVar[, c("meanAnt.mean", "N_aphid", "mean.temp", "Seal_500")], method="pearson")
#factor/continuous variables-->Spearman's rank correlation test (??)

######################

#Standardize all continuous variables:
expVar=mutate_at(expVar, vars(date,meanAnt.mean,N_aphid,mean.temp,Seal_500), funs(s = as.numeric( scale(.) ) ) )

###1.APHID DENSITY
Aphid_density<-merge(expVar,Exp2_plot_date[,c("plot.simple", "plant", "date", "l","N_aphid.mm")],all=T)
###2.ANT ATTENDANCE
Ant_attendance<-merge(expVar,Exp2_plot_date[,c("plot.simple", "plant", "date","AntperAphid.mean")],all=T)
###3.TENDING TIME: only caretaker
Tending_Time<-merge(expVar, cum.task.allocation[which(cum.task.allocation[,"indv_cat"]=="caretaker"), 
                                                 c("plot.simple", "plant", "date", "record.period.sum", 
                                                 "aphid_IA.sum")], all=T)
###4.GROUP REACTION
Group_reaction<-merge(expVar, Exp3a[, c("plot.simple", "plant", "date", "N_ant", "MAXREACT", "MAXATTACKS", 
                                        "MAXREACT_prop")], all=T)
###5.ANT AGGRESSIVITY
Ant_aggressivity<-merge(expVar, Exp3b[, c("plot.simple", "plant", "date","context","aggr_score")],
                        all=F)
Ant_aggressivity$context<-as.factor(Ant_aggressivity$context)
###6.PROPORTION OF PARASITISM 
Parasitism<-merge(expVar,Met_plant[,c("plot.simple", "plant", "date", "Prop_paras")],all=T)
#-->add mean aggressivity score (entire ant colony) + tending time (caretaker ants) as additional
#variables of ant behaviour to explain parasitism within aphid colonies + standardize these variables
#Note: better pick mean aggressivity score of aphid tending ant (???)
Ant_bhv<-summaryBy(aggr_score~plot.simple+plant+date, Ant_aggressivity, FUN=c(mean,sd))
     #Note: total of 52 observations-->10.08 (Nh_05) all obs were excluded due to no reaction to needle irritation
Ant_bhv<-merge(Tending_Time[,c("plot.simple", "plant", "date", "aphid_IA.sum")], Ant_bhv, all=T)
Ant_bhv=mutate_at(Ant_bhv, vars(aphid_IA.sum, aggr_score.mean), funs(s = as.numeric( scale(.) ) ) )
Parasitism<-merge(Ant_bhv, Parasitism, all=T)

###Remove all object that are not needed anymore=clean workspace
rm.all.but(keep=c("scriptwd", "modelresultswd", "Aphid_density", "Parasitism", "Ant_attendance",
                  "Tending_Time", "Group_reaction", "Ant_aggressivity"))
