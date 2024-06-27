###MY RESPONSE VARIABLES###
#-->create tables for each of the modelled response variables that join together all explanatory variables 
   #needed to build the model

###EXPLANATORY VARIABLES
expVar<-Exp2_plot_date[,c("plot.simple", "plant", "date")]
expVar$plantPop<-paste(expVar[,"plot.simple"], expVar[,"plant"], sep=" ") #random effect=plant population
expVar<-merge(expVar, Exp2_plot_date[,c("plot.simple", "plant", "date", "meanAnt.mean", "N_aphid")], all=T)#ant+aphid number
expVar<-merge(expVar, Met_plant[,c("plot.simple", "plant", "date", "plantStade", "prop_paras")], all=T) #+plant stade
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
ggpairs(expVar[,c("date","mean.temp","N_aphid","meanAnt.mean","plantStade")])
#continuous/continuous variables-->Pearson?s correlation coefficient
cor(expVar[, c("meanAnt.mean", "N_aphid", "mean.temp", "Seal_500")], method="pearson")
#factor/continuous variables-->Spearman's rank correlation test (??)

######################

#Standardize all continuous variables:
expVar=mutate_at(expVar, vars(date,meanAnt.mean,N_aphid,prop_paras,mean.temp,Seal_500), funs(s = as.numeric( scale(.) ) ) )

###1.APHID DENSITY
Aphid_density<-merge(expVar,Exp2_plot_date[,c("plot.simple", "plant", "date", "l","N_aphid.mm")],all=T)

###2.ANT ATTENDANCE
Ant_attendance<-merge(expVar,Exp2_plot_date[,c("plot.simple", "plant", "date","AntperAphid.mean")],all=T)

###3.TENDING TIME: only caretaker
#1 lacking observation Nh-04/03.08 (this day no caretaker recorded)
Tending_Time<-merge(expVar, cum.task.allocation[which(cum.task.allocation[,"indv_cat"]=="caretaker"), 
                                                 c("plot.simple", "plant", "date", "record.period.sum", 
                                                 "aphid_IA.sum","move.sum","stand.sum", 
                                                 "ant_IA.sum","other.sum")], all=T)
###4.GROUP REACTION
Group_reaction<-merge(expVar, Exp3a[, c("plot.simple", "plant", "date", "N_ant", "MAXREACT", "MAXATTACKS", 
                                        "MAXREACT_prop")], all=T)


###5.ANT AGGRESSIVITY
Ant_aggressivity<-merge(expVar, Exp3b[, c("plot.simple", "plant", "date","context","aggr_score","aggr")],
                        all=F)
Ant_aggressivity$context<-as.factor(Ant_aggressivity$context)
#reaction vs. no reaction
Ant_aggressivity$reaction <- as.numeric(Ant_aggressivity$aggr_score >0)

#attack given reaction 
# (excluding cases of no reaction -> replaced by NA)
Ant_aggressivity$attack.given.react <- as.numeric(Ant_aggressivity$aggr_score ==2)
Ant_aggressivity$attack.given.react[Ant_aggressivity$aggr_score ==0] <- NA 

#probability of attack
# (NOT excluding cases of no reaction)
Ant_aggressivity$attack <- as.numeric(Ant_aggressivity$aggr_score ==2)


###6. EXCLUDED BEHAVIOUR DATA (Ant aggressivity experiment)
#Excluded curious/tolerant behaviours: to test whether these "failed experiments"
#are homogeneously distributed along urban gradient:
Exp3b_noReact<-merge(expVar,Exp3b_noReact[,c("plot.simple", "plant", "date","context","noReact")])
Exp3b_noReact$noReact<-as.factor(Exp3b_noReact$noReact)
