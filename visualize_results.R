##########VISUALIZE RESULTS#############

#Visualize the task allocation of ant colonies along the Seal_500-axis
#at plant level

Tasks.plant<-merge(expVar[,c("plot.simple", "plant", "Seal_500")], Tasks.plant[ ,
                                                                   c("plot.simple", "plant", "aphid_IA.sum",
                                                                     "move.sum","walk.flower.sum","walk.leave.sum", 
                                                                     "inactive.sum", "selfclean.sum", "eatpollen.sum",
                                                                     "stretching.sum", "ant_IA.sum",
                                                                     "uncategorized.sum", "SUM")])

colnames(Tasks.plant)<-c("plot.simple", "plant", "Seal_500", "tending", "moving", "flowerwalks", "leavewalks",
                   "standing", "selfcleaning", "polleneating", "stretching", "antinteraction",
                   "uncategorized behaviour","SUM")
Tasks.plant<-unique(Tasks.plant)

plotTasks<-as.data.frame(matrix(rep(NA), nrow=0, ncol=5, dimnames=list(NULL, c("plot.simple",
                                                                               "plant", "Seal_500",
                                                                               "Behaviour", "Time_prop"))))
for(i in 1:nrow(Tasks.plant)){
  a<-unname(Tasks.plant[i,c(4:13)])
  for(j in 1:length(a)){
    aa<-c(unname(Tasks.plant[i,c(1:3)]), colnames(Tasks.plant)[3+j], a[j] ) 
    plotTasks<-rbind(plotTasks, aa)
    colnames(plotTasks)<-c("plot.simple",
                           "plant", "Seal_500",
                           "Behaviour", "Time_prop")} }

urb_levels<-unique(plotTasks[,"Seal_500"])[order(unique(plotTasks[,"Seal_500"]), decreasing=FALSE)]
plotTasks$Seal_500<-factor(plotTasks$Seal_500, levels=urb_levels)
plotTasks$Behaviour<-factor(plotTasks$Behaviour, levels=c("uncategorized behaviour","polleneating", "stretching","selfcleaning", 
                                                          "antinteraction","flowerwalks","leavewalks",
                                                          "moving","standing","tending"))




ggplot(plotTasks, aes(fill=Behaviour, y=Time_prop, x=plant))+ 
  theme_light()+
  geom_bar(position = "stack", stat="identity")+
  scale_fill_manual(values=c("lemonchiffon3", "yellow", "limegreen",
                             "gold3", "orangered", "turquoise", "skyblue3",
                             "royalblue", "mediumvioletred", "hotpink"),
                    breaks=c("uncategorized behaviour","polleneating", "stretching",
                             "selfcleaning", "antinteraction","flowerwalks","leavewalks",
                             "moving","standing","tending"))+
  ylab("Time proportion")+
  xlab("Plant")+
  ggtitle("Time allocated in different behaviours averaged for each ant colony",
          subtitle="Left to right: low to high level of sealing (in %)")+
  theme(axis.text.x = element_text(angle = 45, size=6, vjust = 1, hjust=1),
        legend.title=element_text(face="bold")
        #plot.title = element_text(hjust = 0.25, vjust=0, size=10, face="bold")
        )+
  facet_grid(~Seal_500)



###########


######VISUALIZE ANT AGGRESSIVITY

#Prepare data:

#-->proportion of aggressive reacting ants
a<-Ant_aggressivity
a$ant<-1
plotAggressivity<-summaryBy(formula=reaction+ant~Seal_500+plot.simple+plant+date+context, 
                            data=a,FUN=sum)
plotAggressivity$prop_reaction<-plotAggressivity$reaction.sum / plotAggressivity$ant.sum 

#categorical Sealing variable
plotAggressivity$Sealing_level<-NA
plotAggressivity[which(plotAggressivity[,"Seal_500"]>40), "Sealing_level"]<-"[>40%]"
plotAggressivity[which(plotAggressivity[,"Seal_500"]<40), "Sealing_level"]<-"[20-40%]"
plotAggressivity[which(plotAggressivity[,"Seal_500"]<20), "Sealing_level"]<-"[<20%]"
plotAggressivity$Sealing_level<-factor(plotAggressivity$Sealing_level, levels=c("[<20%]","[20-40%]","[>40%]"))

#Produce figure:

ggplot(plotAggressivity, aes(x=date, y=prop_reaction))+
  theme_light()+
  geom_point(size=2.4,aes(color=Seal_500, shape=context))+
  scale_color_gradient2("Percentage of \n sealed surface",midpoint=30,low = "forestgreen", 
                        high="firebrick",mid="gold3")+
  scale_shape_manual("Behavioural context", values=c(16,2),
                     breaks=c("tending aphids", "other behaviour"),
                     labels=c("tending aphids", "other behaviour"))+
  facet_grid(~Sealing_level)

#OR:
urb_levels<-unique(plotAggressivity[,"Seal_500"])[order(unique(plotAggressivity[,"Seal_500"]), decreasing=T)]
plotAggressivity$Seal_500.col<-factor(plotAggressivity$Seal_500, levels=urb_levels)

cols<-c("black","firebrick","orangered2","darkorange","gold3","greenyellow","greenyellow",
        "limegreen", "forestgreen")
#cols<-c("forestgreen","limegreen","greenyellow","greenyellow","gold3","darkorange",
        #"orangered2","firebrick","black")

urb_levels<-unique(plotAggressivity[,"Seal_500"])[order(unique(plotAggressivity[,"Seal_500"]), decreasing=F)]
plotAggressivity$Seal_500<-factor(plotAggressivity$Seal_500, levels=urb_levels)


ggplot(plotAggressivity, aes(x=date, y=prop_reaction))+
  theme_light()+
  geom_point(size=2.4,aes(color=Seal_500.col, shape=context))+
  scale_color_manual("Sealing (in %)",values=cols)+
  scale_shape_manual("Behavioural context", values=c(16,2),
                     breaks=c("tending aphids", "other behaviour"),
                     labels=c("tending aphids", "other behaviour"))+
  ylab("")+
  xlab("Date")+
  ggtitle("Proportion of aggressive reacting ants over time along the gradient of urbanisation")+
  theme(axis.text.x = element_text(angle = 45, size=6, vjust = 1, hjust=1),
        plot.title=element_text(size=13, face="bold", hjust=0.5),
        legend.title=element_text(size=10, face="bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank() )+
  facet_grid(~Seal_500)


######VISUALIZE APHID NUMBER AND ANT ATTENDANCE
#1.aphid number over time
max(Ant_attendance$N_aphid)#264
max(Ant_attendance$meanAnt.mean)#48.40833
y_ext<-270/50#5.4=extension coefficient for ant number y-axis
Ant_attendance$N_ant.plot<-Ant_attendance$meanAnt.mean*y_ext

urb_levels<-unique(Ant_attendance[,"Seal_500"])[order(unique(Ant_attendance[,"Seal_500"]), decreasing=T)]
Ant_attendance$Seal_500.col<-factor(Ant_attendance$Seal_500, levels=urb_levels)
cols<-c("black","firebrick","orangered2","darkorange","gold3","greenyellow","greenyellow",
        "limegreen", "forestgreen")

urb_levels<-unique(Ant_attendance[,"Seal_500"])[order(unique(Ant_attendance[,"Seal_500"]), decreasing=F)]
Ant_attendance$Seal_500<-factor(Ant_attendance$Seal_500, levels=urb_levels)

ggplot(Ant_attendance, aes(x=date)) +
  theme_light()+
  geom_point(aes(y=N_aphid,color=Seal_500.col, shape=plant)) +
  scale_color_manual("Sealing (in %)",values=cols)+
  scale_shape_manual("Plant",values=c(1,2,3,4,5),
                     breaks=c("Alpha", "Beta", "Ceta", "Delta", "Gamma"))+
  ylab("Number of aphids")+
  xlab("Date")+
  ggtitle("Aphid number over time along the gradient of urbanisation ")+
  theme(axis.text.x = element_text(angle = 45, size=6, vjust = 1, hjust=1),
        plot.title=element_text(size=13, face="bold", hjust=0.5),
        legend.title=element_text(size=10, face="bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  facet_grid(~Seal_500)

#2.ant number over time
ggplot(Ant_attendance, aes(x=date)) +
  theme_light()+
  geom_point(aes(y=meanAnt.mean,color=Seal_500.col, shape=plant)) +
  scale_color_manual("Sealing (in %)",values=cols)+
  scale_shape_manual("Plant",values=c(1,2,3,4,5),
                     breaks=c("Alpha", "Beta", "Ceta", "Delta", "Gamma"))+
  ylab("Number of ants")+
  xlab("Date")+
  ggtitle("Ant number over time along the gradient of urbanisation ")+
  theme(axis.text.x = element_text(angle = 45, size=6, vjust = 1, hjust=1),
        plot.title=element_text(size=13, face="bold", hjust=0.5),
        legend.title=element_text(size=10, face="bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  facet_grid(~Seal_500)
