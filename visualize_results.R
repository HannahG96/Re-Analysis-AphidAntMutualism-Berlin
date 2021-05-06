##########VISUALIZE RESULTS#############
#library("RColorBrewer")
library("viridis")

#Visualize the task allocation of ant colonies along the Seal_500-axis
#at plant level

Tasks.plot<-merge(expVar[,c("plot.simple", "Seal_500")], Tasks.plot[ ,
                                                                   c("plot.simple", "aphid_IA.sum",
                                                                     "move.sum", "stand.sum", "ant_IA.sum", 
                                                                     "other.sum", "SUM")])
Tasks.plot<-unique(Tasks.plot)

colnames(Tasks.plot)<-c("plot.simple", "Seal_500", "tending", "moving", 
                   "standing", "antinteraction", "other behaviour","SUM")


plotTasks<-as.data.frame(matrix(rep(NA), nrow=0, ncol=4, dimnames=list(NULL, c("plot.simple",
                                                                               "Seal_500",
                                                                               "Behaviour", "Time_prop"))))
for(i in 1:nrow(Tasks.plot)){
  a<-unname(Tasks.plot[i,c(3:7)])
  for(j in 1:length(a)){
    aa<-c(unname(Tasks.plot[i,c(1:2)]), colnames(Tasks.plot)[2+j], a[j] ) 
    plotTasks<-rbind(plotTasks, aa)
    colnames(plotTasks)<-c("plot.simple","Seal_500",
                           "Behaviour", "Time_prop")} }

#urb_levels<-unique(plotTasks[,"Seal_500"])[order(unique(plotTasks[,"Seal_500"]), decreasing=FALSE)]
#plotTasks$Seal_500<-factor(plotTasks$Seal_500, levels=urb_levels)
plotTasks$Behaviour<-factor(plotTasks$Behaviour, levels=c("other behaviour",
                                                          "antinteraction", "standing",
                                                          "moving", "tending"))




ggplot(plotTasks, aes(fill=Behaviour, y=Time_prop, x=Seal_500))+ 
  theme_light()+
  geom_bar(position = "stack", stat="identity")+
  scale_fill_manual(values=c("yellow", "limegreen",
                             "orangered", "turquoise", "hotpink"),
                    breaks=c("other behaviour",
                             "antinteraction", "standing",
                             "moving", "tending"))+
  ylab("Time proportion")+
  xlab("% of Sealing")+
  ggtitle("Time allocated in different behaviours averaged for each plot",
          subtitle="Left to right: low to high level of sealing (in %)")+
  theme(axis.text.x = element_text(angle = 45, size=6, vjust = 1, hjust=1),
        legend.title=element_text(face="bold")
        #plot.title = element_text(hjust = 0.25, vjust=0, size=10, face="bold")
        )


###########


######VISUALIZE ANT AGGRESSIVITY

#Prepare data:
source("Figures.R")


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
#1.aphid number over time including extinctions
#import aphid extinction data:
Aphids<-fread(file = "data/aphid_extinctions.csv", na.strings = "kA", dec = "," , data.table = FALSE)
Aphids$date<-as.Date(Aphids$date, format = "%d.%m.%Y") 
Aphids<-rbind(Aphids, Ant_attendance[, c("plot.simple", "plant", "date", "N_aphid")])
Aphids<-merge(Aphids, General_plot[, c("plot.simple", "Seal_500")], by="plot.simple", all=T)
Aphids<-unique(Aphids)
Aphids$Seal_500<-as.numeric(Aphids$Seal_500)


#urb_levels<-unique(Aphids[,"Seal_500"])[order(unique(Aphids[,"Seal_500"]), decreasing=T)]
#Aphids$Seal_500.col<-factor(Aphids$Seal_500, levels=urb_levels)
#cols<-c("black","firebrick","orangered2","darkorange","gold3","greenyellow","greenyellow",
       # "limegreen", "forestgreen")

#urb_levels<-unique(Aphids[,"Seal_500"])[order(unique(Aphids[,"Seal_500"]), decreasing=F)]
#Aphids$Seal_500<-factor(Aphids$Seal_500, levels=urb_levels)

Aphids$plot.simple<-factor(Aphids$plot.simple, levels=c("Ol-11","Ol-55", "Nh-04", "Nh-05",
                                                        "Om-02", "Nl-55", "Nh-10", "Oh-02",
                                                        "Nl-200"))

ggplot(Aphids, aes(x=date)) +
  theme_bw()+
  geom_point(aes(y=N_aphid,color=Seal_500, shape=plant), size=2) +
  #geom_line(aes(y=N_aphid,color=Seal_500.col, group=plant))+
  #scale_color_manual("Sealing (in %)",values=cols)+
  #scale_color_distiller(palette="Paired")+
  scale_color_viridis("Sealing in %",option="turbo")+
  scale_shape_manual("Aphid colony",values=c(1,2,0,4,5),
                     breaks=c("Alpha", "Beta", "Ceta", "Delta", "Gamma"))+
  ylab("Number of aphids")+
  xlab("Date")+
  ggtitle("Aphid number over time along the gradient of urbanisation ")+
  theme(axis.text.x = element_text(angle = 45, size=6, vjust = 1, hjust=1),
        plot.title=element_text(size=13, face="bold", hjust=0.5),
        legend.title=element_text(size=10, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
        #strip.text.x = element_blank(),
        #strip.background = element_blank()
  )+
  facet_wrap(~plot.simple)



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
