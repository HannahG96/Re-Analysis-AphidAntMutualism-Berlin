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
  geom_bar(position = "stack", stat="identity", width = 1.75)+
  scale_fill_manual(values=c("bisque3", "limegreen",
                             "darksalmon", "cadetblue1", "deeppink4"),
                    breaks=c("other behaviour",
                             "antinteraction", "standing",
                             "moving", "tending"))+
  ylab("Time proportion")+
  xlab("% of Sealing")+
  ggtitle("Time allocated in different behaviours averaged over each plot")+
  theme(#axis.text.x = element_text(angle = 45, size=6, vjust = 1, hjust=1),
        legend.title=element_text(face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.25, vjust=0, size=10, face="bold"))

#OR

ggplot(plotTasks, aes(fill=Behaviour, y=Time_prop, x=as.factor(Seal_500)))+ 
  theme_light()+
  geom_bar(position = "stack", stat="identity")+
  scale_fill_manual(values=c("bisque3", "limegreen",
                             "darksalmon", "cadetblue1", "deeppink4"),
                    breaks=c("other behaviour",
                             "antinteraction", "standing",
                             "moving", "tending"))+
  ylab("Time proportion")+
  xlab("% of Sealing")+
  ggtitle("Time allocated in different behaviours averaged over each plot")+
  theme(axis.text.x = element_text(angle = 45, size=6, vjust = 1, hjust=1),
    legend.title=element_text(face="bold"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.25, vjust=0, size=10, face="bold"))

###########


######VISUALIZE ANT AGGRESSIVITY

#Prepare data:
source("Figures.R")

#Produce plot based on predicted data:
ggplot(data = newd, mapping = aes(Seal_500, predicted)) +
     geom_point(size=1.75,aes(color=Seal_500, shape=context)) +
  theme_light()+
  geom_smooth(method = "glm", formula = y ~ x, 
  method.args=list(family="binomial"), se = FALSE, color="black") +
  scale_shape_manual("Behavioural context", values=c(16,4),
                     breaks=c("tending aphids", "other behaviour"),
                     labels=c("tending aphids", "other behaviour"))+
  scale_color_viridis("Sealing in %",option="turbo")+
  ggtitle("Ant aggressivity")+
  theme( plot.title=element_text(size=13, face="bold", hjust=0.5),
        legend.title=element_text(size=10, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("% Sealing") +
  ylab("Probability of aggressive reaction")

#OR
ggplot(data = newd, mapping = aes(Seal_500, predicted)) +
  geom_point(size=1.75,aes(color=context)) +
  theme_light()+
  geom_smooth(method = "glm", formula = y ~ x, 
              method.args=list(family="binomial"), se = FALSE, color="black") +
  scale_color_manual("Behavioural context", values=c("deeppink4", "antiquewhite4"),
                     breaks=c("tending aphids", "other behaviour"),
                     labels=c("tending aphids", "other behaviour"))+
  ggtitle("Ant aggressivity")+
  theme( plot.title=element_text(size=13, face="bold", hjust=0.5),
    legend.title=element_text(size=10, face="bold"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  ) +
  xlab("% Sealing") +
  ylab("Probability of aggressive reaction")

####################
#Raw data:
#Plot level:
ggplot(plotAggressivity, aes(x=Seal_500, y=prop_reaction))+
  theme_light()+
  geom_point(size=1.75,aes(color=Seal_500, shape=context))+
  scale_color_viridis("Sealing in %",option="turbo")+
  scale_shape_manual("Behavioural context", values=c(1,2),
                     breaks=c("tending aphids", "other behaviour"),
                     labels=c("tending aphids", "other behaviour"))

#Plot/plant/date level:
plotAggressivity.date <- summaryBy(formula = reaction + ant ~ Seal_500 + 
                                plot.simple + plant + date + context, 
                              data=a,
                              FUN=sum)

# calculate proportion of aggressive reactions
plotAggressivity.date$prop_reaction <- 
  plotAggressivity.date$reaction.sum / plotAggressivity.date$ant.sum 

ggplot(plotAggressivity.date, aes(x=Seal_500, y=prop_reaction))+
  theme_light()+
  geom_point(size=1.75,aes(color=Seal_500, shape=context))+
  scale_color_viridis("Sealing in %",option="turbo")+
  scale_shape_manual("Behavioural context", values=c(1,2),
                     breaks=c("tending aphids", "other behaviour"),
                     labels=c("tending aphids", "other behaviour"))




######VISUALIZE APHID NUMBER AND ANT ATTENDANCE
#1.aphid number over time including extinctions
#import aphid extinction data:
Aphids<-fread(file = "data/aphid_extinctions.csv", na.strings = "kA", dec = "," , data.table = FALSE)
Aphids$date<-as.Date(Aphids$date, format = "%d.%m.%Y") 
Aphids<-rbind(Aphids, Ant_attendance[, c("plot.simple", "plant", "date", "N_aphid")])
Aphids<-merge(Aphids, General_plot[, c("plot.simple", "Seal_500")], by="plot.simple", all=T)
Aphids<-unique(Aphids)
Aphids$Seal_500<-as.numeric(Aphids$Seal_500)

Aphids$plot.simple<-factor(Aphids$plot.simple, levels=c("Ol-11","Ol-55", "Nh-04", "Nh-05",
                                                        "Om-02", "Nl-55", "Nh-10", "Oh-02",
                                                        "Nl-200"))
Aphids$plantPop<-paste(Aphids$plot.simple, Aphids$plant, sep=" ")

ggplot(Aphids, aes(x=date)) +
  theme_bw()+
  geom_jitter(aes(y=N_aphid,shape=plant, color=Seal_500), size=2) +
  #geom_smooth(mapping=aes(y=N_aphid, group=plantPop), method="gam", se=FALSE) +
  geom_line(aes(y=N_aphid,color=Seal_500, group=plant))+
  scale_shape_manual("Aphid colony",values=c(16, 17, 3, 15, 8),
                     breaks=c("Alpha", "Beta", "Ceta", "Delta", "Gamma"))+
  scale_color_viridis("Sealing in %",option="turbo")+
  scale_fill_viridis("Sealing in %",option="turbo")+
  ylab("Number of aphids")+
  xlab("Date")+
  ggtitle("Aphid number over time along the gradient of urbanisation ")+
  theme(axis.text.x = element_text(angle = 45, size=6, vjust = 1, hjust=1),
        plot.title=element_text(size=13, face="bold", hjust=0.5),
        legend.title=element_text(size=10, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text.x = element_text(face="bold", size=10),
        strip.background = element_blank())+
  facet_wrap(~plot.simple)



#2.ant attendance
Ant_attendance$plot.simple<-factor(Ant_attendance$plot.simple, levels=c("Ol-11","Ol-55", "Nh-04", "Nh-05",
                                                        "Om-02", "Nl-55", "Nh-10", "Oh-02",
                                                        "Nl-200"))

ggplot(Ant_attendance, aes(x=N_aphid)) +
  theme_bw()+
  geom_jitter(aes(y=meanAnt.mean,shape=plant, color=Seal_500), size=2) +
  scale_shape_manual("Aphid colony",values=c(16, 17, 3, 15, 8),
                     breaks=c("Alpha", "Beta", "Ceta", "Delta", "Gamma"))+
  scale_color_viridis("Sealing in %",option="turbo")+
  scale_fill_viridis("Sealing in %",option="turbo")+
  ylab("Number of ants")+
  xlab("Number of aphids")+
  ggtitle("Ant attendance")+
  theme(axis.text.x = element_text(angle = 45, size=6, vjust = 1, hjust=1),
        plot.title=element_text(size=13, face="bold", hjust=0.5),
        legend.title=element_text(size=10, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text.x = element_text(face="bold", size=10),
        strip.background = element_blank())+
  facet_wrap(~plot.simple)

