##########VISUALIZE RESULTS#############
#library("viridis")
library("ggsci")#grey colour palettes

###VISUALIZE ANT BEHAVIOUR
#Visualize the task allocation of ant colonies along the Seal_500-axis
#at plant level:

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

plotTasks$Behaviour<-factor(plotTasks$Behaviour, levels=c("other behaviour",
                                                          "antinteraction", "standing",
                                                          "moving", "tending"))



pal_material("blue-grey")(30)
ggplot(plotTasks, aes(fill=Behaviour, y=Time_prop, x=Seal_500))+ 
  theme_light()+
  geom_bar(position = "stack", stat="identity", width = 1.75, color="black")+
  scale_fill_manual(values=c("#CED8DCFF", "#455964FF", "seashell3",
                               "#78909BFF", "gray40"),
                    breaks=c("other behaviour",
                             "antinteraction", "standing",
                             "moving", "tending"),
                    labels=c("other behaviour", "ant interaction", "standing",
                             "moving", "tending aphids"))+
  ylab("Time proportion")+
  xlab("% Sealing")+
  theme(legend.title=element_text(face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#export figure:
#pdf(file="behaviour.pdf", width=11, height=8.5)


######VISUALIZE ANT AGGRESSIVITY

#Prepare data:
source("Figures.R")

#Produce plot based on predicted data:

ggplot(data = newd, mapping = aes(Seal_500, predicted)) +
  geom_point(size=1.75,aes(color=context)) +
  theme_bw()+
  geom_smooth(aes(color=context), method = "glm", formula = y ~ x, 
              method.args=list(family="binomial"), se = FALSE) +
  scale_color_manual("Behavioural context", values=c("black", "#90A4ADFF"),
                     breaks=c("tending aphids", "other behaviour"),
                     labels=c("tending aphids", "other behaviour"))+
  theme(legend.title=element_text(size=9, face="bold"),
        axis.title.y=element_text(size=9),
        axis.title.x=element_text(size=11),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank() ) +
  xlab("% Sealing") +
  ylab("Probability of aggressive reaction")+
  ylim(0,1)

####################
#Raw data:
plotAggressivity.date <- summaryBy(formula = reaction + ant ~ Seal_500 + 
                                plot.simple + plant + date + context, 
                              data=a,
                              FUN=sum)

# calculate proportion of aggressive reactions
plotAggressivity.date$prop_reaction <- 
  plotAggressivity.date$reaction.sum / plotAggressivity.date$ant.sum 

ggplot(plotAggressivity.date, aes(x=Seal_500, y=prop_reaction))+
  theme_bw()+
  geom_jitter(size=1.75,aes(color=context)) +
  geom_smooth(data=newd, aes(x=Seal_500, y=predicted, color=context), 
              method = "glm", formula = y ~ x, 
              method.args=list(family="binomial"), se = FALSE) +
  scale_color_manual("Behavioural context", values=c("black", "#90A4ADFF"),
                     breaks=c("tending aphids", "other behaviour"),
                     labels=c("tending aphids", "other behaviour"))+
  theme(legend.title=element_text(size=9, face="bold"),
        axis.title.y=element_text(size=9),
        axis.title.x=element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank() ) +
  xlab("% Sealing") +
  ylab("Probability of aggressive reaction")

######VISUALIZE APHID NUMBER (AND ANT ATTENDANCE)
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

ggplot(Aphids, aes(x=date, fill=Seal_500)) +
  theme_bw()+
  geom_point(aes(y=N_aphid,shape=plant), size=2, color="black") +
  geom_line(aes(y=N_aphid, group=plant))+
  scale_shape_manual("Aphid colony",values=c(21, 24, 23, 22, 25),
                     breaks=c("Alpha", "Beta", "Ceta", "Delta", "Gamma"))+
  scale_fill_material("blue-grey")+
  labs(fill="% Sealing")+
  ylab("Number of aphids")+
  xlab("Date")+
  theme(axis.text.x = element_text(angle = 45, size=6, vjust = 1, hjust=1),
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
  geom_jitter(aes(y=meanAnt.mean,shape=plant, fill=Seal_500), size=2)  +
  #geom_line(aes(y=meanAnt.mean, group=plant))+
  scale_shape_manual("Aphid colony",values=c(21, 24, 23, 22, 25),
                     breaks=c("Alpha", "Beta", "Ceta", "Delta", "Gamma"))+
  scale_fill_material("blue-grey")+
  labs(fill="% Sealing")+
  ylab("Number of ants")+
  xlab("Number of aphids")+
  theme(axis.text.x = element_text(size=9, vjust = 1, hjust=1),
        legend.title=element_text(size=10, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text.x = element_text(face="bold", size=10),
        strip.background = element_blank())+
  facet_wrap(~plot.simple)

