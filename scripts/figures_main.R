# Produce figures for article

########## MAIN TEXT ################

#1. VISUALIZE TEMPORAL DYNAMICS OF ANT ATTENDANCE

#create a variable to categorize date in begin/mid/end of August
Aphid_density$season<-NA
range(Aphid_density$date_s)#-1.629603  1.759559
Aphid_density[which(Aphid_density[,"date_s"]<= 0.63),"season"]<-"mid"
Aphid_density[which(Aphid_density[,"date_s"]< -0.5),"season"]<-"begin"
Aphid_density[which(Aphid_density[,"date_s"]>0.63),"season"]<-"end"

#Plot data:
pdf(file="figures/ant_aphid_nb.pdf",         # File name
    width = 5, height = 4, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk" )   # Color model (cmyk is required for most publications)


ggplot(Aphid_density, aes(y=N_aphid.mm, x=meanAnt.mean, color=season))+
  theme_minimal()+
  geom_point(size=1.75, aes(shape=season)) +
  scale_color_manual("", values=c("black", "black","#90A4ADFF"),
                     breaks=c("begin", "mid", "end"),
                     labels=c("Start of August", "Mid of August", "End of August"))+
  scale_shape_manual("", values=c(16,1,16),
                     breaks=c("begin", "mid", "end"),
                     labels=c("Start of August", "Mid of August", "End of August"))+
  geom_smooth(aes(linetype=season),method="lm",se=FALSE, size=0.8)+
  scale_linetype_manual("", values=c("solid", "dotted","solid"),
                        breaks=c("begin", "mid", "end"),
                        labels=c("Start of August", "Mid of August", "End of August"))+
  theme(legend.title=element_text(size=10, face="bold", color="gray21"),
        axis.title.y=element_text(size=10, face="bold", color="gray21"),
        axis.title.x=element_text(size=10, face="bold", color="gray21"),
        panel.grid.major = element_line(colour="lightgrey", linetype="dashed"), 
        panel.grid.minor = element_blank(),
        legend.position=c(0.88,0.95)) +
  xlab("Number of ants") +
  ylab("Aphid density (nb./mm)")

# close the graphical device:
dev.off() 

#2. VISUALIZE ANT AGGRESSIVITY
#Prepare data:

#calculate the number of aggressively reacting ants per sampling date and context
a<-Ant_aggressivity
a$ant<-1
plotAggressivity <- summaryBy(formula = reaction + ant ~ Seal_500 +
                                plot.simple + context,
                              data=a,
                              FUN=sum)

# calculate proportion of aggressive reactions
plotAggressivity$prop_reaction <-
  plotAggressivity$reaction.sum / plotAggressivity$ant.sum

# order by sealing
plotAggressivity <- plotAggressivity[
  order(plotAggressivity$Seal_500, plotAggressivity$context),]

# Extract predicted proportions from the fitted binomial model:
newd <- data.frame(
  unique(Ant_aggressivity[, c("Seal_500_s","Seal_500","context",
                              "date", "date_s","N_aphid_s")]),
  plantPop = NA,
  date = NA)

reaction.binom2<-glmer(reaction ~ context + date_s + N_aphid_s + Seal_500_s + 
                         context:date_s + context:N_aphid_s + context:Seal_500_s +
                         date_s:N_aphid_s + date_s:Seal_500_s +
                         N_aphid_s:Seal_500_s + 
                         (1|plantPop/date),
                       family = binomial,
                       data=Ant_aggressivity,
                       glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
newd <- data.frame(
  unique(Ant_aggressivity[, c("Seal_500_s","Seal_500","context",
                              "date", "date_s","N_aphid_s")]),
  plant = NA,
  plot.simple = NA,
  plantPop = NA,
  date = NA)

newd$predicted <- predict(reaction.binom2,
                          newdata=newd,
                          type = "response",re.form = NA)

#open graphical device:
#-->1.5 column width
pdf(file="figures/ant_aggressivity_predicted.pdf",         # File name
    width = 5, height = 4, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk" )   # Color model (cmyk is required for most publications)

#Produce plot based on predicted data:

ggplot(data = newd, mapping = aes(Seal_500, predicted)) +
  geom_point(size=1.75,aes(color=context)) +
  theme_minimal()+
  geom_smooth(aes(color=context), method = "glm", formula = y ~ x, 
              method.args=list(family="binomial"), se = FALSE) +
  scale_color_manual("Behavioural context", values=c("black", "#90A4ADFF"),
                     breaks=c("tending aphids", "other behaviour"),
                     labels=c("tending aphids", "other behaviour"))+
  theme(legend.title=element_text(size=10, face="bold", color="gray21"),
        axis.title.y=element_text(size=10, face="bold", color="gray21"),
        axis.title.x=element_text(size=10, face="bold", color="gray21"),
        panel.grid.major = element_line(colour="lightgrey", linetype="dashed"), 
        panel.grid.minor = element_blank(),
        legend.position=c(0.85,0.2)) +
  xlab("% Sealing") +
  ylab("Probability of aggressive reaction")+
  ylim(0,1)

# close the graphical device:
dev.off() 

########## SUPPLEMENTARY MATERIAL ################

###1. VISUALIZE APHID NUMBERS OVER TIME AT EACH STUDY SITE
#Aphid number over time including extinctions

#Prepare data:
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

#open graphical device:
#-->2 column width
pdf(file="figures/aphid_numbers.pdf",         # File name
    width = 7.2, height = 6, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk" )   # Color model (cmyk is required for most publications)

#create figure:

ggplot(Aphids, aes(x=date, fill=Seal_500)) +
  theme_minimal()+
  geom_point(aes(y=N_aphid,shape=plant), size=2, color="#90A4ADFF") +
  geom_line(aes(y=N_aphid, group=plant, color=Seal_500)) + #, color="#90A4ADFF")+
  scale_shape_manual("Aphid colony",values=c(21, 24, 23, 22, 25),
                     breaks=c("Alpha", "Beta", "Ceta", "Delta", "Gamma"))+
  scale_fill_viridis(option="mako", begin=0.1, end=0.9, direction=-1)+
  scale_color_viridis(option="mako", begin=0.1, end=0.9, direction=-1)+
  labs(fill="% Sealing", color="% Sealing")+
  ylab("Number of aphids")+
  xlab("Date")+
  theme(axis.text.x = element_text(angle = 45, size=7, vjust = 1, hjust=1),
        legend.title=element_text(size=10, face="bold", color="gray21"),
        axis.title.y=element_text(size=10, face="bold", color="gray21"),
        axis.title.x=element_text(size=10, face="bold", color="gray21"),
        panel.grid.major = element_line(colour="lightgrey", linetype="dashed"), 
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(face="bold", size=10, color="gray21"),
        strip.background = element_blank())+
  facet_wrap(~plot.simple)

# close the graphical device:
dev.off() 

###2.PLOT OF APHID DENSITIES & ANT NUMBERS ~ DATE ###################
#Prepare data:
newd<-as.data.frame(matrix(NA,nrow=0,ncol=4,dimnames=list(NULL,c("response","date","Seal_500","value"))))
aphids<-Aphid_density[,c("date","Seal_500","N_aphid.mm")]
aphids<-cbind(data.frame(response=rep("Aphid density\n(nb./mm)",nrow(Aphid_density))),aphids)
colnames(aphids)<-c("response","date","Seal_500","value")
ants<-Aphid_density[,c("date","Seal_500","meanAnt.mean")]
ants<-cbind(data.frame(response=rep("Ant number",nrow(Aphid_density))),ants)
colnames(ants)<-c("response","date","Seal_500","value")
newd<-rbind(newd,aphids,ants)
dummy<-data.frame(response="Aphid density\n(nb./mm)",date=Aphid_density[1,"date"],
                  Seal_500=Aphid_density[1,"Seal_500"],value=5)
#Plot the data:
newd$response<-factor(newd$response,levels=c("Aphid density\n(nb./mm)","Ant number"))
dummy$response<-factor(dummy$response,levels=c("Aphid density\n(nb./mm)","Ant number"))
#open graphical device:
#-->1.5 column width
pdf(file="figures/aphid_density3.pdf",         # File name
    width = 6.5, height = 3, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk" )   # Color model (cmyk is required for most publications)

ggplot(data = newd, aes(x=date, y=value, color = Seal_500)) +
  geom_smooth(method = "lm",color="gray21",se = FALSE,size=0.75) +
  geom_point(size=1.75, aes(color=Seal_500)) +
  geom_point(data=dummy,mapping=aes(x=date,y=value),color="white")+
  theme_minimal()+
  scale_color_viridis(option="mako", begin=0.1, end=0.9, direction=-1)+
  labs(color="% Sealing")+
  theme(axis.text.x = element_text(angle = 45, size=7, vjust = 1, hjust=1),
        legend.title=element_text(size=10, face="bold", color="gray21"),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size=10, face="bold", color="gray21"),
        panel.grid.major = element_line(colour="lightgrey", linetype="dashed"), 
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(face="bold", size=10, color="gray21"),
        strip.background = element_blank())+
  xlab("Date") +
  facet_wrap(~response,ncol=2,scales="free")
# close the graphical device:
dev.off() 

###3. TENDING TIME ~ DATE
#show real data together with model fit

tmp <-  na.omit(Tending_Time) # to be able to apply dredge later
Tend.betareg <- glmmTMB(aphid_IA.sum ~ date_s + N_aphid_s + Seal_500_s + 
                          date_s:N_aphid_s + date_s:Seal_500_s +
                          N_aphid_s:Seal_500_s +
                          (1|plantPop),
                        data= tmp,
                        family=beta_family)

newd <- data.frame(
  unique(Tending_Time[, c("Seal_500_s","Seal_500","plant",
                          "date", "date_s","N_aphid_s")]),
  plot.simple = NA,
  plantPop = NA,
  date = NA)

newd$predicted <- predict(Tend.betareg,
                          newdata=newd,
                          type = "response",re.form = NA)

#Open graphical device
pdf(file="figures/tending_time.pdf",         # File name
    width = 5, height = 4, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk" )   # Color model (cmyk is required for most publications)

ggplot(data = Tending_Time, aes(date, aphid_IA.sum)) +
  geom_point(size=1.75) +
  theme_minimal()+
  geom_smooth(data=newd, aes(x=date, y=predicted), 
              method = "glm", formula = y ~ x, 
              method.args=list(family="beta_family"), se = FALSE, color="black") +
  theme(legend.title=element_text(size=10, face="bold", color="gray21"),
        axis.title.y=element_text(size=10, face="bold", color="gray21"),
        axis.title.x=element_text(size=10, face="bold", color="gray21"),
        panel.grid.major = element_line(colour="lightgrey", linetype="dashed"), 
        panel.grid.minor = element_blank()) +
  xlab("Date") +
  ylab("Tending Time")

# close the graphical device:
dev.off() 

###4.VISUALIZE ANT BEHAVIOUR 
#Visualize the task allocation of ant colonies along the Seal_500-axis
#at plant level.

#Prepare data:
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

#obtain color palette that is consistent with other plots
show_col(viridis_pal(option="mako", begin=0, end=1, direction=-1)(15))

#open graphical device:
#-->2 column width
pdf(file="figures/ant_behaviour.pdf",         # File name
    width = 8.5, height = 4, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk" )   # Color model (cmyk is required for most publications)

#create figure:
ggplot(plotTasks, aes(fill=Behaviour, y=Time_prop, x=Seal_500))+ 
  theme_minimal()+
  geom_bar(position = "stack", stat="identity", width = 1.75, color="#90A4ADFF")+
  scale_fill_manual(values=c("gray80", "#37659EFF", "#8AD9B1FF",
                             "#40B7ADFF", "#3E3367FF"),
                    breaks=c("other behaviour",
                             "antinteraction", "standing",
                             "moving", "tending"),
                    labels=c("other behaviour", "ant interaction", "standing",
                             "moving", "tending aphids"))+
  ylab("Time proportion")+
  xlab("% Sealing")+
  theme(legend.title=element_text(size=10, face="bold", color="gray21"),
        axis.title.y=element_text(size=10, face="bold", color="gray21"),
        axis.title.x=element_text(size=10, face="bold", color="gray21"),
        panel.grid.major = element_line(colour="lightgrey", linetype="dashed"), 
        panel.grid.minor = element_blank())

# close the graphical device:
dev.off() 

###5. VISUALIZE PARASITISM IN RELATION TO SURVIVAL/EXTINCTION OF COLONY
#Use of the maximal proportion of parasitism per aphid colony

#Prepare data:
P4$survival<-as.character(P4$survival)
P4[which(P4[,"survival"]=="1"),"survival"]<-"survived"
P4[which(P4[,"survival"]=="0"),"survival"]<-"extinct"
P4[which(is.na(P4[,"survival"])==T),"survival"]<-"unknown\noutcome"
P4$survival<-factor(P4$survival,level=c("survived","extinct","unknown\noutcome"))
P4$is.paras<-factor(P4$is.paras, levels=c(1,0,NA))
P4[which(is.na(P4[,"prop_paras.cat"])==TRUE),"prop_paras.cat"]<-"no data"

#open graphical device:
#-->2 column width
pdf(file="figures/parasitism.pdf",         # File name
    width = 5, height = 4, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk" )   # Color model (cmyk is required for most publications)

ggplot(P4, aes(x=prop_paras.cat, fill=survival))+
  theme_minimal()+
  geom_bar(position="stack", width = 0.75, color="black")+
  scale_fill_manual("", values=c("white","#90A4ADFF","lightgrey"),
                    breaks=c("survived","extinct","unknown\noutcome"))+
  scale_y_continuous(breaks=c(1:25))+
  theme(axis.text.x=element_text(angle=20,face="bold", color="#90A4ADFF",size=11),
        axis.text.y=element_text(face="bold", color="#90A4ADFF"),
        axis.title.y=element_text(face="bold"),
        axis.title.x=element_text(face="bold"),
        panel.grid.major = element_line(colour="lightgrey", linetype="dashed"), 
        panel.grid.minor = element_blank())+
  ylab("Count")+
  xlab("Proportion of mummies")

# close the graphical device:
dev.off() 
##############################################################################
#Values for writing

#Temperature
mean(Ant_aggressivity$mean.temp)#22.66531
range(Ant_aggressivity$mean.temp)#15.36667 36.36667
ggplot(Ant_aggressivity, aes(x=plot.simple, y=mean.temp))+
  geom_boxplot()
#Aphid counts
sum(Aphid_density$N_aphid)#4001

#Ant counts
sum(Aphid_density$meanAnt.mean)#693.5473

#Ant-over-aphid ratio
range(Ant_attendance$AntperAphid.mean) #0.0553719 0.9625000
a<-Ant_attendance[which(Ant_attendance[,"N_aphid"]<15),]

Aphid_density[which(Aphid_density[,"date_s"]<= 0.63),"date"]
Aphid_density[which(Aphid_density[,"date_s"]< -0.5),"date"]
Aphid_density[which(Aphid_density[,"date_s"]>0.63),"date"]

#Tending time
mean(Tending_Time$aphid_IA.sum, na.rm=TRUE)

#Aggressivity
length(which(Ant_aggressivity[,"reaction"]==1))/332 #0.8283133
a<-Ant_aggressivity[which(Ant_aggressivity[,"context"]=="tending aphids"),]
length(which(a[,"reaction"]==1))/164#0.8963415
b<-Ant_aggressivity[which(Ant_aggressivity[,"context"]=="other behaviour"),]
length(which(b[,"reaction"]==1))/168#0.7619048

