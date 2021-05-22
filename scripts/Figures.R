# Figures for article
library("ggsci")
library(interactions)

#####################################################
###VISUALIZE INTERACTION OF APHID DENSITY WITH SEALING
# Interaction plot of partial residuals:

#Fit the model with square-root transformation to obtain partial residuals:
Aph<-lmer(sqrt(N_aphid.mm) ~ date_s + meanAnt.mean_s + Seal_500_s +
            date_s:meanAnt.mean_s +
            date_s:Seal_500_s +
            meanAnt.mean_s:Seal_500_s + 
            (1|plantPop),
          data=Aphid_density)

interactions::interact_plot(Aph,
                            pred = date_s, 
                            modx = Seal_500_s,
                            legend.main = "% Sealing",
                            plot.points = TRUE , 
                            interval = TRUE, 
                            robust = TRUE,
                            int.type = "confidence",
                            x.label = "Date (standardized)",
                            y.label = "Aphid density (nb./mm)",
                            colors = "blue",
                            point.size = 1,
                            line.thickness = 0.8,
                            vary.lty = FALSE,
                            partial.residuals = FALSE,
                            facet.modx = TRUE,
                            modx.labels = c("- 1SD (= 7%)",
                                            "Mean %Sealing (= 25%)",
                                            "+ 1SD (= 42%)")
)

# Check out the sealing values for legend:
paste(c(round(mean(Aphid_density$Seal_500)-
                        sd(Aphid_density$Seal_500)),
                round(mean(Aphid_density$Seal_500)),
                round(mean(Aphid_density$Seal_500)+
                        sd(Aphid_density$Seal_500))),
      "%", sep = "")


######################################################################
# VISUALIZE ANT AGGRESSIVITY #####

#Prepare data:
#-->proportion of aggressive reacting ants
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


# Extract predicted proportions from the fitted binomial model:

reaction.binom<-glmmTMB(reaction ~ context + date_s + N_aphid_s + Seal_500_s +
                          context:date_s + context:N_aphid_s + context:Seal_500_s +
                          date_s:N_aphid_s + date_s:Seal_500_s +
                          N_aphid_s:Seal_500_s +
                          (1|plantPop/date),
                        family = binomial,
                        data=Ant_aggressivity)
newd <- data.frame(
  unique(Ant_aggressivity[, c("Seal_500_s","Seal_500","context",
                              "date", "date_s","N_aphid_s")]),
  plant = NA,
  plot.simple = NA,
  plantPop = NA,
  date = NA)

newd$predicted <- predict(reaction.binom,
                          newdata=newd,
                          type = "response",re.form = NA)

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
        panel.grid.minor = element_blank() ) +
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
  theme_minimal()+
  geom_jitter(size=1.75,aes(color=context)) +
  geom_smooth(data=newd, aes(x=Seal_500, y=predicted, color=context), 
              method = "glm", formula = y ~ x, 
              method.args=list(family="binomial"), se = FALSE) +
  scale_color_manual("Behavioural context", values=c("black", "#90A4ADFF"),
                     breaks=c("tending aphids", "other behaviour"),
                     labels=c("tending aphids", "other behaviour"))+
  theme(legend.title=element_text(size=10, face="bold", color="gray21"),
        axis.title.y=element_text(size=10, face="bold", color="gray21"),
        axis.title.x=element_text(size=10, face="bold", color="gray21"),
        panel.grid.major = element_line(colour="lightgrey", linetype="dashed"), 
        panel.grid.minor = element_blank() ) +
  xlab("% Sealing") +
  ylab("Probability of aggressive reaction")

############################################################################
###VISUALIZE ANT BEHAVIOUR (Appendix)
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



pal_material("indigo")(30)
ggplot(plotTasks, aes(fill=Behaviour, y=Time_prop, x=Seal_500))+ 
  theme_minimal()+
  geom_bar(position = "stack", stat="identity", width = 1.75, color="#90A4ADFF")+
  scale_fill_manual(values=c("seashell3", "#19227EFF", "#E7EAF6FF",
                             "#9FA7D9FF", "#273492FF"),
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

#############################################################################################
######VISUALIZE APHID NUMBER (Appendix)
#Aphid number over time including extinctions

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
  theme_minimal()+
  geom_point(aes(y=N_aphid,shape=plant), size=2, color="#90A4ADFF") +
  geom_line(aes(y=N_aphid, group=plant), color="#90A4ADFF")+
  scale_shape_manual("Aphid colony",values=c(21, 24, 23, 22, 25),
                     breaks=c("Alpha", "Beta", "Ceta", "Delta", "Gamma"))+
  scale_fill_material("indigo")+
  labs(fill="% Sealing")+
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


######################################################################################
#ant attendance (Probably not relevant)
Ant_attendance$plot.simple<-factor(Ant_attendance$plot.simple, levels=c("Ol-11","Ol-55", "Nh-04", "Nh-05",
                                                                        "Om-02", "Nl-55", "Nh-10", "Oh-02",
                                                                        "Nl-200"))

ggplot(Ant_attendance, aes(x=date, fill=Seal_500)) +
  theme_minimal()+
  geom_point(aes(y=meanAnt.mean,shape=plant), size=2, color="#90A4ADFF") +
  geom_line(aes(y=meanAnt.mean, group=plant), color="#90A4ADFF")+
  scale_shape_manual("Aphid colony",values=c(21, 24, 23, 22, 25),
                     breaks=c("Alpha", "Beta", "Ceta", "Delta", "Gamma"))+
  scale_fill_material("indigo")+
  labs(fill="% Sealing")+
  ylab("Number of ants")+
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

