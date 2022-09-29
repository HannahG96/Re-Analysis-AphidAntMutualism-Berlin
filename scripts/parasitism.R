#EXPLORE PARASITISM DATA FOR FLO

#1.ARE PARASITIZED APHID COLONIES MORE PRONE TO EXTINCTIONS?
#2.DOES THE LIKELIHOOD OF SURVIVAL OF PARASITIZED APHID COLONIES CHANGE ALONG THE URBAN GRADIENT?
#3.ARE APHID COLONIES WITH MORE THEN 10% OF MUMMIES EXPOSED TO EXTINCTION?

#Note:
#-->7 obs excluded due to lack of info about survival/extinction (these systems were sampled for the 1st time on the last visit of the plot)
#-->1 obs excluded due to lack of proportion of parasitism data (Nh_04 Alpha-->first system studied)
#=Total of only 21 obs

#load packages
library("data.table")#import data as data frame
library("doBy")#calculate summary tables
library("ggplot2")#produce nice figures
library("writexl")#export data frame to excel
#set working directory
working_dir <- "C:/Hannah/Bachelorarbeit/BA Publication/Re-analysis-AphidAntMutualism-Berlin"
setwd(working_dir)

##################################
#IMPORT AND PREPARE DATA
#-->Df indicating for each aphid colony: (i)maximal proportion of mummies, 
#                                        (ii)colony dynamics (extinction/persistence/unknown outcome) 

#Extinctions of aphid colonies
P1<-fread(file = "data/aphid_extinctions.csv", na.strings = "kA", dec = "," , data.table = FALSE)

#Max proportion of parasitism of each colony
source("scripts/import data.R")
source("scripts/transform_Met_plant.R")
P2<-as.data.frame(matrix(NA,nrow=0,ncol=3,dimnames=list(NULL,c("prop_paras","plot.simple","plant"))))
Met_plant$ID<-paste(Met_plant$plot.simple, Met_plant$plant)
x<-unique(Met_plant$ID)
for(i in 1:length(x)){
  myplant<-x[i]
  myplant.rows<-which(Met_plant[,"ID"]==myplant)
  select.maxparas<-which(Met_plant[myplant.rows,"prop_paras"]==max(Met_plant[myplant.rows,"prop_paras"]))[1]
  if(is.na(select.maxparas)==T)select.maxparas<-myplant.rows
  myrow<-Met_plant[myplant.rows[select.maxparas], c("plot.simple","plant","prop_paras")]
  P2<-rbind(P2,myrow)}

#Max proportion of parasitism as binary variable: <=10%; >10%
P2$prop_paras.cat<-NA
P2[which(P2[,"prop_paras"]<=10),"prop_paras.cat"]<-"<=10%"
P2[which(P2[,"prop_paras"]>10),"prop_paras.cat"]<-">10%"

#Last sampled system (as row vector) of each plot 
#(=unknown whether colony survived or died)
p3<-vector()
for(i in 1:length(unique(Met_plant$plot.simple))){
  myplot<-unique(Met_plant$plot.simple)[i]
  myobs<-which(Met_plant[,"plot.simple"]==myplot)
  max<-which(Met_plant[myobs,"date"]==max(Met_plant[myobs,"date"]))
  add.row<-myobs[max]
  p3<-c(p3,add.row)}
unknowns<-c()#=all study systems that we only sampled once on the last visit-->snapshots
for(i in 1:length(p3)){
  myplant<-Met_plant[p3[i],"ID"]
  if(length(which(Met_plant[,"ID"]==myplant))==1)unknowns<-c(unknowns,myplant)}

#Merge all information:
source("scripts/transform_General_plot.R")
P4<-merge(P1,P2,by=c("plot.simple","plant"),all=T)
P4$ID<-paste(P4$plot.simple,P4$plant)
P4[which(is.na(P4[,"N_aphid"])==T),"N_aphid"]<-1
colnames(P4)[which(colnames(P4)=="N_aphid")]<-"survival"
for(i in 1:length(unknowns)){
  myplant<-unknowns[i]
  P4[which(P4$ID==myplant),"survival"]<-NA}
P4<-merge(P4,field.summary[,c("plot.simple","Seal_500")],by="plot.simple")
P4$Seal_500<-as.numeric(P4$Seal_500)
P4$is.paras<-0
P4[which(P4[,"prop_paras"]>0),"is.paras"]<-1
P4[which(is.na(P4[,"prop_paras"])==T),"is.paras"]<-NA

######################################################
#CHISQUARED TESTS

#Produce data tables for chisquared tests

#########
#1.Effect of parasitism on extinctions
#Nb of parasitized (p) colonies that survived (1):
length(which(P4[which(P4[,"is.paras"]==1),"survival"]==1))#3
#Nb of parasitized (p) colonies that died (0):
length(which(P4[which(P4[,"is.paras"]==1),"survival"]==0))#6
#Nb of parasitized (p) colonies where survival is unknown (=excluded obs):
length(which(is.na(P4[which(P4[,"is.paras"]==1),"survival"])==T))#3

#Nb of healthy (h) colonies that survived (1):
length(which(P4[which(P4[,"is.paras"]==0),"survival"]==1))#7
#Nb of healthy (h) colonies that died (0):
length(which(P4[which(P4[,"is.paras"]==0),"survival"]==0))#5
#Nb of healthy (h) colonies where survival is unknown (=excluded obs):
length(which(is.na(P4[which(P4[,"is.paras"]==0),"survival"])==T))#4

#Data vector for chisq
X1<-c(p1=3,p0=6,h1=7,h0=5)

#Test
chisq.test(X1) #X-squared = 1.6667, df = 3, p-value = 0.6444
#-->UNSIGNIFICANT

#########
#2.Effect of urbanisation on survival/extinction of parasitized colonies:
P5<-P4[which(P4[,"is.paras"]==1),]#Df with only parasitized aphid colonies

#LOW-MED (<20%): Ol_11; Ol_55; Nh_04; Nh_05; Om_02 (r)
length(which(P5[which(P5[,"Seal_500"]<20), "survival"]==1))#1 survived (1)
length(which(P5[which(P5[,"Seal_500"]<20), "survival"]==0))#4 died (0)
length(which(is.na(P5[which(P5[,"Seal_500"]<20), "survival"])==T))#2 unknown outcome (=excluded obs)

#MED-HIGH (>30% to 60%): Nl_55; Nh_10; Oh_02; Nl_200 (u)
length(which(P5[which(P5[,"Seal_500"]>30), "survival"]==1))#2 survived (1)
length(which(P5[which(P5[,"Seal_500"]>30), "survival"]==0))#2 died (0)
length(which(is.na(P5[which(P5[,"Seal_500"]>30), "survival"])==T))#1 unknown outcome (=excluded obs)

#Data vector for chisq
X2<-c(r1=1,r0=4,u1=2,u0=2)

#Test
chisq.test(X2) #X-squared = 2.1111, df = 3, p-value = 0.5497-->WARNING: In chisq.test(X2) : Chi-squared approximation may be incorrect
#-->UNSIGNIFICANT

#########
#3.Effect of the quantity of mummies (<=10%; >10%) on the faith of (parasitized?) aphid colonies:
#Nb of <=10%-parasitized (p0) colonies that survived (1):
length(which(P4[which(P4[,"prop_paras.cat"]=="<=10%"),"survival"]==1))#10
#Nb of <=10%-parasitized (p0) colonies that died (0):
length(which(P4[which(P4[,"prop_paras.cat"]=="<=10%"),"survival"]==0))#5
#Nb of <=10%-parasitized colonies where survival is unknown (=excluded obs):
length(which(is.na(P4[which(P4[,"prop_paras.cat"]=="<=10%"),"survival"])==T))#6

#Nb of >10%-parasitized (p1) colonies that survived (1):
length(which(P4[which(P4[,"prop_paras.cat"]==">10%"),"survival"]==1))#0
#Nb of >10%-parasitized (p1) colonies that died (0):
length(which(P4[which(P4[,"prop_paras.cat"]==">10%"),"survival"]==0))#6
#Nb of >10%-parasitized colonies where survival is unknown (=excluded obs):
length(which(is.na(P4[which(P4[,"prop_paras.cat"]==">10%"),"survival"])==T))#1

#Data vector for chisq
X3<-c(p01=10,p00=5,p11=0,p10=6)

#Test
chisq.test(X3)#X-squared = 9.6667, df = 3, p-value = 0.02162
#-->SIGNIFICANT

##########################################################
#FIGURES
P4$survival<-as.character(P4$survival)
P4[which(P4[,"survival"]=="1"),"survival"]<-"survived"
P4[which(P4[,"survival"]=="0"),"survival"]<-"extinct"
P4[which(is.na(P4[,"survival"])==T),"survival"]<-"unknown\noutcome"
P4$survival<-factor(P4$survival,level=c("survived","extinct","unknown\noutcome"))
P4$is.paras<-factor(P4$is.paras, levels=c(1,0,NA))

#Figure 1: 
ggplot(P4, aes(x=survival, fill=is.paras))+
  theme_minimal()+
  geom_bar(position="stack", width = 0.75, color="#90A4ADFF")+
  scale_fill_manual("", values=c("navy","darkgoldenrod","lightgrey"),
                    breaks=c(1,0,NA),labels=c("parasitized","healthy","no available data"))+
  scale_y_continuous(breaks=c(1:15))+
  theme(axis.text.x=element_text(angle=20,face="bold", color="#90A4ADFF",size=11),
        axis.title.x=element_blank(),
        axis.text.y=element_text(face="bold", color="#90A4ADFF"),
        axis.title.y=element_text(face="bold"),
        title=element_text(face="bold", size=8))+
  ggtitle("Population dynamics of the 29 studied aphid colonies")

  
  
#Figure 2: 
P5<-P4[which(P4[,"is.paras"]==1),]
P5$Seal_level<-P5$Seal_500
P5[which(P5[,"Seal_500"]<20),"Seal_level"]<-"low sealing\n(<20%)"
P5[which(P5[,"Seal_500"]>30),"Seal_level"]<-"medium-high sealing\n(30-60%)"
ggplot(P5, aes(x=Seal_level, fill=survival))+
  theme_minimal()+
  geom_bar(position="stack", width = 0.75, color="#90A4ADFF")+
  scale_fill_manual("", values=c("cadetblue","burlywood4","lightgrey"),
                    breaks=c("survived","extinct","unknown\noutcome"))+
  scale_y_continuous(breaks=c(1:15))+
  theme(axis.text.x=element_text(angle=20,face="bold", color="#90A4ADFF",size=11),
        axis.title.x=element_blank(),
        axis.text.y=element_text(face="bold", color="#90A4ADFF"),
        axis.title.y=element_text(face="bold"),
        title=element_text(face="bold", size=8))+
  ggtitle("Population dynamics of the 12\nparasitized aphid colonies for two levels of sealing")

#Produce Figure3
P4[which(is.na(P4[,"prop_paras.cat"])==TRUE),"prop_paras.cat"]<-"no data"
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
##################################
#EXPORT PARASITISM DATA
parasitism.data<-Met_plant[,c("plot.simple","plant","date","prop_paras")]
colnames(parasitism.data)<-c("plot.simple","plant","date","%parasitism")
parasitism.data<-merge(parasitism.data, P4[,c("plot.simple","plant",
                                              "survival","prop_paras","Seal_500")], by=c("plot.simple","plant"))
colnames(parasitism.data)[6]<-"Maximum.%parasitism.per.plant"
write_xlsx(parasitism.data,"C:\\Hannah\\Bachelorarbeit\\BA Publication\\Text\\parasitism_data.xlsx")
