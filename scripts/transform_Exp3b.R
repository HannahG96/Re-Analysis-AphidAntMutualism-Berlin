###EXP3B###
#What is relevant?
#-->aggressivity score(0=avoidance, 1=biting, 2=jumping) & behavioural context (tending aphids/other behaviours)
#-->ants showing tolerant/explorative behaviours are excluded
#note: Nh-05/10.08.19-->no ant aggressivity was measured=no data available

###Remove empty columns
Exp3b<-Exp3b[,-c(15:20)]

###Make date information understandable for R
Exp3b$date <- as.Date (Exp3b$date , format = "%d.%m.%Y")

###Create a column specifying the behavioural context of the irritated ant
Exp3b$context<-NA
Exp3b[which(Exp3b[,"t-1_a"]==0),"context"]<-"other behaviour"
Exp3b[which(Exp3b[,"t-1_a"]==1),"context"]<-"tending aphids"
                #34 ants without specified behavioural context

###Fill gaps: ants with no specified behavioural context are classified in "other behaviour"
#-->start of fieldwork: specifying bhv context was not part of protocol, it became it only later
   #But I noted down when the ant was carrying an aphid + sometimes other behaviours (walking, standing)
   #If I did not specify behaviour=ant was NOT carrying an aphid
Exp3b[which(is.na(Exp3b[,"context"])==T),"context"]<-"other behaviour"

###Create a column indicating the aggressive score of the irritated ant
Exp3b$aggr_score<-NA
Exp3b[which(Exp3b[,"aggr"]==1),"aggr_score"]<-0 #avoidance
Exp3b[which(Exp3b[,"aggr"]==4),"aggr_score"]<-0 #avoidance=aggressive pose without approaching the needle
Exp3b[which(Exp3b[,"aggr"]>=5),"aggr_score"]<-1 #attack by bite
Exp3b[which(Exp3b[,"JUMP"]==1),"aggr_score"]<-2 #attack by jump

###Exclude all ants showing explorative(=3)/tolerant(=2) behaviours=artificial stimuli was not successful
OUTs<-Exp3b[which(is.na(Exp3b[,"aggr_score"])==T),]
Exp3b<-Exp3b[which(is.na(Exp3b[,"aggr_score"])==F),] #47 ants excluded



