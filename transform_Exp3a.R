###EXP3A###
#What is relevant?
#->group protective behaviour of ants = max. group reaction out of 3 stimuli *plot*date
                # attacks(=jumps+bites) & total nb of ants with altered behaviour after stimulus 


###Make date information understandable for R
Exp3a$date <- as.Date (Exp3a$date , format = "%d.%m.%Y")

###Correct a typing error:
Exp3a[46,7]<-10

###Calculate the total number of of ants with altered behaviour per stimulus (attacks+hyperactivity)
#=total number of ants present - nb of ants showing no reaction
for(i in 1:3){
  Exp3a[,ncol(Exp3a)+1]<-Exp3a[,i+4] - Exp3a[,i+32]
  colnames(Exp3a)[ncol(Exp3a)]<-paste("TOTREACT",i, sep="")}

#Calculate the total number of attacks per stimulus
#= nb of jumps + nb of bites
for(i in 1:3){
  Exp3a[,ncol(Exp3a)+1]<-Exp3a[,i+16] + Exp3a[,i+20]
  colnames(Exp3a)[ncol(Exp3a)]<-paste("ATTACKS",i, sep="")}

###Identify the stimuli that triggered the most intense group reaction and store corresponding 
#"TOTREACT", "N_ant"(number of ants present) and "ATTACKS":
Exp3a$MAXREACT<-NA
Exp3a$N_ant<-NA
Exp3a$MAXATTACKS<-NA

for(i in 2:nrow(Exp3a)){ 
  max<-max(Exp3a[i,c(38:40)])
  stim<-which(Exp3a[i,c(38:40)]==max)
if(length(stim)>1){ ##more than 1 stimuli triggered the maximal reaction-->pick stimuli based on max nb of attacks
  max<-max(Exp3a[i,c(41:43)]) 
  stim<-which(Exp3a[i,c(41:43)]==max)
  Exp3a[i,"MAXREACT"]<-Exp3a[i,40+stim-3]
  Exp3a[i,"N_ant"]<-Exp3a[i, 4+stim]
  Exp3a[i,"MAXATTACKS"]<-Exp3a[i,40+stim]}
else{
  Exp3a[i,"MAXREACT"]<-Exp3a[i,37+stim]
  Exp3a[i,"N_ant"]<-Exp3a[i, 4+stim]
  Exp3a[i,"MAXATTACKS"]<-Exp3a[i,37+stim+3]}}
             #Error warning: if more than 1 stimuli triggered same max. nb of reactive ants+attacks-->pick any of those stimuli


#Calculate the proportion of reactive ants:
Exp3a$MAXREACT_prop<-Exp3a$MAXREACT / Exp3a$N_ant
