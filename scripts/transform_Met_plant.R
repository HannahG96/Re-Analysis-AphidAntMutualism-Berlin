###MET_PLANT###
#What is relevant?
#-->includes phenological stage & parasitism information

###Make date information understandable for R:
Met_plant$date <- as.Date (Met_plant$date , format = "%d.%m.%Y")

###Simplify the plant stade info:
Met_plant$plantStade[(Met_plant$stade1==1)]<-1
Met_plant$plantStade[(Met_plant$stade2==1)]<-2
Met_plant$plantStade[(Met_plant$stade3==1)]<-3
Met_plant$plantStade[(Met_plant$stade4==1)]<-4
Met_plant<-Met_plant[,-c(8:11)]

###Convert %parasitism in proportion of parasitism (latter will be the response variable):
#Met_plant$Prop_paras<-Met_plant$prop_paras / 100