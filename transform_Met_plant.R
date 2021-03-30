###MET_PLANT###
#What is relevant?
#-->includes plantStade & parasitism info

###Make date information understandable for R:
Met_plant$date <- as.Date (Met_plant$date , format = "%d.%m.%Y")

###Simplify the plant stade info:
Met_plant$plantStade[(Met_plant$stade1==1)]<-1
Met_plant$plantStade[(Met_plant$stade2==1)]<-2
Met_plant$plantStade[(Met_plant$stade3==1)]<-3
Met_plant$plantStade[(Met_plant$stade4==1)]<-4
Met_plant<-Met_plant[,-c(8:11)]

###Convert %parasitism in proportion of parasitism (latter will be the response variable):
Met_plant$Prop_paras<-Met_plant$prop_paras / 100

## MAUD ADDITION : get the actual number of parasitised to fit a nice binomial:

# simplify the plot name:
library(stringr)
Met_plant$plot.simple = stringr::str_remove(Met_plant$plot, pattern = "_ext")
Met_plant$plot.simple = stringr::str_replace(Met_plant$plot.simple,
                                             pattern = "551", replacement = "55")
Met_plant$plot.simple = stringr::str_replace(Met_plant$plot.simple,
                                             pattern = "552", replacement = "55")


# add total number of aphid per plant:
Met_plant <-  merge(x = Met_plant, 
                    y = Aphid_density[,c("plot.simple","date","plant","N_aphid")],
                    by.x = c("plot.simple","date","plant"),
                    by.y = c("plot.simple","date","plant"),
                    all = TRUE) 

# calculate a rounded number for number of parasitised:
Met_plant$N_parasitised <- ceiling(Met_plant$N_aphid * Met_plant$Prop_paras)
Met_plant$N_not_parasitised<- Met_plant$N_aphid - Met_plant$N_parasitised

                                     