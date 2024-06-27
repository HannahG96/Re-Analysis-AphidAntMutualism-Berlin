###EXP2###
#What is relevant?
#->aphid number, ant number, ant attendance ratio, aphid density

###Transform date column in an object understandable for R:
Exp2$date <- as.Date(Exp2$date, format = "%d.%m.%Y")

###Calculate mean, max, min ant number*measurement
Exp2$meanAnt <- apply ( X = Exp2 [, c ( "t0","t1","t2","t3","t4","t5","t6","t7","t8","t9","t10","t11")],
                        MARGIN = 1,
                        FUN = mean, na.rm = TRUE)

###Calculate the mean ant attendance per measurement
Exp2$AntperAphid<-Exp2$meanAnt/Exp2$N_aphid

###Calculate the number of aphids*mm of the measured plant length=aphid density
Exp2$N_aphid.mm <- Exp2$N_aphid / Exp2$l
        #Note: in my BA-analysis I divided all aphid numbers by the mean focal length of 65.66038mm

###Produce a summary table of mean ant attendance, ant number, aphid number, aphid density *plot*plant*date

Exp2_plot_date <- summaryBy(formula = meanAnt + AntperAphid ~ plot.simple + plant + date + l + N_aphid + N_aphid.mm,
                                                                           data = Exp2, FUN = c(mean,sd), na.rm = TRUE)


