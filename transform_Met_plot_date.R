###MET_PLOT_DATE###
#What is relevant?
#-->includes temperature info

###Remove empty columns (=I did not fill them out):
Met_plot_date<-Met_plot_date[,-c(17,18)]

###Transform column date into date object for R:
Met_plot_date$date <- as.Date(Met_plot_date$date, format = "%d.%m.%Y")

###Calculate mean and SD temperature per plot*date:
Met_plot_date$mean.temp <-  apply(X = Met_plot_date [, c("temp1", "temp2", "temp3")], # our dataframe
      MARGIN  = 1,       # apply to each row (row = 1, column = 2)
      FUN = mean, na.rm = T)

Met_plot_date$sd.temp <-  apply(X = Met_plot_date [, c("temp1", "temp2", "temp3")], # our dataframe
                                  MARGIN  = 1,       # apply to each row (row = 1, column = 2)
                                  FUN = sd, na.rm = T)

