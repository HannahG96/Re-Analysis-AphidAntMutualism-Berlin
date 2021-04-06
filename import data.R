#IMPORT DATA

###Set working directory:
#setwd(datawd)

###Load data files:
#plot metadata
General_plot <-fread(file = "data/General_plot.csv", na.strings = "kA", dec = "," , data.table = FALSE)
Met_plot_date<-fread(file="data/Met_plot_date.csv",na.strings ="kA", dec = ",", data.table = FALSE)
Met_plant <- fread (file = "data/Met_plant.csv", na.strings = "kA", dec = ",", data.table = FALSE)

#individual behaviour
Exp1 <-fread(file = "data/Exp1.csv", na.strings = "kA", dec = ",", data.table = FALSE)

#ant / aphid numbers
Exp2 <-fread(file = "data/Exp2.csv", na.strings = "kA", dec = "," , data.table = FALSE)

#group aggressivity
Exp3a <-fread(file = "data/Exp3a.csv", na.strings = "kA", dec = "," , data.table = FALSE)

#individual aggressivity
Exp3b <-fread(file = "data/Exp3b.csv", na.strings = "kA", dec = "," , data.table = FALSE)

#behavioural sequences
activ_seq_raw <-fread(file="data/activity sequence.csv", data.table = FALSE, check.names = TRUE)
  #correct some typing errors that I noted now:
  activ_seq_raw[which(activ_seq_raw[,1]=="E43"),1]<-"E44"#record name as which it appears in Exp1 table
  activ_seq_raw[which(activ_seq_raw[,1]=="E5"),1]<-"E4" #record name as which it appears in Exp1 table

###Add column with simplified plot names
data<-list(General_plot, Met_plot_date, Met_plant, Exp1, Exp2, Exp3a, Exp3b)
data_names<-c("General_plot", "Met_plot_date", "Met_plant", "Exp1", "Exp2", "Exp3a", "Exp3b")
for(i in 1:7){
data[[i]][,ncol(data[[i]])+1] <-  sapply(data[[i]][,"plot"], FUN = function(x) {
  strsplit(x, split  ="_")[[1]][1] })
data[[i]]<-data[[i]][,c(ncol(data[[i]]), 2:ncol(data[[i]])-1)]
colnames(data[[i]])[1]<-"plot.simple"
assign(data_names[i], data[[i]])}

###Load the data about the plots given by CityScapeLabs:
GivenMeta_plots <-fread(file="data/GivenMeta_plots.csv", na.strings = "kA", dec = ",", data.table = FALSE)
###Extract relevant variables from the data
MyGivenMeta_plots <- GivenMeta_plots[c(9,10,11,21,23,40,48,50,51),
                                     c("ID_plot","Long","Lat","Age","Seal_500","Pop_500")]
###Format Seal_500 data for merging with my data files:
colnames(MyGivenMeta_plots)[1] <- "plot.simple" #column name
MyGivenMeta_plots$plot.simple <- sub("_","-",MyGivenMeta_plots$plot.simple ) # change "_"in "-"


