###ACTIV_SEQ_RAW###

### Process raw data ####

# correct a few mistakes in the raw data: 
activ_seq_raw$states <- gsub(activ_seq_raw$states, pattern = "\\.", replacement = "-") # points instead of hyphens

activ_seq_raw$states <- gsub(activ_seq_raw$states, pattern = "- ", replacement = "-") # remove spaces after hyphens
activ_seq_raw$states <- gsub(activ_seq_raw$states, pattern = "--", replacement = "-") # remove double hyphens

activ_seq_raw$states <- gsub(activ_seq_raw$states, pattern = "-\\(", replacement = "_") # replace hypen + first parenthesis with underscore
activ_seq_raw$states <- gsub(activ_seq_raw$states, pattern = " \\(", replacement = "_") # replace space + first parenthesis with underscore
activ_seq_raw$states <- gsub(activ_seq_raw$states, pattern = "\\(", replacement = "_") # replace first parenthesis (no space) with underscore
activ_seq_raw$states <- gsub(activ_seq_raw$states, pattern = " ", replacement = "_") # replace leftover spaces with underscore

### Transform table in unique observations of activity for each individual ####
ind_activ = NULL
for (i in 1: dim(activ_seq_raw)[1]) {     # for each line in the raw data table
  
  # focus on the record number "i":
  obs <- activ_seq_raw[i,]
  
  # isolate each activity state recorded:
  rec <- strsplit(obs$states, split = "-")[[1]]
  
  # separate information about activity type (s) and length (p)
  s <- sapply(rec, FUN = function(x) strsplit(x, split = "_")[[1]][1])
  
  p <-   sapply(rec,FUN = function(x) strsplit(x, split = "_")[[1]][2])
  p <- gsub( pattern = 's)', replacement = '', p)  # remove parentheses and "s"           
      

  # create a data.frame of 5 columns for this record:
  out <- data.frame(record.name = as.character(obs$record.name),
                    record.period = as.numeric(obs$time.length.s.),
                    order = 1:length(s),
                    type = as.character(s),
                    time = as.numeric(p))
  
  # estimate the time length of activities when not recorded explicitly
  # we do it by distributing seconds uniformly amongst activities, 
  # based on the total length of the record : 
  # time.est = [total.length - sum(recorded.lenghts)] /nb.unrecorded.activities
  out$time.est <- out$time
  out$time.est[is.na(out$time.est)] <- unique(out$record.period -  sum(out$time, na.rm = T))/sum(is.na(out$time)) 
  
  # add the resulting data frame for this record to the final datatable:
  ind_activ <- rbind(ind_activ,out)
}

##############################

### Calculate proportion of time spent in each activity
#=time spent in some behavioural state (time.est) / cumulated record period*ant category*date*plot
ind_activ<-merge(ind_activ, Exp1[,c("record.name", "cum.record.period", "record.plant")], by="record.name", all=T)

ind_activ$prop.time <-  ind_activ$time.est / ind_activ$cum.record.period #at plot*plant*date level
ind_activ$prop.time.plant <-  ind_activ$time.est / ind_activ$record.plant #at plot*plant level
  

### total time devoted to each activity per observation/ant: ####
prop_time_activ <- tapply(X = ind_activ$prop.time, INDEX = list(ind_activ$record.name, ind_activ$type), sum, na.rm = T) 
prop_time_activ.plant <- tapply(X = ind_activ$prop.time.plant, INDEX = list(ind_activ$record.name, ind_activ$type), sum, na.rm = T) 

### Create summary table for each individual ant recorded ####
activ_sum <- unique(ind_activ[,c("record.name","record.period")])
activ_sum.plant <- unique(ind_activ[,c("record.name","record.period")])

# total number of time each 
activ_sum$nb.switch <-tapply(X = ind_activ$order, INDEX = list(ind_activ$record.name), max) -1

# Summed time proportions spent in different behaviour types 
#plot-plant-date level:
activ_sum$aphid_IA <- prop_time_activ[ ,"cl"] #tending aphids
activ_sum$move <- prop_time_activ[ ,"w"]#short walks
activ_sum$walk.flower<-prop_time_activ[ ,"wf"]#walking on flower
activ_sum$walk.leave<-prop_time_activ[ ,"exp"]#exploratory walk on leave
activ_sum$inactive <- prop_time_activ[ ,"st"] #standing
activ_sum$selfclean <- prop_time_activ[ ,"li"] #cleaning itself
activ_sum$eatpollen <- prop_time_activ[,"stf"] #eating pollen
activ_sum$stretching <- prop_time_activ[,"str"] #"stretching itself"
#activ_sum$troph<-prop_time_activ[,"k"]
activ_sum$ant_IA <- rowSums(prop_time_activ[ ,c("a","k")],na.rm=T) #interaction with other ant
activ_sum$uncategorized <- prop_time_activ[,"ow"] #uncategorized, extraordinary behaviour

#plot-plant level:
activ_sum.plant$aphid_IA <- prop_time_activ.plant[ ,"cl"] #tending aphids
activ_sum.plant$move <- prop_time_activ.plant[ ,"w"]#short walks
activ_sum.plant$walk.flower<-prop_time_activ.plant[ ,"wf"]#walking on flower
activ_sum.plant$walk.leave<-prop_time_activ.plant[ ,"exp"]#exploratory walk on leave
activ_sum.plant$inactive <- prop_time_activ.plant[ ,"st"] #standing
activ_sum.plant$selfclean <- prop_time_activ.plant[ ,"li"] #cleaning itself
activ_sum.plant$eatpollen <- prop_time_activ.plant[,"stf"] #eating pollen
activ_sum.plant$stretching <- prop_time_activ.plant[,"str"] #"stretching itself"
#activ_sum$troph<-prop_time_activ[,"k"]
activ_sum.plant$ant_IA <- rowSums(prop_time_activ.plant[ ,c("a","k")],na.rm=T) #interaction with other ant
activ_sum.plant$uncategorized <- prop_time_activ.plant[,"ow"] #uncategorized, extraordinary behaviour

###Transform NAs within the behavioural type columns into 0
# = NA indicates that ant did not spent time in this activity during record-->time fraction of 0
bhv<-c("aphid_IA","move","walk.flower","walk.leave", "inactive", "selfclean", "eatpollen","stretching",
       "ant_IA", "uncategorized")
for(i in 1:length(bhv)){
  activ_sum[which(is.na(activ_sum[,bhv[i]])==T), bhv[i]]<-0 
  activ_sum.plant[which(is.na(activ_sum.plant[,bhv[i]])==T), bhv[i]]<-0 }

