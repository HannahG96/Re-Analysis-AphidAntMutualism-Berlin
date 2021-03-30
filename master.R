#MASTER SCRIPT
######################################################
# #data-working directory:
# datawd<-"C:/Hannah/Bachelorarbeit/BA Publication/Re-analysis/data"
# #scripts-working directory:
# scriptwd<-"C:/Hannah/Bachelorarbeit/BA Publication/Re-analysis/scripts"
# #model results-working directory:
# modelresultswd<-"C:/Hannah/Bachelorarbeit/BA Publication/Re-analysis/model results"

# You only need one working directory :) 
# Set all the paths relative to this one in your scripts. this is good practice as it means you can transport your directory anywhere, or share it with anyone, and all the relative paths will still work. 
# Even better: create an Rstudio project from the repository, then you don't need to set the WD at all! It is automatically set as the root directory of the project (ie of your github repository). That's what i always do.
working_dir <- "C:/Hannah/Bachelorarbeit/BA Publication/Re-analysis"
setwd(working_dir)


#LOAD PACKAGES
library("data.table")#import data as data frame
library("doBy")#calculate summary tables
library("ggplot2")#produce nice figures
library("varhandle")#remove all objects except...
library("lme4")#mixed effect models
library("GGally")#correlation of expl. variables (ggpair-plot)
library("dplyr")#standardize explanatory variables
library("arm")#nice visualization of model summary
library("MuMIn")#calculate marginal and conditional Rsquared
library("r2glmm")#partial Rsquared for fixed effects
library("xlsx")#export R-tables to excel
library("ggnewscale")#extended color scales in ggplot

# added by maud:
library(stringr) # for all types of handy string modifications
library(lmerTest) # For chi-square tests on LMER
library(glmmTMB)
library(DHARMa)
library(broom.mixed)
library(dotwhisker)



#setwd(scriptwd)

#IMPORT DATA
source("import data.R")

#setwd(scriptwd)

#FORMAT DATA
source("transform_General_plot.R")
source("transform_Met_plot_date.R")
source("transform_Met_plant.R")
source("transform_Exp1.R")
source("transform_Exp2.R")
source("transform_Exp3a.R")
source("transform_Exp3b.R")
source("myResponseVariables.R")

#ANALYSE DATA
source("models.R")

#FIGURES
