#MASTER SCRIPT
######################################################
#data-working directory:
datawd<-"C:/Hannah/Bachelorarbeit/BA Publication/Re-analysis/data"
#scripts-working directory:
scriptwd<-"C:/Hannah/Bachelorarbeit/BA Publication/Re-analysis/scripts"
#model results-working directory:
modelresultswd<-"C:/Hannah/Bachelorarbeit/BA Publication/Re-analysis/model results"

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


setwd(scriptwd)

#IMPORT DATA
source("import data.R")

setwd(scriptwd)

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
