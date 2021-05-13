#MASTER SCRIPT
######################################################

# If you are not using an Rproject :
working_dir <- "C:/Hannah/Bachelorarbeit/BA Publication/Re-analysis-AphidAntMutualism-Berlin"
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
library(lmerTest) #gives you p-values for LMER models via Satterthwaite's method
library(glmmTMB) # best package to fit the GLMMs
library(DHARMa)  # cool package for testing residuals of model
library(broom.mixed) # format the table of model coefficients and get CI
library(dotwhisker) # represent the model coefficients with CI
#library(bestNormalize) # find the best transformation for normalizing response variable

#IMPORT DATA
source("import data.R")

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
# source("models.R")

#FIGURES
