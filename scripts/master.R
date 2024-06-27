# MASTER SCRIPT

# SET WORKING DIRECTORY ####
working_dir <- "C:/Hannah/Bachelorarbeit/BA Publication/Re-analysis-AphidAntMutualism-Berlin"
setwd(working_dir)

# LOAD PACKAGES ####
library(data.table)   # import data as data frame
library(doBy)         # calculate summary tables
library(dplyr)        # data transformation
library(ggplot2)      # produce nice figures
library(GGally)       # correlation of expl. variables (ggpair-plot)

library(lme4)       # mixed effect models
library(lmerTest)     # Satterthwaite's significance test for LMER models
library(MuMIn)      # calculate marginal and conditional Rsquared
library(r2glmm)     # partial Rsquared for fixed effects (LMMs)
#library(partR2)     # other R package for partial Rsquared for fixed effects (GLMMs)
library(glmmTMB)      # fit Beta regression GLMMs
library(DHARMa)       # package for testing residuals of model
library(broom.mixed)  # format the table of model coefficients and get CI
library(dotwhisker)   # represent the model coefficients with CI
library(xlsx)         # export data as excel file

library(viridis)      # color palette for figures
library(scales)       # obtain color palette that is consistent with other plots

#library(rgdal)        # GIS mapping - not available for newer versions of R
library(sf)           # spatial objects

#If glmmTMB is not working, due to R update: install old Matrix package version v1.6-2:
#require(devtools)
#install_version("Matrix", version = "1.6-2", repos = "http://cran.us.r-project.org")

# IMPORT DATA ####
source("scripts/import_data.R")

# FORMAT DATA ####
source("scripts/transform_General_plot.R")
source("scripts/transform_Met_plot_date.R")
source("scripts/transform_Met_plant.R")
source("scripts/transform_Exp1.R")
source("scripts/transform_Exp2.R")
source("scripts/transform_Exp3a.R")
source("scripts/transform_Exp3b.R")
source("scripts/myResponseVariables.R")

# ANALYSE DATA ####
source("scripts/models_main.R")
source("scripts/parasitism.R")
source("scripts/temperature_check.R")

# PRODUCE FIGURES ####
source("scripts/figures_main.R")
source("scripts/create_map.R") #Note: this script may cause issues because requires R package "rgdal", which does not work with newer R versions

