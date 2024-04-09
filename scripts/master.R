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
#library(partR2)     # partial Rsquared for fixed effects (GLMMs)????????????
library(glmmTMB)      # fit Beta regression GLMMs
library(DHARMa)       # package for testing residuals of model
library(broom.mixed)  # format the table of model coefficients and get CI
library(dotwhisker)   # represent the model coefficients with CI

library(viridis)      # color palette for figures
library(scales)       # obtain color palette that is consistent with other plots

#library(rgdal)        # GIS mapping
library(sf)           # spatial objects

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

# PRODUCE FIGURES ####
source("scripts/figures_main.R")
source("scripts/create_map.R")

