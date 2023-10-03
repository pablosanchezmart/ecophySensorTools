#### INITIALIZATION SENSOR DATA PROCESSING #####################################

## Pablo Sanchez Martinez
## 05/2023

remove(list = ls())


#### CREATE DIRECTIORES -------------------------------------------------------- ####

root.dir <- c("C://Users//pablo//OneDrive - University of Edinburgh//postdoc_UoE//")

dir.create("scripts/", showWarnings = F)
dir.create("scripts/functions", showWarnings = F)
dir.create("scripts/tests", showWarnings = F)
dir.create("scripts/plotting", showWarnings = F)
dir.create("scripts/data_processing", showWarnings = F)
dir.create("scripts/analysis", showWarnings = F)

dir.create("data_raw/", showWarnings = F)
dir.create("data_raw/stem_water_potential", showWarnings = F)
dir.create("data_raw/stem_water_content", showWarnings = F)
dir.create("data_raw/sap_flow", showWarnings = F)
dir.create("data_raw/met", showWarnings = F)

dir.create("data_processed/stem_water_potential", showWarnings = F)
dir.create("data_processed/stem_water_content", showWarnings = F)
dir.create("data_processed/sapflow", showWarnings = F)
dir.create("data_processed/met", showWarnings = F)
dir.create("data_processed/soil_moisture", showWarnings = F)


dir.create("outputs/", showWarnings = F)
dir.create("outputs/data_plots", showWarnings = F)
dir.create("outputs/data_plots/stem_water_potential", showWarnings = F)
dir.create("outputs/data_plots/stem_water_content", showWarnings = F)
dir.create("outputs/data_plots/sapflow", showWarnings = F)
dir.create("outputs/data_plots/increment", showWarnings = F)
dir.create("outputs/data_plots/met", showWarnings = F)
dir.create("outputs/data_plots/soil", showWarnings = F)

dir.create("outputs/analysis", showWarnings = F)
dir.create("outputs/analysis/stem_water_content_met", showWarnings = F)
dir.create("outputs/analysis/sapflow_met", showWarnings = F)

#### PACKAGES ------------------------------------------------------------------ ####

library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(TrEvol)
library(lme4)
library(lmerTest)
library(MuMIn)
library(interactions)
library(stringr)

#### PARAMETERS ---------------------------------------------------------------- ####

h <- 5
w <- 5


color_control <- "#1b9e77ff"
color_tfe <- "#d95f02ff"

