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

dir.create("data_raw/", showWarnings = F)
dir.create("data_raw/stem_water_potential", showWarnings = F)
dir.create("data_raw/stem_water_content", showWarnings = F)
dir.create("data_raw/sap_flow", showWarnings = F)
dir.create("data_raw/met", showWarnings = F)

dir.create("data_processed/stem_water_potential", showWarnings = F)
dir.create("data_processed/stem_water_content", showWarnings = F)
dir.create("data_processed/sapflow", showWarnings = F)
dir.create("data_processed/met", showWarnings = F)


dir.create("outputs/", showWarnings = F)
dir.create("outputs/data_plots", showWarnings = F)
dir.create("outputs/data_plots/stem_water_potential", showWarnings = F)
dir.create("outputs/data_plots/stem_water_content", showWarnings = F)
dir.create("outputs/data_plots/sapflow", showWarnings = F)
dir.create("outputs/data_plots/increment", showWarnings = F)
dir.create("outputs/data_plots/met", showWarnings = F)


#### PACKAGES ------------------------------------------------------------------ ####

library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(lubridate)


#### PARAMETERS ---------------------------------------------------------------- ####

h <- 5
w <- 5

