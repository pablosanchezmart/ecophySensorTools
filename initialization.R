#### INITIALIZATION SENSOR DATA PROCESSING #####################################

## Pablo Sanchez Martinez
## 05/2023

remove(list = ls())


#### CREATE DIRECTIORES -------------------------------------------------------- ####

# root.dir <- c("C://Users//pablo//OneDrive - University of Edinburgh//postdoc_UoE//")
root.dir <- c("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/")

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
dir.create("data_processed/sapflow/complete_datasets", showWarnings = F)

dir.create("data_processed/met", showWarnings = F)
dir.create("data_processed/soil_moisture", showWarnings = F)
dir.create("data_processed/leaf_wp_wc", showWarnings = F)
dir.create("data_processed/leaf_wp_wc/complete_datasets", showWarnings = F)


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
dir.create("outputs/analysis/soil_moisture", showWarnings = F)

#### PACKAGES ------------------------------------------------------------------ ####

library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(hms)
library(TrEvol)
library(lme4)
library(lmerTest)
library(MuMIn)
library(interactions)
library(stringr)
library(data.table)
library(readxl)

#### PARAMETERS ---------------------------------------------------------------- ####

h <- 5
w <- 5


color_control <- "#1b9e77ff"
color_tfe <- "#d95f02ff"


#### MET AND SOIL VARIABLES NAMES ---------------------------------------------- ####

met_variables.names <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/caxiuana_variables_abbreviation.xlsx",
                                          sheet = "Torre PA") %>%
  select(abbreviation_raw, abbreviation_processing, abbreviation_processed) %>%
  na.omit()

tfe_met_variables.names <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/caxiuana_variables_abbreviation.xlsx",
                                          sheet = "Torre PB") %>%
  mutate(abbreviation_raw = tolower(abbreviation_raw)) %>%
  select(abbreviation_raw, abbreviation_processing, abbreviation_processed) %>%
  na.omit()

soil_control_variables.names <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/caxiuana_variables_abbreviation.xlsx",
                                                   sheet = "Solo 02 PA") %>%
  select(abbreviation_raw, abbreviation_processing, abbreviation_processed) %>%
  na.omit() %>%
  mutate(abbreviation_raw = tolower(abbreviation_raw),
         abbreviation_processing  = tolower(abbreviation_processing),
         abbreviation_processed = tolower(abbreviation_processed))

soil_control_variables_2024.names <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/caxiuana_variables_abbreviation.xlsx",
                                                   sheet = "solo 2024 PA") %>%
  select(abbreviation_raw, abbreviation_processing, abbreviation_processed) %>%
  na.omit() %>%
  mutate(abbreviation_raw = tolower(abbreviation_raw),
         abbreviation_processing  = tolower(abbreviation_processing),
         abbreviation_processed = tolower(abbreviation_processed))

soil_tfe_variables.names <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/caxiuana_variables_abbreviation.xlsx",
                                               sheet = "Solo PB") %>%
  select(abbreviation_raw, abbreviation_processing, abbreviation_processed) %>%
  na.omit() %>%
  mutate(abbreviation_raw = tolower(abbreviation_raw),
         abbreviation_processing  = tolower(abbreviation_processing),
         abbreviation_processed = tolower(abbreviation_processed))

### radar trees metadata ####

metadata <- readxl::read_excel(paste0("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data_processed/metadata_cax_radar/cax_radar_metadata_caxiuana_10_2023.xlsx"), sheet = 1)
