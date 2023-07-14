#### TEST TEROS ################################################################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")


### Fetch raw data ####

raw.df <- fetchTeros12(folderIn = "input_data_example/caxuana_stem_water_content",
                    fileOut = "data_raw/stem_water_content/raw_stem_water_content_2023-07-13.csv")


# label to id

labelToID.data <- readxl::read_excel("data_raw/cax_radar_metadata_caxiuana_12_05_2023.xlsx", 
                                       sheet = 1) %>%
  filter(!is.na(teros12_logger)) %>%
  mutate(label = paste0(teros12_logger, "_", teros12_port)) %>%
  select(ID, plot, species, size_class, label)


processed.df <- processTeros12(rawDataFile = "data_raw/stem_water_content/raw_stem_water_content_2023-07-13.csv",
                           rawData = NULL,
                           labelToIDFile = NULL,
                           labelToID = labelToID.data,
                           fileOut = "data_processed/stem_water_content/processed_stem_water_content_2023-07-14.csv")

plotTimeSeries(data = processed.df$processed_data,
               xVar = date,
               yVar = water_content_m3.m3,
               xLab = "date", 
               yLab = "water content (m3/m3)", 
               lineOrPoint = "line", 
               colorVar = ID)


