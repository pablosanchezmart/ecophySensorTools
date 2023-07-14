#### TEST FLORAPULSE ###################################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")


### Fetch raw data ####

raw.df <- fetchFlorapulse(folderIn = "input_data_example/caxuana_stem_water_potential",
                          fileOut = "data_raw/stem_water_potential/test.csv")

raw.df <- raw.df %>% filter(str_detect(label, "AVG"))

unique(raw.df$label)

### Process data ####

offsetMultiplier <- readxl::read_excel("data_raw/cax_radar_metadata_caxiuana_12_05_2023.xlsx", 
                                       sheet = 1) %>%
  select(ID, plot, species, size_class, flora_pulse_sensor, flora_pulse_offset, flora_pulse_multiplier) %>%
  filter(!is.na(flora_pulse_sensor))

processed.list <- processFlorapulse(rawDataFile = "data_raw/stem_water_potential/test.csv", 
                                  offsetMultiplier = offsetMultiplier,
                                  fileOut = "data_processed/wp_05_2023_processed.csv")

processed.list$processed_data %>% 
  filter(ID == "Control_211")

processed.list$processed_data %>% filter(ID == "Control_211")

all.plot <- plotTimeSeries(data = processed.list$processed_data,
               xVar = timestamp,
               yVar = wp_MPa,
               xLab = "time", 
               yLab = "WP (MPa)", 
               lineOrPoint = "line", 
               colorVar = ID)# + ylim(-5, 5)
all.plot
