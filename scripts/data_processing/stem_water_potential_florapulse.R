#### STEM WATER POTENTIAL (FLORAPULSE) DATA PROCESSING  EXAMPLE ################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")

#### STEP 1: set the location of the original data to process and the files where we want the output to be stored ####

## raw_folder_in: is the complete path to the folder where the original data is stored. It is important to have all the files that we want
## to process together in the same folder (e.g., all the florapulse loggers files in the same folder).

## raw_file_out: location and name of the file where we want the unified raw data to be stored.

## processed_file_out: location and name of the file where we want the processed data to be stored.

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_potential/16-10-2023"
raw_file_out <- paste0("data_raw/stem_water_potential/raw_stem_water_potential_", Sys.Date(), ".csv")
processed_file_out <- paste0("data_processed/stem_water_potential/processed_stem_water_potential_", Sys.Date(), ".csv")

#### STEP 2: apply the functions to fetch the original data and process it ####

### Fetch raw data ####

raw.df <- fetchFlorapulse(folderIn = raw_folder_in,
                          fileOut = raw_file_out)

## We will select only average varaibles (mean water potential for the period of measurement)

# raw.df <- raw.df %>% 
  # filter(str_detect(label, "AVG"))

## Manual changes (if needed)
# raw.df[str_detect(raw.df$label, "255"), "label"] <- "B310_AVG"

# save
write_csv(raw.df, raw_file_out)

## Check that we have all sensors present

length(unique(raw.df$label)) # 20 sensors
non_allNAs <- raw.df %>% # Check whether there is any of the sensors with all NAs
  na.omit() %>%
  pull(label)
length(unique(non_allNAs)) # no sensors with all NAs


### Process raw data ####

## Load offset and multiplier values for each sensor and its corresponding tree ID from the metadata file

# No longer needed with SDI12
offsetMultiplier <- readxl::read_excel("data_raw/cax_radar_metadata_caxiuana_12_05_2023.xlsx",
                                       sheet = 1) %>%
  select(ID, plot, species, size_class, flora_pulse_sensor, flora_pulse_offset, flora_pulse_multiplier) %>%
  filter(!is.na(flora_pulse_sensor))

## Process sensor data to get water potential in MPa

processed.list <- processFlorapulse(rawDataFile = raw_file_out, 
                                  offsetMultiplier = offsetMultiplier,
                                  fileOut = processed_file_out)


length(unique(processed.list$processed_data$ID)) # Check that we still have 20 sensors
non_allNAs <- processed.list$processed_data %>% # Check whether there is any of the sensors with all NAs
  na.omit() %>%
  pull(ID)
length(unique(non_allNAs)) # We have one sensor with all NAs

## Some checkings to see whether we have all sensors

unique(processed.list$processed_data$ID)[which(!unique(processed.list$processed_data$ID) %in% unique(non_allNAs))]

unique(offsetMultiplier$ID)[which(!unique(offsetMultiplier$ID) %in% unique(non_allNAs))]

sensors <- unique(str_remove_all(raw.df$label, "_AVG"))
length(sensors)
sensors[which(!sensors %in% unique(offsetMultiplier$flora_pulse_sensor))]


## Observe a specific individual to check structure of data

processed.list$processed_data %>% 
  filter(ID == "TFE_116")

head(processed.list$processing_table)


#### STEP 3: data visualization ####

### All the individuals together ####

all.plot <- plotTimeSeries(data = processed.list$processed_data,
               xVar = timestamp,
               yVar = wp_bar,
               xLab = "time", 
               yLab = "WP (bar)", 
               lineOrPoint = "line", 
               colorVar = ID) #+ ylim(-20, 20)
all.plot

# Save the plot
pdf("outputs/data_plots/stem_water_potential/stem_water_potential_all.pdf")
all.plot
dev.off()


### Control individuals ####

control_data <- processed.list$processed_data %>% 
  filter(str_detect(ID, "Control"))

control.plot <- plotTimeSeries(data = control_data,
                    xVar = timestamp,
                    yVar = wp_bar,
                    xLab = "time", 
                    yLab = "WP (bar)", 
                    lineOrPoint = "line", 
                    colorVar = ID) #+ ylim(-5, 5)

# Save the plot
pdf("outputs/data_plots/stem_water_potential/stem_water_potential_control.pdf")
control.plot
dev.off()


### tfe individuals ####

tfe_data <- processed.list$processed_data %>% 
  filter(str_detect(ID, "TFE"))

tfe.plot <- plotTimeSeries(data = tfe_data,
               xVar = timestamp,
               yVar = wp_bar,
               xLab = "time", 
               yLab = "WP (bar)", 
               lineOrPoint = "line", 
               colorVar = ID) #+ ylim(-5, 5)

# Save the plot
pdf("outputs/data_plots/stem_water_potential/stem_water_potential_tfe.pdf")
tfe.plot
dev.off()


### Individuals one by one ####

for(ind in unique(processed.list$processed_data$ID)){
  
  ind_data <- processed.list$processed_data %>% filter(ID == ind)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/stem_water_potential/stem_water_potential_", ind, "_", str_replace(unique(ind_data$species), " ", "_"),".pdf"))
  ind.plot <- plotTimeSeries(data = ind_data,
                 xVar = timestamp,
                 yVar = wp_bar,
                 xLab = "time", 
                 yLab = "WP (bar)", 
                 lineOrPoint = "line", 
                 colorVar = ID)# + ylim(-5, 5)
  plot(ind.plot)
  dev.off()
}
