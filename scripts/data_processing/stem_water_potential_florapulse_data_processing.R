#### STEM WATER POTENTIAL (FLORAPULSE) DATA PROCESSING  EXAMPLE ################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")

#### COLLECTED 29-09-2023 ------------------------------------------------------ ####

#### STEP 1: set the location of the original data to process and the files where we want the output to be stored ####

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_potential/29-09-2023"
raw_file_out <- paste0("data_raw/stem_water_potential/raw_stem_water_potential_2023_09_29.csv")
processed_file_out <- paste0("data_processed/stem_water_potential/processed_stem_water_potential_2023_09_29.csv")

#### STEP 2: apply the functions to fetch the original data and process it ####

raw.df <- fetchFlorapulse(folderIn = raw_folder_in,
                          fileOut = raw_file_out)

# save
write_csv(raw.df, raw_file_out)


### Process raw data ####

# No longer needed with SDI12
offsetMultiplier <- readxl::read_excel("data_raw/cax_radar_metadata_caxiuana_12_05_2023.xlsx",
                                       sheet = 1) %>%
  select(ID, plot, species, size_class, flora_pulse_sensor, flora_pulse_offset, flora_pulse_multiplier) %>%
  filter(!is.na(flora_pulse_sensor))

## Process sensor data to get water potential in MPa

processed.list <- processFlorapulse(rawDataFile = raw_file_out, 
                                    offsetMultiplier = offsetMultiplier,
                                    fileOut = processed_file_out)


### checkings ####

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




#### COLLECTED 16_10_2023 ------------------------------------------------------ ####

#### STEP 1: set the location of the original data to process and the files where we want the output to be stored ####

## raw_folder_in: is the complete path to the folder where the original data is stored. It is important to have all the files that we want
## to process together in the same folder (e.g., all the florapulse loggers files in the same folder).

## raw_file_out: location and name of the file where we want the unified raw data to be stored.

## processed_file_out: location and name of the file where we want the processed data to be stored.

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_potential/16-10-2023"
raw_file_out <- paste0("data_raw/stem_water_potential/raw_stem_water_potential_2023_10_16.csv")
processed_file_out <- paste0("data_processed/stem_water_potential/processed_stem_water_potential_2023_10_16.csv")

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


### checkings ####

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



#### COLLECTED 21-12-2023 ------------------------------------------------------ ####

#### STEP 1: set the location of the original data to process and the files where we want the output to be stored ####

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_potential/21-12-2023"
raw_file_out <- paste0("data_raw/stem_water_potential/raw_stem_water_potential_2023_12_21.csv")
processed_file_out <- paste0("data_processed/stem_water_potential/processed_stem_water_potential_2023_12_21.csv")

#### STEP 2: apply the functions to fetch the original data and process it ####

raw.df <- fetchFlorapulse(folderIn = raw_folder_in,
                          fileOut = raw_file_out)

# save
write_csv(raw.df, raw_file_out)


### Process raw data ####

# No longer needed with SDI12
offsetMultiplier <- readxl::read_excel("data_raw/cax_radar_metadata_caxiuana_12_05_2023.xlsx",
                                       sheet = 1) %>%
  select(ID, plot, species, size_class, flora_pulse_sensor, flora_pulse_offset, flora_pulse_multiplier) %>%
  filter(!is.na(flora_pulse_sensor))

## Process sensor data to get water potential in MPa

processed.list <- processFlorapulse(rawDataFile = raw_file_out, 
                                    offsetMultiplier = offsetMultiplier,
                                    fileOut = processed_file_out)


### checkings ####

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


#### COLLECTED 14_03_2024 ------------------------------------------------------ ####

#### STEP 1: set the location of the original data to process and the files where we want the output to be stored ####

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_potential/14_03_2024"
raw_file_out <- paste0("data_raw/stem_water_potential/raw_stem_water_potential_2024_03_14.csv")
processed_file_out <- paste0("data_processed/stem_water_potential/processed_stem_water_potential_2024_03_14.csv")

#### STEP 2: apply the functions to fetch the original data and process it ####

raw.df <- fetchFlorapulse(folderIn = raw_folder_in,
                          fileOut = raw_file_out)

# save
write_csv(raw.df, raw_file_out)


### Process raw data ####

# No longer needed with SDI12
offsetMultiplier <- readxl::read_excel("data_raw/cax_radar_metadata_caxiuana_12_05_2023.xlsx",
                                       sheet = 1) %>%
  select(ID, plot, species, size_class, flora_pulse_sensor, flora_pulse_offset, flora_pulse_multiplier) %>%
  filter(!is.na(flora_pulse_sensor))

## Process sensor data to get water potential in MPa

processed.list <- processFlorapulse(rawDataFile = raw_file_out, 
                                    offsetMultiplier = offsetMultiplier,
                                    fileOut = processed_file_out)


### checkings ####

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



#### MERGE DATA TO ENSURE WE HAVE ALL THE TIME SERIES -------------------------- ####

files_path <- list.files("data_processed/stem_water_potential", ".csv", full.names = T)

file <- files_path[1]
data <- as.data.frame(read_csv(file[1]))
head(data)
tail(data)
summary(data)
variablestocombine <- names(data)

data$timestamp_ID <- paste0(data$timestamp, "_", data$ID)

for(i in 2:length(files_path)){
  
  data_i <- as.data.frame(read_csv(files_path[i]))
  data_i$timestamp_ID <- paste0(data_i$timestamp, "_", data_i$ID)
  
  # data <- bind_rows(data, data_i)
  # data <- anti_join(data, data_i, by = "timestamp_ID")
  
  data <- unique(rbindlist(list(data, data_i)), by = "timestamp_ID")
}

length(unique(data$timestamp_ID))
length(data$timestamp_ID)

## Aggreagate per id and timestamp

# data <- aggregate(data,
#                   by = list(data$timestamp_ID),
#                   FUN = meanOrMode) %>%
#   select(-Group.1)
# 
# unique(data$ID)

stem_wp_data <- data %>%
  # filter(!ID %in% c("Control_NA", "Control_Licania", "Control_Protium", "NA")) %>%   # Individuals with problems in the that needs to be double check, removed for now
  filter(!is.na(ID)) %>%
  mutate(timestamp = as_datetime(str_split_fixed(timestamp_ID, "_", n = 3)[, 1]),
         date = as_date(timestamp)) %>%
  filter(date > "2023-05-01") %>%
  filter(date < "2024-05-01") %>%
  arrange(ID, timestamp) %>%
  as.data.frame()

unique(stem_wp_data$ID)
head(stem_wp_data)
tail(stem_wp_data)
summary(stem_wp_data)



#### LAST CLEANING AND OUTLIER REMOVAL AND SAVING ------------------------------ ####

### individual outliers 

stem_wp_data$cleaned_wp_bar <- stem_wp_data$wp_bar
all_stem_wp_data <- data.frame()

for(id in unique(stem_wp_data$ID)){

  id_stem_wp_data <- stem_wp_data %>%
    filter(ID == id)

  mean_wp <- mean(id_stem_wp_data$wp_bar, na.rm = T)
  sd_wp <- sd(id_stem_wp_data$wp_bar, na.rm = T)

  upper_threshold <- mean_wp + (sd_wp * 3)
  lower_threshold <- mean_wp - (sd_wp * 3)

  id_stem_wp_data$cleaned_wp_bar[id_stem_wp_data$wp_bar > upper_threshold] <- NA
  id_stem_wp_data$cleaned_wp_bar[id_stem_wp_data$wp_bar < lower_threshold] <- NA

  all_stem_wp_data <- rbind(all_stem_wp_data, id_stem_wp_data)
}

all_stem_wp_data <- all_stem_wp_data %>%
  arrange(ID, timestamp)

write_csv(all_stem_wp_data, "data_processed/stem_water_potential/complete_datasets/processed_stem_water_potential_27-05-2023_01-04-2024.csv")

## to general

write_csv(all_stem_wp_data, "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data_processed/caxiuana_stem_water_potential/processed_stem_water_potential_27-05-2023_01-04-2024.csv")

summary(all_stem_wp_data)
head(all_stem_wp_data)
tail(all_stem_wp_data)


#### STEP 3: data visualization ####

### All the individuals together ####

all.plot <- plotTimeSeries(data = all_stem_wp_data,
               xVar = timestamp,
               yVar = wp_bar,
               xLab = "time", 
               yLab = "WP (bar)", 
               lineOrPoint = "line", 
               colorVar = ID) #+ ylim(-20, 20)
all.plot

# Save the plot
# pdf("outputs/data_plots/stem_water_potential/stem_water_potential_all.pdf")
# all.plot
# dev.off()


### Control individuals ####

control_data <- all_stem_wp_data %>% 
  filter(str_detect(ID, "Control"))

control.plot <- plotTimeSeries(data = control_data,
                    xVar = timestamp,
                    yVar = wp_bar,
                    xLab = "time", 
                    yLab = "WP (bar)", 
                    lineOrPoint = "line", 
                    colorVar = ID) #+ ylim(-5, 5)
control.plot
# Save the plot
# pdf("outputs/data_plots/stem_water_potential/stem_water_potential_control.pdf")
# control.plot
# dev.off()


### tfe individuals ####

tfe_data <- all_stem_wp_data %>% 
  filter(str_detect(ID, "TFE"))

tfe.plot <- plotTimeSeries(data = tfe_data,
               xVar = timestamp,
               yVar = wp_bar,
               xLab = "time", 
               yLab = "WP (bar)", 
               lineOrPoint = "line", 
               colorVar = ID) #+ ylim(-5, 5)
tfe.plot

# Save the plot
# pdf("outputs/data_plots/stem_water_potential/stem_water_potential_tfe.pdf")
# tfe.plot
# dev.off()


### Individuals one by one ####

for(ind in unique(all_stem_wp_data$ID)){
  
  ind_data <- all_stem_wp_data %>% filter(ID == ind)
  
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
