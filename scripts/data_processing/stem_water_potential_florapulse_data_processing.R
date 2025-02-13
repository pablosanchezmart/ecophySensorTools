#### STEM WATER POTENTIAL (FLORAPULSE) DATA PROCESSING  EXAMPLE ################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")


#### METADATA FILE ####

metadata <- readxl::read_excel(paste0(root.dir, "data_processed/metadata_cax_radar/cax_radar_metadata_caxiuana_11_2024.xlsx"),
                                     sheet = "2024_07_metadata") %>%
  select(ID, plot, species, size_class, contains("flora_pulse_sensor"))

# adjust the code to the fact that we had to replace some loggers
logger_names <- names(metadata)[str_detect(names(metadata), "flora_pulse_sensor")]

labelToID <- data.frame()
for(logger_name in logger_names){
  
  labelToID.data_newLogger <- metadata[!is.na(metadata[, logger_name]), ] %>%
    select(ID, plot, species, size_class, all_of(logger_name))
  
  names(labelToID.data_newLogger)[names(labelToID.data_newLogger) == logger_name] <- "flora_pulse_sensor"
  
  labelToID <- rbind(labelToID, labelToID.data_newLogger)
}


#### COLLECTED 29-09-2023 ------------------------------------------------------ ####

#### STEP 1: set the location of the original data to process and the files where we want the output to be stored ####

## raw_folder_in: is the complete path to the folder where the original data is stored. It is important to have all the files that we want
## to process together in the same folder (e.g., all the florapulse loggers files in the same folder).

## raw_file_out: location and name of the file where we want the unified raw data to be stored.

## processed_file_out: location and name of the file where we want the processed data to be stored.

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_potential/29-09-2023"
raw_file_out <- paste0("data_raw/stem_water_potential/raw_stem_water_potential_2023_09_29.csv")
processed_file_out <- paste0("data_processed/stem_water_potential/processed_stem_water_potential_2023_09_29.csv")

#### STEP 2: apply the functions to fetch the original data and process it ####

raw.df <- fetchFlorapulse(folderIn = raw_folder_in,
                          fileOut = raw_file_out)

## Process raw data

# at the beggining, we did not include offset and multiplier, so we calculated the values here using offset and multiplier
# Also, this data comes from a full bridge connection, which we then realized that was dependant on the cable length, being this not equal for all proves. So we will most likely delete this data 

offsetMultiplier <- readxl::read_excel("data_raw/cax_radar_metadata_caxiuana_12_05_2023.xlsx",
                                       sheet = 1) %>%
  select(ID, plot, species, size_class, flora_pulse_sensor, flora_pulse_offset, flora_pulse_multiplier) %>%
  filter(!is.na(flora_pulse_sensor))

## Process sensor data to get water potential in MPa

processed_data <- processFlorapulse(rawDataFile = raw_file_out, 
                                    useOffsetMultiplier = T,
                                    metaData = offsetMultiplier,
                                    fileOut = processed_file_out)


### checkings ####

# how many sensors do we have? (max. 20)
length(unique(processed_data$ID))

# how many sensors without all NAs?
non_allNAs <- processed_data %>% 
  na.omit() %>%
  pull(ID) %>%
  unique()
length(non_allNAs)

# for which sensors we did not get data here?
unique(offsetMultiplier$ID)[which(!unique(offsetMultiplier$ID) %in% unique(non_allNAs))]

# which sensors where in the raw data and are now not present in process data? (potential problem in the identification of ID and sensor pairs)
sensors <- unique(str_remove_all(raw.df$label, "_AVG"))
length(sensors)
sensors[which(!sensors %in% unique(offsetMultiplier$flora_pulse_sensor))]


## Observe a specific individual to check structure of data

processed_data %>% 
  filter(ID == "TFE_116")

## last checks

head(processed_data)
tail(processed_data)


#### COLLECTED 16-10-2023 ------------------------------------------------------ ####

#### STEP 1: set the location of the original data to process and the files where we want the output to be stored ####

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_potential/16-10-2023"
raw_file_out <- paste0("data_raw/stem_water_potential/raw_stem_water_potential_2023_10_16.csv")
processed_file_out <- paste0("data_processed/stem_water_potential/processed_stem_water_potential_2023_10_16.csv")

#### STEP 2: apply the functions to fetch the original data and process it ####

### Fetch raw data ####

raw.df <- fetchFlorapulse(folderIn = raw_folder_in,
                          fileOut = raw_file_out)


### Process raw data ####

## Load offset and multiplier values for each sensor and its corresponding tree ID from the metadata file

## Process raw data

# at the beggining, we did not include offset and multiplier, so we calculated the values here using offset and multiplier
# Also, this data comes from a full bridge connection, which we then realized that was dependant on the cable length, being this not equal for all proves. So we will most likely delete this data 

offsetMultiplier <- readxl::read_excel("data_raw/cax_radar_metadata_caxiuana_12_05_2023.xlsx",
                                       sheet = 1) %>%
  select(ID, plot, species, size_class, flora_pulse_sensor, flora_pulse_offset, flora_pulse_multiplier) %>%
  filter(!is.na(flora_pulse_sensor))

## Process sensor data to get water potential in MPa

processed_data <- processFlorapulse(rawDataFile = raw_file_out, 
                                    useOffsetMultiplier = T,
                                    metaData = offsetMultiplier,
                                    fileOut = processed_file_out) %>%
  filter(!is.na(ID))

### checkings ####

# how many sensors do we have? (max. 20)
length(unique(processed_data$ID))

# how many sensors without all NAs?
non_allNAs <- processed_data %>% 
  na.omit() %>%
  pull(ID) %>%
  unique()
length(non_allNAs)

# for which sensors we got data here?
unique(offsetMultiplier$ID)[which(!unique(offsetMultiplier$ID) %in% unique(non_allNAs))]

# which sensors where in the raw data and are now not present in process data? (potential problem in the identification of ID and sensor pairs)
sensors <- unique(str_remove_all(raw.df$label, "_AVG"))
length(sensors)
sensors[which(!sensors %in% unique(offsetMultiplier$flora_pulse_sensor))]


## Observe a specific individual to check structure of data

processed_data %>% 
  filter(ID == "TFE_116")

## last checks

head(processed_data)
tail(processed_data)


#### COLLECTED 21-12-2023 ------------------------------------------------------ ####
#### STEP 1: set the location of the original data to process and the files where we want the output to be stored ####

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_potential/21-12-2023"
raw_file_out <- paste0("data_raw/stem_water_potential/raw_stem_water_potential_2023_12_21.csv")
processed_file_out <- paste0("data_processed/stem_water_potential/processed_stem_water_potential_2023_12_21.csv")

#### STEP 2: apply the functions to fetch the original data and process it ####

### Fetch raw data ####

raw.df <- fetchFlorapulse(folderIn = raw_folder_in,
                          fileOut = raw_file_out)


### Process raw data ####

## Process raw data

# 10/2023 we included SDI12 connections which already calculate WP values using offset and multipliers, so that calculation was not needed anymore.

## Process sensor data to get water potential in MPa

processed_data <- processFlorapulse(rawDataFile = raw_file_out, 
                                    useOffsetMultiplier = F,
                                    metaData = labelToID,
                                    fileOut = processed_file_out) %>%
  filter(!is.na(ID))

### checkings ####

# how many sensors do we have? (max. 20)
length(unique(processed_data$ID))

# how many sensors without all NAs?
non_allNAs <- processed_data %>% 
  na.omit() %>%
  pull(ID) %>%
  unique()
length(non_allNAs)

# for which sensors we got data here?
unique(offsetMultiplier$ID)[which(!unique(offsetMultiplier$ID) %in% unique(non_allNAs))]

# which sensors where in the raw data and are now not present in process data? (potential problem in the identification of ID and sensor pairs)
sensors <- unique(str_remove_all(raw.df$label, "_AVG"))
length(sensors)
sensors[which(!sensors %in% unique(offsetMultiplier$flora_pulse_sensor))]


## Observe a specific individual to check structure of data

processed_data %>% 
  filter(ID == "TFE_116")

## last checks

head(processed_data)
tail(processed_data)


#### COLLECTED 14_03_2024 ------------------------------------------------------ ####

#### STEP 1: set the location of the original data to process and the files where we want the output to be stored ####

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_potential/14_03_2024"
raw_file_out <- paste0("data_raw/stem_water_potential/raw_stem_water_potential_2024_03_14.csv")
processed_file_out <- paste0("data_processed/stem_water_potential/processed_stem_water_potential_2024_03_14.csv")


#### STEP 2: apply the functions to fetch the original data and process it ####

### Fetch raw data ####

raw.df <- fetchFlorapulse(folderIn = raw_folder_in,
                          fileOut = raw_file_out)


### Process raw data ####

## Process raw data

# 10/2023 we included SDI12 connections which already calculate WP values using offset and multipliers, so that calculation was not needed anymore.

## Process sensor data to get water potential in MPa

processed_data <- processFlorapulse(rawDataFile = raw_file_out, 
                                    useOffsetMultiplier = F,
                                    metaData = labelToID,
                                    fileOut = processed_file_out) %>%
  filter(!is.na(ID))

### checkings ####

# how many sensors do we have? (max. 20)
length(unique(processed_data$ID))

# how many sensors without all NAs?
non_allNAs <- processed_data %>% 
  na.omit() %>%
  pull(ID) %>%
  unique()
length(non_allNAs)

# for which sensors we got data here?
unique(offsetMultiplier$ID)[which(!unique(offsetMultiplier$ID) %in% unique(non_allNAs))]

# which sensors where in the raw data and are now not present in process data? (potential problem in the identification of ID and sensor pairs)
sensors <- unique(str_remove_all(raw.df$label, "_AVG"))
length(sensors)
sensors[which(!sensors %in% unique(offsetMultiplier$flora_pulse_sensor))]


## Observe a specific individual to check structure of data

processed_data %>% 
  filter(ID == "TFE_116")

## last checks

head(processed_data)
tail(processed_data)

#### COLLECTED 30-05-2024 ------------------------------------------------------ ####

#### STEP 1: set the location of the original data to process and the files where we want the output to be stored ####

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_potential/30-05-2024"
raw_file_out <- paste0("data_raw/stem_water_potential/raw_stem_water_potential_2024_05_30.csv")
processed_file_out <- paste0("data_processed/stem_water_potential/processed_stem_water_potential_2024_05_30.csv")


#### STEP 2: apply the functions to fetch the original data and process it ####

### Fetch raw data ####

raw.df <- fetchFlorapulse(folderIn = raw_folder_in,
                          fileOut = raw_file_out)


### Process raw data ####

## Process raw data

# 10/2023 we included SDI12 connections which already calculate WP values using offset and multipliers, so that calculation was not needed anymore.

## Process sensor data to get water potential in MPa

processed_data <- processFlorapulse(rawDataFile = raw_file_out, 
                                    useOffsetMultiplier = F,
                                    metaData = labelToID,
                                    fileOut = processed_file_out) %>%
  filter(!is.na(ID))

### checkings ####

# how many sensors do we have? (max. 20)
length(unique(processed_data$ID))

# how many sensors without all NAs?
non_allNAs <- processed_data %>% 
  na.omit() %>%
  pull(ID) %>%
  unique()
length(non_allNAs)

# for which sensors we got data here?
unique(offsetMultiplier$ID)[which(!unique(offsetMultiplier$ID) %in% unique(non_allNAs))]

# which sensors where in the raw data and are now not present in process data? (potential problem in the identification of ID and sensor pairs)
sensors <- unique(str_remove_all(raw.df$label, "_AVG"))
length(sensors)
sensors[which(!sensors %in% unique(offsetMultiplier$flora_pulse_sensor))]


## Observe a specific individual to check structure of data

processed_data %>% 
  filter(ID == "TFE_116")

## last checks

head(processed_data)
tail(processed_data)

#### COLLECTED 24-01-2025 ------------------------------------------------------ ####

#### STEP 1: set the location of the original data to process and the files where we want the output to be stored ####

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_potential/24-01-2025"
raw_file_out <- paste0("data_raw/stem_water_potential/raw_stem_water_potential_2025_01_24.csv")
processed_file_out <- paste0("data_processed/stem_water_potential/processed_stem_water_potential_2025_01_24.csv")


#### STEP 2: apply the functions to fetch the original data and process it ####

### Fetch raw data ####

raw.df <- fetchFlorapulse(folderIn = raw_folder_in,
                          fileOut = raw_file_out)


### Process raw data ####

## Process raw data

# 10/2023 we included SDI12 connections which already calculate WP values using offset and multipliers, so that calculation was not needed anymore.

## Process sensor data to get water potential in MPa

processed_data <- processFlorapulse(rawDataFile = raw_file_out, 
                                    useOffsetMultiplier = F,
                                    metaData = labelToID,
                                    fileOut = processed_file_out) %>%
  filter(!is.na(ID))

### checkings ####

# how many sensors do we have? (max. 20)
length(unique(processed_data$ID))

# how many sensors without all NAs?
non_allNAs <- processed_data %>% 
  na.omit() %>%
  pull(ID) %>%
  unique()
length(non_allNAs)

# for which sensors we got data here?
unique(offsetMultiplier$ID)[which(!unique(offsetMultiplier$ID) %in% unique(non_allNAs))]

# which sensors where in the raw data and are now not present in process data? (potential problem in the identification of ID and sensor pairs)
sensors <- unique(str_remove_all(raw.df$label, "_AVG"))
length(sensors)
sensors[which(!sensors %in% unique(offsetMultiplier$flora_pulse_sensor))]


## Observe a specific individual to check structure of data

processed_data %>% 
  filter(ID == "TFE_116")

## last checks

head(processed_data)
tail(processed_data)


#### MERGE DATA TO ENSURE WE HAVE ALL THE TIME SERIES -------------------------- ####

### raw data

files_path <- list.files("data_raw/stem_water_potential", ".csv", full.names = T)

file <- files_path[1]
raw_data <- as.data.frame(read_csv(file[1])) %>%
  mutate(flora_pulse_sensor = str_remove(label, "_AVG"))

raw_data <- merge(raw_data, labelToID, by = "flora_pulse_sensor", all.x = T)

head(raw_data)
tail(raw_data)
summary(raw_data)

raw_data$timestamp_ID <- paste0(raw_data$timestamp, "_", raw_data$ID)

for(i in 2:length(files_path)){
  
  data_i <- as.data.frame(read_csv(files_path[i])) %>%
    mutate(flora_pulse_sensor = str_remove(label, "_AVG"))
  
  data_i <- merge(data_i, labelToID, by = "flora_pulse_sensor", all.x = T)
  data_i$timestamp_ID <- paste0(data_i$timestamp, "_", data_i$ID)
  
  raw_data <- as.data.frame(unique(rbindlist(list(raw_data, data_i), fill = T), by = "timestamp_ID"))
}

length(unique(raw_data$timestamp_ID))
length(raw_data$timestamp_ID)


### processed data


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
  
  # data <- unique(bind_rows(data, data_i))
  # data <- anti_join(data, data_i, by = "timestamp_ID")
  
  data <- unique(rbindlist(list(data, data_i), fill = T), by = "timestamp_ID")
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
  filter(!ID %in% c("Control_NA", "Control_Licania", "Control_Protium", "NA")) %>%   # Individuals with problems in the that needs to be double check, removed for now
  filter(!is.na(ID)) %>%
  mutate(timestamp = as_datetime(str_split_fixed(timestamp_ID, "_", n = 3)[, 1]),
         date = as_date(timestamp)) %>%
  filter(date > "2023-05-01") %>%
  # filter(date > "2023-11-01") %>%  # after SDI 12
  filter(date < "2025-05-01") %>%
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

write_csv(all_stem_wp_data, "data_processed/stem_water_potential/complete_datasets/processed_stem_water_potential_27-05-2023_24-01-2025.csv")

## to general

write_csv(all_stem_wp_data, "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data_processed/caxiuana_stem_water_potential/processed_stem_water_potential_27-05-2023_24-01-2025.csv")

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
  
  # ind <- "TFE_116"
  
  ind_data <- all_stem_wp_data %>%
    filter(ID == ind) %>% 
    mutate(date = as_date(timestamp))
  # filter(date == "2023-11-12")
  
  # raw data from logger
  
  ind_raw_data <- raw_data %>%
    filter(ID == ind) %>% 
    mutate(date = as_date(timestamp))
  
  # first processed data
  raw_ind.plot <- plotTimeSeries(data = ind_raw_data,
                             xVar = timestamp,
                             yVar = value,
                             xLab = "", 
                             yLab = "sensor reading", 
                             lineOrPoint = "line", 
                             colorVar = ID) + 
    # scale_x_datetime(date_breaks = "3 month", date_labels = "%Y-%m")
    scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m", limits = c(min(all_stem_wp_data$timestamp), max(all_stem_wp_data$timestamp))) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))# + ylim(-5, 5)

  # first processed data
  ind.plot <- plotTimeSeries(data = ind_data,
                             xVar = timestamp,
                             yVar = wp_bar,
                             xLab = "", 
                             yLab = "wp (bar)", 
                             lineOrPoint = "line", 
                             colorVar = ID) + 
    scale_x_datetime(date_breaks = "3 month", date_labels = "%Y-%m")
  
  # gap filled and calibrated water content
  clean_ind.plot <- plotTimeSeries(data = ind_data,
                                xVar = timestamp,
                                yVar = cleaned_wp_bar,
                                xLab = "", 
                                yLab = "clean wp (bar)", 
                                lineOrPoint = "line", 
                                colorVar = ID) + 
    scale_x_datetime(date_breaks = "3 months", date_labels = "%Y-%m")
  
  # gap filled and calibrated water content
  clean_ind_whopePeriod.plot <- plotTimeSeries(data = ind_data,
                                   xVar = timestamp,
                                   yVar = cleaned_wp_bar,
                                   xLab = "", 
                                   yLab = "clean wp (bar)", 
                                   lineOrPoint = "line", 
                                   colorVar = ID) + 
    scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m", limits = c(min(all_stem_wp_data$timestamp), max(all_stem_wp_data$timestamp))) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))# + ylim(-5, 5)
  
  
  # temperature corrected gap filled and calibrated water content
  # tc_ind.plot <- plotTimeSeries(data = ind_data,
  #                               xVar = timestamp,
  #                               yVar = tempCor_gf_clean_calibrated_water_content_m3.m3,
  #                               xLab = "", 
  #                               yLab = "tc gf cl stem wc (m3/m3)", 
  #                               lineOrPoint = "line", 
  #                               colorVar = ID) + 
  #   scale_x_datetime(date_breaks = "1 month", date_labels = "%b")
  
  # Save the plot
  pdf(paste0("outputs/data_plots/stem_water_potential/stem_water_potential_", ind, "_", str_replace(unique(ind_data$species), " ", "_"),".pdf"))
  p <- ggarrange(ind.plot,
                 clean_ind.plot,
                 clean_ind_whopePeriod.plot,
                 raw_ind.plot,
                 ncol = 1, nrow = 4, legend = "bottom", common.legend = T)
  plot(p)
  dev.off()
}
