#### SOIL DATA PROCESSING ######################################################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")


labelToID.data <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/metadata_cax_radar/cax_radar_metadata_caxiuana_03_2024.xlsx", 
                                     sheet = "new_soil_sensors") %>%
  select(wc_label, wp_label, temp_label, variable, plot)


#### COLLECTED 15-03-2024 ------------------------------------------------------ ####

### Control ####

# STEP 1: set the location of the original data to process and the files where we want the output to be stored

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_water_content_2023_on/15_03_2024/control"
raw_file_out <- paste0("data_raw/soil_moisture/new_wc_sensors/raw_soil_water_content_2024_03_15.csv")
processed_file_out <- paste0("data_processed/soil_moisture/new_wc_sensors/processed_control_soil_water_content_2024_03_15.csv")

# STEP 2: apply the functions to fetch the original data and process it

raw.df <- fetchTeros10(folderIn = raw_folder_in,
                       fileOut = raw_file_out)

## rename columns

processed.df <- raw.df

wc_cols <- str_subset(colnames(processed.df), "Water_Content")
for(name in wc_cols){
  
  names(processed.df)[which(names(processed.df) == name)] <- labelToID.data %>%
    filter(wc_label == name) %>%
    pull(variable)
}

wp_cols <- str_subset(colnames(processed.df), "Potential")
for(name in wp_cols){
  
  names(processed.df)[which(names(processed.df) == name)] <- labelToID.data %>%
    filter(wp_label == name) %>%
    pull(variable)
}


temp_cols <- str_subset(colnames(processed.df), "Temperature")
for(name in temp_cols){
  
  names(processed.df)[which(names(processed.df) == name)] <- labelToID.data %>%
    filter(temp_label == name) %>%
    pull(variable)
}

processed.df <- processed.df %>%
  mutate(plot = "Control") %>%
  select(timestamp, date, plot, everything())

tail(processed.df)

plotTimeSeries(processed.df, xVar = timestamp, yVar = wc_100cm_m3_m3)

write_csv(processed.df, processed_file_out)


### TFE ####

# STEP 1: set the location of the original data to process and the files where we want the output to be stored

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_water_content_2023_on/15_03_2024/tfe"
raw_file_out <- paste0("data_raw/soil_moisture/new_wc_sensors/raw_tfe_soil_water_content_2024_03_15.csv")
processed_file_out <- paste0("data_processed/soil_moisture/new_wc_sensors/processed_tfe_soil_water_content_2024_03_15.csv")

# STEP 2: apply the functions to fetch the original data and process it

raw.df <- fetchTeros10(folderIn = raw_folder_in,
                       fileOut = raw_file_out)

# we need to translate from label to tree id, to do so we will use the following object: labelToID.data, which comes from the metadata where we have the
# relationship between loggers and tree ID

processed.df <- raw.df

wc_cols <- str_subset(colnames(processed.df), "Water_Content")
for(name in wc_cols){
  
  names(processed.df)[which(names(processed.df) == name)] <- labelToID.data %>%
    filter(wc_label == name) %>%
    pull(variable)
}

wp_cols <- str_subset(colnames(processed.df), "Potential")
for(name in wp_cols){
  
  names(processed.df)[which(names(processed.df) == name)] <- labelToID.data %>%
    filter(wp_label == name) %>%
    pull(variable)
}


temp_cols <- str_subset(colnames(processed.df), "Temperature")
for(name in temp_cols){
  
  names(processed.df)[which(names(processed.df) == name)] <- labelToID.data %>%
    filter(temp_label == name) %>%
    pull(variable)
}

processed.df <- processed.df %>%
  mutate(plot = "TFE") %>%
  select(timestamp, date, plot, everything())


plotTimeSeries(processed.df, xVar = timestamp, yVar = wc_100cm_m3_m3)

head(processed.df)
write_csv(processed.df, processed_file_out)


