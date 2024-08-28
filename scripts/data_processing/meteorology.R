#### METEOROLOGICAL DATA PROCESSING ####################################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")

processed_folder_out <- paste0("data_processed/met/")

#### 30-09-2023 ---------------------------------------------------------------- ####

## It is important to have each one of the plots data in different folders. So first of all we need to determine which tower is the data coming from.

### Determine whether it is control or TFE tower

notIdentified <- dataIdentificator(folderIn = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/raw_data_in/30-09-2023/",
                                   folderOutA = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/raw_data_in/30-09-2023/control/",
                                   folderOutB = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/raw_data_in/30-09-2023/tfe/")

if(!is.null(notIdentified)){
  print("Some plot data cannot be identified")
  write_delim(as.data.frame(notIdentified), "raw_data_not_automatically_identified.csv")
}


### Control ####

## Step 1: set the location of the original data to process and the files where we want the output to be stored

## raw_folder_in: is the complete path to the folder where the original data is stored. It is important to have all the files that we want
## to process together in the same folder (e.g., all the ems81 loggers files in the same folder).
## processed_file_out: location and name of the file where we want the processed data to be stored.

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/raw_data_in/30-09-2023/control/"
processed_folder_out <- paste0("data_processed/met/")


## Step 2: apply the functions to fetch the original data and process it

# Here we need to separate the two types of loggers, as they have different variables: infra and lba. We merge them later.
control_met_all_infra.df <- data.frame()
control_met_all_lba.df <- data.frame()

for(file in list.files(raw_folder_in, pattern = ".dat", full.names = T)){
  
  if(str_detect(file, "INFRA")){
    control_met_infra.df <- fetchMet(file = file,
                               # fileOut = processed_file_out,
                               plot = "Control")
    
    control_met_all_infra.df <- bind_rows(control_met_all_infra.df, control_met_infra.df) %>%
      arrange(timestamp)
  } else {
    if(str_detect(file, "LB")){
      control_met_lba.df <- fetchMet(file = file,
                                     # fileOut = processed_file_out,
                                     plot = "Control")
      
      control_met_all_lba.df <- bind_rows(control_met_all_lba.df, control_met_lba.df) %>%
        arrange(timestamp)
    } else{
     print("logger not specified in file name")
    }

  }
}

# Select meteorological variables only

selected_control_met_all_lba.df <- control_met_all_lba.df %>%
  select(any_of(met_variables.names$abbreviation_raw))
tail(selected_control_met_all_lba.df)

selected_control_met_infra.df <- control_met_infra.df %>%
  select(any_of(met_variables.names$abbreviation_raw))
head(selected_control_met_infra.df)

# Merge loggers data

control_met.df <- merge(selected_control_met_all_lba.df, 
                        selected_control_met_infra.df, 
                        by = "timestamp", 
                        all = T,
                        suffixes = c("_infra", "_lba")) # suffixes = c("_infra", "_lba")
names(control_met.df)

# Combine repeated variables

# variablesToCombine <- str_remove(names(control_met.df)[str_detect(names(control_met.df), ".y")], ".y")
# combined_control_met.df <- combineData(data = control_met.df, variablesToCombine = variablesToCombine)

# Rename final dataset

raw_processed_names <- met_variables.names %>%
  filter(abbreviation_processing %in% names(control_met.df))

named_control_met.df <- data.table::setnames(control_met.df, 
                                                      old = raw_processed_names$abbreviation_processing,
                                                      new = raw_processed_names$abbreviation_processed)
tail(named_control_met.df)

# add year variable to see which years are represented

named_control_met.df$year <- year(named_control_met.df$timestamp)

unique(named_control_met.df$year)

# Aggregate by datetime to make sure we only have one observation per time step

unique_named_control_met.df <- aggregate(named_control_met.df, 
                                         by = list(named_control_met.df$timestamp), 
                                         FUN = mean, 
                                         na.rm = T) %>%
  select(timestamp, year, everything(), -Group.1)


## VPD calculation

vpd_unique_named_control_met.df <- unique_named_control_met.df %>% 
  mutate(vpd2m_kPa = bigleaf::rH.to.VPD(rh2m_perc/100,t2m_C),
         vpd16m_kPa = bigleaf::rH.to.VPD(rh16m_perc/100,t16m_C),
         vpd28m_kPa = bigleaf::rH.to.VPD(rh16m_perc/100,t28m_C),
         vpd42m_kPa = bigleaf::rH.to.VPD(rh42m_perc/100,t42m_C))

## Save

write_csv(vpd_unique_named_control_met.df, paste0(processed_folder_out,
                                                  min(as_date(vpd_unique_named_control_met.df$timestamp)), "-", 
                                                  max(as_date(vpd_unique_named_control_met.df$timestamp)),
                                                  "_met_control_processed.csv"))


### TFE ####

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/tfe"
processed_folder_out <- paste0("data_processed/met/")

## Step 2: apply the functions to fetch the original data and process it

tfe_met_all.df <- data.frame()

for(file in list.files(raw_folder_in, pattern = ".dat", full.names = T)){

    tfe_met.df <- fetchMet(file = file,
                                     # fileOut = processed_file_out,
                                     plot = "TFE")
    
    tfe_met_all.df <- bind_rows(tfe_met_all.df, tfe_met.df) %>%
      arrange(timestamp)
}


# Select meteorological variables only

met_variables.names <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/caxiuana_variables_abbreviation.xlsx",
                                          sheet = "Torre PB") %>%
  select(abbreviation_raw, abbreviation_processing, abbreviation_processed) %>%
  na.omit() %>%
  mutate(abbreviation_raw = tolower(abbreviation_raw))

names(tfe_met_all.df) <- tolower(names(tfe_met_all.df))
selected_tfe_met_all.df <- tfe_met_all.df %>%
  select(timestamp, any_of(met_variables.names$abbreviation_raw))

# Rename final dataset

raw_processed_names <- met_variables.names %>%
  filter(abbreviation_raw %in% names(selected_tfe_met_all.df))

named_selected_tfe_met_all.df <- data.table::setnames(selected_tfe_met_all.df, 
                                                      old = raw_processed_names$abbreviation_raw, 
                                                      new = raw_processed_names$abbreviation_processed)
head(named_selected_tfe_met_all.df)

# add year variable to see which years are represented

# named_selected_tfe_met_all.df <- named_selected_tfe_met_all.df %>%
#   filter(timestamp > "2022-07-01")

named_selected_tfe_met_all.df$year <- year(named_selected_tfe_met_all.df$timestamp)

unique(named_selected_tfe_met_all.df$year) # 2019 to 2023

# Aggregate by datetime to make sure we only have one observation per time step

unique_named_selected_tfe_met_all.df <- aggregate(named_selected_tfe_met_all.df, by = list(named_selected_tfe_met_all.df$timestamp), FUN = mean, na.rm = T) %>%
  select(timestamp, year, everything(), -Group.1) %>%
  filter(year > 2000)


## VPD calculation

vpd_unique_named_selected_tfe_met_all.df <- unique_named_selected_tfe_met_all.df %>% 
  mutate(vpd_belowRoof_kPa = bigleaf::rH.to.VPD(rh_belowRoof_perc/100,t_mean_belowRoof_C),
         vpd_aboveRoof_kPa = bigleaf::rH.to.VPD(rh_aboveRoof_perc/100,t_aboveRoof_C),
         vpd42m_kPa = bigleaf::rH.to.VPD(rh42m_perc/100,t42m_mean_C))

## save

write_csv(vpd_unique_named_selected_tfe_met_all.df, paste0(processed_folder_out,
                                                           min(as_date(vpd_unique_named_selected_tfe_met_all.df$timestamp)), "-", 
                                                           max(as_date(vpd_unique_named_selected_tfe_met_all.df$timestamp)), 
                                                       "_met_tfe_processed.csv"))


#### 23-12-2023 ---------------------------------------------------------------- ####

notIdentified <- dataIdentificator(folderIn = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/raw_data_in/23-12-2023/",
                                   folderOutA = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/raw_data_in/23-12-2023/control/",
                                   folderOutB = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/raw_data_in/23-12-2023/tfe/")

if(!is.null(notIdentified)){
  print("Some plot data cannot be identified")
  write_delim(as.data.frame(notIdentified), "raw_data_not_automatically_identified.csv")
}

### Control ####

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/raw_data_in/23-12-2023/control/"
processed_folder_out <- paste0("data_processed/met/")

control_met_all_infra.df <- data.frame()
control_met_all_lba.df <- data.frame()

for(file in list.files(raw_folder_in, pattern = ".dat", full.names = T)){
  
  if(str_detect(file, "INFRA")){
    control_met_infra.df <- fetchMet(file = file,
                                     # fileOut = processed_file_out,
                                     plot = "Control")
    
    control_met_all_infra.df <- bind_rows(control_met_all_infra.df, control_met_infra.df) %>%
      arrange(timestamp)
  } else {
    if(str_detect(file, "LB")){
      control_met_lba.df <- fetchMet(file = file,
                                     # fileOut = processed_file_out,
                                     plot = "Control")
      
      control_met_all_lba.df <- bind_rows(control_met_all_lba.df, control_met_lba.df) %>%
        arrange(timestamp)
    } else{
      print("logger not specified in file name")
    }
    
  }
}

# Select meteorological variables only

met_variables.names <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/caxiuana_variables_abbreviation.xlsx") %>%
  select(abbreviation_raw, abbreviation_processing, abbreviation_processed) %>%
  na.omit()

selected_control_met_all_lba.df <- control_met_all_lba.df %>%
  select(any_of(met_variables.names$abbreviation_raw))
tail(selected_control_met_all_lba.df)

selected_control_met_infra.df <- control_met_infra.df %>%
  select(any_of(met_variables.names$abbreviation_raw))
head(selected_control_met_infra.df)

# Merge loggers data

control_met.df <- merge(selected_control_met_all_lba.df, 
                        selected_control_met_infra.df, 
                        by = "timestamp", 
                        all = T,
                        suffixes = c("_infra", "_lba")) # suffixes = c("_infra", "_lba")
names(control_met.df)

# Combine repeated variables

# variablesToCombine <- str_remove(names(control_met.df)[str_detect(names(control_met.df), ".y")], ".y")
# combined_control_met.df <- combineData(data = control_met.df, variablesToCombine = variablesToCombine)

# Rename final dataset

raw_processed_names <- met_variables.names %>%
  filter(abbreviation_processing %in% names(control_met.df))

named_control_met.df <- data.table::setnames(control_met.df, 
                                             old = raw_processed_names$abbreviation_processing,
                                             new = raw_processed_names$abbreviation_processed)
head(named_control_met.df)

# add year variable to see which years are represented

named_control_met.df$year <- year(named_control_met.df$timestamp)

unique(named_control_met.df$year) # 2022 to 2023

# Aggregate by datetime to make sure we only have one observation per time step

unique_named_control_met.df <- aggregate(named_control_met.df, 
                                         by = list(named_control_met.df$timestamp), 
                                         FUN = mean, 
                                         na.rm = T) %>%
  select(timestamp, year, everything(), -Group.1)


## VPD calculation

vpd_unique_named_control_met.df <- unique_named_control_met.df %>% 
  mutate(vpd2m_kPa = bigleaf::rH.to.VPD(rh2m_perc/100,t2m_C),
         vpd16m_kPa = bigleaf::rH.to.VPD(rh16m_perc/100,t16m_C),
         vpd28m_kPa = bigleaf::rH.to.VPD(rh16m_perc/100,t28m_C),
         vpd42m_kPa = bigleaf::rH.to.VPD(rh42m_perc/100,t42m_C))

## save

write_csv(vpd_unique_named_control_met.df, paste0(processed_folder_out,
                                                  min(as_date(vpd_unique_named_control_met.df$timestamp)), "-", 
                                                  max(as_date(vpd_unique_named_control_met.df$timestamp)), 
                                                  "_met_control_processed.csv"))


### TFE ####

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/raw_data_in/23-12-2023/tfe/"
processed_folder_out <- paste0("data_processed/met/")

tfe_met_all.df <- data.frame()

for(file in list.files(raw_folder_in, pattern = ".dat", full.names = T)){
  
  tfe_met.df <- fetchMet(file = file,
                         # fileOut = processed_file_out,
                         plot = "TFE")
  
  tfe_met_all.df <- bind_rows(tfe_met_all.df, tfe_met.df) %>%
    arrange(timestamp)
}

# Select meteorological variables only

met_variables.names <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/caxiuana_variables_abbreviation.xlsx",
                                          sheet = "Torre PB") %>%
  select(abbreviation_raw, abbreviation_processing, abbreviation_processed) %>%
  na.omit() %>%
  mutate(abbreviation_raw = tolower(abbreviation_raw))

names(tfe_met_all.df) <- tolower(names(tfe_met_all.df))
selected_tfe_met_all.df <- tfe_met_all.df %>%
  select(timestamp, any_of(met_variables.names$abbreviation_raw))
tail(selected_tfe_met_all.df)

# Rename final dataset

raw_processed_names <- met_variables.names %>%
  filter(abbreviation_raw %in% names(selected_tfe_met_all.df))

named_selected_tfe_met_all.df <- data.table::setnames(selected_tfe_met_all.df, 
                                                      old = raw_processed_names$abbreviation_raw, 
                                                      new = raw_processed_names$abbreviation_processed)
head(named_selected_tfe_met_all.df)

# add year variable to see which years are represented

# named_selected_tfe_met_all.df <- named_selected_tfe_met_all.df %>%
#   filter(timestamp > "2023-07-01")

named_selected_tfe_met_all.df$year <- year(named_selected_tfe_met_all.df$timestamp)

unique(named_selected_tfe_met_all.df$year) # 2023

# Aggregate by datetime to make sure we only have one observation per time step

unique_named_selected_tfe_met_all.df <- aggregate(named_selected_tfe_met_all.df, by = list(named_selected_tfe_met_all.df$timestamp), FUN = mean, na.rm = T) %>%
  select(timestamp, year, everything(), -Group.1) %>%
  filter(year > 2000)


## VPD calculation

vpd_unique_named_selected_tfe_met_all.df <- unique_named_selected_tfe_met_all.df %>% 
  mutate(vpd_belowRoof_kPa = bigleaf::rH.to.VPD(rh_belowRoof_perc/100,t_mean_belowRoof_C),
         vpd_aboveRoof_kPa = bigleaf::rH.to.VPD(rh_aboveRoof_perc/100,t_aboveRoof_C),
         vpd42m_kPa = bigleaf::rH.to.VPD(rh42m_perc/100,t42m_mean_C))

## save
tail(vpd_unique_named_selected_tfe_met_all.df)
write_csv(vpd_unique_named_selected_tfe_met_all.df, paste0(processed_folder_out,
                                                           min(as_date(vpd_unique_named_selected_tfe_met_all.df$timestamp)), "-", 
                                                           max(as_date(vpd_unique_named_selected_tfe_met_all.df$timestamp)), 
                                                           "_met_tfe_processed.csv"))


#### 25-07-2024 ---------------------------------------------------------------- ####

notIdentified <- dataIdentificator(folderIn = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/raw_data_in/25-07-2024/",
                                   folderOutA = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/raw_data_in/25-07-2024/control/",
                                   folderOutB = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/raw_data_in/25-07-2024/tfe/")

if(!is.null(notIdentified)){
  print("Some plot data cannot be identified")
  write_delim(as.data.frame(notIdentified), "raw_data_not_automatically_identified.csv")
}


### Control ####

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/raw_data_in/25-07-2024/control/"
processed_folder_out <- paste0("data_processed/met/")

control_met_all_infra.df <- data.frame()
control_met_all_lba.df <- data.frame()

for(file in list.files(raw_folder_in, pattern = ".dat", full.names = T)){
  
  if(str_detect(file, "INFRA")){
    control_met_infra.df <- fetchMet(file = file,
                                     # fileOut = processed_file_out,
                                     plot = "Control")
    
    control_met_all_infra.df <- bind_rows(control_met_all_infra.df, control_met_infra.df) %>%
      arrange(timestamp)
  } else {
    if(str_detect(file, "LB")){
      control_met_lba.df <- fetchMet(file = file,
                                     # fileOut = processed_file_out,
                                     plot = "Control")
      
      control_met_all_lba.df <- bind_rows(control_met_all_lba.df, control_met_lba.df) %>%
        arrange(timestamp)
      
    } else{
      print("logger not specified in file name")
    }
    
  }
}

# Select meteorological variables only

met_variables.names <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/caxiuana_variables_abbreviation.xlsx") %>%
  select(abbreviation_raw, abbreviation_processing, abbreviation_processed) %>%
  na.omit()

names(control_met_all_lba.df) <- tolower(names(control_met_all_lba.df))
selected_control_met_all_lba.df <- control_met_all_lba.df %>%
  select(any_of(met_variables.names$abbreviation_raw))
head(selected_control_met_all_lba.df)

names(control_met_infra.df) <- tolower(names(control_met_infra.df))
selected_control_met_infra.df <- control_met_infra.df %>%
  select(any_of(met_variables.names$abbreviation_raw))
head(selected_control_met_infra.df)

# Merge loggers data

control_met.df <- merge(selected_control_met_all_lba.df, 
                        selected_control_met_infra.df, 
                        by = "timestamp", 
                        all = T,
                        suffixes = c("_infra", "_lba")) # suffixes = c("_infra", "_lba")
names(control_met.df)

# Combine repeated variables

# variablesToCombine <- str_remove(names(control_met.df)[str_detect(names(control_met.df), ".y")], ".y")
# combined_control_met.df <- combineData(data = control_met.df, variablesToCombine = variablesToCombine)

# Rename final dataset

raw_processed_names <- met_variables.names %>%
  filter(abbreviation_processing %in% names(control_met.df))

named_control_met.df <- data.table::setnames(control_met.df, 
                                             old = raw_processed_names$abbreviation_processing,
                                             new = raw_processed_names$abbreviation_processed)
head(named_control_met.df)

summary(named_control_met.df)

# add year variable to see which years are represented

named_control_met.df$year <- year(named_control_met.df$timestamp)

unique(named_control_met.df$year) # 2022 to 2023

# Aggregate by datetime to make sure we only have one observation per time step

unique_named_control_met.df <- aggregate(named_control_met.df, 
                                         by = list(named_control_met.df$timestamp), 
                                         FUN = mean, 
                                         na.rm = T) %>%
  select(timestamp, year, everything(), -Group.1)

summary(unique_named_control_met.df)

## VPD calculation

vpd_unique_named_control_met.df <- unique_named_control_met.df %>% 
  mutate(vpd2m_kPa = bigleaf::rH.to.VPD(rh2m_perc/100,t2m_C),
         vpd16m_kPa = bigleaf::rH.to.VPD(rh16m_perc/100,t16m_C),
         vpd28m_kPa = bigleaf::rH.to.VPD(rh16m_perc/100,t28m_C),
         vpd42m_kPa = bigleaf::rH.to.VPD(rh42m_perc/100,t42m_C))

## save

write_csv(vpd_unique_named_control_met.df, paste0(processed_folder_out,
                                                  min(as_date(vpd_unique_named_control_met.df$timestamp)), "-", 
                                                  max(as_date(vpd_unique_named_control_met.df$timestamp)), 
                                                  "_met_control_processed.csv"))


### TFE ####

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/raw_data_in/25-07-2024/tfe/"
processed_folder_out <- paste0("data_processed/met/")


tfe_met_all.df <- data.frame()

for(file in list.files(raw_folder_in, pattern = ".dat", full.names = T)){
  
  tfe_met.df <- fetchMet(file = file,
                         # fileOut = processed_file_out,
                         plot = "TFE")
  
  tfe_met_all.df <- bind_rows(tfe_met_all.df, tfe_met.df) %>%
    arrange(timestamp)
}

# Select meteorological variables only

met_variables.names <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/caxiuana_variables_abbreviation.xlsx",
                                          sheet = "Torre PB") %>%
  select(abbreviation_raw, abbreviation_processing, abbreviation_processed) %>%
  na.omit() %>%
  mutate(abbreviation_raw = tolower(abbreviation_raw))

names(tfe_met_all.df) <- tolower(names(tfe_met_all.df))
selected_tfe_met_all.df <- tfe_met_all.df %>%
  select(timestamp, any_of(met_variables.names$abbreviation_raw))
tail(selected_tfe_met_all.df)

# Rename final dataset

raw_processed_names <- met_variables.names %>%
  filter(abbreviation_raw %in% names(selected_tfe_met_all.df))

named_selected_tfe_met_all.df <- data.table::setnames(selected_tfe_met_all.df, 
                                                      old = raw_processed_names$abbreviation_raw, 
                                                      new = raw_processed_names$abbreviation_processed)
head(named_selected_tfe_met_all.df)

# add year variable to see which years are represented

# named_selected_tfe_met_all.df <- named_selected_tfe_met_all.df %>%
#   filter(timestamp > "2023-07-01")

named_selected_tfe_met_all.df$year <- year(named_selected_tfe_met_all.df$timestamp)

unique(named_selected_tfe_met_all.df$year) # 2023

# Aggregate by datetime to make sure we only have one observation per time step

unique_named_selected_tfe_met_all.df <- aggregate(named_selected_tfe_met_all.df, by = list(named_selected_tfe_met_all.df$timestamp), FUN = mean, na.rm = T) %>%
  select(timestamp, year, everything(), -Group.1) %>%
  mutate(rh_belowRoof_perc = ifelse(rh_belowRoof_perc > 100, 100, rh_belowRoof_perc),
         rh_aboveRoof_perc = ifelse(rh_aboveRoof_perc > 100, 100, rh_aboveRoof_perc),
         rh42m_perc = ifelse(rh42m_perc > 100, 100, rh42m_perc)) %>%
  filter(year > 2000)

## VPD calculation

vpd_unique_named_selected_tfe_met_all.df <- unique_named_selected_tfe_met_all.df %>% 
  mutate(vpd_belowRoof_kPa = bigleaf::rH.to.VPD(rh_belowRoof_perc/100,t_mean_belowRoof_C),
         vpd_aboveRoof_kPa = bigleaf::rH.to.VPD(rh_aboveRoof_perc/100,t_aboveRoof_C),
         vpd42m_kPa = bigleaf::rH.to.VPD(rh42m_perc/100,t42m_mean_C))

## save
tail(vpd_unique_named_selected_tfe_met_all.df)
write_csv(vpd_unique_named_selected_tfe_met_all.df, paste0(processed_folder_out,
                                                           min(as_date(vpd_unique_named_selected_tfe_met_all.df$timestamp)), "-", 
                                                           max(as_date(vpd_unique_named_selected_tfe_met_all.df$timestamp)), 
                                                           "_met_tfe_processed.csv"))

#### CONTROL COMPLETE DATASET -------------------------------------------------- ####
### Merge all data ####

files_path <- list.files(processed_folder_out, "control", full.names = T)

file <- files_path[1]
data <- as.data.frame(read_csv(file[1]))
head(data)
tail(data)
summary(data)
variablestocombine <- names(data)

for(i in 2:length(files_path)){
  
  data_i <- as.data.frame(read_csv(files_path[i]))
  data <- bind_rows(data, data_i)
  # data <- unique(rbindlist(list(data, data_i)), by = "timestamp")
}

length(unique(data$timestamp))
length(data$timestamp)
head(data)
tail(data)
summary(data)
## Aggreagate per id and timestamp

# data_aggr <- aggregate(data,
#                   by = list(data$timestamp_ID),
#                   FUN = meanOrMode) %>%
#   select(-Group.1)
# 
# unique(data$ID)

meteo_control_data <- data %>%
  filter(!is.na(timestamp)) %>%
  mutate(plot = "Control",
         timestamp = as_datetime(timestamp)) %>%
  select(timestamp, plot, everything()) %>%
  arrange(timestamp) %>%
  as.data.frame()

unique(meteo_control_data$timestamp)
head(meteo_control_data)
tail(meteo_control_data)
summary(meteo_control_data)


#### Data cleaning ####

### individual outliers 

cleaned_meteo_control_data <- meteo_control_data

for(var in names(cleaned_meteo_control_data)[c(-1, -2)]){

  mean_var <- mean(cleaned_meteo_control_data[, var], na.rm = T)
  sd_var <- sd(cleaned_meteo_control_data[, var], na.rm = T)

  upper_threshold <- mean_var + (sd_var * 3)
  lower_threshold <- mean_var - (sd_var * 3)

  cleaned_meteo_control_data[which(cleaned_meteo_control_data[, var] > upper_threshold), var] <- NA
  cleaned_meteo_control_data[which(cleaned_meteo_control_data[, var] < lower_threshold), var] <- NA
}

cleaned_meteo_control_data <- cleaned_meteo_control_data %>%
  arrange(timestamp) %>%
  filter(!is.na(timestamp))

summary(cleaned_meteo_control_data)
summary(meteo_control_data)


#### Daily aggregation ####

toDaily_cleaned_meteo_control_data <- cleaned_meteo_control_data %>%
  mutate(date = as_date(timestamp)) %>%
  select(date, precip_mm_infra, precip_mm_lba, everything(), -timestamp, -year)
names(toDaily_cleaned_meteo_control_data)

daily_clean_meteo_control_data <- aggregate(toDaily_cleaned_meteo_control_data[, c(-1, -2, -3)],
                                      by = list(toDaily_cleaned_meteo_control_data$date),
                                      FUN = meanOrMode)

daily_clean_precip_control_data <- aggregate(toDaily_cleaned_meteo_control_data[, c(2, 3)],
                                            by = list(toDaily_cleaned_meteo_control_data$date),
                                            FUN = sum, na.rm = T)

daily_clean_meteo_control_data <- merge(daily_clean_precip_control_data, daily_clean_meteo_control_data,
                                        by = "Group.1")

daily_clean_meteo_control_data <- daily_clean_meteo_control_data %>%
  rename(date = Group.1) %>%
  mutate(date = as_date(ymd(date))) %>%
  select(date, plot, everything()) %>%
  arrange(date) %>%
  filter(!is.na(date))

head(daily_clean_meteo_control_data)
tail(daily_clean_meteo_control_data)
summary(daily_clean_meteo_control_data)

#### Save final dataset ####

## hourly data

## to project directory

write_csv(cleaned_meteo_control_data, 
          paste0("data_processed/met/complete_datasets/processed_met_control_", 
                 as_date(min(cleaned_meteo_control_data$timestamp)), "-", 
                 as_date(max(cleaned_meteo_control_data$timestamp)), ".csv")
)

## to general directory

write_csv(cleaned_meteo_control_data, 
          paste0(root.dir, "data_processed/met/processed_met_control_", 
                 as_date(min(cleaned_meteo_control_data$timestamp)), "-", 
                 as_date(max(cleaned_meteo_control_data$timestamp)), ".csv")
)


## daily data

## to project directory

write_csv(daily_clean_meteo_control_data, 
          paste0("data_processed/met/complete_datasets/processed_daily_met_control_",
                 as_date(min(as_datetime(daily_clean_meteo_control_data$date))), "-", 
                 as_date(max(as_datetime(daily_clean_meteo_control_data$date))), ".csv")
)

## to general directory

write_csv(daily_clean_meteo_control_data, 
          paste0(root.dir, "data_processed/met/processed_daily_met_control_", 
                 as_date(min(as_datetime(daily_clean_meteo_control_data$date))), "-", 
                 as_date(max(as_datetime(daily_clean_meteo_control_data$date))), ".csv")
)


#### Subdaily data plotting ####

cleaned_meteo_control_data <- as.data.frame(read_csv(paste0("data_processed/met/complete_datasets/processed_met_control_", 
                                             as_date(min(cleaned_meteo_control_data$timestamp)), "-", 
                                             as_date(max(cleaned_meteo_control_data$timestamp)), ".csv")))

for(variable in names(cleaned_meteo_control_data)[c(-1, -2)]){
  
  cleaned_meteo_control_data$variable <- cleaned_meteo_control_data[, variable]
  
  control.plot <- plotTimeSeries(data = cleaned_meteo_control_data,
                                 xVar = timestamp,
                                 yVar = variable,
                                 xLab = "time", 
                                 yLab = variable, 
                                 lineOrPoint = "line")
  plot(control.plot)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/met/control_", variable, ".pdf"))
  plot(control.plot)
  dev.off()
  
  cleaned_meteo_control_data$variable <- NULL
}


#### Daily data plotting ####

cleaned_daily_meteo_control_data <- as.data.frame(read_csv(paste0("data_processed/met/complete_datasets/processed_daily_met_control_", 
                                                            as_date(min(cleaned_meteo_control_data$timestamp)), "-", 
                                                            as_date(max(cleaned_meteo_control_data$timestamp)), ".csv")))

for(variable in names(cleaned_daily_meteo_control_data)[c(-1, -2)]){
  
  cleaned_daily_meteo_control_data$variable <- cleaned_daily_meteo_control_data[, variable]
  
  control.plot <- plotTimeSeries(data = cleaned_daily_meteo_control_data,
                                 xVar = date,
                                 yVar = variable,
                                 xLab = "time", 
                                 yLab = variable, 
                                 lineOrPoint = "line")
  plot(control.plot)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/met/control_daily_", variable, ".pdf"))
  plot(control.plot)
  dev.off()
  
  cleaned_daily_meteo_control_data$variable <- NULL
}


#### TFE COMPLETE DATASET -------------------------------------------------- ####
### Merge all data ####

files_path <- list.files(processed_folder_out, "tfe", full.names = T)

file <- files_path[1]
data <- as.data.frame(read_csv(file[1]))
head(data)
tail(data)
summary(data)
variablestocombine <- names(data)

for(i in 2:length(files_path)){
  
  data_i <- as.data.frame(read_csv(files_path[i]))
  data <- bind_rows(data, data_i)
  # data <- unique(rbindlist(list(data, data_i)), by = "timestamp")
}

length(unique(data$timestamp))
length(data$timestamp)
head(data)
tail(data)

## Aggreagate per id and timestamp

# data_aggr <- aggregate(data,
#                   by = list(data$timestamp_ID),
#                   FUN = meanOrMode) %>%
#   select(-Group.1)
# 
# unique(data$ID)

meteo_tfe_data <- data %>%
  filter(!is.na(timestamp)) %>%
  mutate(plot = "TFE",
         timestamp = as_datetime(timestamp)) %>%
  select(timestamp, plot, everything()) %>%
  arrange(timestamp) %>%
  as.data.frame()

unique(meteo_tfe_data$timestamp)
head(meteo_tfe_data)
tail(meteo_tfe_data)
summary(meteo_tfe_data)


#### Data cleaning ####

## individual outliers 

cleaned_meteo_tfe_data <- meteo_tfe_data

for(var in names(cleaned_meteo_tfe_data)[c(-1, -2, -3)]){
  
  mean_var <- mean(cleaned_meteo_tfe_data[, var], na.rm = T)
  sd_var <- sd(cleaned_meteo_tfe_data[, var], na.rm = T)
  
  upper_threshold <- mean_var + (sd_var * 3)
  lower_threshold <- mean_var - (sd_var * 3)
  
  cleaned_meteo_tfe_data[which(cleaned_meteo_tfe_data[, var] > upper_threshold), var] <- NA
  cleaned_meteo_tfe_data[which(cleaned_meteo_tfe_data[, var] < lower_threshold), var] <- NA
}

cleaned_meteo_tfe_data <- cleaned_meteo_tfe_data %>%
  arrange(timestamp) %>%
  filter(!is.na(timestamp))

summary(cleaned_meteo_tfe_data)
summary(meteo_tfe_data)


#### Daily aggregation ####

toDaily_cleaned_meteo_tfe_data <- cleaned_meteo_tfe_data %>%
  mutate(date = as_date(timestamp)) %>%
  select(date, year, plot, everything(), -timestamp, -year)

daily_clean_meteo_tfe_data <- aggregate(toDaily_cleaned_meteo_tfe_data[, c(-1)],
                                            by = list(toDaily_cleaned_meteo_tfe_data$date),
                                            FUN = meanOrMode)

daily_clean_meteo_tfe_data <- daily_clean_meteo_tfe_data %>%
  rename(date = Group.1) %>%
  mutate(date = as_date(ymd(date))) %>%
  select(date, plot, everything()) %>%
  arrange(date) %>%
  filter(!is.na(date))

head(daily_clean_meteo_tfe_data)
tail(daily_clean_meteo_tfe_data)


#### Save final dataset ####

## hourly data

## to project directory

write_csv(cleaned_meteo_tfe_data, 
          paste0("data_processed/met/complete_datasets/processed_met_tfe_", 
                 as_date(min(cleaned_meteo_tfe_data$timestamp)), "-", 
                 as_date(max(cleaned_meteo_tfe_data$timestamp)), ".csv")
)

## to general directory

write_csv(cleaned_meteo_tfe_data, 
          paste0(root.dir, "data_processed/met/processed_met_tfe_", 
                 as_date(min(cleaned_meteo_tfe_data$timestamp)), "-", 
                 as_date(max(cleaned_meteo_tfe_data$timestamp)), ".csv")
)


## daily data

## to project directory

write_csv(daily_clean_meteo_tfe_data, 
          paste0("data_processed/met/complete_datasets/processed_daily_met_tfe_",
                 as_date(min(as_datetime(daily_clean_meteo_tfe_data$date))), "-", 
                 as_date(max(as_datetime(daily_clean_meteo_tfe_data$date))), ".csv")
)

## to general directory

write_csv(daily_clean_meteo_tfe_data, 
          paste0(root.dir, "data_processed/met/processed_daily_met_tfe_", 
                 as_date(min(as_datetime(daily_clean_meteo_tfe_data$date))), "-", 
                 as_date(max(as_datetime(daily_clean_meteo_tfe_data$date))), ".csv")
)


#### Subdaily data plotting ####

cleaned_meteo_tfe_data <- as.data.frame(read_csv(paste0("data_processed/met/complete_datasets/processed_met_tfe_", 
                                                            as_date(min(cleaned_meteo_tfe_data$timestamp)), "-", 
                                                            as_date(max(cleaned_meteo_tfe_data$timestamp)), ".csv")))

for(variable in names(cleaned_meteo_tfe_data)[c(-1, -2, -3)]){
  
  cleaned_meteo_tfe_data$variable <- cleaned_meteo_tfe_data[, variable]
  
  tfe.plot <- plotTimeSeries(data = cleaned_meteo_tfe_data,
                                 xVar = timestamp,
                                 yVar = variable,
                                 xLab = "time", 
                                 yLab = variable, 
                                 lineOrPoint = "line")
  plot(tfe.plot)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/met/tfe_", variable, ".pdf"))
  plot(tfe.plot)
  dev.off()
  
  cleaned_meteo_tfe_data$variable <- NULL
}


#### Daily data plotting ####

cleaned_daily_meteo_tfe_data <- as.data.frame(read_csv(paste0("data_processed/met/complete_datasets/processed_daily_met_tfe_", 
                                                                  as_date(min(cleaned_meteo_tfe_data$timestamp)), "-", 
                                                                  as_date(max(cleaned_meteo_tfe_data$timestamp)), ".csv")))

for(variable in names(cleaned_daily_meteo_tfe_data)[c(-1, -2)]){
  
  cleaned_daily_meteo_tfe_data$variable <- cleaned_daily_meteo_tfe_data[, variable]
  
  tfe.plot <- plotTimeSeries(data = cleaned_daily_meteo_tfe_data,
                                 xVar = date,
                                 yVar = variable,
                                 xLab = "time", 
                                 yLab = variable, 
                                 lineOrPoint = "line")
  plot(tfe.plot)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/met/tfe_daily_", variable, ".pdf"))
  plot(tfe.plot)
  dev.off()
  
  cleaned_daily_meteo_tfe_data$variable <- NULL
}


















