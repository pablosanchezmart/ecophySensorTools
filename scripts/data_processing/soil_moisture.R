#### SOIL DATA PROCESSING ######################################################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")

processed_folder_out <- paste0("data_processed/soil_moisture/")

## It is important to have each one of the plots data in different folders. So first of all we need to determine which pits is the data coming from.

#### PREVIOUS DATA (ALREADY PROCESSED, only wc for now) ####

### Control ####

cax_a_obs_2009_2022 <- read_csv(paste0(root.dir, "data_processed/soil_moisture/pablo/cax_a_soil_obs_2009_2023_hourly.csv")) %>%
  select(timestamp = time, contains("water_content"))

names(cax_a_obs_2009_2022)[-1] <- str_replace_all(names(cax_a_obs_2009_2022)[-1], "soil_volumetric_water_content", "vwc")
names(cax_a_obs_2009_2022)[-1] <- paste0(names(cax_a_obs_2009_2022)[-1], "_m3_m3")

cax_a_obs_2009_2022$year <- year(cax_a_obs_2009_2022$timestamp)
cax_a_obs_2009_2022$date <- as_date(cax_a_obs_2009_2022$timestamp)

unique(cax_a_obs_2009_2022$year) # 2009 to 2022

write_csv(cax_a_obs_2009_2022, paste0(processed_folder_out,
                                         min(cax_a_obs_2009_2022$date), "-", max(cax_a_obs_2009_2022$date), 
                                         "_soil_moisture_control_processed.csv"))

### TFE ####

cax_b_obs_2009_2022 <- read_csv(paste0(root.dir, "data_processed/soil_moisture/pablo/cax_b_soil_obs_2009_2023_hourly.csv")) %>%
  select(timestamp = time, contains("water_content"))

names(cax_b_obs_2009_2022)[-1] <- str_replace_all(names(cax_b_obs_2009_2022)[-1], "soil_volumetric_water_content", "vwc")
names(cax_b_obs_2009_2022)[-1] <- paste0(names(cax_b_obs_2009_2022)[-1], "_m3_m3")

cax_b_obs_2009_2022$year <- year(cax_b_obs_2009_2022$timestamp)
cax_b_obs_2009_2022$date <- as_date(cax_b_obs_2009_2022$timestamp)

unique(cax_b_obs_2009_2022$year) # 2008 to 2023

write_csv(cax_b_obs_2009_2022, paste0(processed_folder_out,
                                      min(cax_b_obs_2009_2022$date), "-", max(cax_b_obs_2009_2022$date), 
                                      "_soil_moisture_tfe_processed.csv"))

#### 29-09-2023 -----------------------------------------------------------------####

### Determine whether it is control or TFE pits

notIdentified <- soilDataIdentificator(folderIn = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/29-09-2023/",
                                   folderOutA = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/29-09-2023/control/",
                                   folderOutB = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/29-09-2023/tfe/")

if(!is.null(notIdentified)){
  print("Some plot data cannot be identified")
  write_delim(as.data.frame(notIdentified), "raw_data_not_automatically_identified.csv")
}


### CONTROL ####

## STEP 1: set the location of the original data to process and the files where we want the output to be stored

## raw_folder_in: is the complete path to the folder where the original data is stored. It is important to have all the files that we want
## to process together in the same folder (e.g., all the ems81 loggers files in the same folder).

## processed_file_out: location and name of the file where we want the processed data to be stored.

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/29-09-2023/control/"
processed_folder_out <- paste0("data_processed/soil_moisture/")


### STEP 2: apply the functions to fetch the original data and process it

# Here we need to separate the two types of loggers, as they have different variables: infra and lba. We merge them later.
control_soil_vw_all.df <- data.frame()
control_soil_swp_all.df <- data.frame()

for(file in list.files(raw_folder_in, pattern = ".dat", full.names = T)){
  
  if(str_detect(file, "vw")){
    control_soil_vw.df <- fetchMet(file = file,
                                     # fileOut = processed_file_out,
                                     plot = "Control")
    
    control_soil_vw_all.df <- bind_rows(control_soil_vw_all.df, control_soil_vw.df) %>%
      arrange(timestamp)
  } else {
    if(str_detect(file, "swp")){
      control_soil_swp.df <- fetchMet(file = file,
                                     # fileOut = processed_file_out,
                                     plot = "Control")
      
      control_soil_swp_all.df <- bind_rows(control_soil_swp_all.df, control_soil_swp.df) %>%
        arrange(timestamp)
    } else{
      print("logger not specified in file name")
    }
    
  }
}

# Merge loggers data

control_soil.df <- merge(control_soil_swp_all.df, 
                        control_soil_vw.df, 
                        by = "timestamp", 
                        all = T,
                        suffixes = c("_swp_sensor", "_vw_sensor"))

names(control_soil.df) <- tolower(names(control_soil.df))

# Rename final dataset

raw_processed_names <- soil_control_variables.names %>%
  filter(abbreviation_raw %in% names(control_soil.df))

control_soil.df <- data.table::setnames(control_soil.df, old = raw_processed_names$abbreviation_raw,
                                                      new = raw_processed_names$abbreviation_processed)
head(control_soil.df)
tail(control_soil.df)
# add year variable to see which years are represented

control_soil.df$year <- year(control_soil.df$timestamp)
control_soil.df$date <- as_date(control_soil.df$timestamp)

unique(control_soil.df$year) # 2019 to 2023

# Aggregate by datetime to make sure we only have one observation per time step

unique_control_soil.df <- aggregate(control_soil.df, 
                                   by = list(control_soil.df$timestamp), 
                                   FUN = mean, 
                                   na.rm = T) %>%
  select(timestamp, date, year, everything(), -Group.1)


# save

write_csv(unique_control_soil.df, paste0(processed_folder_out,
                                        min(unique_control_soil.df$date), "-", max(unique_control_soil.df$date), 
                                        "_soil_moisture_control_processed.csv"))


### STEP 3: data visualization

control_soil.df <- as.data.frame(read_csv(paste0(processed_folder_out,
                                                min(unique_control_soil.df$date), "-", max(unique_control_soil.df$date), 
                                                "_soil_moisture_control_processed.csv")))

for(variable in names(control_soil.df)[c(-1, -2, -3, -4, -5)]){
  
  control_soil.df$variable <- control_soil.df[, variable]
  
  control.plot <- plotTimeSeries(data = control_soil.df,
                                 xVar = timestamp,
                                 yVar = variable,
                                 xLab = "time", 
                                 yLab = variable, 
                                 lineOrPoint = "line")
  plot(control.plot)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/soil_moisture/control_", variable, "_",
             min(unique_control_soil.df$date), "-", max(unique_control_soil.df$date),  ".pdf"))
  plot(control.plot)
  dev.off()
  
  control_soil.df$variable <- NULL
}

## repeat for TFE plot


#### TFE ####

### FETCH TFE PITS DATA

## STEP 1: set the location of the original data to process and the files where we want the output to be stored

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/29-09-2023/tfe/"
processed_folder_out <- paste0("data_processed/soil_moisture/")


## STEP 2: apply the functions to fetch the original data and process it

tfe_soil_vw_all.df <- data.frame()
tfe_soil_swp_all.df <- data.frame()

for(file in list.files(raw_folder_in, pattern = ".dat", full.names = T)){
  
  if(str_detect(file, "vw")){
    tfe_soil_vw.df <- fetchMet(file = file,
                                   # fileOut = processed_file_out,
                                   plot = "tfe")
    
    tfe_soil_vw_all.df <- bind_rows(tfe_soil_vw_all.df, tfe_soil_vw.df) %>%
      arrange(timestamp)
  } else {
    if(str_detect(file, "swp")){
      tfe_soil_swp.df <- fetchMet(file = file,
                                      # fileOut = processed_file_out,
                                      plot = "tfe")
      
      tfe_soil_swp_all.df <- bind_rows(tfe_soil_swp_all.df, tfe_soil_swp.df) %>%
        arrange(timestamp)
    } else{
      print("logger not specified in file name")
    }
    
  }
}

# Merge loggers data

tfe_soil.df <- tfe_soil_vw.df

# tfe_soil.df <- merge(tfe_soil_swp_all.df, 
#                          tfe_soil_vw.df, 
#                          by = "timestamp", 
#                          all = T,
#                          suffixes = c("_swp_sensor", "_vw_sensor"))
names(tfe_soil.df)

# Rename final dataset

raw_processed_names <- soil_tfe_variables.names %>%
  filter(abbreviation_raw %in% names(tfe_soil.df))

tfe_soil.df <- data.table::setnames(tfe_soil.df, old = raw_processed_names$abbreviation_raw,
                                              new = raw_processed_names$abbreviation_processed)
head(tfe_soil.df)

## Combine right and left sensors 

tfe_soil.df <- combineData(data = tfe_soil.df, variablesToCombine = c("vwc_sup_m3_m3", "vwc_50cm_m3_m3", "pa_us_sup", "pa_us_50cm_avg"))
head(tfe_soil.df)

# add year variable to see which years are represented

tfe_soil.df$year <- year(tfe_soil.df$timestamp)
tfe_soil.df$date <- as_date(tfe_soil.df$timestamp)

unique(tfe_soil.df$year) # 2022 to 2023

# Aggregate by datetime to make sure we only have one observation per time step

unique_tfe_soil.df <- aggregate(tfe_soil.df, 
                                    by = list(tfe_soil.df$timestamp), 
                                    FUN = mean, 
                                    na.rm = T) %>%
  select(timestamp, date, year, everything(), -Group.1)


# save

write_csv(unique_tfe_soil.df, paste0(processed_folder_out,
                                         min(unique_tfe_soil.df$date), "-", max(unique_tfe_soil.df$date), 
                                         "_soil_moiture_tfe_processed.csv"))

## STEP 3: data visualization

tfe_soil.df <- as.data.frame(read_csv(paste0(processed_folder_out,
                                                 min(unique_tfe_soil.df$date), "-", max(unique_tfe_soil.df$date), 
                                                 "_soil_moiture_tfe_processed.csv")))

for(variable in names(tfe_soil.df)[c(-1, -2, -3, -4, -5)]){
  
  tfe_soil.df$variable <- tfe_soil.df[, variable]
  
  tfe.plot <- plotTimeSeries(data = tfe_soil.df,
                                 xVar = timestamp,
                                 yVar = variable,
                                 xLab = "time", 
                                 yLab = variable, 
                                 lineOrPoint = "line")
  plot(tfe.plot)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/soil_moisture/tfe_", variable, "_",
             min(unique_control_soil.df$date), "-", max(unique_control_soil.df$date),  ".pdf"))
  plot(tfe.plot)
  dev.off()
  
  tfe_soil.df$variable <- NULL
}


#### 14-01-2024 -----------------------------------------------------------------####

### Determine whether it is control or TFE pits

notIdentified <- soilDataIdentificator(folderIn = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/16-01-2024/",
                                       folderOutA = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/16-01-2024/control/",
                                       folderOutB = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/16-01-2024/tfe/")

if(!is.null(notIdentified)){
  print("Some plot data cannot be identified")
  write_delim(as.data.frame(notIdentified), "raw_data_not_automatically_identified.csv")
}


### CONTROL ####

## STEP 1: set the location of the original data to process and the files where we want the output to be stored

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/16-01-2024/control/"
processed_folder_out <- paste0("data_processed/soil_moisture/")


### STEP 2: apply the functions to fetch the original data and process it

control_soil_vw_all.df <- data.frame()
control_soil_swp_all.df <- data.frame()

for(file in list.files(raw_folder_in, pattern = ".dat", full.names = T)){
  
  if(str_detect(file, "vw")){
    control_soil_vw.df <- fetchMet(file = file,
                                   # fileOut = processed_file_out,
                                   plot = "Control")
    
    control_soil_vw_all.df <- bind_rows(control_soil_vw_all.df, control_soil_vw.df) %>%
      arrange(timestamp)
  } else {
    if(str_detect(file, "swp")){
      control_soil_swp.df <- fetchMet(file = file,
                                      # fileOut = processed_file_out,
                                      plot = "Control")
      
      control_soil_swp_all.df <- bind_rows(control_soil_swp_all.df, control_soil_swp.df) %>%
        arrange(timestamp)
    } else{
      print("logger not specified in file name")
    }
    
  }
}

# Merge loggers data
control_soil.df <- control_soil_vw.df
# control_soil.df <- merge(control_soil_swp_all.df, 
#                          control_soil_vw.df, 
#                          by = "timestamp", 
#                          all = T,
#                          suffixes = c("_swp_sensor", "_vw_sensor"))
names(control_soil.df)

# Rename final dataset

raw_processed_names <- soil_control_variables_2024.names %>%
  filter(abbreviation_raw %in% names(control_soil.df))

control_soil.df <- data.table::setnames(control_soil.df, old = raw_processed_names$abbreviation_raw,
                                        new = raw_processed_names$abbreviation_processed)
head(control_soil.df)

# add year variable to see which years are represented

control_soil.df$year <- year(control_soil.df$timestamp)
control_soil.df$date <- as_date(control_soil.df$timestamp)

unique(control_soil.df$year) # 2023

# Aggregate by datetime to make sure we only have one observation per time step

unique_control_soil.df <- aggregate(control_soil.df, 
                                    by = list(control_soil.df$timestamp), 
                                    FUN = mean, 
                                    na.rm = T) %>%
  select(timestamp, date, year, everything(), -Group.1)


# save

tail(unique_control_soil.df)

write_csv(unique_control_soil.df, paste0(processed_folder_out,
                                         min(unique_control_soil.df$date), "-", max(unique_control_soil.df$date), 
                                         "_soil_moisture_control_processed.csv"))


### STEP 3: data visualization

control_soil.df <- as.data.frame(read_csv(paste0(processed_folder_out,
                                                 min(unique_control_soil.df$date), "-", max(unique_control_soil.df$date), 
                                                 "_soil_moisture_control_processed.csv")))

for(variable in names(control_soil.df)[c(-1, -2, -3, -4, -5)]){
  
  control_soil.df$variable <- control_soil.df[, variable]
  
  control.plot <- plotTimeSeries(data = control_soil.df,
                                 xVar = timestamp,
                                 yVar = variable,
                                 xLab = "time", 
                                 yLab = variable, 
                                 lineOrPoint = "line")
  plot(control.plot)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/soil_moisture/control_", variable, ".pdf"))
  plot(control.plot)
  dev.off()
  
  control_soil.df$variable <- NULL
}

## repeat for TFE plot

#### TFE ####

### FETCH TFE PITS DATA

## STEP 1: set the location of the original data to process and the files where we want the output to be stored

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/16-01-2024/tfe/"
processed_folder_out <- paste0("data_processed/soil_moisture/")


## STEP 2: apply the functions to fetch the original data and process it

tfe_soil_vw_all.df <- data.frame()
tfe_soil_swp_all.df <- data.frame()

for(file in list.files(raw_folder_in, pattern = ".dat", full.names = T)){
  
  if(str_detect(file, "vw")){
    tfe_soil_vw.df <- fetchMet(file = file,
                               # fileOut = processed_file_out,
                               plot = "tfe")
    
    tfe_soil_vw_all.df <- bind_rows(tfe_soil_vw_all.df, tfe_soil_vw.df) %>%
      arrange(timestamp)
  } else {
    if(str_detect(file, "swp")){
      tfe_soil_swp.df <- fetchMet(file = file,
                                  # fileOut = processed_file_out,
                                  plot = "tfe")
      
      tfe_soil_swp_all.df <- bind_rows(tfe_soil_swp_all.df, tfe_soil_swp.df) %>%
        arrange(timestamp)
    } else{
      print("logger not specified in file name")
    }
    
  }
}

# Merge loggers data

tfe_soil.df <- tfe_soil_vw.df
# tfe_soil.df <- merge(tfe_soil_swp_all.df, 
#                          tfe_soil_vw.df, 
#                          by = "timestamp", 
#                          all = T,
#                          suffixes = c("_swp_sensor", "_vw_sensor"))
names(tfe_soil.df)

# Rename final dataset

raw_processed_names <- soil_tfe_variables.names %>%
  filter(abbreviation_raw %in% names(tfe_soil.df))

tfe_soil.df <- data.table::setnames(tfe_soil.df, old = raw_processed_names$abbreviation_raw,
                                    new = raw_processed_names$abbreviation_processed)
head(tfe_soil.df)

## Combine right and left sensors 

tfe_soil.df <- combineData(data = tfe_soil.df, variablesToCombine = c("vwc_sup_m3_m3", "vwc_50cm_m3_m3", "pa_us_sup", "pa_us_50cm_avg"))
head(tfe_soil.df)


tfe_soil.df$year <- year(tfe_soil.df$timestamp)
tfe_soil.df$date <- as_date(tfe_soil.df$timestamp)

unique(tfe_soil.df$year) # 2022 to 2023

# Aggregate by datetime to make sure we only have one observation per time step

unique_tfe_soil.df <- aggregate(tfe_soil.df, 
                                by = list(tfe_soil.df$timestamp), 
                                FUN = mean, 
                                na.rm = T) %>%
  select(timestamp, date, year, everything(), -Group.1)


# save

write_csv(unique_tfe_soil.df, paste0(processed_folder_out,
                                     min(unique_tfe_soil.df$date), "-", max(unique_tfe_soil.df$date), 
                                     "_soil_moisture_tfe_processed.csv"))

## STEP 3: data visualization

tfe_soil.df <- as.data.frame(read_csv(paste0(processed_folder_out,
                                             min(unique_tfe_soil.df$date), "-", max(unique_tfe_soil.df$date), 
                                             "_soil_moisture_tfe_processed.csv")))

for(variable in names(tfe_soil.df)[c(-1, -2, -3, -4, -5)]){
  
  tfe_soil.df$variable <- tfe_soil.df[, variable]
  
  tfe.plot <- plotTimeSeries(data = tfe_soil.df,
                             xVar = timestamp,
                             yVar = variable,
                             xLab = "time", 
                             yLab = variable, 
                             lineOrPoint = "line")
  plot(tfe.plot)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/soil_moisture/tfe_", variable, ".pdf"))
  plot(tfe.plot)
  dev.off()
  
  tfe_soil.df$variable <- NULL
}



#### 24-05-2024 -----------------------------------------------------------------####

### Determine whether it is control or TFE pits

notIdentified <- soilDataIdentificator(folderIn = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/24-05-2024/",
                                       folderOutA = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/24-05-2024/control/",
                                       folderOutB = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/24-05-2024/tfe/")

if(!is.null(notIdentified)){
  print("Some plot data cannot be identified")
  write_delim(as.data.frame(notIdentified), "raw_data_not_automatically_identified.csv")
}

### CONTROL ####

## STEP 1: set the location of the original data to process and the files where we want the output to be stored

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/24-05-2024/control/"
processed_folder_out <- paste0("data_processed/soil_moisture/")


### STEP 2: apply the functions to fetch the original data and process it

control_soil_vw_all.df <- data.frame()
control_soil_swp_all.df <- data.frame()

for(file in list.files(raw_folder_in, pattern = ".dat", full.names = T)){
  
  if(str_detect(file, "vw")){
    control_soil_vw.df <- fetchMet(file = file,
                                   # fileOut = processed_file_out,
                                   plot = "Control")
    
    control_soil_vw_all.df <- bind_rows(control_soil_vw_all.df, control_soil_vw.df) %>%
      arrange(timestamp)
  } else {
    if(str_detect(file, "swp")){
      control_soil_swp.df <- fetchMet(file = file,
                                      # fileOut = processed_file_out,
                                      plot = "Control")
      
      control_soil_swp_all.df <- bind_rows(control_soil_swp_all.df, control_soil_swp.df) %>%
        arrange(timestamp)
    } else{
      print("logger not specified in file name")
    }
    
  }
}

# Rename final dataset

control_soil.df <- control_soil_vw.df %>%
  select(-vw_50cm_e, -vw_sup_e, -contains("ptemp"), -contains("avg")) %>%
  rename(vwc_sup_m3_m3  = vw_sup_d, vwc_50cm_m3_m3  = vw_50cm_d, vwc_100cm_m3_m3  = vw_100cm, 
         vwc_250cm_m3_m3  = vw_250cm, vwc_400cm_m3_m3 = vw_400cm) 
head(control_soil.df)

# add year variable to see which years are represented

control_soil.df$year <- year(control_soil.df$timestamp)
control_soil.df$date <- as_date(control_soil.df$timestamp)

unique(control_soil.df$year) # 2019-2024

# Aggregate by datetime to make sure we only have one observation per time step

unique_control_soil.df <- aggregate(control_soil.df, 
                                    by = list(control_soil.df$timestamp), 
                                    FUN = mean, 
                                    na.rm = T) %>%
  select(timestamp, date, year, everything(), -Group.1)


# save

tail(unique_control_soil.df)

write_csv(unique_control_soil.df, paste0(processed_folder_out,
                                         min(unique_control_soil.df$date), "-", max(unique_control_soil.df$date), 
                                         "_soil_moisture_control_processed.csv"))


## repeat for TFE plot

#### TFE ####

### FETCH TFE PITS DATA

## STEP 1: set the location of the original data to process and the files where we want the output to be stored

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/24-05-2024/tfe/"
processed_folder_out <- paste0("data_processed/soil_moisture/")


## STEP 2: apply the functions to fetch the original data and process it

tfe_soil_vw_all.df <- data.frame()
tfe_soil_swp_all.df <- data.frame()

for(file in list.files(raw_folder_in, pattern = ".dat", full.names = T)){
  
  if(str_detect(file, "vw")){
    tfe_soil_vw.df <- fetchMet(file = file,
                               # fileOut = processed_file_out,
                               plot = "tfe")
    
    tfe_soil_vw_all.df <- bind_rows(tfe_soil_vw_all.df, tfe_soil_vw.df) %>%
      arrange(timestamp)
  } else {
    if(str_detect(file, "swp")){
      tfe_soil_swp.df <- fetchMet(file = file,
                                  # fileOut = processed_file_out,
                                  plot = "tfe")
      
      tfe_soil_swp_all.df <- bind_rows(tfe_soil_swp_all.df, tfe_soil_swp.df) %>%
        arrange(timestamp)
    } else{
      print("logger not specified in file name")
    }
    
  }
}

# Rename final dataset
names(tfe_soil_vw.df)
raw_processed_names <- tfe_soil_vw.df %>%
  rename(vwc_sup_m3_m3  = vw_2, vwc_50cm_m3_m3  = vw_4, vwc_100cm_m3_m3  = vw_5, 
         vwc_250cm_m3_m3  = vw_6, vwc_400cm_m3_m3 = vw_7) %>%
  select(timestamp, record, vwc_sup_m3_m3, vwc_50cm_m3_m3, vwc_100cm_m3_m3, vwc_250cm_m3_m3, vwc_400cm_m3_m3)
head(raw_processed_names)

tfe_soil.df$year <- year(tfe_soil.df$timestamp)
tfe_soil.df$date <- as_date(tfe_soil.df$timestamp)

unique(tfe_soil.df$year) # 2022 to 2024

# Aggregate by datetime to make sure we only have one observation per time step

unique_tfe_soil.df <- aggregate(tfe_soil.df, 
                                by = list(tfe_soil.df$timestamp), 
                                FUN = mean, 
                                na.rm = T) %>%
  select(timestamp, date, year, everything(), -Group.1)


# save

write_csv(unique_tfe_soil.df, paste0(processed_folder_out,
                                     min(unique_tfe_soil.df$date), "-", max(unique_tfe_soil.df$date), 
                                     "_soil_moisture_tfe_processed.csv"))

## STEP 3: data visualization

tfe_soil.df <- as.data.frame(read_csv(paste0(processed_folder_out,
                                             min(unique_tfe_soil.df$date), "-", max(unique_tfe_soil.df$date), 
                                             "_soil_moisture_tfe_processed.csv")))

for(variable in names(tfe_soil.df)[c(-1, -2, -3, -4, -5)]){
  
  tfe_soil.df$variable <- tfe_soil.df[, variable]
  
  tfe.plot <- plotTimeSeries(data = tfe_soil.df,
                             xVar = timestamp,
                             yVar = variable,
                             xLab = "time", 
                             yLab = variable, 
                             lineOrPoint = "line")
  plot(tfe.plot)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/soil_moisture/tfe_", variable, ".pdf"))
  plot(tfe.plot)
  dev.off()
  
  tfe_soil.df$variable <- NULL
}




#### MERGE DATA TO ENSURE WE HAVE ALL THE TIME SERIES ##########################
### CONTROL ####

files_path <- list.files("data_processed/soil_moisture", "control", full.names = T)

control_data.list <- lapply(files_path, 
                        read_csv)
# control_data.list[1]
control_data.df <- do.call(bind_rows, control_data.list)

## to prioritize previous data >>
# control_data.df <- control_data.list[[1]] %>%
#   select(timestamp, date, vwc_sup_m3_m3, vwc_50cm_m3_m3, vwc_100cm_m3_m3, vwc_250cm_m3_m3, vwc_400cm_m3_m3)
# vars <- names(control_data.df)[-1]
# 
# for(i in 2:length(control_data.list)){
#   
#   new_control_data.df <- control_data.list[[i]] %>%
#     select(timestamp, date, vwc_sup_m3_m3, vwc_50cm_m3_m3, vwc_100cm_m3_m3, vwc_250cm_m3_m3, vwc_400cm_m3_m3)
#   
#   control_data.df <- left_join(control_data.df, new_control_data.df, 
#                                by = "timestamp")
#   
#   control_data.df <- combineData(data = control_data.df, 
#               variablesToCombine = vars)
# 
# }
# <<

## aggregate per timestamp (ensure we have one record per timestamp) (prioritizing previous data)

unique_control_data.df <- aggregate(control_data.df, by = list(control_data.df$timestamp), FUN = meanOrMode)  %>%
  select(timestamp, date, record, vwc_sup_m3_m3, vwc_50cm_m3_m3, vwc_100cm_m3_m3, vwc_250cm_m3_m3, vwc_400cm_m3_m3)
head(unique_control_data.df)
tail(unique_control_data.df)
length(unique_control_data.df$timestamp)
length(unique(unique_control_data.df$timestamp))

## calibrated values (only for latter data? ask if the previous one was already calibrated, looks like it)

old_selected_unique_control_data.df <- unique_control_data.df %>%
  filter(date < "2022-12-31") %>%
  select(timestamp, date, vwc_sup_m3_m3, vwc_50cm_m3_m3, vwc_100cm_m3_m3, vwc_250cm_m3_m3, vwc_400cm_m3_m3)

new_selected_unique_control_data.df <- unique_control_data.df %>%
  filter(date >= "2022-12-31") %>%
  mutate(vwc_sup_m3_m3 = (vwc_sup_m3_m3 * 1.0229) + 0.0701,
         vwc_50cm_m3_m3 = (vwc_50cm_m3_m3 * 1.0229) + 0.0701,
         vwc_100cm_m3_m3 = (vwc_100cm_m3_m3 * 1.0229) + 0.0701,
         vwc_250cm_m3_m3 = (vwc_250cm_m3_m3 * 1.0229) + 0.0701,
         vwc_400cm_m3_m3 = (vwc_400cm_m3_m3 * 1.0229) + 0.0701) %>%
  select(timestamp, date, vwc_sup_m3_m3, vwc_50cm_m3_m3, vwc_100cm_m3_m3, vwc_250cm_m3_m3, vwc_400cm_m3_m3)

selected_unique_control_data.df <- bind_rows(old_selected_unique_control_data.df,
                                                 new_selected_unique_control_data.df)

### gap fill and plot ####

## make sure we have all timesteps from 2003 on (first years will be extrapolated)

timestamp_backbone <- data.frame("timestamp" = as_datetime(seq(as_datetime("2000-01-01 00:00:00 UTC"), 
                                                   as_datetime(max(selected_unique_control_data.df$timestamp)), 
                                                   by = "1 hour")))

selected_unique_control_data.df <- merge(timestamp_backbone, 
                                         selected_unique_control_data.df, 
                                         by = "timestamp", all.x = T) %>%
  mutate(yday_hour = paste0(yday(timestamp), " ", as_hms(round_date(as_datetime(timestamp), "hour"))),
         date = as_date(timestamp)) %>%
  select(yday_hour, everything())

# variable <- names(selected_unique_control_data.df)[c(-1, -2, -3)][1]
for(variable in names(selected_unique_control_data.df)[c(-1, -2, -3)]){
  
  print(variable)
  
  ## clean outliers
  
  mean_wc <- mean(selected_unique_control_data.df[, variable], na.rm = T)
  sd_wc <- sd(selected_unique_control_data.df[, variable], na.rm = T)
  
  upper_threshold <- mean_wc + (sd_wc * 3)
  lower_threshold <- mean_wc - (sd_wc * 3)
  
  selected_unique_control_data.df[which(selected_unique_control_data.df[, variable] > upper_threshold), variable] <- NA
  selected_unique_control_data.df[which(selected_unique_control_data.df[, variable] < lower_threshold), variable] <- NA
  
  # delete too low values (errors)
  selected_unique_control_data.df[which(selected_unique_control_data.df[, variable] < 0.1), variable] <- NA
  
  ## gap filling (mean of the time for that month and individual)
  
  selected_unique_control_data.df[, paste0("gf_", variable)] <- selected_unique_control_data.df[, variable]
  
  hour_mean_vwc <- aggregate(selected_unique_control_data.df[, paste0("gf_", variable)], 
                             by = list("yday_hour" = selected_unique_control_data.df$yday_hour), 
                             FUN = mean, 
                             na.rm = T)
  names(hour_mean_vwc)[2] <- paste0("gf_", variable)

  selected_unique_control_data.df <- merge(selected_unique_control_data.df, hour_mean_vwc, 
                                           by = "yday_hour", 
                                           all.x = T) %>%
    arrange(timestamp)
  
  selected_unique_control_data.df <- combineData(data = selected_unique_control_data.df, 
                                 variablesToCombine = paste0("gf_", variable))
  
  
  ## plot
  gf_variable <- paste0("gf_", variable)
  selected_unique_control_data.df$variable <- selected_unique_control_data.df[, variable]
  selected_unique_control_data.df$gf_variable <- selected_unique_control_data.df[,  paste0("gf_", variable)]
  
  control.plot <- plotTimeSeries(data = selected_unique_control_data.df,
                             xVar = timestamp,
                             yVar = variable,
                             xLab = "time", 
                             yLab = variable, 
                             lineOrPoint = "line")
  plot(control.plot)
  
  gf_control.plot <- plotTimeSeries(data = selected_unique_control_data.df,
                                 xVar = timestamp,
                                 yVar = gf_variable,
                                 xLab = "time", 
                                 yLab = gf_variable, 
                                 lineOrPoint = "line")
  plot(gf_control.plot)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/soil_moisture/time_series_control_", variable, "_",
             min(selected_unique_control_data.df$date), "-", max(selected_unique_control_data.df$date),  ".pdf"))
  plot(ggarrange(control.plot, 
                 gf_control.plot, 
                 nrow = 2))
  dev.off()
  
  selected_unique_control_data.df$variable <- NULL
  selected_unique_control_data.df$gf_variable <- NULL
}
selected_unique_control_data.df$yday_hour <- NULL

selected_unique_control_data.df <- selected_unique_control_data.df %>% 
  arrange(timestamp)


#### save ####

# project folder

write_csv(selected_unique_control_data.df, 
          paste0("data_processed/soil_moisture/complete_datasets/control_hourly_soil_water_content_", min(selected_unique_control_data.df$date), "_", max(selected_unique_control_data.df$date), ".csv"))

# general folder

write_csv(selected_unique_control_data.df, 
          paste0("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data_processed/soil_moisture/pablo/wc_time_series/control_hourly_soil_water_content_", min(selected_unique_control_data.df$date), "_", max(selected_unique_control_data.df$date), ".csv"))


### TFE ####

files_path <- list.files("data_processed/soil_moisture", "tfe", full.names = T)

tfe_data.list <- lapply(files_path, 
                            read_csv)

tfe_data.df <- do.call(bind_rows, tfe_data.list)
tail(tfe_data.df)
length(unique(tfe_data.df$timestamp))

## aggregate per timestamp (ensure we have one record per timestamp)

unique_tfe_data.df <- aggregate(tfe_data.df, by = list(tfe_data.df$timestamp), FUN = meanOrMode) %>%
  select(timestamp, date, record, vwc_sup_m3_m3, vwc_50cm_m3_m3, vwc_100cm_m3_m3, vwc_250cm_m3_m3, vwc_400cm_m3_m3)
head(unique_tfe_data.df)
tail(unique_tfe_data.df)
length(unique_tfe_data.df$timestamp)
length(unique(unique_tfe_data.df$timestamp))


## calibrated values (only for latter data? ask if the previous one was already calibrated, looks like it)

old_selected_unique_tfe_data.df <- unique_tfe_data.df %>%
  filter(date < "2022-12-31") %>%
  select(timestamp, date, vwc_sup_m3_m3, vwc_50cm_m3_m3, vwc_100cm_m3_m3, vwc_250cm_m3_m3, vwc_400cm_m3_m3)

new_selected_unique_tfe_data.df <- unique_tfe_data.df %>%
  filter(date >= "2022-12-31") %>%
  mutate(vwc_sup_m3_m3 = (vwc_sup_m3_m3 * 1.0229) + 0.0701,
         vwc_50cm_m3_m3 = (vwc_50cm_m3_m3 * 1.0229) + 0.0701,
         vwc_100cm_m3_m3 = (vwc_100cm_m3_m3 * 1.0229) + 0.0701,
         vwc_250cm_m3_m3 = (vwc_250cm_m3_m3 * 1.0229) + 0.0701,
         vwc_400cm_m3_m3 = (vwc_400cm_m3_m3 * 1.0229) + 0.0701) %>%
  select(timestamp, date, vwc_sup_m3_m3, vwc_50cm_m3_m3, vwc_100cm_m3_m3, vwc_250cm_m3_m3, vwc_400cm_m3_m3)

selected_unique_tfe_data.df <- bind_rows(old_selected_unique_tfe_data.df,
                                             new_selected_unique_tfe_data.df)

### gap fill and plot ####

selected_unique_tfe_data.df <- merge(timestamp_backbone, 
                                         selected_unique_tfe_data.df, 
                                         by = "timestamp", all.x = T) %>%
  mutate(yday_hour = paste0(yday(timestamp), " ", as_hms(round_date(as_datetime(timestamp), "hour"))),
         date = as_date(timestamp)) %>%
  select(yday_hour, everything())

variable <- names(selected_unique_tfe_data.df)[c(-1, -2, -3)][1]
for(variable in names(selected_unique_tfe_data.df)[c(-1, -2, -3)]){
  print(variable)
  
  ## clean outliers
  
  mean_wc <- mean(selected_unique_tfe_data.df[, variable], na.rm = T)
  sd_wc <- sd(selected_unique_tfe_data.df[, variable], na.rm = T)
  
  upper_threshold <- mean_wc + (sd_wc * 3)
  lower_threshold <- mean_wc - (sd_wc * 3)
  
  selected_unique_tfe_data.df[which(selected_unique_tfe_data.df[, variable] > upper_threshold), variable] <- NA
  selected_unique_tfe_data.df[which(selected_unique_tfe_data.df[, variable] < lower_threshold), variable] <- NA
  
  # delete too low values (errors)
  selected_unique_tfe_data.df[which(selected_unique_tfe_data.df[, variable] < 0.1), variable] <- NA
  
  ## gap filling (mean of the time for that month and individual)
  
  selected_unique_tfe_data.df[, paste0("gf_", variable)] <- selected_unique_tfe_data.df[, variable]
  
  hour_mean_vwc <- aggregate(selected_unique_tfe_data.df[, paste0("gf_", variable)], 
                             by = list("yday_hour" = selected_unique_tfe_data.df$yday_hour), 
                             FUN = mean, 
                             na.rm = T)
  names(hour_mean_vwc)[2] <- paste0("gf_", variable)
  
  selected_unique_tfe_data.df <- merge(selected_unique_tfe_data.df, hour_mean_vwc, 
                                           by = "yday_hour", 
                                           all.x = T)
  
  selected_unique_tfe_data.df <- combineData(data = selected_unique_tfe_data.df, 
                                                 variablesToCombine = paste0("gf_", variable))
  
  
  ## plot
  gf_variable <- paste0("gf_", variable)
  selected_unique_tfe_data.df$variable <- selected_unique_tfe_data.df[, variable]
  selected_unique_tfe_data.df$gf_variable <- selected_unique_tfe_data.df[,  paste0("gf_", variable)]
  
  control.plot <- plotTimeSeries(data = selected_unique_tfe_data.df,
                                 xVar = timestamp,
                                 yVar = variable,
                                 xLab = "time", 
                                 yLab = variable, 
                                 lineOrPoint = "line")
  plot(control.plot)
  
  gf_control.plot <- plotTimeSeries(data = selected_unique_tfe_data.df,
                                    xVar = timestamp,
                                    yVar = gf_variable,
                                    xLab = "time", 
                                    yLab = gf_variable, 
                                    lineOrPoint = "line")
  plot(gf_control.plot)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/soil_moisture/time_series_tfe_", variable, "_",
             min(selected_unique_tfe_data.df$date), "-", max(selected_unique_tfe_data.df$date),  ".pdf"))
  plot(ggarrange(control.plot, 
                 gf_control.plot, 
                 nrow = 2))
  dev.off()
  
  selected_unique_tfe_data.df$variable <- NULL
  selected_unique_tfe_data.df$gf_variable <- NULL
}
selected_unique_tfe_data.df$yday_hour <- NULL

tail(selected_unique_tfe_data.df)

selected_unique_tfe_data.df <- selected_unique_tfe_data.df %>% 
  arrange(timestamp)


#### save ####

# project folder

write_csv(selected_unique_tfe_data.df, 
          paste0("data_processed/soil_moisture/complete_datasets/tfe_hourly_soil_water_content_", min(selected_unique_tfe_data.df$date), "_", max(selected_unique_tfe_data.df$date), ".csv"))

# general folder
write_csv(selected_unique_tfe_data.df, 
          paste0("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data_processed/soil_moisture/pablo/wc_time_series/tfe_hourly_soil_water_content_", min(selected_unique_tfe_data.df$date), "_", max(selected_unique_tfe_data.df$date), ".csv"))


#### AGGREGATE PER DAY --------------------------------------------------------- ####

### control ####

selected_unique_control_data.df <- read_csv("data_processed/soil_moisture/complete_datasets/control_hourly_soil_water_content_2000-01-01_2024-05-27.csv")

toDaily_selected_unique_control_data.df <- selected_unique_control_data.df %>%
  select(-timestamp)
head(toDaily_selected_unique_control_data.df)
daily_control_data.df <- aggregate(toDaily_selected_unique_control_data.df,
                                   by = list(toDaily_selected_unique_control_data.df$date),
                                   FUN = meanOrMode) %>%
  # rename(date = Group.1) %>%
  # mutate(date = ymd(date)) %>%
  select(-date) %>%
  select(date = Group.1, everything())

head(daily_control_data.df)
tail(daily_control_data.df)

#### save ####

# project folder

write_csv(daily_control_data.df, 
          paste0("data_processed/soil_moisture/complete_datasets/control_daily_soil_water_content_", min(daily_control_data.df$date), "_", max(daily_control_data.df$date), ".csv"))

# general folder

write_csv(daily_control_data.df, 
          paste0("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data_processed/soil_moisture/pablo/wc_time_series/control_daily_soil_water_content_", min(daily_control_data.df$date), "_", max(daily_control_data.df$date), ".csv"))


### TFE ####

selected_unique_tfe_data.df <- read_csv("data_processed/soil_moisture/complete_datasets/tfe_hourly_soil_water_content_2000-01-01_2024-05-27.csv")

toDaily_selected_unique_tfe_data.df <- selected_unique_tfe_data.df %>%
  select(-timestamp)

daily_tfe_data.df <- aggregate(toDaily_selected_unique_tfe_data.df,
                                   by = list(toDaily_selected_unique_tfe_data.df$date),
                                   FUN = meanOrMode) %>%
  # rename(date = Group.1) %>%
  # mutate(date = ymd(date)) %>%
  select(-date) %>%
  select(date = Group.1, everything())

head(daily_tfe_data.df)
tail(daily_tfe_data.df)


#### save ####

# project folder

write_csv(daily_tfe_data.df, 
          paste0("data_processed/soil_moisture/complete_datasets/tfe_daily_soil_water_content_", min(daily_tfe_data.df$date), "_", max(daily_tfe_data.df$date), ".csv"))

# general folder
write_csv(daily_tfe_data.df, 
          paste0("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data_processed/soil_moisture/pablo/wc_time_series/tfe_daily_soil_water_content_", min(daily_tfe_data.df$date), "_", max(daily_tfe_data.df$date), ".csv"))


#### SUBDAILY DATA PLOTTING ---------------------------------------------------- ####

selected_unique_control_data.df <- read_csv("data_processed/soil_moisture/complete_datasets/control_hourly_soil_water_content_2000-01-01_2024-05-27.csv") %>%
  mutate(plot = "Control")

selected_unique_tfe_data.df <- read_csv("data_processed/soil_moisture/complete_datasets/tfe_hourly_soil_water_content_2000-01-01_2024-05-27.csv") %>%
  mutate(plot = "TFE")

all_soil_vwc.df <- bind_rows(selected_unique_control_data.df, selected_unique_tfe_data.df)

  
# vwc_sup_m3_m3
vwc_sup_m3_m3 <- plotTimeSeries(data = all_soil_vwc.df,
                             xVar = timestamp,
                             yVar = vwc_sup_m3_m3,
                             xLab = "", 
                             yLab = "surface vwc (m3/m3)", 
                             lineOrPoint = "line", 
                             colorVar = plot)

gf_vwc_sup_m3_m3 <- plotTimeSeries(data = all_soil_vwc.df,
                                xVar = timestamp,
                                yVar = gf_vwc_sup_m3_m3,
                                xLab = "", 
                                yLab = "gf surface vwc (m3/m3)", 
                                lineOrPoint = "line", 
                                colorVar = plot)
# Save the plot
pdf(paste0("outputs/data_plots/soil_moisture/hourly/soil_vwc_sup_m3_m3.pdf"))
p <- ggarrange(vwc_sup_m3_m3,
                 gf_vwc_sup_m3_m3,
                 ncol = 1, nrow = 2, legend = "bottom", common.legend = T)
plot(p)
dev.off()
  
# vwc_50cm_m3_m3
vwc_50cm_m3_m3 <- plotTimeSeries(data = all_soil_vwc.df,
                                xVar = timestamp,
                                yVar = vwc_50cm_m3_m3,
                                xLab = "", 
                                yLab = "vwc_50cm_m3_m3", 
                                lineOrPoint = "line", 
                                colorVar = plot)

gf_vwc_50cm_m3_m3 <- plotTimeSeries(data = all_soil_vwc.df,
                                   xVar = timestamp,
                                   yVar = gf_vwc_50cm_m3_m3,
                                   xLab = "", 
                                   yLab = "gf vwc_50cm_m3_m3", 
                                   lineOrPoint = "line", 
                                   colorVar = plot)
# Save the plot
pdf(paste0("outputs/data_plots/soil_moisture/hourly/soil_vwc_50cm_m3_m3.pdf"))
p <- ggarrange(vwc_50cm_m3_m3,
               gf_vwc_50cm_m3_m3,
               ncol = 1, nrow = 2, legend = "bottom", common.legend = T)
plot(p)
dev.off()

# vwc_100cm_m3_m3
vwc_100cm_m3_m3 <- plotTimeSeries(data = all_soil_vwc.df,
                                 xVar = timestamp,
                                 yVar = vwc_100cm_m3_m3,
                                 xLab = "", 
                                 yLab = "vwc_100cm_m3_m3", 
                                 lineOrPoint = "line", 
                                 colorVar = plot)

gf_vwc_100cm_m3_m3 <- plotTimeSeries(data = all_soil_vwc.df,
                                    xVar = timestamp,
                                    yVar = gf_vwc_100cm_m3_m3,
                                    xLab = "", 
                                    yLab = "gf vwc_100cm_m3_m3", 
                                    lineOrPoint = "line", 
                                    colorVar = plot)
# Save the plot
pdf(paste0("outputs/data_plots/soil_moisture/hourly/soil_vwc_100cm_m3_m3.pdf"))
p <- ggarrange(vwc_100cm_m3_m3,
               gf_vwc_100cm_m3_m3,
               ncol = 1, nrow = 2, legend = "bottom", common.legend = T)
plot(p)
dev.off()


# vwc_250cm_m3_m3
vwc_250cm_m3_m3 <- plotTimeSeries(data = all_soil_vwc.df,
                                  xVar = timestamp,
                                  yVar = vwc_250cm_m3_m3,
                                  xLab = "", 
                                  yLab = "vwc_250cm_m3_m3", 
                                  lineOrPoint = "line", 
                                  colorVar = plot)

gf_vwc_250cm_m3_m3 <- plotTimeSeries(data = all_soil_vwc.df,
                                     xVar = timestamp,
                                     yVar = gf_vwc_250cm_m3_m3,
                                     xLab = "", 
                                     yLab = "gf vwc_250cm_m3_m3", 
                                     lineOrPoint = "line", 
                                     colorVar = plot)
# Save the plot
pdf(paste0("outputs/data_plots/soil_moisture/hourly/soil_vwc_250cm_m3_m3.pdf"))
p <- ggarrange(vwc_250cm_m3_m3,
               gf_vwc_250cm_m3_m3,
               ncol = 1, nrow = 2, legend = "bottom", common.legend = T)
plot(p)
dev.off()

# vwc_400cm_m3_m3
vwc_400cm_m3_m3 <- plotTimeSeries(data = all_soil_vwc.df,
                                  xVar = timestamp,
                                  yVar = vwc_400cm_m3_m3,
                                  xLab = "", 
                                  yLab = "vwc_400cm_m3_m3", 
                                  lineOrPoint = "line", 
                                  colorVar = plot)

gf_vwc_400cm_m3_m3 <- plotTimeSeries(data = all_soil_vwc.df,
                                     xVar = timestamp,
                                     yVar = gf_vwc_400cm_m3_m3,
                                     xLab = "", 
                                     yLab = "gf vwc_400cm_m3_m3", 
                                     lineOrPoint = "line", 
                                     colorVar = plot)
# Save the plot
pdf(paste0("outputs/data_plots/soil_moisture/hourly/soil_vwc_400cm_m3_m3.pdf"))
p <- ggarrange(vwc_400cm_m3_m3,
               gf_vwc_400cm_m3_m3,
               ncol = 1, nrow = 2, legend = "bottom", common.legend = T)
plot(p)
dev.off()


#### DAILY DATA PLOTTING ------------------------------------------------------ ####

daily_control_data.df <- read_csv("data_processed/soil_moisture/complete_datasets/control_daily_soil_water_content_2000-01-01_2024-05-27.csv") %>%
  mutate(plot = "Control")

daily_tfe_data.df <- read_csv("data_processed/soil_moisture/complete_datasets/tfe_daily_soil_water_content_2000-01-01_2024-05-27.csv") %>%
  mutate(plot = "TFE")

daily_all_soil_vwc.df <- bind_rows(daily_control_data.df, daily_tfe_data.df)
  
# vwc_sup_m3_m3
vwc_sup_m3_m3 <- plotTimeSeries(data = daily_all_soil_vwc.df,
                                xVar = date,
                                yVar = vwc_sup_m3_m3,
                                xLab = "", 
                                yLab = "surface vwc (m3/m3)", 
                                lineOrPoint = "line", 
                                colorVar = plot)

gf_vwc_sup_m3_m3 <- plotTimeSeries(data = daily_all_soil_vwc.df,
                                   xVar = date,
                                   yVar = gf_vwc_sup_m3_m3,
                                   xLab = "", 
                                   yLab = "gf surface vwc (m3/m3)", 
                                   lineOrPoint = "line", 
                                   colorVar = plot)
# Save the plot
pdf(paste0("outputs/data_plots/soil_moisture/daily/soil_vwc_sup_m3_m3.pdf"))
p <- ggarrange(vwc_sup_m3_m3,
               gf_vwc_sup_m3_m3,
               ncol = 1, nrow = 2, legend = "bottom", common.legend = T)
plot(p)
dev.off()

# vwc_50cm_m3_m3
vwc_50cm_m3_m3 <- plotTimeSeries(data = daily_all_soil_vwc.df,
                                 xVar = date,
                                 yVar = vwc_50cm_m3_m3,
                                 xLab = "", 
                                 yLab = "vwc_50cm_m3_m3", 
                                 lineOrPoint = "line", 
                                 colorVar = plot)

gf_vwc_50cm_m3_m3 <- plotTimeSeries(data = daily_all_soil_vwc.df,
                                    xVar = date,
                                    yVar = gf_vwc_50cm_m3_m3,
                                    xLab = "", 
                                    yLab = "gf vwc_50cm_m3_m3", 
                                    lineOrPoint = "line", 
                                    colorVar = plot)
# Save the plot
pdf(paste0("outputs/data_plots/soil_moisture/daily/soil_vwc_50cm_m3_m3.pdf"))
p <- ggarrange(vwc_50cm_m3_m3,
               gf_vwc_50cm_m3_m3,
               ncol = 1, nrow = 2, legend = "bottom", common.legend = T)
plot(p)
dev.off()

# vwc_100cm_m3_m3
vwc_100cm_m3_m3 <- plotTimeSeries(data = daily_all_soil_vwc.df,
                                  xVar = date,
                                  yVar = vwc_100cm_m3_m3,
                                  xLab = "", 
                                  yLab = "vwc_100cm_m3_m3", 
                                  lineOrPoint = "line", 
                                  colorVar = plot)

gf_vwc_100cm_m3_m3 <- plotTimeSeries(data = daily_all_soil_vwc.df,
                                     xVar = date,
                                     yVar = gf_vwc_100cm_m3_m3,
                                     xLab = "", 
                                     yLab = "gf vwc_100cm_m3_m3", 
                                     lineOrPoint = "line", 
                                     colorVar = plot)
# Save the plot
pdf(paste0("outputs/data_plots/soil_moisture/daily/soil_vwc_100cm_m3_m3.pdf"))
p <- ggarrange(vwc_100cm_m3_m3,
               gf_vwc_100cm_m3_m3,
               ncol = 1, nrow = 2, legend = "bottom", common.legend = T)
plot(p)
dev.off()


# vwc_250cm_m3_m3
vwc_250cm_m3_m3 <- plotTimeSeries(data = daily_all_soil_vwc.df,
                                  xVar = date,
                                  yVar = vwc_250cm_m3_m3,
                                  xLab = "", 
                                  yLab = "vwc_250cm_m3_m3", 
                                  lineOrPoint = "line", 
                                  colorVar = plot)

gf_vwc_250cm_m3_m3 <- plotTimeSeries(data = daily_all_soil_vwc.df,
                                     xVar = date,
                                     yVar = gf_vwc_250cm_m3_m3,
                                     xLab = "", 
                                     yLab = "gf vwc_250cm_m3_m3", 
                                     lineOrPoint = "line", 
                                     colorVar = plot)
# Save the plot
pdf(paste0("outputs/data_plots/soil_moisture/daily/soil_vwc_250cm_m3_m3.pdf"))
p <- ggarrange(vwc_250cm_m3_m3,
               gf_vwc_250cm_m3_m3,
               ncol = 1, nrow = 2, legend = "bottom", common.legend = T)
plot(p)
dev.off()

# vwc_400cm_m3_m3
vwc_400cm_m3_m3 <- plotTimeSeries(data = daily_all_soil_vwc.df,
                                  xVar = date,
                                  yVar = vwc_400cm_m3_m3,
                                  xLab = "", 
                                  yLab = "vwc_400cm_m3_m3", 
                                  lineOrPoint = "line", 
                                  colorVar = plot)

gf_vwc_400cm_m3_m3 <- plotTimeSeries(data = daily_all_soil_vwc.df,
                                     xVar = date,
                                     yVar = gf_vwc_400cm_m3_m3,
                                     xLab = "", 
                                     yLab = "gf vwc_400cm_m3_m3", 
                                     lineOrPoint = "line", 
                                     colorVar = plot)
# Save the plot
pdf(paste0("outputs/data_plots/soil_moisture/daily/soil_vwc_400cm_m3_m3.pdf"))
p <- ggarrange(vwc_400cm_m3_m3,
               gf_vwc_400cm_m3_m3,
               ncol = 1, nrow = 2, legend = "bottom", common.legend = T)
plot(p)
dev.off()


