#### SOIL DATA PROCESSING ######################################################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")

## It is important to have each one of the plots data in different folders. So first of all we need to determine which pits is the data coming from.

### Determine whether it is control or TFE pits

notIdentified <- soilDataIdentificator(folderIn = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/29-09-2023/",
                                   folderOutA = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/29-09-2023/control/",
                                   folderOutB = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/29-09-2023/tfe/")

if(!is.null(notIdentified)){
  print("Some plot data cannot be identified")
  write_delim(as.data.frame(notIdentified), "raw_data_not_automatically_identified.csv")
}


### FETCH CONTROL PITS DATA ---------------------------------------------------- ####

#### STEP 1: set the location of the original data to process and the files where we want the output to be stored ####

## raw_folder_in: is the complete path to the folder where the original data is stored. It is important to have all the files that we want
## to process together in the same folder (e.g., all the ems81 loggers files in the same folder).

## processed_file_out: location and name of the file where we want the processed data to be stored.

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/29-09-2023/control/"
processed_folder_out <- paste0("data_processed/soil_moisture/")


#### STEP 2: apply the functions to fetch the original data and process it ####

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
names(control_soil.df)

# Rename final dataset (TO DO)

# raw_processed_names <- met_variables.names %>%
#   filter(abbreviation_raw %in% names(combined_control_soil.df))
# 
# named_combined_control_soil.df <- data.table::setnames(combined_control_soil.df, old = raw_processed_names$abbreviation_raw, 
#                                                       new = raw_processed_names$abbreviation_processed)
# head(combined_control_soil.df)

# add year variable to see which years are represented

control_soil.df$year <- year(control_soil.df$timestamp)

unique(control_soil.df$year) # 2019 to 2023

# Aggregate by datetime to make sure we only have one observation per time step

unique_control_soil.df <- aggregate(control_soil.df, 
                                   by = list(control_soil.df$timestamp), 
                                   FUN = mean, 
                                   na.rm = T) %>%
  select(timestamp, year, everything(), -Group.1)


# save

write_csv(unique_control_soil.df, paste0(processed_folder_out,
                                        min(unique_control_soil.df$year), "-", max(unique_control_soil.df$year), 
                                        "_soil_control_processed.csv"))


#### STEP 3: data visualization ####

control_soil.df <- as.data.frame(read_csv(paste0(processed_folder_out,
                                                min(unique_control_soil.df$year), "-", max(unique_control_soil.df$year), 
                                                "_soil_control_processed.csv")))

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


### FETCH TFE PITS DATA ---------------------------------------------------- ####

#### STEP 1: set the location of the original data to process and the files where we want the output to be stored ####

## raw_folder_in: is the complete path to the folder where the original data is stored. It is important to have all the files that we want
## to process together in the same folder (e.g., all the ems81 loggers files in the same folder).

## processed_file_out: location and name of the file where we want the processed data to be stored.

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_soil_moisture/29-09-2023/tfe/"
processed_folder_out <- paste0("data_processed/soil_moisture/")


#### STEP 2: apply the functions to fetch the original data and process it ####

# Here we need to separate the two types of loggers, as they have different variables: infra and lba. We merge them later.
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

# Rename final dataset (TO DO)

# raw_processed_names <- met_variables.names %>%
#   filter(abbreviation_raw %in% names(combined_tfe_soil.df))
# 
# named_combined_tfe_soil.df <- data.table::setnames(combined_tfe_soil.df, old = raw_processed_names$abbreviation_raw, 
#                                                       new = raw_processed_names$abbreviation_processed)
# head(combined_tfe_soil.df)

# add year variable to see which years are represented

tfe_soil.df$year <- year(tfe_soil.df$timestamp)

unique(tfe_soil.df$year) # 2022 to 2023

# Aggregate by datetime to make sure we only have one observation per time step

unique_tfe_soil.df <- aggregate(tfe_soil.df, 
                                    by = list(tfe_soil.df$timestamp), 
                                    FUN = mean, 
                                    na.rm = T) %>%
  select(timestamp, year, everything(), -Group.1)


# save

write_csv(unique_tfe_soil.df, paste0(processed_folder_out,
                                         min(unique_tfe_soil.df$year), "-", max(unique_tfe_soil.df$year), 
                                         "_soil_tfe_processed.csv"))


#### STEP 3: data visualization ####

tfe_soil.df <- as.data.frame(read_csv(paste0(processed_folder_out,
                                                 min(unique_tfe_soil.df$year), "-", max(unique_tfe_soil.df$year), 
                                                 "_soil_tfe_processed.csv")))

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

