#### METEOROLOGICAL DATA PROCESSING EXAMPLE ####################################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")

## It is important to have each one of the plots data in different folders. So first of all we need to determine which tower is the data coming from.

### Determine whether it is control or TFE tower

notIdentified <- dataIdentificator(folderIn = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/raw_data_in/",
                                   folderOutA = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/control/",
                                   folderOutB = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/tfe/")

write_delim(as.data.frame(notIdentified), "raw_data_not_automatically_identified.csv")

### FETCH CONTROL TOWER DATA --------------------------------------------------- ####

#### STEP 1: set the location of the original data to process and the files where we want the output to be stored ####

## raw_folder_in: is the complete path to the folder where the original data is stored. It is important to have all the files that we want
## to process together in the same folder (e.g., all the ems81 loggers files in the same folder).

## processed_file_out: location and name of the file where we want the processed data to be stored.

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/control"
processed_folder_out <- paste0("data_processed/met/")


#### STEP 2: apply the functions to fetch the original data and process it ####

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

met_variables.names <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/caxiuana_variables_abbreviation.xlsx") %>%
  select(abbreviation_raw, abbreviation_processed) %>%
  na.omit()

selected_control_met_all_lba.df <- control_met_all_lba.df %>%
  select(any_of(met_variables.names$abbreviation_raw))
head(selected_control_met_all_lba.df)

selected_control_met_infra.df <- control_met_infra.df %>%
  select(any_of(met_variables.names$abbreviation_raw))
head(selected_control_met_infra.df)

# Merge loggers data

control_met.df <- merge(selected_control_met_all_lba.df, selected_control_met_infra.df, by = "timestamp", all = T) # suffixes = c("_infra", "_lba")
names(control_met.df)

# Combine repeated variables

variablesToCombine <- str_remove(names(control_met.df)[str_detect(names(control_met.df), ".y")], ".y")

combined_control_met.df <- combineData(data = control_met.df, variablesToCombine = variablesToCombine)

# Rename final dataset

raw_processed_names <- met_variables.names %>%
  filter(abbreviation_raw %in% names(combined_control_met.df))

named_combined_control_met.df <- data.table::setnames(combined_control_met.df, old = raw_processed_names$abbreviation_raw, 
         new = raw_processed_names$abbreviation_processed)
head(combined_control_met.df)

# add year variable to see which years are represented

named_combined_control_met.df$year <- year(named_combined_control_met.df$timestamp)

unique(named_combined_control_met.df$year) # 2017 to 2023

# Aggregate by datetime to make sure we only have one observation per time step

unique_named_combined_control_met.df <- aggregate(named_combined_control_met.df, by = list(named_combined_control_met.df$timestamp), FUN = mean, na.rm = T) %>%
  select(timestamp, year, everything(), -Group.1)

# save

write_csv(unique_named_combined_control_met.df, paste0(processed_folder_out,
                                                       min(unique_named_combined_control_met.df$year), "-", max(unique_named_combined_control_met.df$year), 
                                                       "_met_control_processed.csv"))


#### STEP 3: data visualization ####

control_met.df <- as.data.frame(read_csv(paste0(processed_folder_out,
                                  min(unique_named_combined_control_met.df$year), "-", max(unique_named_combined_control_met.df$year), 
                                  "_met_control_processed.csv")))

for(variable in names(control_met.df)[c(-1, -2)]){
  
  control_met.df$variable <- control_met.df[, variable]
  
  control.plot <- plotTimeSeries(data = control_met.df,
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
  
  control_met.df$variable <- NULL
}

## repeat for TFE plot


### FETCH TFE TOWER DATA --------------------------------------------------- ####

#### STEP 1: set the location of the original data to process and the files where we want the output to be stored ####

## raw_folder_in: is the complete path to the folder where the original data is stored. It is important to have all the files that we want
## to process together in the same folder (e.g., all the ems81 loggers files in the same folder).

## processed_file_out: location and name of the file where we want the processed data to be stored.

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/tfe"
processed_folder_out <- paste0("data_processed/met/")


#### STEP 2: apply the functions to fetch the original data and process it ####

# Here we need to separate the two types of loggers, as they have different variables: infra and lba. We merge them later.
tfe_met_all.df <- data.frame()

for(file in list.files(raw_folder_in, pattern = ".dat", full.names = T)){

    tfe_met.df <- fetchMet(file = file,
                                     # fileOut = processed_file_out,
                                     plot = "TFE")
    
    tfe_met_all.df <- bind_rows(tfe_met_all.df, tfe_met.df) %>%
      arrange(timestamp)
}


# Select meteorological variables only

met_variables.names <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/caxiuana_variables_abbreviation.xlsx") %>%
  select(abbreviation_raw, abbreviation_processed) %>%
  na.omit() #%>%
  # mutate(abbreviation_raw = tolower(abbreviation_raw))


names(tfe_met_all.df) <- tolower(names(tfe_met_all.df))
selected_tfe_met_all.df <- tfe_met_all.df %>%
  select(any_of(met_variables.names$abbreviation_raw))
head(selected_tfe_met_all.df)


# Rename final dataset

raw_processed_names <- met_variables.names %>%
  filter(abbreviation_raw %in% names(selected_tfe_met_all.df))

named_selected_tfe_met_all.df <- data.table::setnames(selected_tfe_met_all.df, old = raw_processed_names$abbreviation_raw, 
                                                      new = raw_processed_names$abbreviation_processed)
head(named_selected_tfe_met_all.df)

# add year variable to see which years are represented

named_selected_tfe_met_all.df$year <- year(named_selected_tfe_met_all.df$timestamp)

unique(named_selected_tfe_met_all.df$year) # 2019 to 2023

# Aggregate by datetime to make sure we only have one observation per time step

unique_named_selected_tfe_met_all.df <- aggregate(named_selected_tfe_met_all.df, by = list(named_selected_tfe_met_all.df$timestamp), FUN = mean, na.rm = T) %>%
  select(timestamp, year, everything(), -Group.1) %>%
  filter(year > 2000)


# save

write_csv(unique_named_selected_tfe_met_all.df, paste0(processed_folder_out,
                                                       min(unique_named_selected_tfe_met_all.df$year), "-", max(unique_named_selected_tfe_met_all.df$year), 
                                                       "_met_tfe_processed.csv"))


#### STEP 3: data visualization ####

tfe_met.df <- as.data.frame(read_csv(paste0(processed_folder_out,
                                                min(unique_named_selected_tfe_met_all.df$year), "-", max(unique_named_selected_tfe_met_all.df$year), 
                                                "_met_tfe_processed.csv")))

for(variable in names(tfe_met.df)[c(-1, -2)]){
  
  tfe_met.df$variable <- tfe_met.df[, variable]
  
  control.plot <- plotTimeSeries(data = tfe_met.df,
                                 xVar = timestamp,
                                 yVar = variable,
                                 xLab = "time", 
                                 yLab = variable, 
                                 lineOrPoint = "line")
  plot(control.plot)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/met/tfe_", variable, ".pdf"))
  plot(control.plot)
  dev.off()
  
  tfe_met.df$variable <- NULL
}

