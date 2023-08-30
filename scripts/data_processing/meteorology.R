#### METEOROLOGICAL DATA PROCESSING EXAMPLE ####################################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")

### FETCH CONTROL TOWER DATA --------------------------------------------------- ####

## It is important to have each one of the plots data in different folders. So first of all we need to determine which tower is the data coming from.

### Determine whether it is control or TFE tower

determineMetTower(folderIn = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/",
                  folderOutA = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/control/",
                  folderOutB = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/tfe/")

# To remove original files
# remove.files(list.files("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/", pattern = ".dat"))

#### STEP 1: set the location of the original data to process and the files where we want the output to be stored ####

## raw_folder_in: is the complete path to the folder where the original data is stored. It is important to have all the files that we want
## to process together in the same folder (e.g., all the ems81 loggers files in the same folder).

## processed_file_out: location and name of the file where we want the processed data to be stored.

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/control"
processed_file_out <- paste0("data_processed/met/met_control_2020-2023_processed", ".csv")


#### STEP 2: apply the functions to fetch the original data and process it ####

# Here we need to separate the two types of loggers, as they have different variables: infra and lba. We merge them later.
control_met_all_infra.df <- data.frame()
control_met_all_lba.df <- data.frame()

for(file in list.files(raw_folder_in, pattern = ".dat", full.names = T)){
  
  if(str_detect(file, "INFRA")){
    control_met_infra.df <- fetchMet(file = file,
                               # fileOut = processed_file_out,
                               plot = "Control")
    
    control_met_all_infra.df <- rbind(control_met_all_infra.df, control_met_infra.df) %>%
      arrange(timestamp)
  } else {
    if(str_detect(file, "LB")){
      control_met_lba.df <- fetchMet(file = file,
                                     # fileOut = processed_file_out,
                                     plot = "Control")
      
      control_met_all_lba.df <- rbind(control_met_all_lba.df, control_met_lba.df) %>%
        arrange(timestamp)
    } else{
     print("logger not specified in file name")
    }

  }
}

# Merge loggers data

control_met.df <- merge(control_met_infra.df, control_met_all_lba.df, by = "timestamp", all = T, suffixes = c("_infra", "_lba"))
names(control_met.df)

# save

write_csv(control_met.df, processed_file_out)


#### STEP 3: data visualization ####

for(variable in names(control_met.df)[c(-1, -2, -3)]){
  
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

#### TFE STEP 1 ####

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/tfe/"
processed_file_out <- paste0("data_processed/met/processed_met_tfe_", Sys.Date(), ".csv")


#### TFE STEP 2 ####

tfe_met.df <- fetchMet(folderIn = raw_folder_in,
                   fileOut = processed_file_out,
                   plot = "TFE")
head(tfe_met.df)

#### TFE STEP 3 ####

for(variable in names(tfe_met.df)[c(-1, -2, -3)]){
  
  tfe_met.df$variable <- tfe_met.df[, variable]
  
  control.plot <- plotTimeSeries(data = tfe_met.df,
                                 xVar = timestamp,
                                 yVar = variable,
                                 xLab = "time", 
                                 yLab = variable, 
                                 lineOrPoint = "line")
  control.plot
  
  # Save the plot
  pdf(paste0("outputs/data_plots/met/control_", variable, ".pdf"))
  plot(control.plot)
  dev.off()
}
