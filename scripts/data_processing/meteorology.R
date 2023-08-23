#### METEOROLOGICAL DATA PROCESSING EXAMPLE ####################################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")

### FETCH CONTROL TOWER DATA --------------------------------------------------- ####

## It is important to have ech one of the plots data in different folders.

#### STEP 1: set the location of the original data to process and the files where we want the output to be stored ####

## raw_folder_in: is the complete path to the folder where the original data is stored. It is important to have all the files that we want
## to process together in the same folder (e.g., all the ems81 loggers files in the same folder).

## processed_file_out: location and name of the file where we want the processed data to be stored.

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/raw_data/control/"
processed_file_out <- paste0("data_processed/met/processed_met_control_", Sys.Date(), ".csv")


#### STEP 2: apply the functions to fetch the original data and process it ####

control_met.df <- fetchMet(folderIn = raw_folder_in,
                   fileOut = processed_file_out,
                   plot = "Control")
head(control_met.df)


#### STEP 3: data visualization ####

for(variable in names(control_met.df)[c(-1, -2, -3)]){
  
  control_met.df$variable <- control_met.df[, variable]
  
  control.plot <- plotTimeSeries(data = control_met.df,
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
