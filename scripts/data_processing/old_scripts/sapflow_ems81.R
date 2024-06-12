#### SAPFLOW (EMS81) DATA PROCESSING EXAMPLE ###################################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")

#### STEP 1: set the location of the original data to process and the files where we want the output to be stored ####

## raw_folder_in: is the complete path to the folder where the original data is stored. It is important to have all the files that we want
## to process together in the same folder (e.g., all the ems81 loggers files in the same folder).

## processed_file_out: location and name of the file where we want the processed data to be stored.

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_sapflow/17-10-2023"
processed_file_out <- paste0("data_processed/sapflow/processed_saplfow_", Sys.Date(), ".csv")

#### STEP 2: apply the functions to fetch the original data and process it ####

## This function also calculates the baseline using quantile regressions and then corrects by it. 
## Baseline results are shown as baseline_sap_flow

sf.df <- fetchEMS81(folderIn = raw_folder_in,
                    fileOut = processed_file_out)



#### STEP 3: data visualization ####

### Sap flow all individuals ####

all.plot <- plotTimeSeries(data = sf.df,
                           xVar = timestamp,
                           yVar = sap_flux_kg_h,
                           xLab = "time", 
                           yLab = "sap flow (kg/h)", 
                           lineOrPoint = "line", 
                           colorVar = ID)

# Save the plot
pdf("outputs/data_plots/sapflow/sapflow_all.pdf")
all.plot
dev.off()

### Sap flow control individuals ####

control_data <- sf.df %>% 
  filter(str_detect(ID, "Control"))

control.plot <- plotTimeSeries(data = control_data,
                               xVar = timestamp,
                               yVar = sap_flux_kg_h,
                               xLab = "time", 
                               yLab = "sap flow (kg/h)", 
                               lineOrPoint = "line", 
                               colorVar = ID)

# Save the plot
pdf("outputs/data_plots/sapflow/sapflow_control.pdf")
control.plot
dev.off()


### Sap flow TFE individuals ####

tfe_data <- sf.df %>% 
  filter(str_detect(ID, "TFE"))

tfe.plot <- plotTimeSeries(data = tfe_data,
                           xVar = timestamp,
                           yVar = sap_flux_kg_h,
                           xLab = "time", 
                           yLab = "sap flow (kg/h)", 
                           lineOrPoint = "line", 
                           colorVar = ID)

# Save the plot
pdf("outputs/data_plots/sapflow/sapflow_tfe.pdf")
tfe.plot
dev.off()


### Sap flow individual one by one ####

for(ind in unique(sf.df$ID)){
  
  ind_data <- sf.df %>% 
    filter(ID == ind)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/sapflow/sapflow_", ind, "_", str_replace(unique(ind_data$species), " ", "_"),".pdf"))
  ind.plot <- plotTimeSeries(data = ind_data,
                             xVar = timestamp,
                             yVar = sap_flux_kg_h,
                             xLab = "time", 
                             yLab = "sap flow (kg/h)", 
                             lineOrPoint = "line", 
                             colorVar = ID)
  plot(ind.plot)
  dev.off()
}


### Dendrometer all individuals ####

all.plot <- plotTimeSeries(data = sf.df,
                           xVar = timestamp,
                           yVar = increment_mm,
                           xLab = "time", 
                           yLab = "Increment (mm)", 
                           lineOrPoint = "line", 
                           colorVar = ID)

# Save the plot
pdf("outputs/data_plots/increment/increment_all.pdf")
all.plot
dev.off()


### Dendrometer control individuals ####

control_data <- sf.df %>% 
  filter(str_detect(ID, "Control"))

control.plot <- plotTimeSeries(data = control_data,
                               xVar = timestamp,
                               yVar = increment_mm,
                               xLab = "time", 
                               yLab = "Increment (mm)", 
                               lineOrPoint = "line", 
                               colorVar = ID)

# Save the plot
pdf("outputs/data_plots/increment/increment_control.pdf")
control.plot
dev.off()


### Dendrometer tfe individuals ####

tfe_data <- sf.df %>% 
  filter(str_detect(ID, "TFE"))

tfe.plot <- plotTimeSeries(data = tfe_data,
                           xVar = timestamp,
                           yVar = increment_mm,
                           xLab = "time", 
                           yLab = "Increment (mm)", 
                           lineOrPoint = "line", 
                           colorVar = ID)

# Save the plot
pdf("outputs/data_plots/increment/increment_tfe.pdf")
tfe.plot
dev.off()


### Dendrometer individuals one by one ####

for(ind in unique(sf.df$ID)){
  
  ind_data <- sf.df %>% 
    filter(ID == ind)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/increment/increment_", ind, "_", str_replace(unique(ind_data$species), " ", "_"),".pdf"))
  ind.plot <- plotTimeSeries(data = ind_data,
                             xVar = timestamp,
                             yVar = increment_mm,
                             xLab = "time", 
                             yLab = "Increment (mm)", 
                             lineOrPoint = "line", 
                             colorVar = ID)
  plot(ind.plot)
  dev.off()
}

### Baselined sap flow individual one by one ####

for(ind in unique(sf.df$ID)){
  
  ind_data <- sf.df %>% 
    filter(ID == ind)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/sapflow/baselined_sapflow_", ind, "_", str_replace(unique(ind_data$species), " ", "_"),".pdf"),
      height = h, width = w)
  ind.plot <- plotTimeSeries(data = ind_data,
                             xVar = timestamp,
                             yVar = bl_sap_flux_Kg_h,
                             xLab = "time", 
                             yLab = "corrected sap flow (kg/h)", 
                             lineOrPoint = "line", 
                             colorVar = ID)
  plot(ind.plot)
  dev.off()
}

