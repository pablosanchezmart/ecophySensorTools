#### SAPFLOW (EMS81) DATA PROCESSING FOR 2022 ###################################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")


#### COLLECTED 21-05-2023 ------------------------------------------------------ ####

raw_folder_in_21_05_2023 <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_sapflow/2023_05_21"
# processed_file_out <- paste0("data_processed/sapflow/processed_saplfow_", Sys.Date(), ".csv")
processed_file_out_21_05_2023 <- paste0("data_processed/sapflow/processed_sapflow_2023_05_21.csv")


sf.df <- fetchEMS81(folderIn = raw_folder_in_21_05_2023,
                    fileOut = processed_file_out_21_05_2023)

sf_11__21_05_2023.df <- sf.df %>%
  filter(!is.na(ID)) %>%
  mutate(date = as_date(timestamp)) %>%
  filter(date > "2022-01-01", date < "2024-01-01") %>%
  select(timestamp, ID, species, plot, sap_flux_kg_h, bl_sap_flux_Kg_h, increment_mm)

unique(sf_11__21_05_2023.df$ID)
head(sf_11__21_05_2023.df)
tail(sf_11__21_05_2023.df)
write_csv(sf_11__21_05_2023.df, processed_file_out_21_05_2023)


#### COLLECTED 23-05-2023 ------------------------------------------------------ ####

raw_folder_in_23_05_2023 <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_sapflow/2023_05_23"
# processed_file_out <- paste0("data_processed/sapflow/processed_saplfow_", Sys.Date(), ".csv")
processed_file_out_23_05_2023 <- paste0("data_processed/sapflow/processed_sapflow_2023_05_23.csv")

sf.df <- fetchEMS81(folderIn = raw_folder_in_23_05_2023,
                    fileOut = processed_file_out_23_05_2023)

sf_31_05_2023.df <- sf.df %>%
  filter(!is.na(ID)) %>%
  mutate(date = as_date(timestamp)) %>%
  filter(date > "2022-01-01") %>%
  select(timestamp, ID, species, plot, sap_flux_kg_h, bl_sap_flux_Kg_h, increment_mm)

unique(sf_31_05_2023.df$ID)
head(sf_31_05_2023.df)
tail(sf_31_05_2023.df)
write_csv(sf_31_05_2023.df, processed_file_out_23_05_2023)


#### COLLECTED 28-05-2023 ------------------------------------------------------ ####

raw_folder_in_28_05_2023 <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_sapflow/2023_05_28"
# processed_file_out <- paste0("data_processed/sapflow/processed_saplfow_", Sys.Date(), ".csv")
processed_file_out_28_05_2023 <- paste0("data_processed/sapflow/processed_sapflow_2023_05_28.csv")

sf.df <- fetchEMS81(folderIn = raw_folder_in_28_05_2023,
                    fileOut = processed_file_out_28_05_2023)

sf__28_05_2023.df <- sf.df %>%
  filter(!is.na(ID)) %>%
  mutate(date = as_date(timestamp)) %>%
  filter(date > "2022-01-01") %>%
  select(timestamp, ID, species, plot, sap_flux_kg_h, bl_sap_flux_Kg_h, increment_mm)

unique(sf__28_05_2023.df$ID)
head(sf__28_05_2023.df)
tail(sf__28_05_2023.df)
write_csv(sf__28_05_2023.df, processed_file_out_28_05_2023)


#### COLLECTED 31-05-2023 ------------------------------------------------------ ####

raw_folder_in_31_05_2023 <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_sapflow/2023_05_31"
# processed_file_out <- paste0("data_processed/sapflow/processed_saplfow_", Sys.Date(), ".csv")
processed_file_out_31_05_2023 <- paste0("data_processed/sapflow/processed_sapflow_2023_05_31.csv")

sf.df <- fetchEMS81(folderIn = raw_folder_in_31_05_2023,
                    fileOut = processed_file_out_31_05_2023)

sf_31_05_2023.df <- sf.df %>%
  filter(!is.na(ID)) %>%
  mutate(date = as_date(timestamp)) %>%
  filter(date > "2022-01-01") %>%
  select(timestamp, ID, species, plot, sap_flux_kg_h, bl_sap_flux_Kg_h, increment_mm)

unique(sf_31_05_2023.df$ID)
head(sf_31_05_2023.df)
tail(sf_31_05_2023.df)
write_csv(sf_31_05_2023.df, processed_file_out_31_05_2023)


#### COLLECTED 26-06-2023 ------------------------------------------------------ ####

raw_folder_in_26_06_2023 <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_sapflow/2023_06_26"
# processed_file_out <- paste0("data_processed/sapflow/processed_saplfow_", Sys.Date(), ".csv")
processed_file_out_26_06_2023 <- paste0("data_processed/sapflow/processed_sapflow_2023_06_26.csv")

sf.df <- fetchEMS81(folderIn = raw_folder_in_26_06_2023,
                    fileOut = processed_file_out_26_06_2023)

sf_26_06_2023.df <- sf.df %>%
  filter(!is.na(ID)) %>%
  mutate(date = as_date(timestamp)) %>%
  filter(date > "2022-01-01") %>%
  select(timestamp, ID, species, plot, sap_flux_kg_h, bl_sap_flux_Kg_h, increment_mm)

unique(sf_26_06_2023.df$ID)
head(sf_26_06_2023.df)
tail(sf_26_06_2023.df)
write_csv(sf_26_06_2023.df, processed_file_out_26_06_2023)


#### COLLECTED 09-28-2023 ------------------------------------------------------ ####

raw_folder_in_09_28_2023 <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_sapflow/2023_09_28"
# processed_file_out <- paste0("data_processed/sapflow/processed_saplfow_", Sys.Date(), ".csv")
processed_file_out_09_28_2023 <- paste0("data_processed/sapflow/processed_sapflow_2023_09_28.csv")

sf.df <- fetchEMS81(folderIn = raw_folder_in_09_28_2023,
                    fileOut = processed_file_out_09_28_2023)

sf_09_28_2023.df <- sf.df %>%
  filter(!is.na(ID)) %>%
  mutate(date = as_date(timestamp)) %>%
  filter(date > "2022-01-01") %>%
  select(timestamp, ID, species, plot, sap_flux_kg_h, bl_sap_flux_Kg_h, increment_mm)

unique(sf_09_28_2023.df$ID)
head(sf_09_28_2023.df)
tail(sf_09_28_2023.df)
write_csv(sf_09_28_2023.df, processed_file_out_09_28_2023)


#### COLLECTED 10-17-2023 ------------------------------------------------------ ####

raw_folder_in_10_17_2023 <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_sapflow/2023_10_17"
# processed_file_out <- paste0("data_processed/sapflow/processed_saplfow_", Sys.Date(), ".csv")
processed_file_out_10_17_2023 <- paste0("data_processed/sapflow/processed_saplfow_2023_10_17.csv")

sf.df <- fetchEMS81(folderIn = raw_folder_in_10_17_2023,
                    fileOut = processed_file_out_10_17_2023)

sf_10_17_2023.df <- sf.df %>%
  filter(!is.na(ID)) %>%
  mutate(date = as_date(timestamp)) %>%
  filter(date > "2022-01-01") %>%
  select(timestamp, ID, species, plot, sap_flux_kg_h, bl_sap_flux_Kg_h, increment_mm)

unique(sf_10_17_2023.df$ID)
head(sf_10_17_2023.df)
tail(sf_10_17_2023.df)
write_csv(sf_10_17_2023.df, processed_file_out_10_17_2023)


#### COLLECTED 09-11-2023 ------------------------------------------------------ ####

raw_folder_in_09_11_2023 <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_sapflow/2023_11_09"
# processed_file_out <- paste0("data_processed/sapflow/processed_saplfow_", Sys.Date(), ".csv")
processed_file_out_09_11_2023 <- paste0("data_processed/sapflow/processed_sapflow_2023_11_09.csv")


sf.df <- fetchEMS81(folderIn = raw_folder_in_09_11_2023,
                    fileOut = processed_file_out_09_11_2023)

sf_09_11_2023.df <- sf.df %>%
  filter(!is.na(ID)) %>%
  mutate(date = as_date(timestamp)) %>%
  filter(date > "2022-01-01") %>%
  select(timestamp, ID, species, plot, sap_flux_kg_h, bl_sap_flux_Kg_h, increment_mm)

unique(sf_09_11_2023.df$ID)
head(sf_09_11_2023.df)
tail(sf_09_11_2023.df)
write_csv(sf_09_11_2023.df, processed_file_out_09_11_2023)


#### COLLECTED 14-12-2023 ------------------------------------------------------ ####

raw_folder_in_14_12_2023 <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_sapflow/2023_12_14"
# processed_file_out <- paste0("data_processed/sapflow/processed_saplfow_", Sys.Date(), ".csv")
processed_file_out_14_12_2023 <- paste0("data_processed/sapflow/processed_sapflow_2023_12_14.csv")


sf.df <- fetchEMS81(folderIn = raw_folder_in_14_12_2023,
                    fileOut = processed_file_out_14_12_2023)

sf_14_12_2023.df <- sf.df %>%
  filter(!is.na(ID)) %>%
  mutate(date = as_date(timestamp)) %>%
  filter(date > "2022-01-01") %>%
  select(timestamp, ID, species, plot, sap_flux_kg_h, bl_sap_flux_Kg_h, increment_mm)

unique(sf_14_12_2023.df$ID)
head(sf_14_12_2023.df)
tail(sf_14_12_2023.df)
write_csv(sf_14_12_2023.df, processed_file_out_14_12_2023)


#### MERGE DATA TO ENSURE WE HAVE ALL THE TIME SERIES -------------------------- ####

files_path <- list.files("data_processed/sapflow", ".csv", full.names = T)

file <- files_path[1]
data <- read_csv(file[1])
head(data)
tail(data)

variablestocombine <- names(data)

data$timestamp_ID <- paste0(data$timestamp, "_", data$ID)

for(i in 2:length(files_path)){
  
  data_i <- read_csv(files_path[i])
  data_i$timestamp_ID <- paste0(data_i$timestamp, "_", data_i$ID)
  
  dta <- merge(data, data_i, by = "timestamp_ID", all = T)
  
  data <- combineData(dta, variablesToCombine = variablestocombine)
  names(data)
}

sapflow_data <- data %>%
  filter(!ID %in% c("Control_NA", "Control_Licania", "TFE_122.1", "TFE_111", "TFE_178", "Control_363", "Control_Protium")) %>%   # Individuals with problems that needs to be double check, removed for now
  mutate(timestamp = as_datetime(str_split_fixed(timestamp_ID, "_", n = 3)[, 1])) %>%
  select(timestamp, ID, species, plot, sap_flux_kg_h, bl_sap_flux_Kg_h, increment_mm)

str(sapflow_data)
unique(sapflow_data$ID)
head(sapflow_data)
tail(sapflow_data)

#### DELETE NEGATIVE VALUES AND SAVE ------------------------------------------- ####

sapflow_data$bl_sap_flux_Kg_h[sapflow_data$bl_sap_flux_Kg_h < 0] <- NA

head(sapflow_data)
tail(sapflow_data)

write_csv(sapflow_data, "data_processed/sapflow/complete_datasets/processed_sapflow_2022_11_17-2023_12_14.csv")


#### Clean data plotting ####

sapflow_data <- read_csv("data_processed/sapflow/complete_datasets/processed_sapflow_2022_11_17-2023_12_14.csv")

### Sap flow all individuals ####

all.plot <- plotTimeSeries(data = sapflow_data,
                           xVar = timestamp,
                           yVar = sap_flux_kg_h,
                           xLab = "time", 
                           yLab = "sap flow (kg/h)", 
                           lineOrPoint = "line", 
                           colorVar = ID)
all.plot
# Save the plot
# pdf("outputs/data_plots/sapflow/sapflow_all.pdf")
# all.plot
# dev.off()

### Sap flow control individuals ####

control_data <- sapflow_data %>% 
  filter(str_detect(ID, "Control"))

control.plot <- plotTimeSeries(data = control_data,
                               xVar = timestamp,
                               yVar = sap_flux_kg_h,
                               xLab = "time", 
                               yLab = "sap flow (kg/h)", 
                               lineOrPoint = "line", 
                               colorVar = ID)
control.plot
# Save the plot
# pdf("outputs/data_plots/sapflow/sapflow_control.pdf")
# control.plot
# dev.off()


### Sap flow TFE individuals ####

tfe_data <- sapflow_data %>% 
  filter(str_detect(ID, "TFE"))

tfe.plot <- plotTimeSeries(data = tfe_data,
                           xVar = timestamp,
                           yVar = sap_flux_kg_h,
                           xLab = "time", 
                           yLab = "sap flow (kg/h)", 
                           lineOrPoint = "line", 
                           colorVar = ID)
tfe.plot
# Save the plot
# pdf("outputs/data_plots/sapflow/sapflow_tfe.pdf")
# tfe.plot
# dev.off()


### Sap flow individual one by one ####

for(ind in unique(sapflow_data$ID)){
  
  ind_data <- sapflow_data %>% 
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


### Baselined sap flow individual one by one ####

for(ind in unique(sapflow_data$ID)){
  
  ind_data <- sapflow_data %>% 
    filter(ID == ind)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/sapflow/baselined_sapflow_", ind, "_", str_replace(unique(ind_data$species), " ", "_"),".pdf"))
  ind.plot <- plotTimeSeries(data = ind_data,
                             xVar = timestamp,
                             yVar = bl_sap_flux_Kg_h,
                             xLab = "time", 
                             yLab = "sap flow (kg/h)", 
                             lineOrPoint = "line", 
                             colorVar = ID)
  plot(ind.plot)
  dev.off()
}

#### LAST CLEANING AND OUTLIER REMOVAL AND SAVING ------------------------------ ####

id_problems <- c("Control_252")
id_parcially_bad <- c("Control_357", "Control_359", "TFE_204", "TFE_211", "TFE_213", "TFE_266")
id_to_remove <- c(id_problems, id_parcially_bad)

clean_sapflow_data <- sapflow_data %>%
  filter(!ID %in% id_to_remove)

### individual outliers 

clean_sapflow_data$cleaned_bl_sap_flux_Kg_h <- clean_sapflow_data$bl_sap_flux_Kg_h
all_clean_sapflow_data <- data.frame()

for(id in unique(clean_sapflow_data$ID)){
  
  id_clean_sapflow_data <- clean_sapflow_data %>%
    filter(ID == id)
  
  mean_sapflow <- mean(id_clean_sapflow_data$bl_sap_flux_Kg_h, na.rm = T)
  sd_sapflow <- sd(id_clean_sapflow_data$bl_sap_flux_Kg_h, na.rm = T)
  
  upper_threshold <- mean_sapflow + (sd_sapflow * 3)
  lower_threshold <- mean_sapflow - (sd_sapflow * 3)
  
  id_clean_sapflow_data$cleaned_bl_sap_flux_Kg_h[id_clean_sapflow_data$cleaned_bl_sap_flux_Kg_h > upper_threshold] <- NA
  id_clean_sapflow_data$cleaned_bl_sap_flux_Kg_h[id_clean_sapflow_data$cleaned_bl_sap_flux_Kg_h < lower_threshold] <- NA
  
  all_clean_sapflow_data <- rbind(all_clean_sapflow_data, id_clean_sapflow_data)
}

all_clean_sapflow_data <- all_clean_sapflow_data %>%
  arrange(ID, timestamp)

### Calculate conductivity ####

clean_sapflow_data$conductivity_kg_h_cm <- clean_sapflow_data$cleaned_bl_sap_flux_Kg_h / (clean_sapflow_data$increment_mm/10)

### Plot clean data ####

for(ind in unique(all_clean_sapflow_data$ID)){
  
  ind_data <- all_clean_sapflow_data %>% 
    filter(ID == ind)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/sapflow/cleaned_baselined_sapflow_", ind, "_", str_replace(unique(ind_data$species), " ", "_"),".pdf"))
  ind.plot <- plotTimeSeries(data = ind_data,
                             xVar = timestamp,
                             yVar = cleaned_bl_sap_flux_Kg_h,
                             xLab = "time", 
                             yLab = "sap flow (kg/h)", 
                             lineOrPoint = "line", 
                             colorVar = ID)
  plot(ind.plot)
  dev.off()
}


### Save clean data

write_csv(clean_sapflow_data, "data_processed/sapflow/complete_datasets/cleaned_processed_sapflow_2022_11_17-2023_12_14.csv")


#### DAILY AGGREGATION --------------------------------------------------------- ####

clean_sapflow_data <- read_csv("data_processed/sapflow/complete_datasets/cleaned_processed_sapflow_2022_11_17-2023_12_14.csv") %>%
  mutate(date = as_date(timestamp),
         date_id = paste0(date, "_", ID)) %>%
  select(date_id, date, everything(), -timestamp)

daily_clean_sapflow_data <- aggregate(clean_sapflow_data[, -1],
                                    by = list(clean_sapflow_data$date_id),
                                    FUN = meanOrMode) %>%
  rename(date_id = Group.1) %>%
  mutate(date = ymd(date)) %>%
  select(date, plot, ID, species, everything(), -date_id)

head(daily_clean_sapflow_data)
tail(daily_clean_sapflow_data)
write_csv(daily_clean_sapflow_data, "data_processed/sapflow/complete_datasets/daily_cleaned_processed_sapflow_2022_11_17-2023_12_14.csv")

