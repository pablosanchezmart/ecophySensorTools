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



#### COLLECTED 03-15-2024 ------------------------------------------------------ ####

raw_folder_in_03_15_2024 <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_sapflow/2024_03_15"
# processed_file_out <- paste0("data_processed/sapflow/processed_saplfow_", Sys.Date(), ".csv")
processed_file_out_03_15_2024 <- paste0("data_processed/sapflow/processed_sapflow_2024_03_15.csv")


sf.df <- fetchEMS81(folderIn = raw_folder_in_03_15_2024,
                    fileOut = processed_file_out_03_15_2024)

sf_03_15_2024.df <- sf.df %>%
  filter(!is.na(ID)) %>%
  mutate(date = as_date(timestamp)) %>%
  filter(date > "2022-01-01") %>%
  select(timestamp, ID, species, plot, sap_flux_kg_h, bl_sap_flux_Kg_h, increment_mm)

unique(sf_03_15_2024.df$ID)
length(unique(sf_03_15_2024.df$ID))
head(sf_03_15_2024.df)
tail(sf_03_15_2024.df)

write_csv(sf_03_15_2024.df, processed_file_out_03_15_2024)


#### COLLECTED 30-05-2024 ------------------------------------------------------ ####

raw_folder_in_30_05_2024 <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_sapflow/2024_05_30"
# processed_file_out <- paste0("data_processed/sapflow/processed_saplfow_", Sys.Date(), ".csv")
processed_file_out_30_05_2024 <- paste0("data_processed/sapflow/processed_sapflow_2024_05_30.csv")

sf.df <- fetchEMS81(folderIn = raw_folder_in_30_05_2024,
                    fileOut = processed_file_out_30_05_2024)

sf_30_05_2024.df <- sf.df %>%
  filter(!is.na(ID)) %>%
  mutate(date = as_date(timestamp)) %>%
  filter(date > "2022-01-01") %>%
  select(timestamp, ID, species, plot, sap_flux_kg_h, bl_sap_flux_Kg_h, increment_mm)

unique(sf_30_05_2024.df$ID)
length(unique(sf_30_05_2024.df$ID))
head(sf_30_05_2024.df)
tail(sf_30_05_2024.df)

write_csv(sf_30_05_2024.df, processed_file_out_30_05_2024)


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
  filter(!ID %in% c("Control_NA", "Control_Licania", "Control_Protium")) %>%   # Individuals with problems that needs to be double check, removed for now
  mutate(timestamp = as_datetime(str_split_fixed(timestamp_ID, "_", n = 3)[, 1])) %>%
  select(timestamp, ID, species, plot, sap_flux_kg_h, bl_sap_flux_Kg_h, increment_mm)

str(sapflow_data)
unique(sapflow_data$ID)
head(sapflow_data)
tail(sapflow_data)


#### DATA CLEANING ------------------------------------------------------------- ####

### id with problems 

id_problems <- c("Control_252")
id_parcially_bad <- c("Control_357", "Control_359", "TFE_204", "TFE_211", "TFE_213", "TFE_266")
id_to_remove <- c(id_problems, id_parcially_bad)

clean_sapflow_data <- sapflow_data %>%
  filter(!ID %in% id_to_remove)

### negative values cleaning

clean_sapflow_data$bl_sap_flux_Kg_h[clean_sapflow_data$bl_sap_flux_Kg_h < 0] <- NA

head(clean_sapflow_data)
tail(clean_sapflow_data)

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


#### GAP FILLING USING MONTHLY MEAN PER HOUR ----------------------------------- ####

gf_all_clean_sapflow_data <- gapFillTimeSeries(data = all_clean_sapflow_data, 
                                                    variable = "cleaned_bl_sap_flux_Kg_h")

gf_all_clean_sapflow_data <- gapFillTimeSeries(data = gf_all_clean_sapflow_data, 
                                                    variable = "increment_mm")

summary(gf_all_clean_sapflow_data)


#### CALCULATE SAPFLOW PER UNIT AREA ------------------------------------------- ####

### Calculate sapflow per unit area ####

metadata_sapflow <- metadata %>%
  select(ID, perimeter_2023_cm, phloem_depth_cm)

gf_clean_sapflow_data_metadata <- merge(gf_all_clean_sapflow_data, metadata_sapflow, 
                                     by = "ID",
                                     all.x = T)

gf_clean_sapflow_data_metadata$sap_flux_kg_h_cm2 <- gf_clean_sapflow_data_metadata$cleaned_bl_sap_flux_Kg_h / 
                                                 (gf_clean_sapflow_data_metadata$perimeter_2023_cm - ((2*pi) * gf_clean_sapflow_data_metadata$phloem_depth_cm))

gf_clean_sapflow_data_metadata$gf_sap_flux_kg_h_cm2 <- gf_clean_sapflow_data_metadata$gf_cleaned_bl_sap_flux_Kg_h / 
  (gf_clean_sapflow_data_metadata$perimeter_2023_cm - ((2*pi) * gf_clean_sapflow_data_metadata$phloem_depth_cm))

gf_clean_sapflow_data_metadata <- gf_clean_sapflow_data_metadata %>%
  arrange(ID, timestamp) %>%
  filter(!is.na(ID))


#### DAILY AGGREGATION --------------------------------------------------------- ####

clean_sapflow_data <- gf_clean_sapflow_data_metadata %>%
  mutate(date = as_date(timestamp),
         date_id = paste0(date, "_", ID)) %>%
  select(date_id, date, everything(), -timestamp)

daily_clean_sapflow_data <- aggregate(clean_sapflow_data[, -1],
                                      by = list(clean_sapflow_data$date_id),
                                      FUN = meanOrMode)

sum_daily_clean_sapflow_data <- aggregate(clean_sapflow_data[, "gf_cleaned_bl_sap_flux_Kg_h"],
                                      by = list(clean_sapflow_data$date_id),
                                      FUN = sum, na.rm = F) %>%
  rename(total_daily_sap_flux_kg = x)

daily_clean_sapflow_data <- merge(daily_clean_sapflow_data, sum_daily_clean_sapflow_data, by = "Group.1", all = T) %>%
  rename(date_id = Group.1) %>%
  mutate(date = ymd(date)) %>%
  select(date, plot, ID, species, everything(), -date_id) %>%
  arrange(ID, date)

head(daily_clean_sapflow_data)
tail(daily_clean_sapflow_data)


#### SAVE FINAL DATASET -------------------------------------------------------- ####

### hourly data ####
tail(gf_clean_sapflow_data_metadata)

## to project directory

write_csv(gf_clean_sapflow_data_metadata, 
          paste0("data_processed/sapflow/complete_datasets/cleaned_processed_sapflow_", 
                 as_date(min(gf_clean_sapflow_data_metadata$timestamp)), "-", 
                 as_date(max(gf_clean_sapflow_data_metadata$timestamp)), ".csv")
)

## to general directory

write_csv(gf_clean_sapflow_data_metadata, 
          paste0(root.dir, "data_processed/caxuana_sapflow/cleaned_processed_sapflow_", 
                 as_date(min(gf_clean_sapflow_data_metadata$timestamp)), "-", 
                 as_date(max(gf_clean_sapflow_data_metadata$timestamp)), ".csv")
)


### daily data ####

## to project directory

write_csv(daily_clean_sapflow_data, 
          paste0("data_processed/sapflow/complete_datasets/daily_cleaned_processed_sapflow_",
                 as_date(min(gf_clean_sapflow_data_metadata$timestamp)), "-", 
                 as_date(max(gf_clean_sapflow_data_metadata$timestamp)), ".csv")
          )

## to general directory

write_csv(daily_clean_sapflow_data, 
          paste0(root.dir, "data_processed/caxuana_sapflow/daily_cleaned_processed_sapflow_", 
                 as_date(min(gf_clean_sapflow_data_metadata$timestamp)), "-", 
                 as_date(max(gf_clean_sapflow_data_metadata$timestamp)), ".csv")
)


#### HOURLY DATA PLOTTING ------------------------------------------------------ ####

gf_clean_sapflow_data_metadata <- read_csv("data_processed/sapflow/complete_datasets/cleaned_processed_sapflow_2022-11-17-2024-06-01.csv")


for(ind in unique(gf_clean_sapflow_data_metadata$ID)){
  
  # ind <- "Control_211"
  
  ind_data <- gf_clean_sapflow_data_metadata %>%
    filter(ID == ind) %>% 
    mutate(date = as_date(timestamp))
    # filter(date == "2023-11-12")
  
  # sap flow
  ind.plot <- plotTimeSeries(data = ind_data,
                             xVar = timestamp,
                             yVar = sap_flux_kg_h,
                             xLab = "", 
                             yLab = "sapflow (kg/h)", 
                             lineOrPoint = "line", 
                             colorVar = ID)
  
  # gap filled baselined sap flow
  bl_ind.plot <- plotTimeSeries(data = ind_data,
                                xVar = timestamp,
                                yVar = gf_cleaned_bl_sap_flux_Kg_h,
                                xLab = "", 
                                yLab = "gf bl cl sapflow (kg/h)", 
                                lineOrPoint = "line", 
                                colorVar = ID)
  
  # gap filled baselined sap flow
  area_bl_ind.plot <- plotTimeSeries(data = ind_data,
                                xVar = timestamp,
                                yVar = gf_sap_flux_kg_h_cm2,
                                xLab = "", 
                                yLab = "gf sapflow per area (kg/h cm2)", 
                                lineOrPoint = "line", 
                                colorVar = ID)
  
  
  # Save the plot
  pdf(paste0("outputs/data_plots/sapflow/hourly/sapflow_", ind, "_", str_replace(unique(ind_data$species), " ", "_"),".pdf"))
  p <- ggarrange(ind.plot,
            bl_ind.plot,
            area_bl_ind.plot, ncol = 1, nrow = 3, legend = "bottom", common.legend = T)
  plot(p)
  dev.off()
}


#### DAILY DATA PLOTTING ------------------------------------------------------ ####

daily_clean_sapflow_data <- read_csv("data_processed/sapflow/complete_datasets/daily_cleaned_processed_sapflow_2022-11-17-2024-06-01.csv")

### Sap flow individual one by one ####

for(ind in unique(daily_clean_sapflow_data$ID)){
  
  
  # ind <- "Control_211"
  
  ind_data <- daily_clean_sapflow_data %>%
    filter(ID == ind)
  # filter(date == "2023-11-12")
  
  # gap filled baselined sap flow
  bl_ind.plot <- plotTimeSeries(data = ind_data,
                                xVar = date,
                                yVar = gf_cleaned_bl_sap_flux_Kg_h,
                                xLab = "", 
                                yLab = "gf bl cl sapflow (kg/h)", 
                                lineOrPoint = "line", 
                                colorVar = ID)
  
  # gap filled baselined sap flow
  area_bl_ind.plot <- plotTimeSeries(data = ind_data,
                                     xVar = date,
                                     yVar = gf_sap_flux_kg_h_cm2,
                                     xLab = "", 
                                     yLab = "gf sapflow per area (kg/h cm2)", 
                                     lineOrPoint = "line", 
                                     colorVar = ID)
  
  # gap filled baselined sap flow
  daily_ind.plot <- plotTimeSeries(data = ind_data,
                                     xVar = date,
                                     yVar = total_daily_sap_flux_kg,
                                     xLab = "", 
                                     yLab = "daily sapflow (kg)", 
                                     lineOrPoint = "line", 
                                     colorVar = ID)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/sapflow/daily/sapflow_", ind, "_", str_replace(unique(ind_data$species), " ", "_"),".pdf"))
  p <- ggarrange(bl_ind.plot,
            area_bl_ind.plot,
            daily_ind.plot, ncol = 1, legend = "bottom", common.legend = T)
  plot(p)
  dev.off()
}

