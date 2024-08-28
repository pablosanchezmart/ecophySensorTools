#### STEM WATER CONTENT (TEROS12) DATA PROCESSING EXAMPLE ######################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")

#### COLLECTED 17-10-2023 (Sept 2023-oct 2023) --------------------------------- ####

# STEP 1: set the location of the original data to process and the files where we want the output to be stored

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_content/17-10-2023"
raw_file_out <- paste0("data_raw/stem_water_content/raw_stem_water_content_2023_10_17.csv")
processed_file_out <- paste0("data_processed/stem_water_content/processed_stem_water_content_2023_10_17.csv")

# STEP 2: apply the functions to fetch the original data and process it

raw.df <- fetchTeros12(folderIn = raw_folder_in,
                       fileOut = raw_file_out) %>%
arrange(timestamp) %>%
  mutate(date = as_date(timestamp)) %>%
  filter(date > "2023-01-01") %>%
  filter(date < "2025-01-01")

head(raw.df)
tail(raw.df)

# we need to translate from label to tree id, to do so we will use the following object: labelToID.data, which comes from the metadata where we have the
# relationship between loggers and tree ID


labelToID.data <- readxl::read_excel(paste0(root.dir, "data_processed/metadata_cax_radar/cax_radar_metadata_caxiuana_03_2024.xlsx"),
                                     sheet = "2023_05_metadata") %>%
  filter(!is.na(teros12_logger)) %>%
  mutate(label = paste0(teros12_logger, "_", teros12_port)) %>%
  select(ID, plot, species, size_class, label)

## process data

processed.list <- processTeros12(rawDataFile = raw_file_out,
                                 rawData = NULL,
                                 labelToIDFile = NULL,
                                 labelToID = labelToID.data,
                                 fileOut = processed_file_out)

# here we can see how it looks
tail(processed.list$processed_data)

summary(processed.list$processed_data)

#### COLLECTED 26-06-2023 (feb 2023-jun 2023) ---------------------------------- ####

# STEP 1: set the location of the original data to process and the files where we want the output to be stored

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_content/26-06-2023"
raw_file_out <- paste0("data_raw/stem_water_content/raw_stem_water_content_2023_06_26.csv")
processed_file_out <- paste0("data_processed/stem_water_content/processed_stem_water_content_2023_06_26.csv")

# STEP 2: apply the functions to fetch the original data and process it

raw.df <- fetchTeros12(folderIn = raw_folder_in,
                       fileOut = raw_file_out) %>%
  arrange(timestamp) %>%
  mutate(date = as_date(timestamp)) %>%
  filter(date > "2023-01-01") %>%
  filter(date < "2025-01-01")

head(raw.df)
tail(raw.df)

# we need to translate from label to tree id, to do so we will use the following object: labelToID.data, which comes from the metadata where we have the
# relationship between loggers and tree ID

labelToID.data <- readxl::read_excel(paste0(root.dir, "data_processed/metadata_cax_radar/cax_radar_metadata_caxiuana_03_2024.xlsx"),
                                     sheet = "2023_05_metadata") %>%
  filter(!is.na(teros12_logger)) %>%
  mutate(label = paste0(teros12_logger, "_", teros12_port)) %>%
  select(ID, plot, species, size_class, label)

## process data

processed.list <- processTeros12(rawDataFile = raw_file_out,
                                 rawData = NULL,
                                 labelToIDFile = NULL,
                                 labelToID = labelToID.data,
                                 fileOut = processed_file_out)

# here we can see how it looks
tail(processed.list$processed_data)

#### COLLECTED 29-09-2023 (jan 2023-Sept 2023) --------------------------------- ####

# STEP 1: set the location of the original data to process and the files where we want the output to be stored

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_content/29-09-2023"
raw_file_out <- paste0("data_raw/stem_water_content/raw_stem_water_content_2023_09_29.csv")
processed_file_out <- paste0("data_processed/stem_water_content/processed_stem_water_content_2023_09_29.csv")

# STEP 2: apply the functions to fetch the original data and process it

raw.df <- fetchTeros12(folderIn = raw_folder_in,
                       fileOut = raw_file_out) %>%
  arrange(timestamp) %>%
  mutate(date = as_date(timestamp)) %>%
  filter(date > "2023-01-01") %>%
  filter(date < "2025-01-01")

head(raw.df)
tail(raw.df)

# we need to translate from label to tree id, to do so we will use the following object: labelToID.data, which comes from the metadata where we have the
# relationship between loggers and tree ID

labelToID.data <- readxl::read_excel(paste0(root.dir, "data_processed/metadata_cax_radar/cax_radar_metadata_caxiuana_03_2024.xlsx"),
                                     sheet = "2023_05_metadata") %>%
  filter(!is.na(teros12_logger)) %>%
  mutate(label = paste0(teros12_logger, "_", teros12_port)) %>%
  select(ID, plot, species, size_class, label)

## process data

processed.list <- processTeros12(rawDataFile = raw_file_out,
                               rawData = NULL,
                               labelToIDFile = NULL,
                               labelToID = labelToID.data,
                               fileOut = processed_file_out)

# here we can see how it looks
tail(processed.list$processed_data)

summary(processed.list$processed_data)


#### COLLECTED 2024-03-14 (feb 2023-March 2024) -------------------------------- ####

# STEP 1: set the location of the original data to process and the files where we want the output to be stored

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_content/14-03-2024"
raw_file_out <- paste0("data_raw/stem_water_content/raw_stem_water_content_2024_03_14.csv")
processed_file_out <- paste0("data_processed/stem_water_content/processed_stem_water_content_2024_03_14.csv")

# STEP 2: apply the functions to fetch the original data and process it

raw.df <- fetchTeros12(folderIn = raw_folder_in,
                       fileOut = raw_file_out) 
#   arrange(timestamp) %>%
#   mutate(date = as_date(timestamp)) %>%
#   filter(date > "2023-01-01") %>%
#   filter(date < "2025-01-01")
# 
# head(raw.df)
# tail(raw.df)

# we need to translate from label to tree id, to do so we will use the following object: labelToID.data, which comes from the metadata where we have the
# relationship between loggers and tree ID

# labelToID.data <- readxl::read_excel("data_raw/cax_radar_metadata_caxiuana_12_05_2023.xlsx",
#                                      sheet = 1) %>%
#   filter(!is.na(teros12_logger)) %>%
#   mutate(label = paste0(teros12_logger, "_", teros12_port)) %>%
#   select(ID, plot, species, size_class, label)

labelToID.data <- readxl::read_excel(paste0(root.dir, "data_processed/metadata_cax_radar/cax_radar_metadata_caxiuana_03_2024.xlsx"),
                                     sheet = "2024_03_metadata") %>%
  # filter(!is.na(teros12_logger)) %>%
  # mutate(label = paste0(teros12_logger, "_", teros12_port)) %>%
  select(ID, plot, species, size_class, contains("teros12_logger"), teros12_port)

# adjust the code to the fact that we had to replace some loggers
logger_names <- names(labelToID.data)[str_detect(names(labelToID.data), "teros12_logger")]

labelToID.data_newLoggers <- data.frame()
for(logger_name in logger_names){
  
  labelToID.data_newLogger <- labelToID.data[!is.na(labelToID.data[, logger_name]), ] %>%
    select(ID, plot, species, size_class, all_of(logger_name), teros12_port)
  
  names(labelToID.data_newLogger)[names(labelToID.data_newLogger) == logger_name] <- "teros12_logger"
  
  labelToID.data_newLoggers <- rbind(labelToID.data_newLoggers, labelToID.data_newLogger)
}
labelToID.data_newLoggers <- labelToID.data_newLoggers %>%
  mutate(label = paste0(teros12_logger, "_", teros12_port)) %>%
  select(ID, plot, species, size_class, label)

## process data

processed.list <- processTeros12(rawDataFile = raw_file_out,
                                 rawData = NULL,
                                 labelToIDFile = NULL,
                                 labelToID = labelToID.data_newLoggers,
                                 fileOut = processed_file_out) 

# here we can see how it looks
head(processed.list$processed_data)
summary(processed.list$processed_data)



#### COLLECTED 2024-04-19 (ar 2024-jun 2024) ---------------------------------- ####

# STEP 1: set the location of the original data to process and the files where we want the output to be stored

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_content/19-04-2024"
raw_file_out <- paste0("data_raw/stem_water_content/raw_stem_water_content_19-04-2024.csv")
processed_file_out <- paste0("data_processed/stem_water_content/processed_stem_water_content_19-04-2024.csv")

# STEP 2: apply the functions to fetch the original data and process it

raw.df <- fetchTeros12(folderIn = raw_folder_in,
                       fileOut = raw_file_out) %>%
  arrange(timestamp)
unique(raw.df$label)

# we need to translate from label to tree id, to do so we will use the following object: labelToID.data, which comes from the metadata where we have the
# relationship between loggers and tree ID

labelToID.data <- readxl::read_excel(paste0(root.dir, "data_processed/metadata_cax_radar/cax_radar_metadata_caxiuana_03_2024.xlsx"),
                                     sheet = "2024_03_metadata") %>%
  # filter(!is.na(teros12_logger)) %>%
  # mutate(label = paste0(teros12_logger, "_", teros12_port)) %>%
  select(ID, plot, species, size_class, contains("teros12_logger"), teros12_port)

# adjust the code to the fact that we had to replace some loggers
logger_names <- names(labelToID.data)[str_detect(names(labelToID.data), "teros12_logger")]

labelToID.data_newLoggers <- data.frame()
for(logger_name in logger_names){
  
  labelToID.data_newLogger <- labelToID.data[!is.na(labelToID.data[, logger_name]), ] %>%
    select(ID, plot, species, size_class, all_of(logger_name), teros12_port)
  
  names(labelToID.data_newLogger)[names(labelToID.data_newLogger) == logger_name] <- "teros12_logger"
  
  labelToID.data_newLoggers <- rbind(labelToID.data_newLoggers, labelToID.data_newLogger)
}

labelToID.data_newLoggers <- labelToID.data_newLoggers %>%
  mutate(label = paste0(teros12_logger, "_", teros12_port)) %>%
  select(ID, plot, species, size_class, label)

## process data
# unique(raw.df$label)
processed.list <- processTeros12(rawDataFile = raw_file_out,
                                 rawData = NULL,
                                 labelToIDFile = NULL,
                                 labelToID = labelToID.data_newLoggers,
                                 fileOut = processed_file_out)
# View(labelToID.data_newLoggers)

ind <- "TFE_266"

ind_data <- processed.list$processed_data %>%
  filter(ID == ind) %>%
  mutate(date = as_date(timestamp))

# raw water content
ind.plot <- plotTimeSeries(data = ind_data,
                           xVar = timestamp,
                           yVar = water_content_m3.m3,
                           xLab = "",
                           yLab = "stem wc (m3/m3)",
                           lineOrPoint = "line",
                           colorVar = ID) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b")
ind.plot


#### COLLECTED 2024-05-24 (feb 2024-jun 2024) ---------------------------------- ####

# STEP 1: set the location of the original data to process and the files where we want the output to be stored

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_content/24-05-2024"
raw_file_out <- paste0("data_raw/stem_water_content/raw_stem_water_content_2024_05_24.csv")
processed_file_out <- paste0("data_processed/stem_water_content/processed_stem_water_content_2024_05_24.csv")

# STEP 2: apply the functions to fetch the original data and process it

raw.df <- fetchTeros12(folderIn = raw_folder_in,
                       fileOut = raw_file_out) %>%
  arrange(timestamp)
tail(raw.df)

unique(raw.df$label)

# we need to translate from label to tree id, to do so we will use the following object: labelToID.data, which comes from the metadata where we have the
# relationship between loggers and tree ID


labelToID.data <- readxl::read_excel(paste0(root.dir, "data_processed/metadata_cax_radar/cax_radar_metadata_caxiuana_03_2024.xlsx"),
                                     sheet = "2024_05_metadata") %>%
  # filter(!is.na(teros12_logger)) %>%
  # mutate(label = paste0(teros12_logger, "_", teros12_port)) %>%
  select(ID, plot, species, size_class, contains("teros12_logger"), teros12_port)

# adjust the code to the fact that we had to replace some loggers
logger_names <- names(labelToID.data)[str_detect(names(labelToID.data), "teros12_logger")]

labelToID.data_newLoggers <- data.frame()
for(logger_name in logger_names){
  
  labelToID.data_newLogger <- labelToID.data[!is.na(labelToID.data[, logger_name]), ] %>%
    select(ID, plot, species, size_class, all_of(logger_name), teros12_port)
  
  names(labelToID.data_newLogger)[names(labelToID.data_newLogger) == logger_name] <- "teros12_logger"
  
  labelToID.data_newLoggers <- rbind(labelToID.data_newLoggers, labelToID.data_newLogger)
}

labelToID.data_newLoggers <- labelToID.data_newLoggers %>%
  mutate(label = paste0(teros12_logger, "_", teros12_port)) %>%
  select(ID, plot, species, size_class, label)

## process data
# unique(raw.df$label)
processed.list <- processTeros12(rawDataFile = raw_file_out,
                                 rawData = NULL,
                                 labelToIDFile = NULL,
                                 labelToID = labelToID.data_newLoggers,
                                 fileOut = processed_file_out)
# View(labelToID.data_newLoggers)

ind <- "TFE_266"

ind_data <- processed.list$processed_data %>%
  filter(ID == ind) %>%
  mutate(date = as_date(timestamp))

# raw water content
ind.plot <- plotTimeSeries(data = ind_data,
                           xVar = timestamp,
                           yVar = water_content_m3.m3,
                           xLab = "",
                           yLab = "stem wc (m3/m3)",
                           lineOrPoint = "line",
                           colorVar = ID) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b")
ind.plot


#### COLLECTED 2024-07-25 (feb 2024-jul 2024) ---------------------------------- ####

# STEP 1: set the location of the original data to process and the files where we want the output to be stored

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_content/25-07-2024"
raw_file_out <- paste0("data_raw/stem_water_content/raw_stem_water_content_2024_07_25.csv")
processed_file_out <- paste0("data_processed/stem_water_content/processed_stem_water_content_2024_07_25.csv")

# STEP 2: apply the functions to fetch the original data and process it

raw.df <- fetchTeros12(folderIn = raw_folder_in,
                       fileOut = raw_file_out) %>%
  arrange(timestamp)
# head(raw.df)

unique(raw.df$label)

a <- raw.df %>%
  filter(label == "z6-20609_Port_2")

# we need to translate from label to tree id, to do so we will use the following object: labelToID.data, which comes from the metadata where we have the
# relationship between loggers and tree ID

labelToID.data <- readxl::read_excel(paste0(root.dir, "data_processed/metadata_cax_radar/cax_radar_metadata_caxiuana_03_2024.xlsx"),
                                     sheet = "2024_05_metadata") %>%
  # filter(!is.na(teros12_logger)) %>%
  # mutate(label = paste0(teros12_logger, "_", teros12_port)) %>%
  select(ID, plot, species, size_class, contains("teros12_logger"), teros12_port)

# adjust the code to the fact that we had to replace some loggers
logger_names <- names(labelToID.data)[str_detect(names(labelToID.data), "teros12_logger")]

labelToID.data_newLoggers <- data.frame()
for(logger_name in logger_names){
  
  labelToID.data_newLogger <- labelToID.data[!is.na(labelToID.data[, logger_name]), ] %>%
    select(ID, plot, species, size_class, all_of(logger_name), teros12_port)
  
  names(labelToID.data_newLogger)[names(labelToID.data_newLogger) == logger_name] <- "teros12_logger"
  
  labelToID.data_newLoggers <- rbind(labelToID.data_newLoggers, labelToID.data_newLogger)
}

labelToID.data_newLoggers <- labelToID.data_newLoggers %>%
  mutate(label = paste0(teros12_logger, "_", teros12_port)) %>%
  select(ID, plot, species, size_class, label)

## process data
# unique(raw.df$label)
processed.list <- processTeros12(rawDataFile = raw_file_out,
                                 rawData = NULL,
                                 labelToIDFile = NULL,
                                 labelToID = labelToID.data_newLoggers,
                                 fileOut = processed_file_out)
# View(labelToID.data_newLoggers)

ind <- "TFE_168"

ind_data <- processed.list$processed_data %>%
  filter(ID == ind) %>%
  mutate(date = as_date(timestamp)) %>%
  filter(date > "2023-01-01")
tail(ind_data)

# raw water content
ind.plot <- plotTimeSeries(data = ind_data,
                           xVar = timestamp,
                           yVar = water_content_m3.m3,
                           xLab = "",
                           yLab = "stem wc (m3/m3)",
                           lineOrPoint = "line",
                           colorVar = ID) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b")
ind.plot


#### MERGE DATA TO ENSURE WE HAVE ALL THE TIME SERIES -------------------------- ####

files_path <- list.files("data_processed/stem_water_content", ".csv", full.names = T)

file <- files_path[1]
data <- as.data.frame(read_csv(file[1]))
head(data)
tail(data)
summary(data)
variablestocombine <- names(data)

data$timestamp_ID <- paste0(data$timestamp, "_", data$ID)

for(i in 2:length(files_path)){
  
  data_i <- as.data.frame(read_csv(files_path[i]))
  data_i$timestamp_ID <- paste0(data_i$timestamp, "_", data_i$ID)
  
  # data <- bind_rows(data, data_i)
  # data <- anti_join(data, data_i, by = "timestamp_ID")
  
  data <- unique(rbindlist(list(data, data_i)), by = "timestamp_ID")
}

length(unique(data$timestamp_ID))
length(data$timestamp_ID)

## Aggreagate per id and timestamp

# data_aggr <- aggregate(data,
#                   by = list(data$timestamp_ID),
#                   FUN = meanOrMode) %>%
#   select(-Group.1)
# 
# unique(data$ID)

stem_wc_data <- data %>%
  # filter(!ID %in% c("Control_NA", "Control_Licania", "Control_Protium", "NA")) %>%   # Individuals with problems in the that needs to be double check, removed for now
  filter(!is.na(ID)) %>%
  mutate(timestamp = as_datetime(str_split_fixed(timestamp_ID, "_", n = 3)[, 1])) %>%
  filter(date > "2023-05-01") %>%
  filter(date < "2025-01-01") %>%
  rename(stem_temperature_C = soil_temperature_C) %>%
  arrange(ID, timestamp) %>%
  as.data.frame()

unique(stem_wc_data$ID)
head(stem_wc_data)
tail(stem_wc_data)
summary(stem_wc_data)


#### DATA CLEANING ------------------------------------------------------------- ####

### id with problems 

# id_problems <- c("Control_252")
# id_parcially_bad <- c("Control_357", "Control_359", "TFE_204", "TFE_211", "TFE_213", "TFE_266")
# id_to_remove <- c(id_problems, id_parcially_bad)
# 
# stem_wc_data <- sapflow_data %>%
#   filter(!ID %in% id_to_remove)

### out of range values

stem_wc_data$calibrated_water_content_m3.m3[stem_wc_data$calibrated_water_content_m3.m3 < 0] <- NA
stem_wc_data$calibrated_water_content_m3.m3[stem_wc_data$calibrated_water_content_m3.m3 > 1] <- NA

stem_wc_data$water_content_m3.m3[stem_wc_data$water_content_m3.m3 < 0] <- NA
stem_wc_data$water_content_m3.m3[stem_wc_data$water_content_m3.m3 > 1] <- NA

stem_wc_data$stem_temperature_C[stem_wc_data$stem_temperature_C < 10] <- NA
stem_wc_data$stem_temperature_C[stem_wc_data$stem_temperature_C > 50] <- NA

head(stem_wc_data)
tail(stem_wc_data)

### individual outliers 

stem_wc_data$clean_calibrated_water_content_m3.m3 <- stem_wc_data$calibrated_water_content_m3.m3
all_stem_wc_data <- data.frame()

for(id in unique(stem_wc_data$ID)){
  
  id_stem_wc_data <- stem_wc_data %>%
    filter(ID == id)
  
  mean_sapflow <- mean(id_stem_wc_data$calibrated_water_content_m3.m3, na.rm = T)
  sd_sapflow <- sd(id_stem_wc_data$calibrated_water_content_m3.m3, na.rm = T)
  
  upper_threshold <- mean_sapflow + (sd_sapflow * 3)
  lower_threshold <- mean_sapflow - (sd_sapflow * 3)
  
  id_stem_wc_data$clean_calibrated_water_content_m3.m3[id_stem_wc_data$clean_calibrated_water_content_m3.m3 > upper_threshold] <- NA
  id_stem_wc_data$clean_calibrated_water_content_m3.m3[id_stem_wc_data$clean_calibrated_water_content_m3.m3 < lower_threshold] <- NA
  
  all_stem_wc_data <- rbind(all_stem_wc_data, id_stem_wc_data)
}

all_stem_wc_data <- all_stem_wc_data %>%
  arrange(ID, timestamp) %>%
  filter(!is.na(ID))


#### GAP FILLING USING MONTHLY MEAN PER HOUR ----------------------------------- ####

# monthly mean for a given hour

# all_stem_wc_data <- all_stem_wc_data %>%
#   filter(date < "2024-06-01")

gf_all_stem_wc_data_1 <- gapFillTimeSeries(data = all_stem_wc_data, 
                                               variable = "clean_calibrated_water_content_m3.m3", 
                                             temporal_resolution = "60 min", 
                                           method = "interpolation", 
                                           id_vars = c("plot", "species", "size_class")) %>%
  mutate(date = as_date(timestamp))

gf_all_stem_wc_data <- gapFillTimeSeries(data = gf_all_stem_wc_data_1, 
                                         variable = "stem_temperature_C", 
                                         temporal_resolution = "60 min",
                                         method = "interpolation", 
                                         id_vars = c("plot", "species", "size_class")) %>%
  mutate(date = as_date(timestamp))

summary(gf_all_stem_wc_data)
head(gf_all_stem_wc_data)
tail(gf_all_stem_wc_data)

# # data <- ind_data
# ind_data <- gf_all_stem_wc_data %>%
#   filter(ID == "Control_204") %>%
#   mutate(date = as_date(timestamp)) %>%
#   filter(date > "2023-10-01")  %>% filter(date < "2023-11-01")
# 
# # raw water content
# plotTimeSeries(data = ind_data,
#                            xVar = timestamp,
#                            yVar = gf_clean_calibrated_water_content_m3.m3,
#                            xLab = "",
#                            yLab = "stem wc (m3/m3)",
#                            lineOrPoint = "line",
#                            colorVar = ID)

# for months without temperature, use the whole period mean

gf_all_stem_wc_data[is.na(gf_all_stem_wc_data$gf_stem_temperature_C), "gf_stem_temperature_C"] <- mean(gf_all_stem_wc_data$gf_stem_temperature_C, na.rm = T)


summary(gf_all_stem_wc_data$clean_calibrated_water_content_m3.m3)
summary(gf_all_stem_wc_data$gf_clean_calibrated_water_content_m3.m3)

summary(gf_all_stem_wc_data$stem_temperature_C)
summary(gf_all_stem_wc_data$gf_stem_temperature_C)


### TEMPERATURE CORRECTION ----------------------------------------------------- ####

# Apply temperature correction
mean_t <- mean(gf_all_stem_wc_data$stem_temperature_C, na.rm = T)               # mean T is a suitable reference point
# 25.89°C
t_effect <- -0.000974                                                           # temperature effect coefficient (from our study)

gf_all_stem_wc_data <- gf_all_stem_wc_data %>%
  mutate(temp_diff = gf_stem_temperature_C  - mean_t,                           # calculate t - difference from reference (mean)
         tempCor_gf_clean_calibrated_water_content_m3.m3 = gf_clean_calibrated_water_content_m3.m3 - (t_effect * temp_diff)  # Temperature correction - StWC.T
  ) %>%
  select(-temp_diff) %>%
  arrange(ID, timestamp) %>%
  filter(!is.na(ID))

summary(gf_all_stem_wc_data)


#### DAILY AGGREGATION --------------------------------------------------------- ####

clean_stem_wc_data <- gf_all_stem_wc_data %>%
  mutate(date = as_date(timestamp),
         date_id = paste0(date, "_", ID)) %>%
  select(date_id, date, everything(), -timestamp, -timestamp_ID)

daily_clean_stem_wc_data <- aggregate(clean_stem_wc_data[, -1],
                                      by = list(clean_stem_wc_data$date_id),
                                      FUN = meanOrMode)

daily_clean_stem_wc_data <- daily_clean_stem_wc_data %>%
  rename(date_id = Group.1) %>%
  mutate(date = as_date(ymd(date))) %>%
  select(date, plot, ID, species, everything(), -date_id) %>%
  arrange(ID, date) %>%
  filter(!is.na(ID))

head(daily_clean_stem_wc_data)
tail(daily_clean_stem_wc_data)


#### SAVE FINAL DATASET -------------------------------------------------------- ####

### hourly data ####
tail(gf_all_stem_wc_data)

## to project directory

write_csv(gf_all_stem_wc_data, 
          paste0("data_processed/stem_water_content/complete_datasets/processed_stem_water_content_", 
                 as_date(min(gf_all_stem_wc_data$timestamp)), "-", 
                 as_date(max(gf_all_stem_wc_data$timestamp)), ".csv")
)

## to general directory

write_csv(gf_all_stem_wc_data, 
          paste0(root.dir, "data_processed/caxiuana_stem_water_content/processed_stem_water_content_", 
                 as_date(min(gf_all_stem_wc_data$timestamp)), "-", 
                 as_date(max(gf_all_stem_wc_data$timestamp)), ".csv")
)


### daily data ####

## to project directory

write_csv(daily_clean_stem_wc_data, 
          paste0("data_processed/stem_water_content/complete_datasets/daily_processed_stem_water_content_",
                 as_date(min(as_datetime(daily_clean_stem_wc_data$date))), "-", 
                 as_date(max(as_datetime(daily_clean_stem_wc_data$date))), ".csv")
)

## to general directory

write_csv(daily_clean_stem_wc_data, 
          paste0(root.dir, "data_processed/caxiuana_stem_water_content/daily_processed_stem_water_content_", 
                 as_date(min(as_datetime(daily_clean_stem_wc_data$date))), "-", 
                 as_date(max(as_datetime(daily_clean_stem_wc_data$date))), ".csv")
)


#### SUBDAILY DATA PLOTTING ---------------------------------------------------- ####

gf_all_stem_wc_data <- read_csv("data_processed/stem_water_content/complete_datasets/processed_stem_water_content_2023-05-02-2024-07-25.csv")
# gf_all_stem_wc_data <- read_csv("data_processed/stem_water_content/complete_datasets/processed_stem_water_content_2023-05-02-2024-05-31.csv")


for(ind in unique(gf_all_stem_wc_data$ID)){
  
  # ind <- "Control_204"
  
  ind_data <- gf_all_stem_wc_data %>%
    filter(ID == ind) %>% 
    mutate(date = as_date(timestamp))
  # filter(date == "2023-11-12")
  
  # raw water content
  ind.plot <- plotTimeSeries(data = ind_data,
                             xVar = timestamp,
                             yVar = water_content_m3.m3,
                             xLab = "", 
                             yLab = "stem wc (m3/m3)", 
                             lineOrPoint = "line", 
                             colorVar = ID) + 
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b")
  
  # gap filled and calibrated water content
  gf_ind.plot <- plotTimeSeries(data = ind_data,
                                xVar = timestamp,
                                yVar = gf_clean_calibrated_water_content_m3.m3,
                                xLab = "", 
                                yLab = "gf cl stem wc (m3/m3)", 
                                lineOrPoint = "line", 
                                colorVar = ID) + 
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b")
  
  # temperature corrected gap filled and calibrated water content
  tc_ind.plot <- plotTimeSeries(data = ind_data,
                                xVar = timestamp,
                                yVar = tempCor_gf_clean_calibrated_water_content_m3.m3,
                                xLab = "", 
                                yLab = "tc gf cl stem wc (m3/m3)", 
                                lineOrPoint = "line", 
                                colorVar = ID) + 
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b")
  
  # Save the plot
  pdf(paste0("outputs/data_plots/stem_water_content/hourly/stem_wc_", ind, "_", str_replace(unique(ind_data$species), " ", "_"),".pdf"))
  p <- ggarrange(ind.plot,
                 gf_ind.plot,
                 tc_ind.plot,
                 ncol = 1, nrow = 3, legend = "bottom", common.legend = T)
  plot(p)
  dev.off()
}


#### DAILY DATA PLOTTING ------------------------------------------------------- ####

daily_gf_all_stem_wc_data <- read_csv("data_processed/stem_water_content/complete_datasets/daily_processed_stem_water_content_2023-05-02-2024-07-25.csv")

for(ind in unique(daily_gf_all_stem_wc_data$ID)){
  
  
  # ind <- "Control_211"
  ind_data <- daily_gf_all_stem_wc_data %>%
    filter(ID == ind)
  # filter(date == "2023-11-12")
  
  # raw water content
  ind.plot <- plotTimeSeries(data = ind_data,
                             xVar = date,
                             yVar = water_content_m3.m3,
                             xLab = "", 
                             yLab = "stem wc (m3/m3)", 
                             lineOrPoint = "line", 
                             colorVar = ID)
  
  # gap filled and calibrated water content
  gf_ind.plot <- plotTimeSeries(data = ind_data,
                                xVar = date,
                                yVar = gf_clean_calibrated_water_content_m3.m3,
                                xLab = "", 
                                yLab = "gf cl stem wc (m3/m3)", 
                                lineOrPoint = "line", 
                                colorVar = ID)
  
  # temperature corrected gap filled and calibrated water content
  tc_ind.plot <- plotTimeSeries(data = ind_data,
                                xVar = date,
                                yVar = tempCor_gf_clean_calibrated_water_content_m3.m3,
                                xLab = "", 
                                yLab = "tc gf cl stem wc (m3/m3)", 
                                lineOrPoint = "line", 
                                colorVar = ID)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/stem_water_content/daily/stem_wc_", ind, "_", str_replace(unique(ind_data$species), " ", "_"),".pdf"))
  p <- ggarrange(ind.plot,
                 gf_ind.plot,
                 tc_ind.plot, ncol = 1, legend = "bottom", common.legend = T)
  plot(p)
  dev.off()
}

