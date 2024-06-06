#### STEM WATER CONTENT (TEROS12) DATA PROCESSING EXAMPLE ######################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")


#### COLLECTED 17-10-2023 ------------------------------------------------------ ####

# STEP 1: set the location of the original data to process and the files where we want the output to be stored

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_content/17-10-2023"
raw_file_out <- paste0("data_raw/stem_water_content/raw_stem_water_content_2023_10_17.csv")
processed_file_out <- paste0("data_processed/stem_water_content/processed_stem_water_content_2023_10_17.csv")

# STEP 2: apply the functions to fetch the original data and process it

raw.df <- fetchTeros12(folderIn = raw_folder_in,
                       fileOut = raw_file_out)

# we need to translate from label to tree id, to do so we will use the following object: labelToID.data, which comes from the metadata where we have the
# relationship between loggers and tree ID

labelToID.data <- readxl::read_excel("data_raw/cax_radar_metadata_caxiuana_12_05_2023.xlsx", 
                                     sheet = 1) %>%
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

#### COLLECTED 26-06-2023 ------------------------------------------------------ ####

# STEP 1: set the location of the original data to process and the files where we want the output to be stored

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_content/26-06-2023"
raw_file_out <- paste0("data_raw/stem_water_content/raw_stem_water_content_2023_06_26.csv")
processed_file_out <- paste0("data_processed/stem_water_content/processed_stem_water_content_2023_06_26.csv")

# STEP 2: apply the functions to fetch the original data and process it

raw.df <- fetchTeros12(folderIn = raw_folder_in,
                       fileOut = raw_file_out)

# we need to translate from label to tree id, to do so we will use the following object: labelToID.data, which comes from the metadata where we have the
# relationship between loggers and tree ID

labelToID.data <- readxl::read_excel("data_raw/cax_radar_metadata_caxiuana_12_05_2023.xlsx", 
                                     sheet = 1) %>%
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

#### COLLECTED 29-09-2023 ------------------------------------------------------ ####

# STEP 1: set the location of the original data to process and the files where we want the output to be stored

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_content/29-09-2023"
raw_file_out <- paste0("data_raw/stem_water_content/raw_stem_water_content_2023_09_29.csv")
processed_file_out <- paste0("data_processed/stem_water_content/processed_stem_water_content_2023_09_29.csv")

# STEP 2: apply the functions to fetch the original data and process it

raw.df <- fetchTeros12(folderIn = raw_folder_in,
                       fileOut = raw_file_out)

# we need to translate from label to tree id, to do so we will use the following object: labelToID.data, which comes from the metadata where we have the
# relationship between loggers and tree ID

labelToID.data <- readxl::read_excel("data_raw/cax_radar_metadata_caxiuana_12_05_2023.xlsx", 
                                     sheet = 1) %>%
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

#### COLLECTED 2024-03-13 ------------------------------------------------------ ####

# # STEP 1: set the location of the original data to process and the files where we want the output to be stored
# 
# raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_content/2024-03-13"
# raw_file_out <- paste0("data_raw/stem_water_content/raw_stem_water_content_2024_03_13.csv")
# processed_file_out <- paste0("data_processed/stem_water_content/processed_stem_water_content_2024_03_13.csv")
# 
# # STEP 2: apply the functions to fetch the original data and process it
# 
# raw.df <- fetchTeros12(folderIn = raw_folder_in,
#                        fileOut = raw_file_out)
# 
# # we need to translate from label to tree id, to do so we will use the following object: labelToID.data, which comes from the metadata where we have the
# # relationship between loggers and tree ID
# 
# labelToID.data <- readxl::read_excel("data_raw/cax_radar_metadata_caxiuana_12_05_2023.xlsx", 
#                                      sheet = 1) %>%
#   filter(!is.na(teros12_logger)) %>%
#   mutate(label = paste0(teros12_logger, "_", teros12_port)) %>%
#   select(ID, plot, species, size_class, label)
# 
# ## process data
# 
# processed.list <- processTeros12(rawDataFile = raw_file_out,
#                                  rawData = NULL,
#                                  labelToIDFile = NULL,
#                                  labelToID = labelToID.data,
#                                  fileOut = processed_file_out)
# 
# # here we can see how it looks
# tail(processed.list$processed_data)


#### COLLECTED 2024-03-14 ------------------------------------------------------ ####

# STEP 1: set the location of the original data to process and the files where we want the output to be stored

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_content/14-03-2024"
raw_file_out <- paste0("data_raw/stem_water_content/raw_stem_water_content_2024_03_14.csv")
processed_file_out <- paste0("data_processed/stem_water_content/processed_stem_water_content_2024_03_14.csv")

# STEP 2: apply the functions to fetch the original data and process it

raw.df <- fetchTeros12(folderIn = raw_folder_in,
                       fileOut = raw_file_out)

# we need to translate from label to tree id, to do so we will use the following object: labelToID.data, which comes from the metadata where we have the
# relationship between loggers and tree ID

labelToID.data <- readxl::read_excel("data_raw/cax_radar_metadata_caxiuana_12_05_2023.xlsx", 
                                     sheet = 1) %>%
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
head(processed.list$processed_data)
summary(processed.list$processed_data)

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
  filter(date < "2024-05-01") %>%
  arrange(ID, timestamp) %>%
  as.data.frame()

unique(stem_wc_data$ID)
head(stem_wc_data)
tail(stem_wc_data)
summary(stem_wc_data)


#### DELETE VALUES OUT OF RANGE AND SAVE --------------------------------------- ####

stem_wc_data$calibrated_water_content_m3.m3[stem_wc_data$calibrated_water_content_m3.m3 < 0] <- 0
stem_wc_data$calibrated_water_content_m3.m3[stem_wc_data$calibrated_water_content_m3.m3 > 1] <- 1

stem_wc_data$water_content_m3.m3[stem_wc_data$water_content_m3.m3 < 0] <- 0
stem_wc_data$water_content_m3.m3[stem_wc_data$water_content_m3.m3 > 1] <- 1

head(stem_wc_data)
tail(stem_wc_data)

#### LAST CLEANING AND OUTLIER REMOVAL AND SAVING ------------------------------ ####

### individual outliers 

# stem_wc_data$cleaned_bl_sap_flux_Kg_h <- stem_wc_data$bl_sap_flux_Kg_h
# all_stem_wc_data <- data.frame()

# for(id in unique(stem_wc_data$ID)){
# 
#   id_stem_wc_data <- stem_wc_data %>%
#     filter(ID == id)
# 
#   mean_wc <- mean(id_stem_wc_data$calibrated_water_content_m3.m3, na.rm = T)
#   sd_wc <- sd(id_stem_wc_data$calibrated_water_content_m3.m3, na.rm = T)
# 
#   upper_threshold <- mean_wc + (sd_wc * 3)
#   lower_threshold <- mean_wc - (sd_wc * 3)
# 
#   id_stem_wc_data$calibrated_water_content_m3.m3[id_stem_wc_data$calibrated_water_content_m3.m3 > upper_threshold] <- NA
#   id_stem_wc_data$calibrated_water_content_m3.m3[id_stem_wc_data$calibrated_water_content_m3.m3 < lower_threshold] <- NA
# 
#   all_stem_wc_data <- rbind(all_stem_wc_data, id_stem_wc_data)
# }

all_stem_wc_data <- stem_wc_data %>%
  arrange(ID, timestamp)

write_csv(all_stem_wc_data, "data_processed/stem_water_content/complete_datasets/processed_stem_water_content_17-10-2023_15-03-2024.csv")

summary(all_stem_wc_data)
head(all_stem_wc_data)
tail(all_stem_wc_data)

#### STEP 3: data visualization ####


all_stem_wc_data <- read_csv("data_processed/stem_water_content/complete_datasets/processed_stem_water_content_17-10-2023_15-03-2024.csv")


### All the individuals together ####

# all.plot <- plotTimeSeries(data = all_stem_wc_data,
#                xVar = timestamp,
#                yVar = calibrated_water_content_m3.m3,
#                xLab = "date", 
#                yLab = "water content (m3/m3)", 
#                lineOrPoint = "line", 
#                colorVar = ID) #+ ylim(0, 1)

all.plot <- ggplot(data = all_stem_wc_data, aes(x = timestamp, 
                                                y = calibrated_water_content_m3.m3, 
                                                colour = plot)) +
  geom_point() +
  geom_smooth(method = "gam")
all.plot

# Save the plot
pdf("outputs/data_plots/stem_water_content/stem_water_content_scatterplot_all.pdf")
all.plot
dev.off()


### Control individuals ####

control_data <- all_stem_wc_data %>% 
  filter(str_detect(ID, "Control"))

control.plot <- plotTimeSeries(data = control_data,
                               xVar = timestamp,
                               yVar = calibrated_water_content_m3.m3,
                               xLab = "date", 
                               yLab = "water content (m3/m3)", 
                               lineOrPoint = "line", 
                               colorVar = ID) #+ ylim(0, 1)

# Save the plot
pdf("outputs/data_plots/stem_water_content/stem_water_content_control.pdf")
control.plot
dev.off()


### tfe individuals ####

tfe_data <- all_stem_wc_data %>% 
  filter(str_detect(ID, "TFE"))

tfe.plot <- plotTimeSeries(data = tfe_data,
                           xVar = timestamp,
                           yVar = calibrated_water_content_m3.m3,
                           xLab = "date", 
                           yLab = "water content (m3/m3)", 
                           lineOrPoint = "line", 
                           colorVar = ID) #+ ylim(0, 1)

# Save the plot
pdf("outputs/data_plots/stem_water_content/stem_water_content_tfe.pdf")
tfe.plot
dev.off()


### Individuals one by one ####

for(ind in unique(all_stem_wc_data$ID)){
  
  ind <- "Control_211"
  
  tail(all_stem_wc_data)
  ind_data <- all_stem_wc_data %>% 
    filter(ID == ind) %>%
    filter(date == "2023-11-12")
  
  # ind_data <- all_stem_wc_data %>% filter(ID == ind)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/stem_water_content/stem_water_content_", ind, "_", str_replace(unique(ind_data$species), " ", "_"),".pdf"))
  ind.plot <- plotTimeSeries(data = ind_data,
                             xVar = timestamp,
                             yVar = calibrated_water_content_m3.m3,
                             xLab = "date", 
                             yLab = "water content (m3/m3)", 
                             lineOrPoint = "line", 
                             colorVar = ID) #+ ylim(0, 1)
  plot(ind.plot)
  dev.off()
}
