#### STEM WATER CONTENT (TEROS12) DATA PROCESSING EXAMPLE ######################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")

#### STEP 1: set the location of the original data to process and the files where we want the output to be stored ####

## raw_folder_in: is the complete path to the folder where the original data is stored. It is important to have all the files that we want
## to process together in the same folder (e.g., all the florapulse loggers files in the same folder).

## raw_file_out: location and name of the file where we want the unified raw data to be stored.

## processed_file_out: location and name of the file where we want the processed data to be stored.

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_content"
raw_file_out <- paste0("data_raw/stem_water_content/raw_stem_water_content_", Sys.Date(), ".csv")
processed_file_out <- paste0("data_processed/stem_water_content/processed_stem_water_content_", Sys.Date(), ".csv")

#### STEP 2: apply the functions to fetch the original data and process it ####

### Fetch raw data ####

raw.df <- fetchTeros12(folderIn = raw_folder_in,
                       fileOut = raw_file_out)

### Process raw data ####

## we need to translate from label to tree id, to do so we will use the following object: labelToID.data, which comes from the metadata where we have the
## relationship between loggers and tree ID

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

#### STEP 3: data visualization ####

### All the individuals together ####

all.plot <- plotTimeSeries(data = processed.list$processed_data,
               xVar = timestamp,
               yVar = water_content_m3.m3,
               xLab = "date", 
               yLab = "water content (m3/m3)", 
               lineOrPoint = "line", 
               colorVar = ID) #+ ylim(0, 1)

# Save the plot
pdf("outputs/data_plots/stem_water_content/stem_water_content_all.pdf")
all.plot
dev.off()


### Control individuals ####

control_data <- processed.list$processed_data %>% 
  filter(str_detect(ID, "Control"))

control.plot <- plotTimeSeries(data = control_data,
                               xVar = timestamp,
                               yVar = water_content_m3.m3,
                               xLab = "date", 
                               yLab = "water content (m3/m3)", 
                               lineOrPoint = "line", 
                               colorVar = ID) #+ ylim(0, 1)

# Save the plot
pdf("outputs/data_plots/stem_water_content/stem_water_content_control.pdf")
control.plot
dev.off()


### tfe individuals ####

tfe_data <- processed.list$processed_data %>% 
  filter(str_detect(ID, "TFE"))

tfe.plot <- plotTimeSeries(data = tfe_data,
                           xVar = timestamp,
                           yVar = water_content_m3.m3,
                           xLab = "date", 
                           yLab = "water content (m3/m3)", 
                           lineOrPoint = "line", 
                           colorVar = ID) #+ ylim(0, 1)

# Save the plot
pdf("outputs/data_plots/stem_water_content/stem_water_content_tfe.pdf")
tfe.plot
dev.off()


### Individuals one by one ####

for(ind in unique(processed.list$processed_data$ID)){
  
  ind_data <- processed.list$processed_data %>% filter(ID == ind)
  
  # Save the plot
  pdf(paste0("outputs/data_plots/stem_water_content/stem_water_content_", ind, "_", str_replace(unique(ind_data$species), " ", "_"),".pdf"))
  ind.plot <- plotTimeSeries(data = ind_data,
                             xVar = timestamp,
                             yVar = water_content_m3.m3,
                             xLab = "date", 
                             yLab = "water content (m3/m3)", 
                             lineOrPoint = "line", 
                             colorVar = ID) #+ ylim(0, 1)
  plot(ind.plot)
  dev.off()
}
