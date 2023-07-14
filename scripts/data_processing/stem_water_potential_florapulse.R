#### STEM WATER POTENTIAL (FLORAPULSE) DATA PROCESSING #########################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")

## Paths

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_stem_water_potential"
raw_file_out <- paste0("data_raw/stem_water_potential/raw_stem_water_potential_", Sys.Date(), ".csv")
processed_file_out <- paste0("data_processed/stem_water_potential/processed_stem_water_potential_", Sys.Date(), ".csv")

### UNIFY RAW DATA ------------------------------------------------------------- ####

## Fetch florapulse data and unify all in one table

raw.df <- fetchFlorapulse(folderIn = raw_folder_in,
                          fileOut = raw_file_out)

## Only average

raw.df <- raw.df %>% 
  filter(str_detect(label, "AVG"))

## Manual changes

raw.df[str_detect(raw.df$label, "255"), "label"] <- "B310_AVG"

write_csv(raw.df, raw_file_out)


## Check that we have all sensors present

length(unique(raw.df$label)) # 20 sensors

non_allNAs <- raw.df %>% # Check whether there is any of the sensors with all NAs
  na.omit() %>%
  pull(label)

length(unique(non_allNAs)) # no sensors with all NAs

### PROCESS DATA --------------------------------------------------------------- ####

## Load offset and multiplier values

offsetMultiplier <- readxl::read_excel("data_raw/cax_radar_metadata_caxiuana_12_05_2023.xlsx", 
                                       sheet = 1) %>%
  select(ID, plot, species, size_class, flora_pulse_sensor, flora_pulse_offset, flora_pulse_multiplier) %>%
  filter(!is.na(flora_pulse_sensor))

## Process sensor data to get water potential in MPa

processed.list <- processFlorapulse(rawDataFile = raw_file_out, 
                                  offsetMultiplier = offsetMultiplier,
                                  fileOut = processed_file_out)


length(unique(processed.list$processed_data$ID)) # Check that we still have 20 sensors

non_allNAs <- processed.list$processed_data %>% # Check whether there is any of the sensors with all NAs
  na.omit() %>%
  pull(ID)

length(unique(non_allNAs))

# We have one sensor with all NAs, 

unique(processed.list$processed_data$ID)[which(!unique(processed.list$processed_data$ID) %in% unique(non_allNAs))]

unique(offsetMultiplier$ID)[which(!unique(offsetMultiplier$ID) %in% unique(non_allNAs))]

sensors <- unique(str_remove_all(raw.df$label, "_AVG"))
length(sensors)

sensors[which(!sensors %in% unique(offsetMultiplier$flora_pulse_sensor))]



## Observe a specific individual to check structure of data

processed.list$processed_data %>% 
  filter(ID == "Control_211")


### DATA VISUALIZATION --------------------------------------------------------- ####

all.plot <- plotTimeSeries(data = processed.list$processed_data,
               xVar = timestamp,
               yVar = wp_MPa,
               xLab = "time", 
               yLab = "WP (MPa)", 
               lineOrPoint = "line", 
               colorVar = ID) #+ ylim(-20, 20)
all.plot

pdf("outputs/data_plots/stem_water_potential/stem_water_potential_all.pdf")
all.plot
dev.off()

## Visualize control data

control_data <- processed.list$processed_data %>% 
  filter(str_detect(ID, "Control"))

control.plot <- plotTimeSeries(data = control_data,
                    xVar = timestamp,
                    yVar = wp_MPa,
                    xLab = "time", 
                    yLab = "WP (MPa)", 
                    lineOrPoint = "line", 
                    colorVar = ID) #+ ylim(-5, 5)

pdf("outputs/data_plots/stem_water_potential/stem_water_potential_control.pdf")
control.plot
dev.off()

# Visualize TFE data 

tfe_data <- processed.list$processed_data %>% 
  filter(str_detect(ID, "TFE"))

tfe.plot <- plotTimeSeries(data = tfe_data,
               xVar = timestamp,
               yVar = wp_MPa,
               xLab = "time", 
               yLab = "WP (MPa)", 
               lineOrPoint = "line", 
               colorVar = ID) #+ ylim(-5, 5)

pdf("outputs/data_plots/stem_water_potential/stem_water_potential_tfe.pdf")
tfe.plot
dev.off()


## Visualize individual plot

for(ind in unique(processed.list$processed_data$ID)){
  
  ind_data <- processed.list$processed_data %>% filter(ID == ind)
  
  pdf(paste0("outputs/data_plots/stem_water_potential/stem_water_potential_", ind, "_", str_replace(unique(ind_data$species), " ", "_"),".pdf"))
  ind.plot <- plotTimeSeries(data = ind_data,
                 xVar = timestamp,
                 yVar = wp_MPa,
                 xLab = "time", 
                 yLab = "WP (MPa)", 
                 lineOrPoint = "line", 
                 colorVar = ID)# + ylim(-5, 5)
  plot(ind.plot)
  dev.off()
}


