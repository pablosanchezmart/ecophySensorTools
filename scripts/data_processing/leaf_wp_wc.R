#### DATA PROCESSING ###########################################################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")


#### DATA FOR EACH CAMPAIGN ---------------------------------------------------- ####

#### 05/2023 big campaign ####

## WP data

wp_05_2023.data <- read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_05_2023.xlsx",
                      sheet = 1) %>%
  mutate(plot = ifelse(plot == "A", "Control", "TFE"),
         id_radar_replique = paste0(plot, "_", ID, "_", radar_replique),
         id = paste0(plot, "_", ID),
         WP_md = abs(rowMeans(select(., WP_md_1, WP_md_2, WP_md_3), na.rm = T)) * -1,
         WP_pd = abs(rowMeans(select(., WP_pd_1, WP_pd_2, WP_pd_3), na.rm = T)) * -1,
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = dmy(date_md)
         ) %>%
  # filter(radar == T) %>%
  select(id_radar_replique, id, radar, radar_replique, plot, date,
         WP_md, WP_pd,
         Hour_WP_pd, Hour_WP_md
         )

## Save big database

write_csv(wp_05_2023.data, "data_processed/leaf_wp_wc/big_campaign_wp_wc_05_2023.csv")

### Radar trees ####

wp_05_2023.data <-  wp_05_2023.data  %>%
  filter(radar == T)
  

## WC data

wc_05_2023.data <- read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_05_2023.xlsx",
                          sheet = 2) %>%
      mutate(plot = ifelse(plot == "A", "Control", "TFE"),
             id_radar_replique = paste0(plot, "_", ID, "_", radar_replique)) %>%
      select(-ID, -radar_replique, -plot, -date, -notes)

## Merge WP and WC

wp_wc_05_2023.data <- merge(wp_05_2023.data, wc_05_2023.data, 
                            by = "id_radar_replique", 
                            all = T, 
                            suffixes = c("wp", "wc"))
names(wp_wc_05_2023.data)

# bring all names to lower case to avoid double identification of variables
names(wp_wc_05_2023.data) <- tolower(names(wp_wc_05_2023.data))
head(wp_wc_05_2023.data)

# bring all names to lower case to avoid double identification of variables
names(wp_wc_05_2023.data) <- tolower(names(wp_wc_05_2023.data))

radar_wp_wc_05_2023.data <- wp_wc_05_2023.data %>%
  filter(radar == TRUE) %>%
  select(-radar) %>%
  mutate(campaign = "05-2023")

## Save campaign data

write_csv(radar_wp_wc_05_2023.data, file = "data_processed/leaf_wp_wc/radar_wp_wc_05_2023.csv")

### 07/2023 radar campaign ####

wp_07_2023 <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_07_2023.xlsx",
                                 sheet = 1)

wc_07_2023 <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_07_2023.xlsx",
                                 sheet = 2)

## data needs to be changed from wide to long format 

## WP

wp_07_2023_a <- wp_07_2023 %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp_07_2023)[str_detect(names(wp_07_2023), "_1")])) %>%
  mutate(radar_replique = "a",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)
         ) %>%
  select(ID, radar_replique, everything())
names(wp_07_2023_a) <- str_remove_all(names(wp_07_2023_a), "_1")

wp_07_2023_b <- wp_07_2023 %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp_07_2023)[str_detect(names(wp_07_2023), "_2")])) %>%
  mutate(radar_replique = "b",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_07_2023_b) <- str_remove_all(names(wp_07_2023_b), "_2")

wp_07_2023_c <- wp_07_2023 %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp_07_2023)[str_detect(names(wp_07_2023), "_3")])) %>%
  mutate(radar_replique = "c",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_07_2023_c) <- str_remove_all(names(wp_07_2023_c), "_3")

wp_07_2023_long <- bind_rows(wp_07_2023_a, wp_07_2023_b, wp_07_2023_c) %>%
  mutate(id_radar_replique = paste0(plot, "_", ID, "_", radar_replique),
         id = paste0(plot, "_", ID)) %>%
  select(id_radar_replique, id, radar, radar_replique, date, plot, 
         WP_md, WP_pd,
         Hour_WP_pd, Hour_WP_md
  )


## WC

wc_07_2023_a <- wc_07_2023 %>%
  select(ID, plot, date, all_of(names(wc_07_2023)[str_detect(names(wc_07_2023), "f1")])) %>%
  mutate(radar_replique = "a",
         plot = ifelse(plot == "A", "Control", "TFE")) %>%
  select(ID, radar_replique, everything())
names(wc_07_2023_a) <- str_remove_all(names(wc_07_2023_a), "_f1")

wc_07_2023_b <- wc_07_2023 %>%
  select(ID, plot, date, all_of(names(wc_07_2023)[str_detect(names(wc_07_2023), "f2")])) %>%
  mutate(radar_replique = "b",
         plot = ifelse(plot == "A", "Control", "TFE")) %>%
  select(ID, radar_replique, everything())
names(wc_07_2023_b) <- str_remove_all(names(wc_07_2023_b), "_f2")

wc_07_2023_c <- wc_07_2023 %>%
  select(ID, plot, date, all_of(names(wc_07_2023)[str_detect(names(wc_07_2023), "f3")])) %>%
  mutate(radar_replique = "c",
         plot = ifelse(plot == "A", "Control", "TFE")) %>%
  select(ID, radar_replique, everything())
names(wc_07_2023_c) <- str_remove_all(names(wc_07_2023_c), "_f3")

wc_07_2023_long <- bind_rows(wc_07_2023_a, wc_07_2023_b, wc_07_2023_c) %>%
  mutate(id_radar_replique = paste0(plot, "_", ID, "_", radar_replique)) %>%
  select(-ID, -radar_replique, -plot, -date)


## Merge WP and WC

wp_wc_07_2023.data <- merge(wp_07_2023_long, wc_07_2023_long, 
                            by = "id_radar_replique", 
                            all = T, 
                            suffixes = c("wp", "wc"))


# bring all names to lower case to avoid double identification of variables
names(wp_wc_07_2023.data) <- tolower(names(wp_wc_07_2023.data))

# negative values

wp_wc_07_2023.data <- wp_wc_07_2023.data %>%
  filter(!is.na(plot)) %>%
  # make sure there are no positive values for wp
  mutate(wp_md = abs(wp_md)*-1,
         wp_pd = abs(wp_pd)*-1)

radar_wp_wc_07_2023.data <- wp_wc_07_2023.data %>%
  filter(radar == TRUE) %>%
  select(-radar) %>%
  mutate(campaign = "07-2023")


## Write campaign data
head(radar_wp_wc_07_2023.data)
write_csv(radar_wp_wc_07_2023.data, file = "data_processed/leaf_wp_wc/radar_wp_wc_07_2023.csv")


### 10/2023 radar campaign ####

## WP data 

wp_10_2023.data <- read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_10_2023.xlsx",
                              sheet = 1) %>%
  mutate(id_radar_replique = paste0(plot, "_", ID, "_", radar_replique),
         id = paste0(plot, "_", ID),
         WP_md = abs(rowMeans(select(., WP_md_1, WP_md_2, WP_md_3), na.rm = T)) * -1,
         WP_pd = abs(rowMeans(select(., WP_pd_1, WP_pd_2, WP_pd_3), na.rm = T)) * -1,
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)
         ) %>%
  select(id_radar_replique, id, radar_replique, date, plot, 
         WP_md, WP_pd,
         Hour_WP_pd, Hour_WP_md
  )

## WC data

wc_10_2023.data <- read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_10_2023.xlsx",
                              sheet = 2) %>%
  mutate(id_radar_replique = paste0(plot, "_", ID, "_", radar_replique)) %>%
  select(-ID, -radar_replique, -plot, -date, -notes)

## Merge WP and WC

radar_wp_wc_10_2023.data <- merge(wp_10_2023.data, wc_10_2023.data, 
                            by = "id_radar_replique", 
                            all = T, 
                            suffixes = c("wp", "wc")) %>%
  mutate(campaign = "10-2023")


# bring all names to lower case to avoid double identification of variables
names(radar_wp_wc_10_2023.data) <- tolower(names(radar_wp_wc_10_2023.data))


## Save campaign data

write_csv(radar_wp_wc_10_2023.data, file = "data_processed/leaf_wp_wc/radar_wp_wc_10_2023.csv")


### 12/2023 radar campaign ####

wp_12_2023 <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_12_2023.xlsx",
                                 sheet = 1)

wc_12_2023 <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_12_2023.xlsx",
                                 sheet = 2)


## data needs to be changed from wide to long format 

## WP

wp_12_2023_a <- wp_12_2023 %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp_12_2023)[str_detect(names(wp_12_2023), "_1")])) %>%
  mutate(radar_replique = "a",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)
  ) %>%
  select(ID, radar_replique, everything())
names(wp_12_2023_a) <- str_remove_all(names(wp_12_2023_a), "_1")

wp_12_2023_b <- wp_12_2023 %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp_12_2023)[str_detect(names(wp_12_2023), "_2")])) %>%
  mutate(radar_replique = "b",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_12_2023_b) <- str_remove_all(names(wp_12_2023_b), "_2")

wp_12_2023_c <- wp_12_2023 %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp_12_2023)[str_detect(names(wp_12_2023), "_3")])) %>%
  mutate(radar_replique = "c",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_12_2023_c) <- str_remove_all(names(wp_12_2023_c), "_3")

wp_12_2023.data <- bind_rows(wp_12_2023_a, wp_12_2023_b, wp_12_2023_c) %>%
  mutate(id_radar_replique = paste0(plot, "_", ID, "_", radar_replique),
         id = paste0(plot, "_", ID)) %>%
  select(id_radar_replique, id, radar, radar_replique, date, plot, 
         WP_md, WP_pd,
         Hour_WP_pd, Hour_WP_md
  )

## WC data

wc_12_2023.data <- read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_12_2023.xlsx",
                              sheet = 2) %>%
  mutate(plot = ifelse(plot == "A", "Control", "TFE"),
         id_radar_replique = paste0(plot, "_", ID, "_", radar_replique)) %>%
  select(-ID, -radar_replique, -plot, -date, -notes)

## Merge WP and WC

radar_wp_wc_12_2023.data <- merge(wp_12_2023.data, wc_12_2023.data, 
                                  by = "id_radar_replique", 
                                  all = T, 
                                  suffixes = c("wp", "wc")) %>%
  mutate(campaign = "10-2023")


# bring all names to lower case to avoid double identification of variables
names(radar_wp_wc_12_2023.data) <- tolower(names(radar_wp_wc_12_2023.data))


# negative values

radar_wp_wc_12_2023.data <- radar_wp_wc_12_2023.data %>%
  filter(!is.na(plot)) %>%
  # make sure there are no positive values for wp
  mutate(wp_md = abs(wp_md)*-1,
         wp_pd = abs(wp_pd)*-1)

radar_wp_wc_12_2023.data <- radar_wp_wc_12_2023.data %>%
  filter(radar == TRUE) %>%
  select(-radar) %>%
  mutate(campaign = "12-2023")


## Save campaign data

write_csv(radar_wp_wc_12_2023.data, file = "data_processed/leaf_wp_wc/radar_wp_wc_12_2023.csv")


### 02/2024 radar campaign ####

wp_02_2024 <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_02_2024.xlsx",
                                 sheet = 1)

wc_12_2023 <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_02_2024.xlsx",
                                 sheet = 2)

## data needs to be changed from wide to long format 

## WP

wp_02_2024_a <- wp_02_2024 %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp_02_2024)[str_detect(names(wp_02_2024), "_1")])) %>%
  mutate(radar_replique = "a",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)
  ) %>%
  select(ID, radar_replique, everything())
names(wp_02_2024_a) <- str_remove_all(names(wp_02_2024_a), "_1")

wp_02_2024_b <- wp_02_2024 %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp_02_2024)[str_detect(names(wp_02_2024), "_2")])) %>%
  mutate(radar_replique = "b",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_02_2024_b) <- str_remove_all(names(wp_02_2024_b), "_2")

wp_02_2024_c <- wp_02_2024 %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp_02_2024)[str_detect(names(wp_02_2024), "_3")])) %>%
  mutate(radar_replique = "c",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_02_2024_c) <- str_remove_all(names(wp_02_2024_c), "_3")

wp_02_2024.data <- bind_rows(wp_02_2024_a, wp_02_2024_b, wp_02_2024_c) %>%
  mutate(id_radar_replique = paste0(plot, "_", ID, "_", radar_replique),
         id = paste0(plot, "_", ID)) %>%
  select(id_radar_replique, id, radar, radar_replique, date, plot, 
         WP_md, WP_pd,
         Hour_WP_pd, Hour_WP_md
  )

## WC data

wc_02_2024.data <- read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_12_2023.xlsx",
                              sheet = 2) %>%
  mutate(plot = ifelse(plot == "A", "Control", "TFE"),
         id_radar_replique = paste0(plot, "_", ID, "_", radar_replique)) %>%
  select(-ID, -radar_replique, -plot, -date, -notes)

## Merge WP and WC

radar_wp_wc_02_2024.data <- merge(wp_02_2024.data, wc_02_2024.data, 
                                  by = "id_radar_replique", 
                                  all = T, 
                                  suffixes = c("wp", "wc")) %>%
  mutate(campaign = "10-2023")


# bring all names to lower case to avoid double identification of variables
names(radar_wp_wc_02_2024.data) <- tolower(names(radar_wp_wc_02_2024.data))


# negative values

radar_wp_wc_02_2024.data <- radar_wp_wc_02_2024.data %>%
  filter(!is.na(plot)) %>%
  # make sure there are no positive values for wp
  mutate(wp_md = abs(wp_md)*-1,
         wp_pd = abs(wp_pd)*-1)

radar_wp_wc_02_2024.data <- radar_wp_wc_02_2024.data %>%
  filter(radar == TRUE) %>%
  select(-radar) %>%
  mutate(campaign = "12-2023")


## Save campaign data

write_csv(radar_wp_wc_02_2024.data, file = "data_processed/leaf_wp_wc/radar_wp_wc_02_2024.csv")


### Bind rows for different campaings ####

radar_wp_wc_05_2023.data <- read_csv( "data_processed/leaf_wp_wc/radar_wp_wc_05_2023.csv")
radar_wp_wc_07_2023.data <- read_csv( "data_processed/leaf_wp_wc/radar_wp_wc_07_2023.csv")
radar_wp_wc_10_2023.data <- read_csv( "data_processed/leaf_wp_wc/radar_wp_wc_10_2023.csv")
radar_wp_wc_12_2023.data <- read_csv( "data_processed/leaf_wp_wc/radar_wp_wc_12_2023.csv")
radar_wp_wc_02_2024.data <- read_csv( "data_processed/leaf_wp_wc/radar_wp_wc_02_2024.csv")

## 05_2023,  07_2023 and 10_2023

wp_wc_052023_072023_102023_122023_022024.data <- bind_rows(
  radar_wp_wc_05_2023.data, 
  radar_wp_wc_07_2023.data, 
  radar_wp_wc_10_2023.data,
  radar_wp_wc_12_2023.data,
  radar_wp_wc_02_2024.data
  )

names(wp_wc_052023_072023_102023_122023_022024.data)
head(wp_wc_052023_072023_102023_122023_022024.data)

write_csv(wp_wc_052023_072023_102023_122023_022024.data, file = "data_processed/leaf_wp_wc/complete_datasets/radar_wp_wc_05_2023_02_2024.csv")

# to general

write_csv(wp_wc_052023_072023_102023_122023_022024.data, paste0(root.dir, "data_processed/leaf_water_potential_water_content/complete_datasets/radar_wp_wc_05_2023_02_2024.csv"))

