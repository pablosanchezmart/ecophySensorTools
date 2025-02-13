#### DATA PROCESSING ###########################################################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")


#### DATA FOR EACH CAMPAIGN ---------------------------------------------------- ####

#### 05/2023 big campaign ####

## WP data

wp <- read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_05_2023.xlsx",
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
head(wp)
write_csv(wp, "data_processed/leaf_wp_wc/big_campaign_wp_wc_05_2023.csv")


### 05/2023 Radar trees ####

wp <-  wp  %>%
  filter(radar == T)
  
## WC data

wc <- read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_05_2023.xlsx",
                          sheet = 2) %>%
      mutate(plot = ifelse(plot == "A", "Control", "TFE"),
             id_radar_replique = paste0(plot, "_", ID, "_", radar_replique)) %>%
      select(-ID, -radar_replique, -plot, -date, -notes)

## Merge WP and WC

wp_wc <- merge(wp, wc, 
               by = "id_radar_replique", 
               all = T, 
               suffixes = c("wp", "wc"))

# bring all names to lower case to avoid double identification of variables
names(wp_wc) <- tolower(names(wp_wc))

radar_wp_wc <- wp_wc %>%
  filter(radar == TRUE) %>%
  select(-radar) %>%
  mutate(campaign = format(date, "%m-%Y"))

## Save campaign data
head(radar_wp_wc)
tail(radar_wp_wc)
write_csv(radar_wp_wc, file = "data_processed/leaf_wp_wc/radar_wp_wc_05_2023.csv")


### 07/2023 radar campaign ####

wp <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_07_2023.xlsx",
                                 sheet = 1)

wc <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_07_2023.xlsx",
                                 sheet = 2)

## data needs to be changed from wide to long format 

## WP

wp_a <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_1")])) %>%
  mutate(radar_replique = "a",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)
         ) %>%
  select(ID, radar_replique, everything())
names(wp_a) <- str_remove_all(names(wp_a), "_1")

wp_b <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_2")])) %>%
  mutate(radar_replique = "b",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_b) <- str_remove_all(names(wp_b), "_2")

wp_c <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_3")])) %>%
  mutate(radar_replique = "c",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_c) <- str_remove_all(names(wp_c), "_3")

wp_long <- bind_rows(wp_a, wp_b, wp_c) %>%
  mutate(id_radar_replique = paste0(plot, "_", ID, "_", radar_replique),
         id = paste0(plot, "_", ID)) %>%
  select(id_radar_replique, id, radar, radar_replique, date, plot, 
         WP_md, WP_pd,
         Hour_WP_pd, Hour_WP_md
  )


## WC

wc_a <- wc %>%
  select(ID, plot, date, all_of(names(wc)[str_detect(names(wc), "f1")])) %>%
  mutate(radar_replique = "a",
         plot = ifelse(plot == "A", "Control", "TFE")) %>%
  select(ID, radar_replique, everything())
names(wc_a) <- str_remove_all(names(wc_a), "_f1")

wc_b <- wc %>%
  select(ID, plot, date, all_of(names(wc)[str_detect(names(wc), "f2")])) %>%
  mutate(radar_replique = "b",
         plot = ifelse(plot == "A", "Control", "TFE")) %>%
  select(ID, radar_replique, everything())
names(wc_b) <- str_remove_all(names(wc_b), "_f2")

wc_c <- wc %>%
  select(ID, plot, date, all_of(names(wc)[str_detect(names(wc), "f3")])) %>%
  mutate(radar_replique = "c",
         plot = ifelse(plot == "A", "Control", "TFE")) %>%
  select(ID, radar_replique, everything())
names(wc_c) <- str_remove_all(names(wc_c), "_f3")

wc_long <- bind_rows(wc_a, wc_b, wc_c) %>%
  mutate(id_radar_replique = paste0(plot, "_", ID, "_", radar_replique)) %>%
  select(-ID, -radar_replique, -plot, -date)


## Merge WP and WC

wp_wc <- merge(wp_long, wc_long, 
                            by = "id_radar_replique", 
                            all = T, 
                            suffixes = c("wp", "wc"))


# bring all names to lower case to avoid double identification of variables
names(wp_wc) <- tolower(names(wp_wc))

# negative values

wp_wc <- wp_wc %>%
  filter(!is.na(plot)) %>%
  # make sure there are no positive values for wp
  mutate(wp_md = abs(wp_md)*-1,
         wp_pd = abs(wp_pd)*-1)

radar_wp_wc <- wp_wc %>%
  filter(radar == TRUE) %>%
  select(-radar) %>%
  mutate(campaign = format(date, "%m-%Y"))


## Write campaign data
head(radar_wp_wc)
tail(radar_wp_wc)
write_csv(radar_wp_wc, file = "data_processed/leaf_wp_wc/radar_wp_wc_07_2023.csv")


### 10/2023 radar campaign ####

## WP data 

wp <- read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_10_2023.xlsx",
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

wc <- read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_10_2023.xlsx",
                              sheet = 2) %>%
  mutate(id_radar_replique = paste0(plot, "_", ID, "_", radar_replique)) %>%
  select(-ID, -radar_replique, -plot, -date, -notes)

## Merge WP and WC

radar_wp_wc <- merge(wp, wc, 
                            by = "id_radar_replique", 
                            all = T, 
                            suffixes = c("wp", "wc")) %>%
  mutate(campaign = format(date, "%m-%Y"))


# bring all names to lower case to avoid double identification of variables
names(radar_wp_wc) <- tolower(names(radar_wp_wc))


## Save campaign data
head(radar_wp_wc)
tail(radar_wp_wc)
write_csv(radar_wp_wc, file = "data_processed/leaf_wp_wc/radar_wp_wc_10_2023.csv")


### 12/2023 radar campaign ####

wp <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_12_2023.xlsx",
                                 sheet = 1)

wc <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_12_2023.xlsx",
                                 sheet = 2)


## data needs to be changed from wide to long format 

## WP

wp_a <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_1")])) %>%
  mutate(radar_replique = "a",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)
  ) %>%
  select(ID, radar_replique, everything())
names(wp_a) <- str_remove_all(names(wp_a), "_1")

wp_b <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_2")])) %>%
  mutate(radar_replique = "b",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_b) <- str_remove_all(names(wp_b), "_2")

wp_c <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_3")])) %>%
  mutate(radar_replique = "c",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_c) <- str_remove_all(names(wp_c), "_3")

wp <- bind_rows(wp_a, wp_b, wp_c) %>%
  mutate(id_radar_replique = paste0(plot, "_", ID, "_", radar_replique),
         id = paste0(plot, "_", ID)) %>%
  select(id_radar_replique, id, radar, radar_replique, date, plot, 
         WP_md, WP_pd,
         Hour_WP_pd, Hour_WP_md
  )


## WC data

wc <- read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_12_2023.xlsx",
                              sheet = 2) %>%
  mutate(plot = ifelse(plot == "A", "Control", "TFE"),
         id_radar_replique = paste0(plot, "_", ID, "_", radar_replique)) %>%
  select(-ID, -radar_replique, -plot, -date, -notes)

## Merge WP and WC

radar_wp_wc <- merge(wp, wc, 
                     by = "id_radar_replique", 
                     all = T, 
                     suffixes = c("wp", "wc"))


# bring all names to lower case to avoid double identification of variables
names(radar_wp_wc) <- tolower(names(radar_wp_wc))


# negative values

radar_wp_wc <- radar_wp_wc %>%
  filter(!is.na(plot)) %>%
  # make sure there are no positive values for wp
  mutate(wp_md = abs(wp_md)*-1,
         wp_pd = abs(wp_pd)*-1)

radar_wp_wc <- radar_wp_wc %>%
  filter(radar == TRUE) %>%
  select(-radar) %>%
  mutate(campaign = format(date, "%m-%Y"))


## Save campaign data
head(radar_wp_wc)
tail(radar_wp_wc)
write_csv(radar_wp_wc, file = "data_processed/leaf_wp_wc/radar_wp_wc_12_2023.csv")


### 02/2024 radar campaign ####

wp <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_02_2024.xlsx",
                                 sheet = 1)

wc <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_02_2024.xlsx",
                                 sheet = 2)

## data needs to be changed from wide to long format 

## WP

wp_a <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_1")])) %>%
  mutate(radar_replique = "a",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)
  ) %>%
  select(ID, radar_replique, everything())
names(wp_a) <- str_remove_all(names(wp_a), "_1")

wp_b <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_2")])) %>%
  mutate(radar_replique = "b",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_b) <- str_remove_all(names(wp_b), "_2")

wp_c <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_3")])) %>%
  mutate(radar_replique = "c",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_c) <- str_remove_all(names(wp_c), "_3")

wp <- bind_rows(wp_a, wp_b, wp_c) %>%
  mutate(id_radar_replique = paste0(plot, "_", ID, "_", radar_replique),
         id = paste0(plot, "_", ID)) %>%
  select(id_radar_replique, id, radar, radar_replique, date, plot, 
         WP_md, WP_pd,
         Hour_WP_pd, Hour_WP_md
  )

## WC data

wc <- read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_12_2023.xlsx",
                              sheet = 2) %>%
  mutate(plot = ifelse(plot == "A", "Control", "TFE"),
         id_radar_replique = paste0(plot, "_", ID, "_", radar_replique)) %>%
  select(-ID, -radar_replique, -plot, -date, -notes)

## Merge WP and WC

radar_wp_wc <- merge(wp, wc,
                     by = "id_radar_replique", 
                     all = T, 
                     suffixes = c("wp", "wc"))


# bring all names to lower case to avoid double identification of variables
names(radar_wp_wc) <- tolower(names(radar_wp_wc))


# negative values

radar_wp_wc <- radar_wp_wc %>%
  filter(!is.na(plot)) %>%
  # make sure there are no positive values for wp
  mutate(wp_md = abs(wp_md)*-1,
         wp_pd = abs(wp_pd)*-1)

radar_wp_wc <- radar_wp_wc %>%
  filter(radar == TRUE) %>%
  select(-radar) %>%
  mutate(campaign = format(date, "%m-%Y"))


## Save campaign data
head(radar_wp_wc)
tail(radar_wp_wc)
write_csv(radar_wp_wc, file = "data_processed/leaf_wp_wc/radar_wp_wc_02_2024.csv")


### 05/2024 radar campaign ####

wp <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_05_2024.xlsx",
                                 sheet = 1)

wc <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_05_2024.xlsx",
                                 sheet = 2) %>%
  mutate(plot = ifelse(plot == "A", "Control", "TFE"),
         id_radar_replique = paste0(plot, "_", ID, "_", radar_replique)) %>%
  select(-ID, -radar_replique, -plot, -date, -notes)

## data needs to be changed from wide to long format 

## WP

wp_a <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_1")])) %>%
  mutate(radar_replique = "a",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)
  ) %>%
  select(ID, radar_replique, everything())
names(wp_a) <- str_remove_all(names(wp_a), "_1")

wp_b <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_2")])) %>%
  mutate(radar_replique = "b",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_b) <- str_remove_all(names(wp_b), "_2")

wp_c <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_3")])) %>%
  mutate(radar_replique = "c",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_c) <- str_remove_all(names(wp_c), "_3")

wp <- bind_rows(wp_a, wp_b, wp_c) %>%
  mutate(id_radar_replique = paste0(plot, "_", ID, "_", radar_replique),
         id = paste0(plot, "_", ID)) %>%
  select(id_radar_replique, id, radar, radar_replique, date, plot, 
         WP_md, WP_pd,
         Hour_WP_pd, Hour_WP_md
  )


## Merge WP and WC

radar_wp_wc <- merge(wp, wc, 
                     by = "id_radar_replique", 
                     all = T, 
                     suffixes = c("wp", "wc"))


# bring all names to lower case to avoid double identification of variables
names(radar_wp_wc) <- tolower(names(radar_wp_wc))


# negative values

radar_wp_wc <- radar_wp_wc %>%
  filter(!is.na(plot)) %>%
  # make sure there are no positive values for wp
  mutate(wp_md = abs(wp_md)*-1,
         wp_pd = abs(wp_pd)*-1)

radar_wp_wc <- radar_wp_wc %>%
  filter(radar == TRUE) %>%
  select(-radar) %>%
  mutate(campaign = format(date, "%m-%Y"))


## Save campaign data

write_csv(radar_wp_wc, file = "data_processed/leaf_wp_wc/radar_wp_wc_05_2024.csv")


### 09/2024 radar campaign ####

wp <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_09_2024.xlsx",
                                 sheet = 1)

wc <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_09_2024.xlsx",
                                 sheet = 2) %>%
  mutate(plot = ifelse(plot == "A", "Control", "TFE"),
         id_radar_replique = paste0(plot, "_", ID, "_", radar_replique)) %>%
  select(-ID, -radar_replique, -plot, -date, -notes)

## data needs to be changed from wide to long format 

## WP

wp_a <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_1")])) %>%
  mutate(radar_replique = "a",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)
  ) %>%
  select(ID, radar_replique, everything())
names(wp_a) <- str_remove_all(names(wp_a), "_1")

wp_b <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_2")])) %>%
  mutate(radar_replique = "b",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_b) <- str_remove_all(names(wp_b), "_2")

wp_c <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_3")])) %>%
  mutate(radar_replique = "c",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_c) <- str_remove_all(names(wp_c), "_3")

wp <- bind_rows(wp_a, wp_b, wp_c) %>%
  mutate(id_radar_replique = paste0(plot, "_", ID, "_", radar_replique),
         id = paste0(plot, "_", ID)) %>%
  select(id_radar_replique, id, radar, radar_replique, date, plot, 
         WP_md, WP_pd,
         Hour_WP_pd, Hour_WP_md
  )


## Merge WP and WC

radar_wp_wc <- merge(wp, wc, 
                                  by = "id_radar_replique", 
                                  all = T, 
                                  suffixes = c("wp", "wc"))


# bring all names to lower case to avoid double identification of variables
names(radar_wp_wc) <- tolower(names(radar_wp_wc))


# negative values

radar_wp_wc <- radar_wp_wc %>%
  filter(!is.na(plot)) %>%
  # make sure there are no positive values for wp
  mutate(wp_md = abs(wp_md)*-1,
         wp_pd = abs(wp_pd)*-1)

radar_wp_wc <- radar_wp_wc %>%
  filter(radar == TRUE) %>%
  select(-radar) %>%
  mutate(campaign = format(date, "%m-%Y"))


## Save campaign data
head(radar_wp_wc)
tail(radar_wp_wc)
write_csv(radar_wp_wc, file = "data_processed/leaf_wp_wc/radar_wp_wc_09_2024.csv")


### 10/2024 radar campaign ####

wp <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_10_2024.xlsx",
                         sheet = 1)

wc <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_10_2024.xlsx",
                         sheet = 2) %>%
  mutate(plot = ifelse(plot == "A", "Control", "TFE"),
         id_radar_replique = paste0(plot, "_", ID, "_", radar_replique)) %>%
  select(-ID, -radar_replique, -plot, -date, -notes)

## data needs to be changed from wide to long format 

## WP

wp_a <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_1")])) %>%
  mutate(radar_replique = "a",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)
  ) %>%
  select(ID, radar_replique, everything())
names(wp_a) <- str_remove_all(names(wp_a), "_1")

wp_b <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_2")])) %>%
  mutate(radar_replique = "b",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_b) <- str_remove_all(names(wp_b), "_2")

wp_c <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_3")])) %>%
  mutate(radar_replique = "c",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_c) <- str_remove_all(names(wp_c), "_3")

wp <- bind_rows(wp_a, wp_b, wp_c) %>%
  mutate(id_radar_replique = paste0(plot, "_", ID, "_", radar_replique),
         id = paste0(plot, "_", ID)) %>%
  select(id_radar_replique, id, radar, radar_replique, date, plot, 
         WP_md, WP_pd,
         Hour_WP_pd, Hour_WP_md
  )


## Merge WP and WC

radar_wp_wc <- merge(wp, wc, 
                     by = "id_radar_replique", 
                     all = T, 
                     suffixes = c("wp", "wc"))


# bring all names to lower case to avoid double identification of variables
names(radar_wp_wc) <- tolower(names(radar_wp_wc))


# negative values

radar_wp_wc <- radar_wp_wc %>%
  filter(!is.na(plot)) %>%
  # make sure there are no positive values for wp
  mutate(wp_md = abs(wp_md)*-1,
         wp_pd = abs(wp_pd)*-1)

radar_wp_wc <- radar_wp_wc %>%
  filter(radar == TRUE) %>%
  select(-radar) %>%
  mutate(campaign = format(date, "%m-%Y"))


## Save campaign data
head(radar_wp_wc)
tail(radar_wp_wc)
write_csv(radar_wp_wc, file = "data_processed/leaf_wp_wc/radar_wp_wc_10_2024.csv")


### 11/2024 radar campaign ####

wp <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_11_2024.xlsx",
                         sheet = 1) %>%
  mutate(radar = TRUE)

wc <- readxl::read_excel("C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_physiology/caxuana_leaf_water_content_water_potential/caxiuana_water_potentials_vwc_11_2024.xlsx",
                         sheet = 2) %>%
  mutate(plot = ifelse(plot == "A", "Control", "TFE"),
         id_radar_replique = paste0(plot, "_", ID, "_", radar_replique)) %>%
  select(-ID, -radar_replique, -plot, -date, -notes)

## data needs to be changed from wide to long format 

## WP

wp_a <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_1")])) %>%
  mutate(radar_replique = "a",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)
  ) %>%
  select(ID, radar_replique, everything())
names(wp_a) <- str_remove_all(names(wp_a), "_1")

wp_b <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_2")])) %>%
  mutate(radar_replique = "b",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_b) <- str_remove_all(names(wp_b), "_2")

wp_c <- wp %>%
  select(ID, radar, plot, date_pd, Hour_WP_pd, date_md, Hour_WP_md, collector, all_of(names(wp)[str_detect(names(wp), "_3")])) %>%
  mutate(radar_replique = "c",
         plot = ifelse(plot == "A", "Control", "TFE"),
         Hour_WP_md = format(as.POSIXct(Hour_WP_md), format = "%H:%M"),
         Hour_WP_pd = format(as.POSIXct(Hour_WP_pd), format = "%H:%M"),
         date = ymd(date_md)) %>%
  select(ID, radar_replique, everything())
names(wp_c) <- str_remove_all(names(wp_c), "_3")

wp <- bind_rows(wp_a, wp_b, wp_c) %>%
  mutate(id_radar_replique = paste0(plot, "_", ID, "_", radar_replique),
         id = paste0(plot, "_", ID)) %>%
  select(id_radar_replique, id, radar, radar_replique, date, plot, 
         WP_md, WP_pd,
         Hour_WP_pd, Hour_WP_md
  )


## Merge WP and WC

radar_wp_wc <- merge(wp, wc, 
                     by = "id_radar_replique", 
                     all = T, 
                     suffixes = c("wp", "wc"))


# bring all names to lower case to avoid double identification of variables
names(radar_wp_wc) <- tolower(names(radar_wp_wc))


# negative values

radar_wp_wc <- radar_wp_wc %>%
  filter(!is.na(plot)) %>%
  # make sure there are no positive values for wp
  mutate(wp_md = abs(wp_md)*-1,
         wp_pd = abs(wp_pd)*-1)

radar_wp_wc <- radar_wp_wc %>%
  filter(radar == TRUE) %>%
  select(-radar) %>%
  mutate(campaign = format(date, "%m-%Y"))


## Save campaign data
head(radar_wp_wc)
tail(radar_wp_wc)
write_csv(radar_wp_wc, file = "data_processed/leaf_wp_wc/radar_wp_wc_11_2024.csv")


### Bind rows for different campaings ####

radar_wp_wc_05_2023 <- read_csv( "data_processed/leaf_wp_wc/radar_wp_wc_05_2023.csv")
radar_wp_wc_07_2023 <- read_csv( "data_processed/leaf_wp_wc/radar_wp_wc_07_2023.csv")
radar_wp_wc_10_2023 <- read_csv( "data_processed/leaf_wp_wc/radar_wp_wc_10_2023.csv")
radar_wp_wc_12_2023 <- read_csv( "data_processed/leaf_wp_wc/radar_wp_wc_12_2023.csv")
radar_wp_wc_02_2024 <- read_csv( "data_processed/leaf_wp_wc/radar_wp_wc_02_2024.csv")
radar_wp_wc_05_2024 <- read_csv( "data_processed/leaf_wp_wc/radar_wp_wc_05_2024.csv")
radar_wp_wc_09_2024 <- read_csv( "data_processed/leaf_wp_wc/radar_wp_wc_09_2024.csv")
radar_wp_wc_10_2024 <- read_csv( "data_processed/leaf_wp_wc/radar_wp_wc_10_2024.csv")
radar_wp_wc_11_2024 <- read_csv( "data_processed/leaf_wp_wc/radar_wp_wc_11_2024.csv")

## 05_2023,  07_2023 and 10_2023

all_wp_wc <- bind_rows(
  radar_wp_wc_05_2023, 
  radar_wp_wc_07_2023, 
  radar_wp_wc_10_2023,
  radar_wp_wc_12_2023,
  radar_wp_wc_02_2024,
  radar_wp_wc_05_2024,
  radar_wp_wc_09_2024,
  radar_wp_wc_10_2024,
  radar_wp_wc_11_2024
  )

names(all_wp_wc)
head(all_wp_wc)
tail(all_wp_wc)
all_wp_wc %>% arrange(date)
write_csv(all_wp_wc, file = "data_processed/leaf_wp_wc/complete_datasets/radar_wp_wc_05_2023_11_2024.csv")

# to general

write_csv(all_wp_wc, paste0(root.dir, "data_processed/leaf_water_potential_water_content/complete_datasets/radar_wp_wc_05_2023_11_2024.csv"))

