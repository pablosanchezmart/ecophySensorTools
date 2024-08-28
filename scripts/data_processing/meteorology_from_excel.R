#### METEOROLOGICAL DATA PROCESSING (to do, now used only for recent data) #####

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")


### FETCH CONTROL TOWER DATA --------------------------------------------------- ####

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/observations/excel_in/TORRE PA/"
processed_folder_out <- paste0("data_processed/met/from_excel/")


### 2023 ####

file.name <- list.files(raw_folder_in, pattern = "2023", full.names = T)
met_2023_raw <- readxl::read_excel(file.name, sheet = 1)

### 2024 ####

file.name <- list.files(raw_folder_in, pattern = "2024", full.names = T)
met_2024_raw <- readxl::read_excel(file.name, sheet = 1)

# names(met_2024_raw)  %in% names(met_2024_raw)

### combine and clean ####

met_2023_2024_processed <- bind_rows(met_2023_raw, met_2024_raw) %>%
  rename("year" = "Ano", day = "Dia", month = "Mês", "time" = "Hora", yday = "Dias Juliano")  %>%
  mutate(time = as_hms(time),
         date = as_date(make_datetime(year, month, day)),
         timestamp = as_datetime(paste0(date, " ", time))) %>% 
  select(timestamp, date, yday, everything(), -day, -month, -year, -time) %>%
  arrange(timestamp)

### rename and save ####

names(met_2023_2024_processed) <- tolower(names(met_2023_2024_processed))

raw_processed_names <- met_variables.names %>%
  filter(abbreviation_raw %in% names(met_2023_2024_processed))

named_met_2023_2024_processed <- data.table::setnames(met_2023_2024_processed, 
                                                      old = raw_processed_names$abbreviation_raw, 
                                                      new = raw_processed_names$abbreviation_processed)

vpd_named_met_2023_2024_processed <- named_met_2023_2024_processed %>%
  mutate(vpd2m_kPa = bigleaf::rH.to.VPD(rh2m_perc/100,t2m_C),
       vpd16m_kPa = bigleaf::rH.to.VPD(rh16m_perc/100,t16m_C),
       vpd28m_kPa = bigleaf::rH.to.VPD(rh16m_perc/100,t28m_C),
       vpd42m_kPa = bigleaf::rH.to.VPD(rh42m_perc/100,t42m_C)) %>%
  select(-battv_min, ptemp_c_avg)


### GAP FILL ####

varsToGapfill <- names(vpd_named_met_2023_2024_processed)[!names(vpd_named_met_2023_2024_processed) %in% c("timestamp", "date", "yday", "precip_mm")]

vpd_named_met_2023_2024_processed$ID <- "Control"

for(var in varsToGapfill){
  vpd_named_met_2023_2024_processed <- gapFillTimeSeries(data = vpd_named_met_2023_2024_processed, 
                                                 variable = var)
}

vpd_named_met_2023_2024_processed$ID <- NULL

names(vpd_named_met_2023_2024_processed)


#### SAVE ---------------------------------------------------------------------- ####


write_csv(vpd_named_met_2023_2024_processed, paste0(processed_folder_out,
                                          min(as_date(vpd_named_met_2023_2024_processed$date), na.rm = T), "-", 
                                          max(as_date(vpd_named_met_2023_2024_processed$date), na.rm = T),
                                          "_met_control_processed.csv"))

# to general
write_csv(vpd_named_met_2023_2024_processed, paste0(root.dir, "data_processed/met/",
                                                    min(as_date(vpd_named_met_2023_2024_processed$date), na.rm = T), "-", 
                                                    max(as_date(vpd_named_met_2023_2024_processed$date), na.rm = T),
                                                    "_met_control_processed.csv"))


### FETCH TFE TOWER DATA ------------------------------------------------------- ####

raw_folder_in <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data/caxuana_meteo/observations/excel_in/TORRE PB/"
processed_folder_out <- paste0("data_processed/met/from_excel/")


### 2023 ####

file.name <- list.files(raw_folder_in, pattern = "2023", full.names = T)
met_2023_raw <- readxl::read_excel(file.name, sheet = 1)


### 2024 ####

file.name <- list.files(raw_folder_in, pattern = "2024", full.names = T)
met_2024_raw <- readxl::read_excel(file.name, sheet = 1)

# names(met_2024_raw)%in% names(met_2024_raw)  

### combine and clean ####

met_2023_2024_processed <- bind_rows(met_2023_raw, met_2024_raw) %>%
  rename("year" = "Ano", day = "Dia", month = "Mês", "time" = "Hora", yday = "Dias Juliano")  %>%
  mutate(time = as_hms(time),
         date = as_date(make_datetime(year, month, day)),
         timestamp = as_datetime(paste0(date, " ", time))) %>%
  select(timestamp, date, yday, everything(), -day, -month, -year, -time) %>%
  arrange(timestamp)

### rename ####

names(met_2023_2024_processed) <- tolower(names(met_2023_2024_processed))

raw_processed_names <- tfe_met_variables.names %>%
  filter(abbreviation_raw %in% names(met_2023_2024_processed))

named_met_2023_2024_processed <- data.table::setnames(met_2023_2024_processed, 
                                                      old = raw_processed_names$abbreviation_raw, 
                                                      new = raw_processed_names$abbreviation_processed)

vpd_named_met_2023_2024_processed <- named_met_2023_2024_processed %>%
  mutate(vpd_belowRoof_kPa = bigleaf::rH.to.VPD(rh_belowRoof_perc/100, t_mean_belowRoof_C),
         vpd_aboveRoof_kPa = bigleaf::rH.to.VPD(rh_aboveRoof_perc/100, t_aboveRoof_C),
         vpd42m_kPa = bigleaf::rH.to.VPD(rh42m_perc/100, t42m_mean_C)) %>%
  select(-battv_min, ptemp_c_avg)

names(vpd_named_met_2023_2024_processed)


### GAP FILL ####

varsToGapfill <- names(vpd_named_met_2023_2024_processed)[!names(vpd_named_met_2023_2024_processed) %in% c("timestamp", "date", "yday", "precip_mm")]

vpd_named_met_2023_2024_processed$ID <- "TFE"

for(var in varsToGapfill){
  vpd_named_met_2023_2024_processed <- gapFillTimeSeries(data = vpd_named_met_2023_2024_processed, 
                                                            variable = var)
}

vpd_named_met_2023_2024_processed$ID <- NULL

summary(vpd_named_met_2023_2024_processed)


#### SAVE ---------------------------------------------------------------------- ####

write_csv(vpd_named_met_2023_2024_processed, paste0(processed_folder_out,
                                          min(as_date(vpd_named_met_2023_2024_processed$date), na.rm = T), "-", 
                                          max(as_date(vpd_named_met_2023_2024_processed$date), na.rm = T),
                                          "_met_tfe_processed.csv"))

# to general
write_csv(vpd_named_met_2023_2024_processed, paste0(root.dir, "data_processed/met/",
                                                    min(as_date(vpd_named_met_2023_2024_processed$date), na.rm = T), "-", 
                                                    max(as_date(vpd_named_met_2023_2024_processed$date), na.rm = T),
                                                    "_met_tfe_processed.csv"))
