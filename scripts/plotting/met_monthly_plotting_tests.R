source("initialization.R")
source("scripts/functions/functions.R")

met_control <- read.csv("data_processed/met/2022-2023_met_control_processed.csv", header = T)


met_control_11_2022 <- met_control %>%
  filter(timestamp > "2022-11-01", timestamp < "2022-12-01") %>%
  mutate(timestamp = as_datetime(timestamp))

met_control_11_2023 <- met_control %>%
  filter(timestamp > "2023-09-01", timestamp < "2023-09-01") %>%
  mutate(timestamp = as_datetime(timestamp))

plotTimeSeries(data = met_control_11_2022,
               xVar = timestamp,
               yVar = vpd28m_kPa,
               xLab = "time", 
               yLab = "VPD (KPa)", 
               lineOrPoint = "line")


plotTimeSeries(data = met_control_11_2023,
               xVar = timestamp,
               yVar = vpd28m_kPa,
               xLab = "time", 
               yLab = "VPD (KPa)", 
               lineOrPoint = "line")
