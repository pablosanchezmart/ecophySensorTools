#### RELATIONSHIP STEM WATER CONTENT - VPD #####################################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")
source("scripts/functions/functions.R")


#### DATA ---------------------------------------------------------------------- ####

## Meteorological data

met_2023 <- read_csv("data_processed/met/2022-2023_met_control_processed.csv") %>%
  filter(timestamp > "2023-05-01")

met.variables <- c("vpd28m_kPa", "rh28m_perc", "t28m_C")


## Water content

stem_wc_2023 <- read_csv("data_processed/stem_water_content/processed_stem_water_content_2023-09-29.csv") %>%
  filter(timestamp > "2023-05-01") %>%
  filter(!is.na(ID))

stem_wc.variables <- c("calibrated_water_content_m3.m3", "soil_temperature_C")

stem_wc_met_2023 <- merge(stem_wc_2023,
                          met_2023,
                          by = "timestamp",
                          all.x = T) %>%
  filter(timestamp < "2023-10-01") %>%
  mutate(hour = hour(timestamp),
         date = as_date(timestamp),
         date_hour = paste0(date, "_", hour),
         date_hour_id = paste0(date_hour, "_", ID))

head(stem_wc_met_2023)


## Aggregate per hour

hourly_stem_wc_met_2023 <- aggregate(stem_wc_met_2023[, c(met.variables, stem_wc.variables, c("date_hour", "plot", "ID", "species"))],
                                     by = list(stem_wc_met_2023$date_hour_id),
                                     FUN = meanOrMode) %>%
  rename(date_hour_id = Group.1) %>%
  mutate(timestamp = ymd_h(date_hour)) %>%
  select(timestamp, plot, ID, species, everything(), -date_hour_id, -date_hour)

head(hourly_stem_wc_met_2023)

write_csv(hourly_stem_wc_met_2023, "data_processed/analysis_hourly_stem_water_content_met.csv")

## Aggregate per day

stem_wc_met_2023$date_id <- paste0(stem_wc_met_2023$date, "_", stem_wc_met_2023$ID)

daily_stem_wc_met_2023 <- aggregate(stem_wc_met_2023[, c(met.variables, 
                                                         stem_wc.variables, 
                                                         c("date", "plot", "ID", "species"))],
                                          by = list(stem_wc_met_2023$date_id),
                                          FUN = meanOrMode) %>%
  rename(date_id = Group.1) %>%
  mutate(timestamp = ymd(date)) %>%
  select(timestamp, plot, ID, species, everything(), -date)

head(daily_stem_wc_met_2023)

write_csv(daily_stem_wc_met_2023, "data_processed/analysis_daily_stem_water_content_met.csv")


#### PLOTTING ------------------------------------------------------------------ ####

hourly_stem_wc_met_2023 <- read_csv("data_processed/analysis_hourly_stem_water_content_met.csv")
daily_stem_wc_met_2023 <- read_csv("data_processed/analysis_daily_stem_water_content_met.csv")

### met vpd ####

vpd_28m.plot <- ggplot(data = met_2023, 
                       aes(x = timestamp, 
                           y = vpd28m)) + 
  geom_point(color = "grey") +
  geom_smooth(method = "gam", color = "black") +
  theme(legend.position = "none") + 
  theme_minimal()
vpd_28m.plot


### met rh ####

rh28m.plot <- ggplot(data = met_2023, 
                       aes(x = timestamp, 
                           y = rh28m)) + 
  geom_point(color = "grey") +
  geom_smooth(method = "gam", color = "black") +
  theme(legend.position = "none") + 
  theme_minimal()
rh28m.plot

### met temperature ####

t28m.plot <- ggplot(data = met_2023, 
                     aes(x = timestamp, 
                         y = t28m)) + 
  geom_point(color = "grey") +
  geom_smooth(method = "gam", color = "black") +
  theme(legend.position = "none") + 
  theme_minimal()
t28m.plot


#### Individual plots met vpd vs. stem wc ####

# ind <- unique(stem_wc_met_2023$ID)[1]
for(ind in unique(hourly_stem_wc_met_2023$ID)){
  
  ind_data <- hourly_stem_wc_met_2023 %>% 
    filter(ID == ind)
  
  # Save the plot
  pdf(paste0("outputs/analysis/stem_water_content_met/stem_water_content_vpd_", ind, "_", str_replace(unique(ind_data$species), " ", "_"),".pdf"))
  ind_stem_wc.plot <- ggplot(data = ind_data) + 
    geom_line(aes(x = timestamp, 
                  y = calibrated_water_content_m3.m3)) + 
    theme(legend.position = "none") + 
    xlab("time") + ylab("Stem water content (m³ m³)") + 
    theme_minimal()
  # ind_stem_wc.plot
  
  ind_vpd_28m.plot <- ggplot(data = ind_data, 
                     aes(x = timestamp,
                         y = vpd28m)) + 
    # geom_point(color = "grey") +
    # geom_smooth(method = "gam", color = "black") +
    geom_line() +
    xlab("time") + ylab("VPD") + 
    theme_minimal()
  # ind_vpd_28m.plot
  
  ind_vpd_28m_stem_wc.plot <- ggplot(data = ind_data, 
                             aes(x = vpd28m,
                                 y = calibrated_water_content_m3.m3)) + 
    geom_point(color = "grey") +
    geom_smooth(method = "lm", color = "black") +
    xlab("VPD") + ylab("Stem water content (m³ m³)") +
    theme_minimal() +
    # stat_regline_equation(aes(label = ..eq.label..)) +
    stat_regline_equation(aes(label = ..rr.label..))
  # ind_vpd_28m_stem_wc.plot
  
  plot(ggarrange(ind_vpd_28m.plot, 
            ind_stem_wc.plot,
            ind_vpd_28m_stem_wc.plot,
            nrow = 3))
  dev.off()
}


#### ALL DATA LINEAR MODELS----------------------------------------------------- ####

## Distribution

ggplot(data = daily_stem_wc_met_2023, aes (x = calibrated_water_content_m3.m3)) +
  geom_density()

ggplot(data = daily_stem_wc_met_2023, aes (x = log(calibrated_water_content_m3.m3))) +
  geom_density()


### Plot effect on stem wc ####

stem_wc.mmod <- lmer(formula = log(calibrated_water_content_m3.m3) ~ plot + (1|ID), 
                         data = daily_stem_wc_met_2023)
summary(stem_wc.mmod)
r.squaredGLMM(stem_wc.mmod)


### Stem wc vs. vpd ####

stem_wc_vpd.mmod <- lmer(formula = log(calibrated_water_content_m3.m3) ~ vpd28m_kPa * plot + (1|ID), 
                               data = daily_stem_wc_met_2023)
summary(stem_wc_vpd.mmod)
r.squaredGLMM(stem_wc_vpd.mmod)

interact_plot(stem_wc_vpd.mmod, 
              pred = "vpd28m_kPa", 
              modx = "plot", 
              # plot.points = T, 
              x.label = "VPD")

# Save the plot
pdf("outputs/analysis/stem_water_content_met/all_stem_water_content_vs_vpd.pdf")
stem_wc_vpd.plot <- ggplot(data = daily_stem_wc_met_2023, 
                           aes(x = vpd28m_kPa, 
                               y = log(calibrated_water_content_m3.m3),
                               color = plot)) + 
  geom_point(alpha = 0.005) + 
  geom_smooth(method = "lm") +
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("VPD") + ylab("ln(water content (m³ m³)") + 
  theme_minimal()
stem_wc_vpd.plot
dev.off()

