#### RELATIONSHIP SAP FLOW - VPD #####################################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")
source("scripts/functions/functions.R")


#### DATA ---------------------------------------------------------------------- ####

## Meteorological data

met_2023 <- read_csv("data_processed/met/2022-2023_met_control_processed.csv") %>%
  filter(timestamp > "2023-05-01")

met.variables <- c("vpd28m_kPa", "rh28m_perc", "t28m_C")


## Sap flow

sap_flow_2023 <- read_csv("data_processed/sapflow/processed_saplfow_2023-10-01.csv") %>%
  filter(timestamp > "2023-05-01") %>%
  filter(!is.na(ID))
names(sap_flow_2023)
sapflow.variables <- c("bl_sap_flux_Kg_h", "increment_mm")

sapflow_met_2023 <- merge(sap_flow_2023,
                          met_2023,
                          by = "timestamp",
                          all.x = T) %>%
  filter(timestamp < "2023-10-01") %>%
  mutate(hour = hour(timestamp),
         date = as_date(timestamp),
         date_hour = paste0(date, "_", hour),
         date_hour_id = paste0(date_hour, "_", ID))

## Example plot

pdf("outputs/analysis/sapflow_met/example_subdaily_sapflow.pdf", height = h, width = w)
sap_flow_2023 %>%
  mutate(date = as_date(timestamp)) %>%
  filter(ID == "Control_216") %>%
  filter(date == "2023-07-01") %>%
  ggplot(aes(x = timestamp, y = bl_sap_flux_Kg_h)) +
  geom_smooth(method = "gam", color = "black", se = F) +
  ylab("sap flow (kg/h)") + xlab("time") +
  theme_minimal()
dev.off()


pdf("outputs/analysis/sapflow_met/example_subdaily_vpd.pdf", height = h, width = w)
met_2023 %>%
  mutate(date = as_date(timestamp)) %>%
  filter(date == "2023-07-01") %>%
  ggplot(aes(x = timestamp, y = vpd28m_kPa)) +
  geom_smooth(method = "gam", color = "black", se = F) +
  ylab("VPD (kPa)") + xlab("time") +
  theme_minimal()
dev.off()


pdf("outputs/analysis/sapflow_met/example_subdaily_sapflow_vpd.pdf", height = h, width = w)
sapflow_met_2023 %>%
  mutate(date = as_date(timestamp)) %>%
  filter(date == "2023-07-01") %>%
  ggplot(aes(x = vpd28m_kPa, y = bl_sap_flux_Kg_h)) +
  geom_point(color = "grey") +
  geom_smooth(method = "gam", color = "black", se = F) +
  ylab("sap flow (kg/h)") + xlab("VPD (kPa)") +
  theme_minimal()
dev.off()


## Aggregate per hour

hourly_sapflow_met_2023 <- aggregate(sapflow_met_2023[, c(met.variables, sapflow.variables,  c("date_hour", "plot", "ID", "species"))],
                                          by = list(sapflow_met_2023$date_hour_id),
                                          FUN = meanOrMode) %>%
  rename(date_hour_id = Group.1) %>%
  mutate(timestamp = ymd_h(date_hour)) %>%
  select(timestamp, plot, ID, species, everything(), -date_hour_id, -date_hour)

head(hourly_sapflow_met_2023)
write_csv(hourly_sapflow_met_2023, "data_processed/analysis_hourly_sapflow_met.csv")

## Aggregate per day

sapflow_met_2023$date_id <- paste0(sapflow_met_2023$date, "_", sapflow_met_2023$ID)

daily_sapflow_met_2023 <- aggregate(sapflow_met_2023[, c(met.variables, sapflow.variables, c("date", "plot", "ID", "species"))],
                                         by = list(sapflow_met_2023$date_id),
                                         FUN = meanOrMode) %>%
  rename(date_id = Group.1) %>%
  mutate(timestamp = ymd(date)) %>%
  select(timestamp, plot, ID, species, everything(), -date_id, -date)

head(daily_sapflow_met_2023)
write_csv(daily_sapflow_met_2023, "data_processed/analysis_daily_sapflow_met.csv")


#### PLOTTING ------------------------------------------------------------------ ####

hourly_sapflow_met_2023 <- read_csv("data_processed/analysis_hourly_sapflow_met.csv")
daily_sapflow_met_2023 <- read_csv("data_processed/analysis_daily_sapflow_met.csv")


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

# ind <- unique(sapflow_met_2023$ID)[1]
for(ind in unique(hourly_sapflow_met_2023$ID)){
  
  ind_data <- hourly_sapflow_met_2023 %>% 
    filter(ID == ind)
  
  # Save the plot
  pdf(paste0("outputs/analysis/sapflow_met/sapflow_vpd_", ind, "_", str_replace(unique(ind_data$species), " ", "_"),".pdf"))
  ind_sapflow.plot <- ggplot(data = ind_data) + 
    geom_line(aes(x = timestamp, 
                  y = bl_sap_flux_Kg_h)) + 
    theme(legend.position = "none") + 
    xlab("time") + ylab("Sap flux (Kg/h)") + 
    theme_minimal()
  # ind_sapflow.plot
  
  ind_vpd_28m.plot <- ggplot(data = ind_data, 
                             aes(x = timestamp,
                                 y = vpd28m)) + 
    # geom_point(color = "grey") +
    # geom_smooth(method = "gam", color = "black") +
    geom_line() +
    xlab("time") + ylab("VPD") + 
    theme_minimal()
  # ind_vpd_28m.plot
  
  ind_vpd_28m_sapflow.plot <- ggplot(data = ind_data, 
                                     aes(x = vpd28m,
                                         y = bl_sap_flux_Kg_h)) + 
    geom_point(color = "grey") +
    geom_smooth(method = "lm", color = "black") +
    xlab("VPD") + ylab("Sap flux (Kg/h)") +
    theme_minimal() +
    # stat_regline_equation(aes(label = ..eq.label..)) +
    stat_regline_equation(aes(label = ..rr.label..))
  # ind_vpd_28m_sapflow.plot
  
  plot(ggarrange(ind_vpd_28m.plot, 
                 ind_sapflow.plot,
                 ind_vpd_28m_sapflow.plot,
                 nrow = 3))
  dev.off()
}


#### LINEAR MODELS ------------------------------------------------------------- ####

## Distribution

ggplot(data = daily_sapflow_met_2023, aes (x = bl_sap_flux_Kg_h)) +
  geom_density()

daily_sapflow_met_2023$log_bl_sap_flux_Kg_h <- log(daily_sapflow_met_2023$bl_sap_flux_Kg_h + 10)

ggplot(data = daily_sapflow_met_2023, aes (x = log(log_bl_sap_flux_Kg_h))) +
  geom_density()


### Sapflow vs. plot ####

sapflow_plot.mmod <- lmer(formula = log_bl_sap_flux_Kg_h ~ plot + (1|ID), 
                         data = daily_sapflow_met_2023)
summary(sapflow_plot.mmod)
r.squaredGLMM(sapflow_plot.mmod)

# Save the plot
pdf("outputs/analysis/sapflow_met/all_sapflow_vs_plot.pdf", height = h, width = w)
sapflow_plot.plot <- ggplot(data = daily_sapflow_met_2023, 
                            aes(x = plot, 
                                y = bl_sap_flux_Kg_h,
                                color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("sap flow (kg/h)") + 
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(-1, 6)
sapflow_plot.plot
dev.off()


### Sapflow vs. vpd ####

sapflow_vpd.mmod <- lmer(formula = log_bl_sap_flux_Kg_h ~ vpd28m_kPa * plot + (1|ID), 
                         data = daily_sapflow_met_2023)
summary(sapflow_vpd.mmod)
r.squaredGLMM(sapflow_vpd.mmod)

interact_plot(sapflow_vpd.mmod, 
              pred = "vpd28m_kPa", 
              modx = "plot", 
              plot.points = T,
              x.label = "VPD",
              point.alpha = 0.05)

# Save the plot
pdf("outputs/analysis/sapflow_met/all_sapflow_vs_vpd.pdf", height = h, width = w)
sapflow_vpd.plot <- ggplot(data = daily_sapflow_met_2023, 
                           aes(x = vpd28m_kPa, 
                               y = log_bl_sap_flux_Kg_h,
                               color = plot)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth(method = "lm") +
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("VPD (kPa)") + ylab("ln(sap flow (kg/h) + 10)") + 
  theme_minimal() + 
  xlim(-0.1, 1)
sapflow_vpd.plot
dev.off()


#### DIFFERENCES IN SENSIBILITY ------------------------------------------------ ####

## Sensitivity per day per individual. Then plot sensitivities through time and see whether there
## is differences between plots

hourly_sapflow_met_2023$date <- as_date(hourly_sapflow_met_2023$timestamp)

sapflow_vpd_slopes.data <- data.frame()
for(ind in unique(hourly_sapflow_met_2023$ID)){
  print(ind)
  for(i in 1:length(as_date(unique(hourly_sapflow_met_2023$date)))){

    day <- as_date(unique(hourly_sapflow_met_2023$date))[i]
    
    id_day_data <- hourly_sapflow_met_2023 %>%
      filter(ID == ind) %>%
      filter(date == day) %>%
      mutate(time = format(as.POSIXct(timestamp), format = "%H:%M")) %>%
      arrange(timestamp) %>%
      filter(time > "05:00") %>%
      filter(time < "14:00") 
    
    if(dim(id_day_data[, 1])[1] < 2 | any(is.na(id_day_data$bl_sap_flux_Kg_h)) | any(is.na(id_day_data$vpd28m))){
      warning("no data")
      next()
    }
    
    mod <- lm(bl_sap_flux_Kg_h ~ vpd28m, data = id_day_data)
    coef(mod)
    
    sapflow_vpd_slopes.id <- data.frame(ID = ind,
                                        plot = unique(id_day_data$plot),
                                        date = as_date(day),
                                        vpd_max_28m = max(id_day_data$vpd28m, na.rm = T),
                                        slope = as.numeric(coef(mod)[2])
                                        )
    
    sapflow_vpd_slopes.data <- bind_rows(sapflow_vpd_slopes.data, sapflow_vpd_slopes.id)
  }
}

mean_slope <- mean(sapflow_vpd_slopes.data$slope, na.rm = T)
sd_slope <- sd(sapflow_vpd_slopes.data$slope, na.rm = T)

sapflow_vpd_slopes.data$slope_without_outliers <- sapflow_vpd_slopes.data$slope

sapflow_vpd_slopes.data[sapflow_vpd_slopes.data$slope < mean_slope-3.5*sd_slope, "slope_without_outliers"] <- NA
sapflow_vpd_slopes.data[sapflow_vpd_slopes.data$slope > mean_slope+3.5*sd_slope, "slope_without_outliers"] <- NA


sapflow_slope_vpdmax.mmod <- lmer(formula = slope_without_outliers ~ vpd_max_28m * plot + (1|ID), 
                                  data = sapflow_vpd_slopes.data)
summary(sapflow_slope_vpdmax.mmod)
r.squaredGLMM(sapflow_slope_vpdmax.mmod)

pdf("outputs/analysis/sapflow_met/slope_vpd_max.pdf", height = h, width = w)
ggplot(data = sapflow_vpd_slopes.data, aes(x = vpd_max_28m, y = slope_without_outliers, color = plot)) +
  geom_point(alpha = 0.1) + 
  geom_smooth(method = "lm")  + scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("VPD max 28m (kPa)") + ylab("Slope subdaily sap flow ~ vpd") +
  theme_minimal()
dev.off()
