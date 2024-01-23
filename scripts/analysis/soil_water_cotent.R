#### SOIL WC ###################################################################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")
source("scripts/functions/functions.R")


#### DATA ---------------------------------------------------------------------- ####

## Meteorological data

soil_b_2023 <- read_csv("data_processed/soil_moisture/2022-2023_soil_tfe_processed.csv") %>%
  filter(timestamp > "2023-05-01") %>%
  mutate(date = as_date(timestamp)) %>%
  select(timestamp, date, vw, vw_2, vw_3, vw_4, vw_5, vw_6, vw_7)

soil_a_2023 <- read_csv("data_processed/soil_moisture/2019-2023_soil_control_processed.csv") %>%
  filter(timestamp > "2023-05-01") %>%
  mutate(date = as_date(timestamp)) %>%
  select(timestamp, date, vw, vw_2, vw_3, vw_4, vw_5, vw_6, vw_7)

## Aggregate

# Mean 

daily_mean_soil_a_2023 <- aggregate(soil_a_2023[, c(-1, -2)], 
                                  by = list(soil_a_2023$date), 
                                  FUN = mean, na.rm = T) %>%
  mutate(plot = "Control") %>%
  rename(date = Group.1)

daily_mean_soil_b_2023 <- aggregate(soil_b_2023[, c(-1, -2)], 
                                    by = list(soil_b_2023$date), 
                                    FUN = mean, na.rm = T) %>%
  mutate(plot = "TFE") %>%
  rename(date = Group.1)

daily_mean_soil_2023 <- bind_rows(daily_mean_soil_a_2023, daily_mean_soil_b_2023)


# Min

daily_min_soil_a_2023 <- aggregate(soil_a_2023[, c(-1, -2)], 
                                    by = list(soil_a_2023$date), 
                                    FUN = min, na.rm = T) %>%
  mutate(plot = "Control") %>%
  rename(date = Group.1)

daily_min_soil_b_2023 <- aggregate(soil_b_2023[, c(-1, -2)], 
                                    by = list(soil_b_2023$date), 
                                    FUN = min, na.rm = T) %>%
  mutate(plot = "TFE") %>%
  rename(date = Group.1)

daily_min_soil_2023 <- bind_rows(daily_min_soil_a_2023, daily_min_soil_b_2023)

daily_min_soil_2023[sapply(daily_min_soil_2023, is.infinite)] <- NA

### Models ####

soil_wc_mean.lm <- lm(vw ~ plot, data = daily_mean_soil_2023)
summary(soil_wc_mean.lm)

soil_wc_min.lm <- lm(vw ~ plot, data = daily_min_soil_2023)
summary(soil_wc_min.lm)

# pdf("outputs/analysis/soil_moisture/soil_wc_plot.pdf", 
    # height = h, 
    # width = w)
soil_wc.boxplot <- ggplot(data = daily_mean_soil_2023, 
                            aes(x = plot, 
                                y = vw,
                                color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("Soil water content") + 
  theme_minimal() +
  theme(legend.position = "none")
soil_wc.boxplot
# dev.off()
