library(tidyverse)
library(lubridate)
library(glue)
library(ggtext)
library(gt)
library(gtExtras)

setwd("/Users/takayukitamura/Documents/R_Computing/pittsburgh_climate_r")

# -----------------------------
# 1) Read data
# -----------------------------

pit <- read_csv("pit.csv")
# # 
updates <- read_csv("/Users/takayukitamura/Desktop/4252326.csv") %>%
  # select(-SNWD) %>%
  filter(DATE >= "2026-03-01")
# # 
pit <- rbind(pit, updates)
# # 
write_csv(pit, "pit.csv")

pittsburgh <- read_csv("pit.csv") %>% 
  rename_all(tolower)

highlight_year <- 2026

tmax_ds <- pittsburgh %>% 
  mutate(date = ymd(date)) %>%
  select(date, tmax) %>%
  drop_na(date, tmax) %>% 
  mutate(
    year = year(date),
    doy = yday(date)
  ) %>% 
  filter(!(month(date) == 2 & day(date) == 29))

# Plot: all years (thin gray lines)
tmax_ds %>%
  ggplot(aes(x = doy, y = tmax, group = year)) +
  geom_line(color = "gray70", alpha = 0.4) +
  scale_x_continuous(
    breaks = yday(ymd(paste0("2001-", c("01-01","03-01","05-01","07-01","09-01","11-01")))),
    labels = c("Jan","Mar","May","Jul","Sep","Nov")
  ) +
  labs(
    x = "Month",
    y = "Daily maximum temperature (°F)",
    title = "Daily Maximum Temperature in Pittsburgh",
    subtitle = "Each line represents one year"
  ) +
  theme_minimal(base_size = 13)

# highlight a specific year(2026)
tmax_ds %>%
  mutate(is_highlight = year == highlight_year) %>%
  ggplot(aes(x = doy, y = tmax, group = year, color = is_highlight)) +
  geom_line(alpha = 0.6) +
  scale_color_manual(values = c("TRUE" = "dodgerblue", "FALSE" = "gray80")) +
  scale_x_continuous(
    breaks = yday(ymd(paste0("2001-", c("01-01","03-01","05-01","07-01","09-01","11-01")))),
    labels = c("Jan","Mar","May","Jul","Sep","Nov")
  ) +
  labs(
    x = "Month",
    y = "Daily maximum temperature (°F)",
    title = glue::glue("Daily Maximum Temperature in Pittsburgh ({highlight_year} highlighted)")
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# Plot with histrical band + a specific year (2026)
tmax_band <- tmax_ds %>%
  group_by(doy) %>%
  summarise(
    p10 = quantile(tmax, 0.10),
    p50 = quantile(tmax, 0.50),
    p90 = quantile(tmax, 0.90),
    .groups = "drop"
  )

# chart with p10 & p90
tmax_ds %>%
  filter(year == highlight_year) %>%
  ggplot() +
  geom_ribbon(
    data = tmax_band,
    aes(x = doy, ymin = p10, ymax = p90),
    fill = "gray80",
    alpha = 0.6
  ) +
  geom_line(
    data = tmax_band,
    aes(x = doy, y = p50),
    color = "gray50",
    linewidth = 1
  ) +
  geom_line(
    aes(x = doy, y = tmax),
    color = "dodgerblue",
    linewidth = 1.2
  ) +
  scale_x_continuous(
    breaks = yday(ymd(paste0("2001-", c("01-01","03-01","05-01","07-01","09-01","11-01")))),
    labels = c("Jan","Mar","May","Jul","Sep","Nov")
  ) +
  labs(
    x = "Month",
    y = "Daily maximum temperature (°F)",
    title = glue::glue("Daily Maximum Temperature in Pittsburgh — {highlight_year}"),
    subtitle = "Blue line vs. historical 10–90% range (gray)"
  ) +
  theme_minimal(base_size = 13)

# chart with 2 standard diviation 
tmax_band_sd <- tmax_ds %>%
  group_by(doy) %>%
  summarise(
    mean_m2sd = mean(tmax, na.rm = TRUE) - 2 * sd(tmax, na.rm = TRUE),
    p50       = quantile(tmax, 0.50, na.rm = TRUE),
    mean_p2sd = mean(tmax, na.rm = TRUE) + 2 * sd(tmax, na.rm = TRUE),
    .groups = "drop"
  )

tmax_ds %>%
  filter(year == highlight_year) %>%
  ggplot() +
  geom_ribbon(
    data = tmax_band_sd,
    aes(x = doy, ymin = mean_m2sd, ymax = mean_p2sd),
    fill = "gray80",
    alpha = 0.6
  ) +
  geom_line(
    data = tmax_band,
    aes(x = doy, y = p50),
    color = "gray50",
    linewidth = 1
  ) +
  geom_line(
    aes(x = doy, y = tmax),
    color = "dodgerblue",
    linewidth = 1.2
  ) +
  scale_x_continuous(
    breaks = yday(ymd(paste0("2001-", c("01-01","03-01","05-01","07-01","09-01","11-01")))),
    labels = c("Jan","Mar","May","Jul","Sep","Nov")
  ) +
  labs(
    x = "Month",
    y = "Daily maximum temperature (°F)",
    title = glue::glue("Daily Maximum Temperature in Pittsburgh — {highlight_year}"),
    subtitle = "Blue line vs. Historical Mean \u00B1 2 SD (gray)"
  ) +
  theme_minimal(base_size = 13)

# historical band by day-of-year
tmax_band <- tmax_ds %>%
  group_by(doy) %>%
  summarise(
    p10 = quantile(tmax, 0.10, na.rm = T),
    p50 = quantile(tmax, 0.50, na.rm = T),
    p90 = quantile(tmax, 0.90, na.rm = T),
    min = min(tmax, na.rm = T),
    max = max(tmax, na.rm = T),
    .groups = "drop"
  )


# standard deviation
tmax_st <- tmax_ds %>% 
  group_by(doy) %>% 
  summarise(
    tmax_mean = mean(tmax, na.rm = T),
    tmax_median = median(tmax, na.rm = T),
    tmax_sd = sd(tmax, na.rm = T),
    "mean+sd" = tmax_mean + tmax_sd,
    "mean+2sd" = tmax_mean + 2*tmax_sd,
    "mean-sd" = tmax_mean - tmax_sd,
    "mean-2*sd" = tmax_mean - 2*tmax_sd,
    .groups = "drop"
  )

# plot
tmax_ds %>%
  filter(year == highlight_year) %>%
  ggplot() +
  geom_ribbon(
    data = tmax_band,
    aes(x = doy, ymin = min, ymax = max),
    fill = "gray90",
    alpha = 0.4
  ) +
  geom_ribbon(
    data = tmax_band,
    aes(x = doy, ymin = p10, ymax = p90),
    fill = "red",
    alpha = 0.6
  ) +
  geom_line(
    data = tmax_ds,
    aes(x = doy, y = tmax, group = year),
    color = "gray80",
    linewidth = 0.25,
    alpha = 0.25
  ) +
  geom_line(
    data = tmax_band,
    aes(x = doy, y = p50),
    color = "gray40",
    linewidth = 1
  ) +
  geom_line(
    data = tmax_ds %>% filter(year == highlight_year),
    aes(x = doy, y = tmax),
    color = "dodgerblue",
    linewidth = 1.0
  ) +
  scale_x_continuous(
    breaks = yday(ymd(paste0("2001-", c("01-01","03-01","05-01","07-01","09-01","11-01")))),
    labels = c("Jan","Mar","May","Jul","Sep","Nov")
  ) +
  labs(
    x = "Month",
    y = "Daily maximum temperature (°F)",
    title = glue::glue("Daily maximum Temperature in Pittsburgh: 1950 - {highlight_year}"),
    subtitle = "**<span style='color:dodgerblue'>Blue line in 2026**</span> vs. historical 10–90%-tile(red) and max & min range (gray)",
    caption = "source: NOAA, by Takayuki Tamura"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.subtitle = element_markdown(size = 16),
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_line(linewidth = 0.1)
  )

ggsave("pittsburgh_tmin.png", width = 8.5, height = 7)

# chart with 2 standard diviation 

tmax_ds %>%
  # filter(year == highlight_year) %>%
  ggplot() +
  geom_ribbon(
    data = tmax_band,
    aes(x = doy, ymin = min, ymax = max),
    fill = "gray90",
    alpha = 0.4
  ) +
  geom_ribbon(
    data = tmax_band_sd,
    aes(x = doy, ymin = mean_m2sd, ymax = mean_p2sd),
    fill = "red",
    alpha = 0.6
  ) +
  geom_line(
    data = tmax_ds,
    aes(x = doy, y = tmax, group = year),
    color = "gray80",
    linewidth = 0.25,
    alpha = 0.25
  ) +
  geom_line(
    data = tmax_band,
    aes(x = doy, y = p50),
    color = "gray40",
    linewidth = 1
  ) +
  geom_line(
    data = tmax_ds %>% filter(year == highlight_year),
    aes(x = doy, y = tmax),
    color = "dodgerblue",
    linewidth = 1.0
  ) +
  scale_x_continuous(
    breaks = yday(ymd(paste0("2001-", c("01-01","03-01","05-01","07-01","09-01","11-01")))),
    labels = c("Jan","Mar","May","Jul","Sep","Nov")
  ) +
  labs(
    x = "Month",
    y = "Daily maximum temperature (°F)",
    title = glue::glue("Daily Maximum Temperature in Pittsburgh: 1950 - {highlight_year}"),
    subtitle = "**<span style='color:dodgerblue'>Blue line in 2026**</span> vs. Historical Mean \u00B1 2 SD(red) and max & min range (gray)",
    caption = "source: NOAA, by Takayuki Tamura"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.subtitle = element_markdown(size = 16),
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_line(linewidth = 0.1)
  )

# tmin_band_sd2
tmax_band_sd2 <- tmax_ds %>%
  group_by(doy) %>%
  summarise(
    mean_m2sd = mean(tmax, na.rm = TRUE) - 2 * sd(tmax, na.rm = TRUE),
    mean_m1sd = mean(tmax, na.rm = TRUE) - sd(tmax, na.rm = TRUE),
    p50       = quantile(tmax, 0.50, na.rm = TRUE),
    mean_p1sd = mean(tmax, na.rm = TRUE) + sd(tmax, na.rm = TRUE),
    mean_p2sd = mean(tmax, na.rm = TRUE) + 2 * sd(tmax, na.rm = TRUE),
    .groups = "drop"
  )

b <- tmax_ds %>%
  # filter(year == highlight_year) %>%
  ggplot() +
  geom_ribbon(
    data = tmax_band_sd2,
    aes(x = doy, ymin = mean_m2sd, ymax = mean_p2sd),
    fill = "#F8D7DA",
    alpha = 0.6
  ) +
  geom_ribbon(
    data = tmax_band_sd2,
    aes(x = doy, ymin = mean_m1sd, ymax = mean_p1sd),
    fill = "#D6EAF8",
    alpha = 0.4
  ) +
  geom_line(
    data = tmax_band,
    aes(x = doy, y = p50),
    color = "#1F3A5F",
    linewidth = 1
  ) +
  geom_line(
    data = tmax_ds %>% filter(year == highlight_year),
    aes(x = doy, y = tmax),
    color = "#0165fc",
    linewidth = 1.0
  ) +
  scale_x_continuous(
    breaks = yday(ymd(paste0("2001-", c("01-01","03-01","05-01","07-01","09-01","11-01")))),
    labels = c("Jan","Mar","May","Jul","Sep","Nov")
  ) +
  coord_cartesian(ylim = c(NA, NA), clip = "off", expand = FALSE) +
  labs(
    x = "Month",
    y = "Daily maximum temperature (°F)",
    title = glue::glue("Daily maximum Temperature in Pittsburgh: 1950 - {highlight_year}"),
    subtitle = "**<span style='color:dodgerblue'>Blue line in 2026**</span> vs. Historical Mean \u00B1 2 SD(pink) and Mean \u00B1 1 SD (gray)",
    caption = "source: NOAA, by Takayuki Tamura"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.subtitle = element_markdown(size = 16),
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_line(linewidth = 0.1)
  )
