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
updates <- read_csv("/Users/takayukitamura/Desktop/4241962.csv") %>%
# select(-SNWD) %>%
filter(DATE >= "2026-02-18")
# # 
pit <- rbind(pit, updates)
# # 
write_csv(pit, "pit.csv")

pittsburgh <- read_csv("pit.csv") %>% 
  rename_all(tolower)

highlight_year <- 2026

tmin_ds <- pittsburgh %>% 
  mutate(date = ymd(date)) %>%
  select(date, tmin) %>%
  drop_na(date, tmin) %>% 
  mutate(
    year = year(date),
    doy = yday(date)
  ) %>% 
  filter(!(month(date) == 2 & day(date) == 29))

# Plot: all years (thin gray lines)
tmin_ds %>%
  ggplot(aes(x = doy, y = tmin, group = year)) +
  geom_line(color = "gray70", alpha = 0.4) +
  scale_x_continuous(
    breaks = yday(ymd(paste0("2001-", c("01-01","03-01","05-01","07-01","09-01","11-01")))),
    labels = c("Jan","Mar","May","Jul","Sep","Nov")
  ) +
  labs(
    x = "Month",
    y = "Daily minimum temperature (°F)",
    title = "Daily Minimum Temperature in Pittsburgh",
    subtitle = "Each line represents one year"
  ) +
  theme_minimal(base_size = 13)

# highlight a specific year(2026)
tmin_ds %>%
  mutate(is_highlight = year == highlight_year) %>%
  ggplot(aes(x = doy, y = tmin, group = year, color = is_highlight)) +
  geom_line(alpha = 0.6) +
  scale_color_manual(values = c("TRUE" = "dodgerblue", "FALSE" = "gray80")) +
  scale_x_continuous(
    breaks = yday(ymd(paste0("2001-", c("01-01","03-01","05-01","07-01","09-01","11-01")))),
    labels = c("Jan","Mar","May","Jul","Sep","Nov")
  ) +
  labs(
    x = "Month",
    y = "Daily minimum temperature (°F)",
    title = glue::glue("Daily Minimum Temperature in Pittsburgh ({highlight_year} highlighted)")
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# Plot with histrical band + a specific year (2026)
tmin_band <- tmin_ds %>%
  group_by(doy) %>%
  summarise(
    p10 = quantile(tmin, 0.10),
    p50 = quantile(tmin, 0.50),
    p90 = quantile(tmin, 0.90),
    .groups = "drop"
  )


# chart with p10 & p90
tmin_ds %>%
  filter(year == highlight_year) %>%
  ggplot() +
  geom_ribbon(
    data = tmin_band,
    aes(x = doy, ymin = p10, ymax = p90),
    fill = "gray80",
    alpha = 0.6
  ) +
  geom_line(
    data = tmin_band,
    aes(x = doy, y = p50),
    color = "gray50",
    linewidth = 1
  ) +
  geom_line(
    aes(x = doy, y = tmin),
    color = "dodgerblue",
    linewidth = 1.2
  ) +
  scale_x_continuous(
    breaks = yday(ymd(paste0("2001-", c("01-01","03-01","05-01","07-01","09-01","11-01")))),
    labels = c("Jan","Mar","May","Jul","Sep","Nov")
  ) +
  labs(
    x = "Month",
    y = "Daily minimum temperature (°F)",
    title = glue::glue("Daily Minimum Temperature in Pittsburgh — {highlight_year}"),
    subtitle = "Blue line vs. historical 10–90% range (gray)"
  ) +
  theme_minimal(base_size = 13)

# chart with 2 standard diviation 
tmin_band_sd <- tmin_ds %>%
  group_by(doy) %>%
  summarise(
    mean_m2sd = mean(tmin, na.rm = TRUE) - 2 * sd(tmin, na.rm = TRUE),
    p50       = quantile(tmin, 0.50, na.rm = TRUE),
    mean_p2sd = mean(tmin, na.rm = TRUE) + 2 * sd(tmin, na.rm = TRUE),
    .groups = "drop"
  )

tmin_ds %>%
  filter(year == highlight_year) %>%
  ggplot() +
  geom_ribbon(
    data = tmin_band_sd,
    aes(x = doy, ymin = mean_m2sd, ymax = mean_p2sd),
    fill = "gray80",
    alpha = 0.6
  ) +
  geom_line(
    data = tmin_band,
    aes(x = doy, y = p50),
    color = "gray50",
    linewidth = 1
  ) +
  geom_line(
    aes(x = doy, y = tmin),
    color = "dodgerblue",
    linewidth = 1.2
  ) +
  scale_x_continuous(
    breaks = yday(ymd(paste0("2001-", c("01-01","03-01","05-01","07-01","09-01","11-01")))),
    labels = c("Jan","Mar","May","Jul","Sep","Nov")
  ) +
  labs(
    x = "Month",
    y = "Daily minimum temperature (°F)",
    title = glue::glue("Daily Minimum Temperature in Pittsburgh — {highlight_year}"),
    subtitle = "Blue line vs. Historical Mean \u00B1 2 SD (gray)"
  ) +
  theme_minimal(base_size = 13)


# prep data
tmin_ds <- pittsburgh %>% 
  mutate(date = ymd(date)) %>%
  select(date, tmin) %>%
  drop_na(date, tmin) %>% 
  mutate(
    year = year(date),
    doy = yday(date)
  ) %>% 
  filter(!(month(date) == 2 & day(date) == 29))

# historical band by day-of-year
tmin_band <- tmin_ds %>% 
  group_by(doy) %>% 
  summarise(
    p10 = quantile(tmin, 0.10, na.rm = T),
    p50 = quantile(tmin, 0.50, na.rm = T),
    p90 = quantile(tmin, 0.90, na.rm = T),
    min = min(tmin, na.rm = T),
    max = max(tmin, na.rm = T),
    .groups = "drop"
  )


# standard deviation
tmin_st <- tmin_ds %>% 
  group_by(doy) %>% 
  summarise(
    tmin_mean = mean(tmin, na.rm = T),
    tmin_median = median(tmin, na.rm = T),
    tmin_sd = sd(tmin, na.rm = T),
    "mean+sd" = tmin_mean + tmin_sd,
    "mean+2sd" = tmin_mean + 2*tmin_sd,
    "mean-sd" = tmin_mean - tmin_sd,
    "mean-2*sd" = tmin_mean - 2*tmin_sd,
    .groups = "drop"
  )

# plot
tmin_ds %>%
  filter(year == highlight_year) %>%
  ggplot() +
  geom_ribbon(
    data = tmin_band,
    aes(x = doy, ymin = min, ymax = max),
    fill = "gray90",
    alpha = 0.4
  ) +
  geom_ribbon(
    data = tmin_band,
    aes(x = doy, ymin = p10, ymax = p90),
    fill = "red",
    alpha = 0.6
  ) +
  geom_line(
    data = tmin_ds,
    aes(x = doy, y = tmin, group = year),
    color = "gray80",
    linewidth = 0.25,
    alpha = 0.25
  ) +
  geom_line(
    data = tmin_band,
    aes(x = doy, y = p50),
    color = "gray40",
    linewidth = 1
  ) +
  geom_line(
    data = tmin_ds %>% filter(year == highlight_year),
    aes(x = doy, y = tmin),
    color = "dodgerblue",
    linewidth = 1.0
  ) +
  annotate(geom = "point",
           x = 31, y = -11,
           size = 4,
           shape = 8,
           color = "dodgerblue") +
  annotate(geom = "text",
           x = 37, y = -10,
           label = "-11\u00B0F\n(Jan-31, 2026)",
           color = "dodgerblue",
           fontface = "bold",
           hjust = 0,
           vjust =0.7) +
  scale_x_continuous(
    breaks = yday(ymd(paste0("2001-", c("01-01","03-01","05-01","07-01","09-01","11-01")))),
    labels = c("Jan","Mar","May","Jul","Sep","Nov")
  ) +
  labs(
    x = "Month",
    y = "Daily minimum temperature (°F)",
    title = glue::glue("Daily Minimum Temperature in Pittsburgh: 1950 - {highlight_year}"),
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

tmin_ds %>%
  # filter(year == highlight_year) %>%
  ggplot() +
  geom_ribbon(
    data = tmin_band,
    aes(x = doy, ymin = min, ymax = max),
    fill = "gray90",
    alpha = 0.4
  ) +
  geom_ribbon(
    data = tmin_band_sd,
    aes(x = doy, ymin = mean_m2sd, ymax = mean_p2sd),
    fill = "red",
    alpha = 0.6
  ) +
  # geom_ribbon(
  #   data = tmin_band,
  #   aes(x = doy, ymin = , ymax = p90),
  #   fill = "red",
  #   alpha = 0.6
  # ) +
  geom_line(
    data = tmin_ds,
    aes(x = doy, y = tmin, group = year),
    color = "gray80",
    linewidth = 0.25,
    alpha = 0.25
  ) +
  geom_line(
    data = tmin_band,
    aes(x = doy, y = p50),
    color = "gray40",
    linewidth = 1
  ) +
  geom_line(
    data = tmin_ds %>% filter(year == highlight_year),
    aes(x = doy, y = tmin),
    color = "dodgerblue",
    linewidth = 1.0
  ) +
  annotate(geom = "point",
           x = 31, y = -11,
           size = 4,
           shape = 8,
           color = "dodgerblue") +
  annotate(geom = "text",
           x = 37, y = -10,
           label = "-11\u00B0F\n(Jan-31, 2026)",
           color = "dodgerblue",
           fontface = "bold",
           hjust = 0,
           vjust =0.7) +
  scale_x_continuous(
    breaks = yday(ymd(paste0("2001-", c("01-01","03-01","05-01","07-01","09-01","11-01")))),
    labels = c("Jan","Mar","May","Jul","Sep","Nov")
  ) +
  labs(
    x = "Month",
    y = "Daily minimum temperature (°F)",
    title = glue::glue("Daily Minimum Temperature in Pittsburgh: 1950 - {highlight_year}"),
    subtitle = "**<span style='color:dodgerblue'>Blue line in 2026**</span> vs. Historical Mean \u00B1 2 SD(red) and max & min range (gray)",
    caption = "source: NOAA, by Takayuki Tamura"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.subtitle = element_markdown(size = 16),
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_line(linewidth = 0.1)
  )

       
tmin_ds$period <- cut(tmin_ds$year,
                      breaks = c(1949, 1959, 1969, 1979, 1989, 1999, 2009, 2019, 2027),
                      labels = c("1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2019", "2020-2026"))

tmin_ds %>% 
  group_by(period) %>% 
  mutate(mean_tmin = mean(tmin, na_rm = T)) %>% 
  slice_head(n = 1)

tmin_ds %>% 
  drop_na(period) %>%
  ggplot(aes(x = period, y = tmin, fill = period)) +
  geom_boxplot(show.legend = F) +
  labs(x = NULL,
       y = 'Daily Lowest Temperature (\u00B0 F)',
       title = "Daily Lowest Temperature (TMIN) in Pittsburgh since 1950",
       subtitle = "Average daily lowest temperature increased by 4.1\u00B0F to 44\u00B0F past 50 years",
       caption = "source: NOAA, by Takayuki Tamura") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  )

tmax_ds <- pittsburgh %>% 
  mutate(date = ymd(date)) %>%
  select(date, tmax) %>%
  drop_na(date, tmax) %>% 
  mutate(
    year = year(date),
    doy = yday(date)
  ) %>% 
  filter(!(month(date) == 2 & day(date) == 29))

tmax_ds$period <- cut(tmax_ds$year,
                      breaks = c(1949, 1959, 1969, 1979, 1989, 1999, 2009, 2019, 2027),
                      labels = c("1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2019", "2020-2026"))
tmax_ds %>% 
  group_by(period) %>% 
  mutate(mean_tmax = mean(tmax, na_rm = T)) %>% 
  slice_head(n = 1)
  
tmax_ds %>% 
  drop_na(period) %>%
  ggplot(aes(x = period, y = tmax, fill = period)) +
  geom_boxplot(show.legend = F) +
  labs(x = NULL,
       y = 'Daily highest Temperature (\u00B0 F)',
       title = "Daily Highest Temperature (TMAN) in Pittsburgh since 1950",
       subtitle = "Average daily highest temperature increased by 3.1\u00B0F to 62.8\u00B0F past 50 years",
       caption = "source: NOAA, by Takayuki Tamura") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  )

# tmin_band_sd2
tmin_band_sd2 <- tmin_ds %>%
  group_by(doy) %>%
  summarise(
    mean_m2sd = mean(tmin, na.rm = TRUE) - 2 * sd(tmin, na.rm = TRUE),
    mean_m1sd = mean(tmin, na.rm = TRUE) - sd(tmin, na.rm = TRUE),
    p50       = quantile(tmin, 0.50, na.rm = TRUE),
    mean_p1sd = mean(tmin, na.rm = TRUE) + sd(tmin, na.rm = TRUE),
    mean_p2sd = mean(tmin, na.rm = TRUE) + 2 * sd(tmin, na.rm = TRUE),
    .groups = "drop"
  )

tmin_ds %>%
  # filter(year == highlight_year) %>%
  ggplot() +
  geom_ribbon(
    data = tmin_band_sd2,
    aes(x = doy, ymin = mean_m2sd, ymax = mean_p2sd),
    fill = "#F8D7DA",
    alpha = 0.6
  ) +
  geom_ribbon(
    data = tmin_band_sd2,
    aes(x = doy, ymin = mean_m1sd, ymax = mean_p1sd),
    fill = "#D6EAF8",
    alpha = 0.4
  ) +
  # geom_ribbon(
  #   data = tmin_band,
  #   aes(x = doy, ymin = , ymax = p90),
  #   fill = "red",
  #   alpha = 0.6
  # ) +
  # geom_line(
  #   data = tmin_ds,
  #   aes(x = doy, y = tmin, group = year),
  #   color = "gray80",
  #   linewidth = 0.25,
  #   alpha = 0.25
  # ) +
  geom_line(
    data = tmin_band,
    aes(x = doy, y = p50),
    color = "#1F3A5F",
    linewidth = 1
  ) +
  geom_line(
    data = tmin_ds %>% filter(year == highlight_year),
    aes(x = doy, y = tmin),
    color = "#0165fc",
    linewidth = 1.0
  ) +
  annotate(geom = "point",
           x = 31, y = -11,
           size = 4,
           shape = 8,
           color = "dodgerblue") +
  annotate(geom = "text",
           x = 37, y = -10,
           label = "-11\u00B0F\n(Jan-31, 2026)",
           color = "dodgerblue",
           fontface = "bold",
           hjust = 0,
           vjust =0.7) +
  scale_x_continuous(
    breaks = yday(ymd(paste0("2001-", c("01-01","03-01","05-01","07-01","09-01","11-01")))),
    labels = c("Jan","Mar","May","Jul","Sep","Nov")
  ) +
  coord_cartesian(ylim = c(-25, 80), clip = "off", expand = FALSE) +
  labs(
    x = "Month",
    y = "Daily minimum temperature (°F)",
    title = glue::glue("Daily Minimum Temperature in Pittsburgh: 1950 - {highlight_year}"),
    subtitle = "**<span style='color:dodgerblue'>Blue line in 2026**</span> vs. Historical Mean \u00B1 2 SD(pink) and Mean \u00B1 1 SD (gray)",
    caption = "source: NOAA, by Takayuki Tamura"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.subtitle = element_markdown(size = 16),
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_line(linewidth = 0.1)
  )
