library(tidyverse)
library(lubridate)
library(glue)
library(ggtext)

setwd("/Users/takayukitamura/Documents/R_Computing/pittsburgh_snow_data")

# -----------------------------
# 1) Read data
# -----------------------------

# pit <- read_csv("/Users/takayukitamura/Desktop/4209937.csv")
# 
# updates <- read_csv("/Users/takayukitamura/Desktop/4215227.csv") %>%
#   filter(DATE >= "2026-01-19")
# 
# pit <- rbind(pit, updates)
# 
# write_csv(pit, "pit.csv")

pittsburgh <- read_csv("pit.csv") %>% 
  rename_all(tolower)

pittsburgh <- pittsburgh %>% 
  mutate(date = ymd(date)) %>%
  select(date, snow) %>%
  drop_na(date, snow)

# -----------------------------
# 2) Define snow year + snow-season month index (Aug..Jul)
#    Your snow_year definition: July 1 boundary
#    snow_year 2025 = 2025-07-01 to 2026-06-30
# -----------------------------
snow_df <- pittsburgh %>%
  mutate(
    cal_year = year(date),
    month    = month(date),
    snow_year = if_else(date < ymd(glue("{cal_year}-07-01")),
                        cal_year - 1, cal_year),
    season_m = if_else(month >= 8, month - 7, month + 5)   # Aug=1 ... Jul=12
  )

# -----------------------------
# 3) Identify the latest snow year in your file and its last available season month
# -----------------------------
last_date <- max(snow_df$date, na.rm = TRUE)

current_snow_year <- snow_df %>%
  filter(date == last_date) %>%
  slice(1) %>%
  pull(snow_year)

last_season_m <- snow_df %>%
  filter(date == last_date) %>%
  slice(1) %>%
  pull(season_m)

# -----------------------------
# 4) Monthly totals by snow_year + season_m
#    Then complete every snow_year to 12 months
#    Key logic:
#      - for current_snow_year, months AFTER last_season_m => NA (stop line)
#      - all other missing months => 0 (keep full curves)
# -----------------------------
monthly <- snow_df %>%
  group_by(snow_year, season_m) %>%
  summarise(snow_in = sum(snow, na.rm = TRUE), .groups = "drop") %>%
  complete(snow_year, season_m = 1:12) %>%
  arrange(snow_year, season_m) %>%
  mutate(
    snow_in = case_when(
      snow_year == current_snow_year & season_m > last_season_m ~ NA_real_,  # STOP line
      TRUE ~ replace_na(snow_in, 0)                                          # fill past gaps
    )
  )

# Total snowfall so far for the current snow year
total_snow_so_far <- monthly %>%
  filter(snow_year == current_snow_year) %>%
  summarise(total = sum(snow_in, na.rm = TRUE)) %>%
  pull(total)

# -----------------------------
# 5) Plot: highlight current snow year
# -----------------------------
monthly %>%
  mutate(
    season_lab = factor(
      season_m, levels = 1:12,
      labels = c("Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul")
    ),
    is_current = snow_year == current_snow_year
  ) %>%
  ggplot(aes(x = season_lab, y = snow_in, group = snow_year, color = is_current)) +
  geom_line(show.legend = FALSE, linewidth = 1) +
  scale_color_manual(values = c("TRUE" = "dodgerblue", "FALSE" = "gray70")) +
  labs(
    x = NULL,
    y = "Total monthly snowfall (inches)",
    title = glue(
      "The <span style='color:dodgerblue'>Snow Year {current_snow_year}</span> has a total of
      <span style='color:dodgerblue'>{round(total_snow_so_far, 1)} inches</span> of snow so far
      (through {format(last_date, '%b %d, %Y')})."
    )
  ) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line()
  )
