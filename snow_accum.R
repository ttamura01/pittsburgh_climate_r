library(tidyverse)
library(glue)
library(ggtext)
library(gt)
library(gtExtras)
library(scales)
library(plotly)
setwd("/Users/takayukitamura/Documents/R_Computing/pittsburgh_snow_data")

pittsburgh <- read_csv("pit.csv") %>% 
  rename_all(tolower) %>% 
  mutate(date = ymd(date))

# write_csv(pittsburgh, "pit.csv")

snow_data <- pittsburgh %>% 
  select(date, snow) %>% 
  drop_na(snow) %>% 
  mutate(cal_year = year(date),
         month = month(date),
         snow_year = if_else(date < ymd(glue("{cal_year}-07-01")),
                             cal_year - 1,
                             cal_year)) %>%
  select(month, snow_year, snow) 

snow_yr <- 2025

snow_data %>% 
  group_by(snow_year) %>% 
  # filter(snow_year == 2025) %>% 
  # summarise(total_snow = sum(snow)) %>% 
  ggplot(aes(x = snow_year, y = snow)) +
  geom_line()

dummy_df <- crossing(snow_year = 1947:2025,
                     month = 1:12) %>% 
  mutate(dummy = 0)

total_snow <- snow_data %>% 
  group_by(snow_year) %>% 
  summarise(total_snow = sum(snow)) %>% 
  filter(snow_year == snow_yr) %>% 
  pull(total_snow)

p <- snow_data %>% 
  right_join(.,dummy_df, by = c("snow_year", "month" )) %>% 
  # filter(is.na(snow)) %>% 
  mutate(snow = if_else(is.na(snow), dummy, snow)) %>% 
  group_by(snow_year, month) %>% 
  summarise(snow = sum(snow), .groups = "drop") %>% 
  mutate(month = factor(month, levels = c(8:12, 1:7)),
         is_this_year = snow_year == snow_yr) %>%
  ggplot(aes(x = month, y = snow, group = snow_year, colour = is_this_year)) +
  geom_line(show.legend = FALSE) +
  scale_color_manual(name = NULL,
                     breaks = c(T, F),
                     values = c("dodgerblue", "gray")) +
  scale_x_discrete(breaks = c(9, 11, 1, 3, 5),
                   labels = month.abb[c(9, 11, 1, 3, 5)],
                   expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 50, 20),
                     labels = seq(0, 50, 20)) +
  labs(x = NULL,
       y = "Total monthly snowfall (cm)",
       title = glue(("The <span style = 'color: dodgerblue'> Snow Year {snow_yr}</span> 
                     had a  total of <span style = 'color: dodgerblue'>{total_snow} inches of snow</span>"))) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line()
  )

ggsave("snow_by_snow_year.png", width = 6, height = 4)
ggplotly(p)

pittsburgh <- read_csv("pit.csv") %>%
  rename_all(tolower) %>%
  mutate(date = ymd(date))

snow_monthly_cum <- pittsburgh %>%
  select(date, snow) %>%
  drop_na(snow) %>%
  mutate(
    cal_year = year(date),
    m = month(date),
    snow_year = if_else(m >= 8, cal_year, cal_year -1),      # Aug-Dec stays, Jan-Jul goes to previous year
    snow_month = if_else(m >= 8, m - 7, m + 5)                # Aug=1 ... Jul=12
  ) %>%
  group_by(snow_year, snow_month) %>%
  summarise(snow_in = sum(snow), .groups = "drop") %>%
  complete(snow_year, snow_month = 1:12, fill = list(snow_in = 0)) %>%
  arrange(snow_year, snow_month) %>%
  group_by(snow_year) %>%
  mutate(cum_snow_in = cumsum(snow_in)) %>%
  ungroup()

this_snow_year <- snow_yr  # Aug 2025–Jul 2026

p1 <- snow_monthly_cum %>%
  mutate(is_this_year = snow_year == this_snow_year,
         month_label = factor(snow_month, levels = 1:12,
                              labels = c("Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul"))) %>%
  ggplot(aes(x = month_label, y = cum_snow_in, group = snow_year, color = is_this_year)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("TRUE" = "dodgerblue", "FALSE" = "grey70")) +
  labs(x = NULL, y = "Cumulative snowfall (inches)",
       title = "Cumulative snowfall by snow year (Aug–Jul)",
       subtitle = glue::glue("Highlighted: snow year {this_snow_year} (Aug {this_snow_year}–Jul {this_snow_year+1})")) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave("accum_snow_by_snow_year.png", width = 6, height = 4)
ggplotly(p1)
