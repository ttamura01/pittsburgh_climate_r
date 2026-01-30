library(tidyverse)
library(glue)
library(ggtext)
library(gt)
library(gtExtras)

pittsburgh <- read_csv("pit.csv") %>% 
  rename_all(tolower) 
  
summary(pittsburgh)

snow_ds <- pittsburgh %>% 
  filter(snow > 0) %>% 
  select(date, snow) 

snow_ds%>% 
  summarise(snow_mean = mean(snow),
            snow_median = median(snow),
            snow_sd = sd(snow),
            n = format(round(n(),0), big.mark = ",", scientific = F))
snow_ds %>% 
  arrange(desc(snow))

snow_ds %>% 
  ggplot(aes(x = snow)) +
  geom_histogram()

snow_ds %>% 
  ggplot(aes(x = snow)) +
  geom_density()

snow_ds %>% 
  ggplot(aes(x = snow)) +
  geom_boxplot() +
  coord_flip()

# rank
snow_ds %>% 
  arrange(desc(snow)) %>% 
  slice(1:10) %>% 
  gt() %>% 
  tab_header(title = "Daily Snowfall at Pittsburgh Airport, since 1950") %>% 
  cols_align(align = "left") %>% 
  # gt_theme_538() %>% 
  gt_theme_pff()


# normalized rank
snow_ds %>% 
  mutate(rank = dense_rank(desc(snow))) %>% 
  select(rank, date, snow) %>% 
  arrange(rank) %>% 
  slice(1:10) %>%
  gt() %>% 
  tab_header(title = "Daily Snowfall at Pittsburgh Airport, since 1950") %>% 
  cols_align(align = "left") %>% 
  # gt_theme_538() %>% 
  gt_theme_pff() %>% 
  opt_table_font(size = 18, font = "bold")

# Empirical percentage

ecdf_snow <- ecdf(snow_ds$snow)
ecdf_snow(17)

# 1. Histogram / density with log scale + vertical line
snow_ds %>%
  ggplot(aes(x = snow)) +
  geom_histogram(bins = 50, fill = "grey70", color = "white") +
  geom_vline(xintercept = 17, color = "red", linewidth = 1.2) +
  scale_x_log10() +
  labs(
    title = "Daily Snowfall Distribution (Log Scale)",
    subtitle = "Red line = today’s ~17-inch snowfall",
    x = "Snowfall (inches, log scale)",
    y = "Count of days"
  )

# 2. ECDF plot
snow_ds %>%
  ggplot(aes(x = snow)) +
  stat_ecdf(geom = "step") +
  geom_vline(xintercept = 17, color = "red", linewidth = 1.2) +
  labs(
    title = "Empirical CDF of Daily Snowfall",
    subtitle = "17 inches is beyond ~99.9% of historical observations",
    x = "Daily Snowfall (inches)",
    y = "Cumulative probability"
  )

# 3. boxplot

snow_ds %>%
  ggplot(aes(y = snow)) +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_point(aes(x = 0, y = 17), color = "red", size = 3) +
  coord_flip()

# Extreme Value THeory
mean(snow_ds$snow >= 17)

1/mean(snow_ds$snow >= 17)

library(tidyverse)
library(scales)

snow_ds <- pittsburgh %>% 
  filter(snow > 0) %>% 
  select(date, snow)

ggplot(snow_ds, aes(x = snow)) +
  stat_ecdf(
    geom = "step",
    linewidth = 1.2,
    color = "steelblue"
  ) +
  geom_vline(
    xintercept = 17,
    color = "red",
    linewidth = 1.4
  ) +
  annotate(
    "text",
    x = 17,
    y = 0.15,
    label = "Today\n~17 inches",
    color = "red",
    hjust = -0.1,
    size = 4
  ) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "How Extreme Is This Week’s Pittsburgh Snowstorm?",
    subtitle = "Daily snowfall at Pittsburgh Airport since 1950 (snow > 0 days)",
    x = "Daily Snowfall (inches)",
    y = "Share of historical snow days",
    caption = "source: NOAA by Takayuki Tamura"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 15),
    axis.title = element_text(size = 13)
  )



pct <- ecdf(snow_ds$snow)(17)

pct

annotate(
  "text",
  x = 8,
  y = 0.95,
  label = paste0(
    "More extreme than\n",
    percent(pct, accuracy = 0.01),
    " of snow days"
  ),
  size = 4.5,
  fontface = "bold"
)

