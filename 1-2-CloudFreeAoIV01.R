library(tidyverse)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(cowplot)

# Import Metadata
s2data <- read.csv2("1-Data/Sentinel2CloudFreeDays.csv", header = TRUE)
head(s2data)

# Select columns
s2select <- s2data %>% select(Type, Area, Date, Quality)

# Filter for Quality A and define Date2
s2quality <- s2select %>% filter(Quality == 'A') %>% mutate(Date = as.Date(Date, "%d.%m.%Y"))
head(s2quality)

# Get beginning of the month
s2group_date <- s2quality %>% mutate(Month = floor_date(Date, unit = "month", ))
s2group_date$Area <- factor(s2group_date$Area)
s2group_date$Month <- factor(s2group_date$Month)
head(s2group_date)

# Group by Area and Month
s2grouped <- s2group_date %>% count(Area, Month, .drop = FALSE) %>%
                        add_row(Area = 'Adventdalen', Month = '2018-08-01', n = 0) %>%
                        add_row(Area = 'Adventdalen', Month = '2018-10-01', n = 0) %>%
                        add_row(Area = 'Adventdalen', Month = '2019-10-01', n = 0) %>%
                        add_row(Area = 'Adventdalen', Month = '2020-09-01', n = 0) %>%
                        add_row(Area = 'Sassendalen', Month = '2018-08-01', n = 0) %>%
                        add_row(Area = 'Sassendalen', Month = '2018-10-01', n = 0) %>%
                        add_row(Area = 'Sassendalen', Month = '2019-10-01', n = 0) %>%
                        add_row(Area = 'Sassendalen', Month = '2020-09-01', n = 0) %>%
                        add_row(Area = 'Site - 2', Month = '2018-08-01', n = 0) %>%
                        add_row(Area = 'Site - 2', Month = '2018-10-01', n = 0) %>%
                        add_row(Area = 'Site - 2', Month = '2019-10-01', n = 0) %>%
                        add_row(Area = 'Site - 2', Month = '2020-09-01', n = 0) %>%
                        add_row(Area = 'Snow model site', Month = '2018-08-01', n = 0) %>%
                        add_row(Area = 'Snow model site', Month = '2018-10-01', n = 0) %>%
                        add_row(Area = 'Snow model site', Month = '2019-10-01', n = 0) %>%
                        add_row(Area = 'Snow model site', Month = '2020-09-01', n = 0) %>%
                        add_row(Area = 'Central Spitzbergen', Month = '2018-08-01', n = 0) %>%
                        add_row(Area = 'Central Spitzbergen', Month = '2018-10-01', n = 0) %>%
                        add_row(Area = 'Central Spitzbergen', Month = '2019-10-01', n = 0) %>%
                        add_row(Area = 'Central Spitzbergen', Month = '2020-09-01', n = 0)

s2grouped$Month <- as.Date(s2grouped$Month)
head(s2grouped)


# Plot
p2018 <- ggplot(s2grouped, aes(x = Month, y = n)) + # Plot/group by Area
  geom_line(aes(colour = factor(Area)), size = 1, position = position_jitter(w=1, h=0.04)) +
  theme_bw() +
  labs(
    x = NULL, 
    y = "Cloud free days",
    colour = "Area"
  ) + 
  scale_x_date(date_breaks = "1 month",  limits = as.Date(c('2018-01-01', '2018-12-01'))) +
  scale_y_continuous(breaks = c(0, 4, 8, 12))


p2019 <- ggplot(s2grouped, aes(x = Month, y = n)) + # Plot/group by Area
  geom_line(aes(colour = factor(Area)), size = 1, position = position_jitter(w=1, h=0.04)) +
  theme_bw() +
  labs(
    x = NULL, 
    y = "Cloud free days"
  ) + 
  scale_x_date(date_breaks = "1 month",  limits = as.Date(c('2019-01-01', '2019-12-01'), format = '%Y-%m-%d'), date_labels = '%b-%Y') +
  scale_y_continuous(breaks = c(0, 4, 8, 12))

p2020 <- ggplot(s2grouped, aes(x = Month, y = n)) + # Plot/group by Area
  geom_line(aes(colour = factor(Area)), size = 1, position = position_jitter(w=1, h=0.04)) +
  theme_bw() +
  labs(
    x = "Months",
    y = "Cloud free days"
  ) + 
  scale_x_date(date_breaks = "1 month",  limits = as.Date(c('2020-01-01', '2020-12-01'), format = '%Y-%m-%d'), date_labels = '%b') +
  scale_y_continuous(breaks = c(0, 4, 8, 12))

legend <- plot_grid(
  get_legend(p2018),
  nrow = 3,
  ncol = 1
)

pcol <- plot_grid(
  p2018 + theme(legend.position = "none", axis.title = element_text(size = 13), axis.text.x = element_text(size = 10.5)),
  p2019 + theme(legend.position = "none", axis.title = element_text(size = 13), axis.text.x = element_text(size = 10.5)),
  p2020 + theme(legend.position = "none", axis.title = element_text(size = 13), axis.text.x = element_text(size = 10.5)),
  #labels = c('2018', '2019', '2020'),
  #label_x = + 0.985,
  #label_y = 0,
  nrow = 3,
  ncol = 1
)

# Make one plot for each ear on top of each other
plot_grid(pcol, legend, nrow = 1, ncol = 2, label_size = 12, rel_widths = c(3, .6))

