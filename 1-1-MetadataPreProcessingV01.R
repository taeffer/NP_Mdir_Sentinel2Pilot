library(tidyverse)
library(geofacet)
library(ggplot2)
library(ggthemes)
# Import Metadata
s2data <- read.csv("1-Data/Sentinel2-Metadata2018-2020.csv", header = TRUE)

# AddOn: Filter for IMAGE_AREA (Only include whole tiles in graph)

# Select columns
s2select <- s2data %>% select(system.index, PRODUCT_ID, system.asset_size, system.footprint, MGRS_TILE, system.time_start,
                              CLOUDY_PIXEL_PERCENTAGE, CLOUD_COVERAGE_ASSESSMENT, CLOUD_SHADOW_PERCENTAGE, HIGH_PROBA_CLOUDS_PERCENTAGE, MEDIUM_PROBA_CLOUDS_PERCENTAGE, 
                              NODATA_PIXEL_PERCENTAGE, NOT_VEGETATED_PERCENTAGE, SNOW_ICE_PERCENTAGE, 
                              THIN_CIRRUS_PERCENTAGE, UNCLASSIFIED_PERCENTAGE, VEGETATION_PERCENTAGE,
                              WATER_PERCENTAGE)

svalbard_tiles = c("33XWK", "35XMK", "35XNK", 
            "33XVJ", "33XWJ", "33XXJ", "35XMJ", "35XNJ", 
            "33XVH", "33XWH", "33XXH", "35XMH", "35XNH",
            "33XVG", "33XWG", "33XXG",
            "33XVF", "33XWF", "33XXF", "35XMF",
                                       "35XME")

#s2tile <- s2select %>% filter(MGRS_TILE == '33XWH') %>% select(system.index, CLOUDY_PIXEL_PERCENTAGE, system.time_start)
s2tile <- s2select %>% select(MGRS_TILE, system.index,CLOUDY_PIXEL_PERCENTAGE, system.time_start) %>% filter(MGRS_TILE %in% svalbard_tiles)
head(s2tile)

# Convert date_img string to date
s2date <- s2tile %>% separate(system.index, c("date_img", "a", "b", "c"), sep = 'T') %>% select(-a, -b, -c, -system.time_start)
s2date <- s2date %>% mutate(date_img = as.Date(date_img, "%Y%m%d"))
head(s2date)

# Categories cloudiness percentage
s2categorize_cloudiness <- s2date %>% mutate(cloudiness = ifelse(CLOUDY_PIXEL_PERCENTAGE <= 20, '0-20%',
                                                          ifelse(CLOUDY_PIXEL_PERCENTAGE > 20 & CLOUDY_PIXEL_PERCENTAGE <= 40, '20-40%',
                                                          ifelse(CLOUDY_PIXEL_PERCENTAGE > 40 & CLOUDY_PIXEL_PERCENTAGE <= 60, '40-60%',
                                                          ifelse(CLOUDY_PIXEL_PERCENTAGE > 60 & CLOUDY_PIXEL_PERCENTAGE <= 80, '60-80%', '80-100%')))))
head(s2categorize_cloudiness)

############################################
## PLOT ##
############################################

# Create Sentinel 2 tilling grid
s2grid <- data.frame(
  row = c(1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6),
  col = c(2, 3, 4, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 1, 2, 3, 4, 4),
  name = svalbard_tiles,
  code_name = svalbard_tiles,
  stringsAsFactors = FALSE
)
#geofacet::grid_preview(s2grid)


# Plot

for (year in 2018:2020){ # CHANGE TO 2018
  print(paste("Processing", year))
  s2categorize_year <- s2categorize_cloudiness %>% filter(date_img > paste0(year,"-01-01") & date_img < paste0(year,"-12-31"))
  s2group_week <-s2categorize_year %>% mutate(week_img = as.numeric(strftime(date_img, format = "%V")))
  print(head(s2group_week))
  
  print(ggplot(s2group_week, aes(x = week_img, y = ..count.., fill = cloudiness)) +
          geom_bar(position = "fill") + # position = "stack" or "fill"
          facet_geo(~ MGRS_TILE, grid = s2grid) +
          theme_light () + # theme_economist() or theme_bw()
          xlim(9, 41) +
          scale_fill_manual(values=c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c")) +
          labs(title = paste('Sentinel 2 - Cloudiness in Svalbard', year),
               x = 'Week of the year',
               y = 'Ratio of Cloudiness of images/Number of images',
               fill = 'Cloudiness of Images') +
          theme(axis.text.x = element_text(colour = "grey40", size = 12, angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
                axis.text.y = element_text(colour = "grey40", size = 12),
                strip.text = element_text(size = 12),
                text = element_text(size = 16),
                legend.position = c(0.15, 0.035)) # "bottom") #c(0.85, -0.5))
  )
}


