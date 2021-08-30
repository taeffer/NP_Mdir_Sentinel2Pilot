install.packages('gridBase')
install.packages('raster')
install.packages('gridExtra')
install.packages('ggpubr')
install.packages('ggExtra')

library('sp')
library('sf')
library('rgdal')
library('raster')
library('gridExtra')
library('ggplot2')
library('ggExtra')
library('hrbrthemes')
library('ggpubr')
library(cowplot)
require("gridBase")
require("grid")
require("gridExtra")
require(tidyverse)

## Prepare lists for all the needed files ##

Endalen_2019_05_28 <- list('D:/1-Sentinel2Project/3-Sentinel2eBeeX/1-Data/Endalen_2019_05_28/Sentinel2/20190528T121659_20190528T121658_T33XWG.tif',
                           'D:/1-Sentinel2Project/3-Sentinel2eBeeX/1-Data/Endalen_2019_05_28/eBeeX/2019_05_28_endalen_4band_47_index_ndvi.tif',
                           'Endalen')

Endalen_2019_07_30 <- list('D:/1-Sentinel2Project/3-Sentinel2eBeeX/1-Data/Endalen_2019_07_30/Sentinel2/20190728T123701_20190728T123756_T33XWG.tif',
                           'D:/1-Sentinel2Project/3-Sentinel2eBeeX/1-Data/Endalen_2019_07_30/eBeeX/2019-07-30_endalen_4band_index_ndvi.tif',
                           'Endalen')

Janssonhaugen_2019_07_20 <- list('D:/1-Sentinel2Project/3-Sentinel2eBeeX/1-Data/Janssonhaugen_2019_07_20/Sentinel2/20190728T123701_20190728T123756_T33XWG.tif',
                           'D:/1-Sentinel2Project/3-Sentinel2eBeeX/1-Data/Janssonhaugen_2019_07_20/eBeeX/jan_4band_64_new_index_ndvi.tif',
                           'Janssonhaugen')

Sassendalen_2019_07_23 <- list('D:/1-Sentinel2Project/3-Sentinel2eBeeX/1-Data/Sassendalen_2019_07_23/Sentinel2/20190727T130721_20190727T130715_T33XWG.tif',
                           'D:/1-Sentinel2Project/3-Sentinel2eBeeX/1-Data/Sassendalen_2019_07_23/eBeeX/sassen_4band_67_index_ndvi.tif',
                           'Sassendalen')

Todalen_2019_05_10 <- list('D:/1-Sentinel2Project/3-Sentinel2eBeeX/1-Data/Todalen_2019_05_10/Sentinel2/20190510T120651_20190510T120649_T33XWG.tif',
                         'D:/1-Sentinel2Project/3-Sentinel2eBeeX/1-Data/Todalen_2019_05_10/eBeeX/2019_05_10_todalen_4band_24_index_ndvi.tif',
                         'Todalen small')

Todalen_2019_07_28 <- list('D:/1-Sentinel2Project/3-Sentinel2eBeeX/1-Data/Todalen_2019_07_28/Sentinel2/20190728T123701_20190728T123756_T33XWG.tif',
                           'D:/1-Sentinel2Project/3-Sentinel2eBeeX/1-Data/Todalen_2019_07_28/eBeeX/todalen_merged_last_try_index_ndvi.tif',
                           'Todalen small')


# spring_list_ndvi_landsat <- list('1-Data/Todalen_2019_05_10/20190510_Landsat.tif',
#                                  '1-Data/Todalen_2019_05_10/eBeeX/2019_05_10_todalen_4band_24_index_ndvi.tif')


# summer_list_ndvi_landsat <- list('1-Data/Todalen_2019_07_28/20190727_Landsat.tif',
#                                  '1-Data/Todalen_2019_07_28/eBeeX/todalen_merged_last_try_index_ndvi.tif')


data_list <- list(Endalen_2019_05_28, Endalen_2019_07_30, Janssonhaugen_2019_07_20, Sassendalen_2019_07_23, Todalen_2019_05_10, Todalen_2019_07_28)

for (i in data_list) {

  if(str_sub(unlist(i[1]), -11) == 'Landsat.tif'){

    s2_data <- stack(i[1])
    # Import Drone Raster
    ebee_data <- stack(i[2])
    names(ebee_data) <- c('ndvi')

    # Import and Filter shapefile and filter for AoI
    aoi_coat <- readOGR('1-Data/COATAoI', layer='COATAoI')
    aoi_coat <- aoi_coat[aoi_coat$Name == i[3], ]

    # Calculate NDVI Values for eBeeX
    ebee_ndvi <- ebee_data
    names(ebee_ndvi) <- c('NDVI')

    # Calculate NDVI for Sentinel 2
    s2_ndvi <- (s2_data[[5]] - s2_data[[4]]) / (s2_data[[5]] + s2_data[[4]])

    # Resample to 10x10m resolution (raster given by Sentinel2)
    ebee_10 <- resample(ebee_ndvi, s2_ndvi, method = 'bilinear') # or method = 'ngb'
    s2_10 <- s2_ndvi

    # Clip according to defined AoI
    s2_crop <- mask(s2_10, aoi_coat)
    ebee_crop <- mask(ebee_10, aoi_coat)

    # Stack S2 and eBee raster and add raster that shows differences
    aoi_ndvi <- stack(s2_crop, ebee_crop, (s2_crop-ebee_crop))
    names(aoi_ndvi) <- c('name_s2_ndvi', 'name_eBee_ndvi', 'name_s2_eBee')

    # Convert rasters to DFs to use in ggplot
    df_ndvi <- as.data.frame(aoi_ndvi, row.names = NULL, optional = FALSE, xy = FALSE,
                             na.rm = FALSE, long = FALSE)

    df_s2_ndvi <- as.data.frame(aoi_ndvi$name_s2_ndvi, xy = TRUE) %>% drop_na()
    df_ebee_ndvi <- as.data.frame(aoi_ndvi$name_eBee_ndvi, xy = TRUE) %>% drop_na()
    df_diff_ndvi <- as.data.frame(aoi_ndvi$name_s2_eBee, xy = TRUE) %>% drop_na()

    ## PREPARING PLOTS ##

    p_ebee_ndvi <- ggplot() +
      geom_raster(data = df_ebee_ndvi, aes(x = x, y = y, fill = name_eBee_ndvi)) +
      scale_fill_distiller(name = 'NDVI', direction = 1,
                           limits = c(-1, 1), palette = 'BrBG') +
      labs(x = 'East', y = 'North') +
      theme_cowplot() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 11, hjust = 0.5),
        axis.title.y = element_text(size = 11, hjust = 0.5),
        plot.margin = margin(0.5, 0.5, 0.0, 0.5, 'cm')
      ) +
      coord_fixed(ratio = 1)

    p_s2_ndvi <- ggplot() +
      geom_raster(data = df_s2_ndvi, aes(x = x, y = y, fill = name_s2_ndvi)) +
      scale_fill_distiller(name = 'NDVI', direction = 1,  # scale_fill_viridis_C  option = 'viridis'
                           limits = c(-1, 1), palette = 'BrBG') +
      labs(x = 'East', y = 'North') +
      theme_cowplot() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 11, hjust = 0.5),
        axis.title.y = element_text(size = 11, hjust = 0.5),
        plot.margin = margin(0.5, 0.5, 0.5, -2.5, 'cm')
      )  +
      coord_fixed(ratio = 1)

    p_diff <- ggplot() +
      geom_raster(data = df_diff_ndvi, aes(x = x, y = y, fill = name_s2_eBee)) +
      scale_fill_distiller(name = 'Difference', direction = 1,
                           limits = c(-0.5, 0.5), palette = 'PRGn') +
      labs(x = 'East', y = 'North') +
      theme_cowplot() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 11, hjust = 0.5),
        axis.title.y = element_text(size = 11, hjust = 0.5),
        plot.margin = margin(0.5, 0.5, 0.0, 0.5, 'cm')
      ) +
      coord_fixed(ratio = 1)
    print(summary(df_diff_ndvi$name_s2_eBee))

    p_graph <- ggplot(data = df_ndvi, aes(x = name_eBee_ndvi, y = name_s2_ndvi)) +
      geom_point(color = '#69b3a2', size = 0.15) +
      geom_abline(linetype = 'dashed', size = 1) +
      xlim(-0.5, 1) +
      ylim(-0.5, 1) +
      labs(x = 'Drone - NDVI', y = 'Landsat - NDVI') +
      theme_ipsum() +
      theme(
        axis.title.x = element_text(size = 12, face = 'bold', hjust = 0.5),
        axis.title.y = element_text(size = 12, face = 'bold', hjust = 0.5),
        plot.margin = margin(0.5, 0.5, 0.5, -4.5, 'cm')
      ) +
      stat_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
      stat_cor(aes(label = ..r.label..), method = 'spearman', size = 4.25) +
      coord_fixed(ratio = 1)

    proj_name <- sub('_', ' ', str_split(unlist(i[1]), '/')[[1]][[2]])
    title <- ggdraw() + draw_label(paste('Pixel-by-pixel correlation between\nDrone and Landsat in', proj_name, sep = ' '),
                                   fontface='bold', hjust = 0, vjust = 1) +
      theme(plot.margin = margin(0.0, 10.0, 0.0, 0.0, 'cm'))

    print(plot_grid(
      title, NULL,
      p_ebee_ndvi, p_s2_ndvi,
      p_diff, p_graph,
      nrow = 3,
      ncol = 2,
      rel_heights = c(0.2, 1, 1),
      labels = c('', '', 'Drone 30 m', 'Landsat', 'Comparison', ''),
      hjust = -1.5,
      align = 'hv'
    ))
  }

  else{
    s2_data <- stack(i[1])
    # Import Drone Raster
    ebee_data <- stack(i[2])
    names(ebee_data) <- c('ndvi')

    # Import and Filter shapefile and filter for AoI
    aoi_coat <- readOGR('1-Data/COATAoI', layer='COATAoI')
    aoi_coat <- aoi_coat[aoi_coat$Name == i[3], ]

    # Calculate NDVI Values for eBeeX
    ebee_ndvi <- ebee_data
    names(ebee_ndvi) <- c('NDVI')

    # Calculate NDVI for Sentinel 2
    s2_ndvi <- (s2_data[[8]] - s2_data[[4]]) / (s2_data[[8]] + s2_data[[4]])

    # Resample to 10x10m resolution (raster given by Sentinel2)
    ebee_10 <- resample(ebee_ndvi, s2_ndvi, method = 'bilinear') # or method = 'ngb'
    s2_10 <- s2_ndvi

    # Clip according to defined AoI
    s2_crop <- mask(s2_10, aoi_coat)
    ebee_crop <- mask(ebee_10, aoi_coat)

    # Stack S2 and eBee raster and add raster that shows differences
    aoi_ndvi <- stack(s2_crop, ebee_crop, (s2_crop-ebee_crop))
    names(aoi_ndvi) <- c('name_s2_ndvi', 'name_eBee_ndvi', 'name_s2_eBee')

    # Convert rasters to DFs to use in ggplot
    df_ndvi <- as.data.frame(aoi_ndvi, row.names = NULL, optional = FALSE, xy = FALSE,
                             na.rm = FALSE, long = FALSE)

    df_s2_ndvi <- as.data.frame(aoi_ndvi$name_s2_ndvi, xy = TRUE) %>% drop_na()
    df_ebee_ndvi <- as.data.frame(aoi_ndvi$name_eBee_ndvi, xy = TRUE) %>% drop_na()
    df_diff_ndvi <- as.data.frame(aoi_ndvi$name_s2_eBee, xy = TRUE) %>% drop_na()

    ## PREPARING PLOTS ##

    p_ebee_ndvi <- ggplot() +
      geom_raster(data = df_ebee_ndvi, aes(x = x, y = y, fill = name_eBee_ndvi)) +
      scale_fill_distiller(name = 'NDVI', direction = 1,
                           limits = c(-1, 1), palette = 'BrBG') +
      labs(x = 'East', y = 'North') +
      theme_cowplot() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 11, hjust = 0.5),
        axis.title.y = element_text(size = 11, hjust = 0.5),
        plot.margin = margin(0.5, 0.5, 0.0, 0.5, 'cm')
      ) +
      coord_fixed(ratio = 1)

    p_s2_ndvi <- ggplot() +
      geom_raster(data = df_s2_ndvi, aes(x = x, y = y, fill = name_s2_ndvi)) +
      scale_fill_distiller(name = 'NDVI', direction = 1,  # scale_fill_viridis_C  option = 'viridis'
                           limits = c(-1, 1), palette = 'BrBG') +
      labs(x = 'East', y = 'North') +
      theme_cowplot() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 11, hjust = 0.5),
        axis.title.y = element_text(size = 11, hjust = 0.5),
        plot.margin = margin(0.5, 0.5, 0.5, -2.5, 'cm')
      )  +
      coord_fixed(ratio = 1)

    p_diff <- ggplot() +
      geom_raster(data = df_diff_ndvi, aes(x = x, y = y, fill = name_s2_eBee)) +
      scale_fill_distiller(name = 'Difference', direction = 1,
                           limits = c(-0.5, 0.5), palette = 'PRGn') +
      labs(x = 'East', y = 'North') +
      theme_cowplot() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 11, hjust = 0.5),
        axis.title.y = element_text(size = 11, hjust = 0.5),
        plot.margin = margin(0.5, 0.5, 0.0, 0.5, 'cm')
      ) +
      coord_fixed(ratio = 1)
    print(summary(df_diff_ndvi$name_s2_eBee))
    p_graph <- ggplot(data = df_ndvi, aes(x = name_eBee_ndvi, y = name_s2_ndvi)) +
      geom_point(color = '#69b3a2', size = 0.15) +
      geom_abline(linetype = 'dashed', size = 1) +
      xlim(-0.5, 1) +
      ylim(-0.5, 1) +
      labs(x = 'Drone - NDVI', y = 'Sentinel 2 - NDVI') +
      theme_ipsum() +
      theme(
        axis.title.x = element_text(size = 12, face = 'bold', hjust = 0.5),
        axis.title.y = element_text(size = 12, face = 'bold', hjust = 0.5),
        plot.margin = margin(0.5, 0.5, 0.5, -4.5, 'cm')
      ) +
      stat_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
      stat_cor(aes(label = ..r.label..), method = 'spearman', size = 4.25) +
      coord_fixed(ratio = 1)

    proj_name <- sub('_', ' ', str_split(unlist(i[1]), '/')[[1]][[2]])
    title <- ggdraw() + draw_label(paste('Pixel-by-pixel correlation between\nDrone and Sentinel 2 in', proj_name, sep = ' '),
                                   fontface='bold', hjust = 0, vjust = 1) +
      theme(plot.margin = margin(0.0, 10.0, 0.0, 0.0, 'cm'))

    print(plot_grid(
      title, NULL,
      p_ebee_ndvi, p_s2_ndvi,
      p_diff, p_graph,
      nrow = 3,
      ncol = 2,
      rel_heights = c(0.2, 1, 1),
      labels = c('', '', 'Drone 10 m', 'Sentinel 2', 'Comparison', ''),
      hjust = -1.5,
      align = 'hv'
    ))
  }
}
