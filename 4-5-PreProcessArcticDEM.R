# Import libraries
library(raster)
library(tidyverse)
library(ggplot2)
library(rgdal)
library(sf)
library(spatialEco)
library(tictoc)
library(gdalUtils)
library(sp)
library(parallel)

# Calculate Roughness and TRI of Arctic DEM

######################################
### GDAL RUNS - Process Ruggedness ###
######################################

path_sr <- "1-Data/6-ArcticDEM/1-Extracted/"
path_ds <- "1-Data/6-ArcticDEM/2-Processed/"

calculateRuggedness <- function(x) {
  
  x <- substr(x, 1, nchar(x)-4)
 
  dem <- paste0(path_sr, x, ".tif")
  crs_target <- '+proj=utm +zone=33 +datum=WGS84'
  
  gdalUtils::gdalwarp(srcfile = dem, dstfile = paste0(path_ds, x, "_reproj.tif"), t_srs = crs_target)
  print(paste('Reproject DEM', x))
  
  dem_84 <- paste0(path_ds, x, "_reproj.tif")
  print(paste('Import reprojected DEM', x))
  
  gdalUtils::gdaldem("TRI", dem_84, paste0(path_ds, x, "_TRI.tif")) # , alg = 'Riley'
  print(paste('Calculate TRI', x))
  
  gdalUtils::gdaldem("roughness", dem_84, paste0(path_ds, x, "_roughness.tif"))
  print(paste('Calculate Roughness', x))
  
  return(x)
  
}

dem_files_list <- list.files(path = path_sr, pattern = "\\.tif$")
dem_files_vector <- unlist(dem_files_list)
tic()
dem_processed <- lapply(dem_files_vector, calculateRuggedness)
toc()

###########################################
### GDAL RUNS - Merge Ruggedness Layers ###
###########################################

tic()
dem_list_reproj <- list.files(path = path_ds, pattern = "_reproj.tif$", full.names = TRUE)
dem_vect_reproj <- unlist(dem_list_reproj)
print(dem_vect_reproj)
gdalUtils::gdalwarp(srcfile = dem_vect_reproj, dstfile = paste0(path_ds, 'ArcticDEM_Elevation.tif'))
file.remove(dem_vect_reproj)
  
dem_list_TRI <- list.files(path = path_ds, pattern = "_TRI.tif$", full.names = TRUE)
dem_vect_TRI <- unlist(dem_list_TRI)
print(dem_vect_TRI)
gdalUtils::gdalwarp(srcfile = dem_vect_TRI, dstfile = paste0(path_ds, 'ArcticDEM_TRI.tif'))
file.remove(dem_vect_TRI)
 
dem_list_roughn <- list.files(path = path_ds, pattern = "_roughness.tif$", full.names = TRUE)
dem_vect_roughn <- unlist(dem_list_roughn)
print(dem_vect_roughn)
gdalUtils::gdalwarp(srcfile = dem_vect_roughn, dstfile = paste0(path_ds, 'ArcticDEM_Roughness.tif'))
file.remove(dem_vect_roughn)
toc()
