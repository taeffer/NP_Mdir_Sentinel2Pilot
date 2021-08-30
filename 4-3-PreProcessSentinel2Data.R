library(gdalUtils)

#######################################################
### GDAL RUNS - MERGE SENTINEL 2 IMAGES 2018 - 2020 ###
#######################################################

s2_2018_list <- list.files(path = '1-Data/5-Sentinel2LCCData/Sentinel2_2018', pattern = "\\.tif$", full.names = TRUE)
s2_2018_vector <- unlist(s2_2018_list)
print(s2_2018_vector)
gdalUtils::gdalwarp(srcfile = s2_2018_vector, dstfile = '1-Data/5-Sentinel2LCCData/Sentinel2_2018/20180730.tif')


s2_2019_list <- list.files(path = '1-Data/5-Sentinel2LCCData/Sentinel2_2019', pattern = "\\.tif$", full.names = TRUE)
s2_2019_vector <- unlist(s2_2019_list)
print(s2_2019_vector)
gdalUtils::gdalwarp(srcfile = s2_2019_vector, dstfile = '1-Data/5-Sentinel2LCCData/Sentinel2_2019/20190727.tif')


s2_2020_list <- list.files(path = '1-Data/5-Sentinel2LCCData/Sentinel2_2020', pattern = "\\.tif$", full.names = TRUE)
s2_2020_vector <- unlist(s2_2020_list)
print(s2_2020_vector)
gdalUtils::gdalwarp(srcfile = s2_2020_vector, dstfile = '1-Data/5-Sentinel2LCCData/Sentinel2_2020/20200727.tif')





