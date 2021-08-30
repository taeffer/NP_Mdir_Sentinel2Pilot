library(gdalUtils)

##################################################################
### GDAL RUNS - CHANGE COORDINATE SYSTEM AND REPROJ DRONE DATA ###
##################################################################

## TRANSFORM TIF FILES TO INT16 USING gdal_translate ##
# gdalUtils::gdal_translate was not working, therefore it has to be
# started direct from the command line
#     1. Check if GDAL is installed (gdalinfo --version) -> if not install GDAL using conda (conda install -c conda-forge gdal)
#     2. Navigate to the folder where you have your data
#     3. Run the following command for all drone files, is needed for using resampling method 'mode'
#           gdal_translate -ot Int16 jan_result_optimized_re.tif jan_drone_int16.tif

## RESAMPLE DRONE FILES USING MODE METHOD ##

gdalUtils::gdalwarp(srcfile = '1-Data/8-DroneData/jan_drone_int16.tif',
                    dstfile = '1-Data/8-DroneData/jan_drone_resample_mode.tif', 
                    s_srs = '+proj=utm +zone=33 +ellps=WGS84 +units=m +no_defs', 
                    t_srs = '+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs',
                    tr = c(10, 10),
                    tap = TRUE,
                    r = 'mode'
)

gdalUtils::gdalwarp(srcfile = '1-Data/8-DroneData/sas_drone_int16.tif',
                    dstfile = '1-Data/8-DroneData/sas_drone_resample_mode.tif', 
                    s_srs = '+proj=utm +zone=33 +ellps=WGS84 +units=m +no_defs', 
                    t_srs = '+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs',
                    tr = c(10, 10),
                    tap = TRUE,
                    r = 'mode'
)

gdalUtils::gdalwarp(srcfile = '1-Data/8-DroneData/tod_drone_int16.tif',
                    dstfile = '1-Data/8-DroneData/tod_drone_resample_mode.tif', 
                    s_srs = '+proj=utm +zone=33 +ellps=WGS84 +units=m +no_defs', 
                    t_srs = '+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs',
                    tr = c(10, 10),
                    tap = TRUE,
                    r = 'mode'
)

