#################################
### INSTALL AND LOAD PACKAGES ###
#################################

library(raster)
library(dplyr)
library(anomalize)
library(rgdal)
library(gdalUtils)
library(tictoc)
library(sp)
library(snow)
library(rminer)
library(splitstackshape)
library(tidyverse)
library(ggplot2)
library(viridis)
library(caret)
library(cluster)

setwd("/uitserver/dst038/ResearchData/AMB/Nor_Pop_and_Eco/NPE-norpec/DavidStuder/")
cpu <- 20 # Number of used CPU's

#################################
### PRE PROCESS TRAINING DATA ###
#################################

### Imort Ground Truth ###
ground_truth <- read.csv2('1-Data/1-TrainingData/b_lcc_training_data.csv', header = TRUE, sep = ";")
ground_truth <- ground_truth %>% select(DataSource, Date, LandCoverClass, POINT_X, POINT_Y)

### Rename LandCoverClasses ###
ground_truth <- ground_truth %>% transmute(DataSource, Date, POINT_X, POINT_Y,
                                           LandCoverClass = ifelse(LandCoverClass == 'barren', 'bar',
                                                                   ifelse(LandCoverClass == 'barren_veg_mix', 'REMOVE',
                                                                          ifelse(LandCoverClass == 'cassiope', 'cass',
                                                                                 ifelse(LandCoverClass == 'cassiope_ice', 'cass',
                                                                                        ifelse(LandCoverClass == 'crust', 'cru',
                                                                                               ifelse(LandCoverClass == 'dryas', 'dry',
                                                                                                      ifelse(LandCoverClass == 'dryas_established', 'dry',
                                                                                                             ifelse(LandCoverClass == 'dryas_exposed', 'dry',
                                                                                                                    ifelse(LandCoverClass == 'dryas_ice', 'dry',
                                                                                                                           ifelse(LandCoverClass == 'flooded_veg_desc', 'REMOVE',
                                                                                                                                  ifelse(LandCoverClass == 'heath', 'hea',
                                                                                                                                         ifelse(LandCoverClass == 'heath_luz', 'hea',
                                                                                                                                                ifelse(LandCoverClass == 'heath_sal', 'hea',
                                                                                                                                                       ifelse(LandCoverClass == 'heath_sax_upp', 'hea',
                                                                                                                                                              ifelse(LandCoverClass == 'moss_crust', 'REMOVE',
                                                                                                                                                                     ifelse(LandCoverClass == 'moss_thin', 'hea',
                                                                                                                                                                            ifelse(LandCoverClass == 'moss_tundra', 'mos_tu',
                                                                                                                                                                                   ifelse(LandCoverClass == 'moss_tundra_disturbed', 'mos_tu',       
                                                                                                                                                                                          ifelse(LandCoverClass == 'moss_tundra_equ', 'mos_tu',
                                                                                                                                                                                                 ifelse(LandCoverClass == 'moss_tundra_fertilized', 'mos_tu',
                                                                                                                                                                                                        ifelse(LandCoverClass == 'river', 'REMOVE',
                                                                                                                                                                                                               ifelse(LandCoverClass == 'river_bed', 'bar',
                                                                                                                                                                                                                      ifelse(LandCoverClass == 'water_blue', 'water_bl',
                                                                                                                                                                                                                             ifelse(LandCoverClass == 'water_brown', 'water_br',
                                                                                                                                                                                                                                    ifelse(LandCoverClass == 'water_river', 'water_ri',
                                                                                                                                                                                                                                           ifelse(LandCoverClass == 'rocks', 'rocks',
                                                                                                                                                                                                                                                  ifelse(LandCoverClass == 'snow', 'snow',       
                                                                                                                                                                                                                                                         ifelse(LandCoverClass == 'snow_bed', 'REMOVE',
                                                                                                                                                                                                                                                                ifelse(LandCoverClass == 'shadow', 'shadow',       
                                                                                                                                                                                                                                                                       ifelse(LandCoverClass == 'wetland', 'wet', 'FAILURE')))))))))))))))))))))))))))))))

print(paste('FAILURES IN RENAMING: ', ground_truth %>% filter(LandCoverClass == 'FAILURE') %>% dplyr::count()))
print(paste('REMOVED TRAINING POINTS: ', ground_truth %>% filter(LandCoverClass == 'REMOVE') %>% dplyr::count()))
ground_truth <- ground_truth %>% filter(LandCoverClass != 'REMOVE')
head(ground_truth)

utm_33 <- ('+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs')
gt_sp <- ground_truth
sp::coordinates(gt_sp) <- c('POINT_X', 'POINT_Y')
sp::proj4string(gt_sp) <- CRS('+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs')
print(gt_sp)

#####################################
### PRE PROCESS RASTER LAYER 2017 ###
#####################################

## Imort, Align and Stack Sentinel 2 & DEM Rasters ##
tic()
closeAllConnections()
beginCluster(cpu)
s2_2017 <- raster::stack('1-Data/2-Sentinel2Data/2017-Sentinel2/20170731-1-utm.tif')
arctic_elevation <- raster::stack('1-Data/3-ArcticDEM/ArcticDEM_Elevation.tif')
arctic_elevation_align <- raster::resample(arctic_elevation, s2_2017, method = 'bilinear')
arctic_roughness <- raster::stack('1-Data/3-ArcticDEM/ArcticDEM_roughness.tif')
arctic_roughness_align <- raster::resample(arctic_roughness, s2_2017, method = 'bilinear')
arctic_tri <- raster::stack('1-Data/3-ArcticDEM/ArcticDEM_tri.tif')
arctic_tri_align <- raster::resample(arctic_tri, s2_2017, method = 'bilinear')
raster_data <- raster::stack(s2_2017, arctic_elevation_align, arctic_roughness_align, arctic_tri_align)
names(raster_data) <- c('blue', 'green', 'red', 'nir', 'elevation', 'roughness', 'tri')
fun1 <- function(x) (x$nir - x$red) / (x$nir + x$red)
raster_data$ndvi <- clusterR(raster_data, fun = fun1)
endCluster()
print(raster_data)

## Mask Raster files with imported AoI and Glacier Shapefiles ##
closeAllConnections()
beginCluster(cpu)
AoI <- readOGR('1-Data/0-AoI/LCCAoI.shp')
AoI <- sp::spTransform(AoI, utm_33)
MskGlr <- readOGR('1-Data/4-GlacierMask/GI2020_Perimeters_Svalbard_AllOutlines.shp')
MskGlr <- sp::spTransform(MskGlr, utm_33)
raster_aoi <- clusterR(raster_data, raster::mask, args = list(mask = AoI))
raster_msk_gl <- clusterR(raster_aoi, raster::mask, args = list(mask = MskGlr, inverse = TRUE))
names(raster_msk_gl) <- c('blue', 'green', 'red', 'nir', 'elevation', 'roughness', 'tri', 'ndvi')
endCluster()
print(raster_msk_gl)

## Save outputs ##
writeRaster(raster_msk_gl, filename = '1-Data/5-TmpOutputs/raster_svalbard.grd', overwrite = TRUE)
toc()

#############################
### PROCESS TRAINING DATA ###
#############################

# IMPORT GRD FILE #
raster_data_imp <- raster::stack('1-Data/5-TmpOutputs/raster_svalbard.grd')
names(raster_data_imp) <- c('blue', 'green', 'red', 'nir', 'elevation', 'roughness', 'tri', 'ndvi')

# EXTRACT TRAINING POINTS #
closeAllConnections()
beginCluster(cpu)
sp_ground_truth <- raster::extract(raster_data_imp, gt_sp, sp = TRUE)
endCluster()
df_gt_all <- raster::as.data.frame(sp_ground_truth, xy = TRUE)
df_gt_all$class <- as.factor(df_gt_all$LandCoverClass)
df_gt_all$class <- as.integer(df_gt_all$class)
df_gt <- df_gt_all %>% tidyr::drop_na() %>% tibble::rowid_to_column('ID') %>% 
  select(ID, POINT_X, POINT_Y, LandCoverClass, class, blue, green, red, nir, elevation, roughness, tri, ndvi)
code <- print(df_gt %>% select(LandCoverClass, class) %>% dplyr::distinct() %>% dplyr::arrange(class))
df_gt_plot <- as.data.frame(df_gt %>% select(LandCoverClass) %>% group_by(LandCoverClass) %>% count())
print(df_gt_plot)
ggplot(df_gt_plot, aes(LandCoverClass, n)) + geom_col() + geom_text(aes(label = n), position = position_dodge(0.9))

# REMOVE OUTLIERS FROM TRAINING DATA #
df_gt_an <- df_gt %>% group_by(LandCoverClass) %>% 
  summarise(median = median(ndvi), 
            qs25 = stats::quantile(ndvi, probs = 0.05),
            qs75 = stats::quantile(ndvi, probs = 0.95))
df_gt_jn <- left_join(df_gt, df_gt_an, by = "LandCoverClass") %>%
  dplyr::filter(ndvi >= qs25 & ndvi <= qs75)
df_gt_jn_plot <- as.data.frame(df_gt_jn %>% select(LandCoverClass) %>% group_by(LandCoverClass) %>% count())
print(df_gt_jn_plot)
ggplot(df_gt_jn_plot, aes(LandCoverClass, n)) + geom_col() + geom_text(aes(label = n), position = position_dodge(0.9))

# SAMPLING OF MOS_TU, DRY & HEA POINTS #
df_gt_bag_dry <- df_gt_jn %>% filter(LandCoverClass == 'dry') %>% dplyr::slice_sample(n = 100)
df_gt_bag_hea <- df_gt_jn %>% filter(LandCoverClass == 'hea') %>% dplyr::slice_sample(n = 100)
df_gt_bag_mos <- df_gt_jn %>% filter(LandCoverClass == 'mos_tu') %>% dplyr::slice_sample(n = 100)
df_gt_bag <- rbind(df_gt_jn %>% dplyr::filter(LandCoverClass != 'dry' & LandCoverClass != 'hea' & LandCoverClass != 'mos_tu'), df_gt_bag_dry, df_gt_bag_hea, df_gt_bag_mos)
df_gt_holdout <- df_gt_jn %>% anti_join(df_gt_bag) # Holdout data for accuracy assessment
df_gt_bag_plot <- as.data.frame(df_gt_bag %>% select(LandCoverClass) %>% group_by(LandCoverClass) %>% count())
print(df_gt_bag_plot)
ggplot(df_gt_bag_plot, aes(LandCoverClass, n)) + geom_col() + geom_text(aes(label = n), position = position_dodge(0.9))


# VISUALIZE #
p_distribution_ndvi <- ggplot(df_gt_jn, aes(x=LandCoverClass, y=ndvi, fill=LandCoverClass)) +
  geom_boxplot() +
  geom_jitter(color="grey", alpha=0.3, size=0.9) +
  scale_fill_viridis(discrete=TRUE) +
  theme(
    legend.position="none"
  ) +
  coord_flip() +
  labs(title = 'Distribution of NDVI values') +
  xlab("") +
  ylab('NDVI')
plot(p_distribution_ndvi)

########################################
### CLASSIFIER WITH ALL GROUND TRUTH ###
########################################

main_cl_df <- df_gt_bag
# main_cl_df <- main_cl_df[ , c('LandCoverClass', 'blue', 'green', 'red', 'nir', 'elevation', 'roughness', 'tri', 'ndvi')] ## INCLUDE DEM
main_cl_df <- main_cl_df[ , c('LandCoverClass', 'blue', 'green', 'red', 'nir', 'ndvi')] # EXCLUDE DEM
main_cl_df$LandCoverClass <- as.factor(main_cl_df$LandCoverClass)
model_rf_all = rminer::fit(LandCoverClass~., main_cl_df, model = 'randomForest', task = 'class')
print(model_rf_all)
table_res_all <- as.data.frame(model_rf_all@object$importance) # create dataframe whilst extracting the descriptor 
table_res_all_s <- table_res_all[order(table_res_all$MeanDecreaseAccuracy), ] # change order of values in column from highest to lowest
print(table_res_all_s)
barplot(table_res_all_s$MeanDecreaseAccuracy,
        horiz="TRUE", col="darkgreen", xlab="importance", xlim=c(0,0.9),
        names.arg=row.names(table_res_all_s), cex.names=0.5, las=1, border=F)

# Classify raster
closeAllConnections() 
beginCluster(cpu)
# raster_data_imp_sel <- raster_data_imp ## INCLUDE DEM
raster_data_imp_sel <- raster::stack(raster_data_imp$blue, raster_data_imp$green, raster_data_imp$red, raster_data_imp$nir, raster_data_imp$ndvi) ## EXCLUDE DEM
sas_preds_rf <- clusterR(raster_data_imp_sel, raster::predict, args = list(model = model_rf_all))
endCluster()

#save the outputs
writeRaster(sas_preds_rf, "1-Data/6-Outputs/19_lcc_2017_all_q5_bag100_noDEM_OOB25.tif", overwrite=TRUE)
write.csv(df_gt_holdout, "1-Data/6-Outputs/19_lcc_2017_holdout.csv")
write.csv(code, "1-Data/6-Outputs/19_lcc_2017_code.txt" )


#######################################################
### BUILD RF MODEL USING TRAINING AND VALIDATION DF ###
#######################################################

# Split df_ground_truth_mer to training and validation data 30 to 70 %
out_sas <- stratified(df_gt_bag, c("LandCoverClass"), 0.7)
training_df <- df_gt_bag[df_gt_bag$ID %in% out_sas$ID, ]
validation_df <- df_gt_bag[!df_gt_bag$ID %in% out_sas$ID, ]

# Remove rows not to be included
training_df <- training_df[ ,  c('LandCoverClass', 'blue', 'green', 'red', 'nir', 'elevation', 'roughness', 'tri', 'ndvi')]
validation_df <- validation_df[ , c('LandCoverClass', 'blue', 'green', 'red', 'nir', 'elevation', 'roughness', 'tri', 'ndvi')]

training_df$LandCoverClass <- as.factor(training_df$LandCoverClass)
validation_df$LandCoverClass <- as.factor(validation_df$LandCoverClass)

# Run random forest model 
model_rf = rminer::fit(LandCoverClass~., training_df, model = 'randomForest', task = 'class')
table_res <- as.data.frame(model_rf@object$importance) # create dataframe whilst extracting the descriptor 
table_res_s <- table_res[order(table_res$MeanDecreaseAccuracy), ] # change order of values in column from highest to lowest
print(table_res_s)
#plot the decriptor importance as barplot
par(mar=c(4,5,4,4))
barplot(table_res_s$MeanDecreaseAccuracy,
        horiz="TRUE", col="darkgreen", xlab="importance", xlim=c(0,0.9),
        names.arg=row.names(table_res_s), cex.names=0.5, las=1, border=F)


# use the model 'model_rf' to predict the classes in the validation dataset
validation_df$LandCoverClass_pred = rminer::predict(model_rf, validation_df)

# compare predicted versus observed data
sas_c_all <- rminer::mmetric(validation_df$LandCoverClass, validation_df$LandCoverClass_pred, metric=c("ALL"))
sas_c_acc <- rminer::mmetric(validation_df$LandCoverClass, validation_df$LandCoverClass_pred, metric=c("ACC"))
sas_c_con <- rminer::mmetric(validation_df$LandCoverClass, validation_df$LandCoverClass_pred, metric=c("CONF"))
print(sas_c_all)
print(sas_c_acc)
print(sas_c_con)

# Save model
savemodel(model_rf,"1-Data/5-TmpOutputs/sas_rf_4b_dem_2017") # saves to file

#############

# Load model
sas_rf_reload<-loadmodel("1-Data/5-TmpOutputs/sas_rf_4b_dem_2017")

# Classify raster
closeAllConnections() 
beginCluster(cpu)
sas_preds_rf <- clusterR(raster_data_imp, raster::predict, args = list(model = model_rf))
endCluster()

#save the output
writeRaster(sas_preds_rf, "1-Data/6-Outputs/sas_pred_2017_q25_bag05_07_03.tif", overwrite=TRUE)
