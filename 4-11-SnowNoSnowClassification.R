
# Load libraries
library(raster)
library(rgdal)
library(glcm)
library(rgeos)
library(snow)
library(rminer)
library(splitstackshape)
library(data.table)
library(rpart)
library(randomForest)
library(snow)

#### Import ground truthing points and generate sample points ####
# Snow/NoSnow ground truthing or manual registration
# Drone data (2017-2019)
# Permanent sites (Excel)
# Transects 2015 (arcmap)
# Pixel squares 2016 (Excel)



#### Import data and preprocess ####
# s2_data <- stack('1-Data/1-SnowNoSnow/20190614T120649_20190614T120652_T33XWG.tif', bands = c(2, 3, 4, 8))
s2_data <- stack('1-Data/1-SnowNoSnow/20190527T124709_20190527T124710_T33XWG.tif', bands = c(2, 3, 4, 8))

names(s2_data) <- c('blue', 'green', 'red', 'nir')
s2_ndvi <- (s2_data$nir - s2_data$red) / (s2_data$nir + s2_data$red) # Calculate NDVI
names(s2_ndvi) <- c('ndvi')
s2_data <- stack(s2_data$blue, s2_data$green, s2_data$red, s2_ndvi)

# Import ground truthing points and convert coordinate system
ground_truth <- read.csv2('1-Data/0-SnowNoSnowGroundTruth/20190614_lcc_groundtruthing.csv')
ground_truth$longitude <- as.numeric(ground_truth$longitude)
ground_truth$latitude <- as.numeric(ground_truth$latitude)
coordinates(ground_truth) <- ~longitude + latitude
proj4string(ground_truth) <- CRS('+init=epsg:4326')
ground_truth <- spTransform(ground_truth, CRS('+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs'))
plot(s2_data$ndvi)
plot(ground_truth, add = TRUE)

# Generate sample points
sample_values <- raster::extract(s2_data, ground_truth, df = TRUE, sp = TRUE)
sample_values$landcover <- as.factor(sample_values$landcover)
sample_values$class <- as.numeric(sample_values$landcover)
sample_df <- as.data.frame(sample_values)
sample_df <- sample_df[, c('blue', 'green', 'red', 'ndvi', 'class'), drop = FALSE] # Drop landcover and nir bands
sample_df$class <- factor(sample_df$class, levels = c(1:4)) # Number of land cover types

# Add ID
sample_df <- tibble::rowid_to_column(sample_df, 'ID')

# Visualization distribution of covariates
val_ice <- subset(sample_df, class == 1)
val_no_snow <- subset(sample_df, class == 2)
val_snow <- subset(sample_df, class == 3)
val_water <- subset(sample_df, class == 4)

par(mfrow = c(4, 1))
hist(val_ice$ndvi, main = "ice", xlab = "NDVI", xlim = c(-1, 1), ylim = c(0, 20), col = "light blue")
hist(val_no_snow$ndvi, main = "no_snow", xlab = "NDVI", xlim = c(-1, 1), ylim = c(0, 20), col = "green")
hist(val_snow$ndvi, main = "snow", xlab = "NDVI", xlim = c(-1, 1), ylim = c(0, 20), col = "brown")
hist(val_water$ndvi, main = "water", xlab = "NDVI", xlim = c(-1, 1), ylim = c(0, 20), col = "dark blue")


### RF MODEL (without loop) ###

# Split sample_df to training and validation data 30 to 70 %
head(sample_df)
out_sas <- stratified(sample_df, c("class"), 0.7)
training_df <- sample_df[sample_df$ID %in% out_sas$ID, ]
validation_df <- sample_df[!sample_df$ID %in% out_sas$ID, ]

# Remove rows not to be included
training_df <- training_df[ , c(2:6)]
validation_df <- validation_df[ , c(2:6)]
head(training_df)
head(validation_df)

#Run random forest model
model_rf = rminer::fit(class~., training_df, model = 'randomForest', task = 'class')

table_res <- as.data.frame(model_rf@object$importance) # create dataframe whilst extracting the descriptor 
table_res_s <- table_res[order(table_res$MeanDecreaseAccuracy), ] # change order of values in column from highest to lowest

#plot the decriptor importance as barplot
par(mar=c(4,5,4,4))
barplot(table_res_s$MeanDecreaseAccuracy,
        horiz = "TRUE", col = "darkgreen", xlab = "importance", xlim = c(0,0.9),
        names.arg = row.names(table_res_s), cex.names = 0.5, las = 1, border = F)

savemodel(model_rf,"model_rf_snow_no_snow_2019") # saves to file


# use the model 'model_rf' to predict the classes in the validation dataset
validation_df$class_pred = rminer::predict(model_rf, validation_df)
validation_df$class <- as.factor(validation_df$class)

# compare predicted versus observed data
sas_c_m <- rminer::mmetric(validation_df$class, validation_df$class_pred, metric=c("ALL"))
sas_c_m1<-rminer::mmetric(validation_df$class, validation_df$class_pred, metric=c("CONF"))


### CREATING MAP ###

model_rf_reload<-loadmodel("model_rf_snow_no_snow_2019") # Load model
 
pred_lc <- raster::predict(s2_data, model = model_rf_reload) # Predict LC

#load the raster stack you want to use (in this case sas image map), be careful the raster stack and model use the same column names
#closeAllConnections() 
#beginCluster()
# sas_preds_rf <- clusterR(s2_data, raster::predict, args = list(model = model_rf_reload))
#endCluster()

writeRaster(pred_lc, "pred_lc_result.tif", overwrite=TRUE) # save the output
plot(pred_lc)

#Rename if necessary (I renamed it here to fit the the labels used #for the other ares.)
sas_preds_rf<-raster("sas_result_optimized.tif")

m <- c(1,101, 2,102, 3,103, 4,106, 5, 107, 6, 108, 7, 109, 8, 110, 9, 111, 10, 112, 11, 113)
rclmat <- matrix(m, ncol=2, byrow=TRUE)
sas_preds_rf_re <- reclassify(sas_preds_rf, rclmat)

#save the output
writeRaster(sas_preds_rf_re, "sas_result_optimized_re.tif", overwrite=TRUE)

