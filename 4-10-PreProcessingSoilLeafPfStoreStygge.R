library(tidyverse)
library(geofacet)
library(ggplot2)
library(ggthemes)
library(readxl)

# Import Table
s2data <- read_excel("1-Data/3-VegetationTransectAndPlots/soil_leaf_pf_store_stygge_16102020IP.xlsx", 
                     sheet = 'soil_leaf_pf_store_stygge_16102', col_names = TRUE)

# Select columns
s2select <- s2data %>% dplyr::select(PixelID_from_leaf_file, Locality_from_leaf_file, habitat_type_from_pf_file,
                              gps_east_from_pf_file, gps_north_from_pf_file)

# Remove NA Values
s2filter <- s2select %>% filter(habitat_type_from_pf_file != 'NA') %>% filter(gps_north_from_pf_file != 'NA') %>%
                                            filter(gps_east_from_pf_file != 'NA')
# Only keep Distinct entries
s2distinct <- s2filter %>% distinct(habitat_type_from_pf_file, gps_north_from_pf_file, gps_east_from_pf_file, .keep_all = TRUE)

# Coordiantes to float and round
s2coord <- s2distinct %>% mutate_at(vars(gps_north_from_pf_file, gps_east_from_pf_file), as.integer)

# Rename and Finalize attributes
s2final <- s2coord %>% mutate(DataSource = 'soil_leaf_pf_store_stygge') %>% mutate(QualityEstimation = as.integer(1)) %>% 
                      rename(LandCoverClass = habitat_type_from_pf_file) %>% 
                      dplyr::select(DataSource, QualityEstimation, LandCoverClass, gps_north_from_pf_file, gps_east_from_pf_file)
head(s2final)



# Import LandCoverData
lcdata <- read_excel("1-Data/4-TrainingValidationData/LandCoverSampleDataExcel.xls", col_names = TRUE)

lcselect <- lcdata %>% rename(QualityEstimation = QualityEst) %>% rename(LandCoverClass = LandCoverC) %>% 
                          rename(gps_north_from_pf_file = POINT_Y) %>% rename(gps_east_from_pf_file = POINT_X) %>% 
                          dplyr::select(! c(FID, OBJECTID))

lcformat <- lcselect %>% mutate_at(vars(QualityEstimation, gps_east_from_pf_file, gps_north_from_pf_file), as.integer)

# Bind DF
training_data <- bind_rows(s2final, lcformat)
head(training_data)

# Export as csv
write.csv(training_data, '1-Data/4-TrainingValidationData/a_training_data_part1.csv')

