# rm(list=ls())
setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")
library(nlme)
source("00_utility.R")

# Load data on community surveys:
all_surveys <- read.csv("data/community_studies_w_habitat_categories.csv")

#---------------------------------
# Extracting climate and altitude medians per ecoregion
bio <- raster::getData('worldclim', var='bio', res=2.5) # 19 worldclim vars
alt <- raster::getData('worldclim', var='alt', res=2.5) # altitude
ai <- raster("layers/ai_v3_yr.tif")
npp <- raster("layers/MOD17A3H_Y_NPP_2023-01-01_rgb_720x360.TIFF")
prop_bee_angios <- raster("layers/prop_raster.tif")

#---------------------------------
# Subset dataset for surveys with bees (before we standartize dataset?)
subset_bees <- subset(all_surveys, all_surveys$bee!="")
subset_bees <- subset(subset_bees, !grepl("did", subset_bees$bee))
subset_bees$bee <- as.numeric(subset_bees$bee)
coordinates <- subset_bees[,c("latitude","longitude")]

points=coordinates
layers=c(bio[[1]],bio[[12]],alt, ai, npp, prop_bee_angios)
names(layers) <- c("temperature", "precipitation","altitude","ai","npp", "prop_bee_angios")
for(layer_index in 1:length(layers)) {
  layer <- layers[[layer_index]]
  medians <- c()
  for(i in 1:nrow(points)) {
    one_point <- points[i,]
    one_point <- as.data.frame(one_point)
    sp::coordinates(one_point) <- ~ longitude + latitude
    crs(one_point) <- "+proj=longlat +datum=WGS84 +no_defs"
    buffered_point <- buffer(one_point, width = 10000)
    values <- raster::extract(layer, buffered_point)
    values <- subset(values[[1]], !is.na(values[[1]]))
    one_median <- median(values)
    medians <- c(medians, one_median)
  }  
  coordinates <- cbind(coordinates, medians)
  colnames(coordinates)[2+layer_index] <- names(layers)[layer_index]
}
all_surveys <- cbind(all_surveys,coordinates[,3:8])

write.csv(all_surveys, "data/community_studies_w_habitat_categories_&_env_vars.csv", row.names=F)