# rm(list=ls())
setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")
source("00_utility.R")

# Load data on community surveys:
all_surveys <- read.csv("data/community_studies_5Jul2024.csv")

#---------------------------------
# Extracting climate and altitude medians per ecoregion
bio <- raster::getData('worldclim', var='bio', res=2.5) # 19 worldclim vars

#alt <- raster::getData('worldclim', var='alt', res=2.5) # altitude
ai <- raster("layers/ai_v3_yr.tif")
npp <- raster("layers/MOD17A3H_Y_NPP_2023-01-01_rgb_720x360.TIFF")
prop_bee_angios <- raster("layers/prop_raster.tif")
bee_rich <- raster("layers/bee_rich.tif")
wind <- raster("layers/mean_wind.tif")
srad <- raster("layers/mean_srad.tif")
et0 <- raster("layers/et0_v3_yr.tif")

#---------------------------------
coordinates <- all_surveys[,c("latitude","longitude")]

points <- coordinates
layers <- c(as.list(bio), ai, npp, prop_bee_angios, bee_rich, wind, srad, et0)
names(layers) <- c(names(bio),"ai","npp", "prop_bee_angios","bee_rich","wind","srad","et0")

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
    if(names(layers)[layer_index]=="prop_bee_angios") {
      if(length(values[[1]])==1 && values[[1]][1]==0) { #increase buffer 
        buffered_point <- buffer(one_point, width = 50000)
        values <- raster::extract(layer, buffered_point)
      }
      values[[1]] <- subset(values[[1]], values[[1]]!=0)
    }
    values <- subset(values[[1]], !is.na(values[[1]]))
    one_median <- median(values)
    medians <- c(medians, one_median)
  }  
  coordinates <- cbind(coordinates, medians)
  colnames(coordinates)[2+layer_index] <- names(layers)[layer_index]
}
all_surveys <- cbind(all_surveys,coordinates[,3:(2+length(layers))])

#---------------------------------
# Adding biome data
coordinates <- all_surveys[,c("latitude","longitude")]
biomes_for_points <- localityToBiome(coordinates, lat="latitude",lon="longitude")
all_surveys <- cbind(all_surveys, biomes_for_points)

#---------------------------------
# Binarizing biome type:
closed_canopy <- c("Tropical & Subtropical Moist Broadleaf Forests", "Tropical & Subtropical Dry Broadleaf Forests", "Tropical & Subtropical Coniferous Forests", "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Boreal Forests/Taiga")
open_canopy <- c("Tropical & Subtropical Grasslands, Savannas & Shrubland", "Temperate Grasslands, Savannas & Shrublands", "Flooded Grasslands & Savannas", "Montane Grasslands & Shrublands", "Tundra","Deserts & Xeric Shrublands", "Mediterranean Forests, Woodlands & Scrub",  "Mangroves")
all_surveys$bin_biome <- unlist(ifelse(all_surveys$biome%in%closed_canopy, "closed","open"))

#--------------------------------- 
# By "super-biome":
temperate <- c("Temperate Broadleaf & Mixed Forests", 
               "Temperate Conifer Forests", 
               "Temperate Grasslands, Savannas & Shrublands",
               "Boreal Forests/Taiga",
               "Montane Grasslands & Shrublands", 
               "Tropical & Subtropical Coniferous Forests",
               "Tundra")
tropical <- c("Tropical & Subtropical Grasslands, Savannas & Shrubland", 
              "Tropical & Subtropical Moist Broadleaf Forests", 
              "Tropical & Subtropical Dry Broadleaf Forests", 
              "Mangroves")
arid <- c("Mediterranean Forests, Woodlands & Scrub",
          "Deserts & Xeric Shrublands")
all_surveys$super_biome <- NA
for(i in 1:nrow(all_surveys)) {
  if(all_surveys$biome[i] %in% temperate) {
    all_surveys$super_biome[i] <- "temperate"
  } 
  if(all_surveys$biome[i] %in% tropical) {
    all_surveys$super_biome[i] <- "tropical"
  }
  if(all_surveys$biome[i] %in% arid) {
    all_surveys$super_biome[i] <- "arid"
  }
}

#--------------------------------- 
# Binarizing tropical vs. temperate:
tropical <- which(all_surveys$latitude > -23 & all_surveys$latitude < 23) 
temperate <- which(all_surveys$latitude <= -23 | all_surveys$latitude >= 23) 
all_surveys$tropical <- NA
all_surveys$tropical[tropical] <- "tropical"
all_surveys$tropical[temperate] <- "temperate"


#--------------------------------- 
saveRDS(all_surveys, "data/community_studies_w_habitat_categories_&_env_vars.Rdata")
write.csv(all_surveys, "data/community_studies_w_habitat_categories_&_env_vars.csv")
