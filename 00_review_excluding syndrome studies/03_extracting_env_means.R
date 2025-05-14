# rm(list=ls())
setwd("/Users/alinemartins/community_surveys/00_review_excluding syndrome studies")
source("00_utility.R")

# Load data on community surveys:
all_surveys <- read.csv("data/community_studies_5Jul2024.csv") 


#---------------------------------
# Extracting climate and altitude medians per ecoregion
# Download bioclimatic variables at 2.5 arc-minute resolution
bio <- worldclim_global(var = "bio", res = 2.5, path = tempdir())  # 19 worldclim vars


#alt <- raster::getData('worldclim', var='alt', res=2.5) # altitude
#below change to terra::rast()
ai <- rast("layers/ai_v3_yr.tif") 
npp <- rast("layers/MOD17A3H_Y_NPP_2023-01-01_rgb_720x360.TIFF")
prop_bee_angios <- rast("layers/prop_raster.tif") 
bee_rich <- rast("layers/bee_rich.tif") 
wind <- rast("layers/mean_wind.tif")
srad <- rast("layers/mean_srad.tif")
et0 <- rast("layers/et0_v3_yr.tif")

#---------------------------------
coordinates <- all_surveys[,c("latitude","longitude")]

points <- coordinates
layers <- c(as.list(bio), ai, npp, prop_bee_angios, bee_rich, wind, srad, et0)
names(layers) <- c(names(bio),"ai","npp", "prop_bee_angios","bee_rich","wind","srad","et0")

#function changed in favor of using terra as raster is being phased out in favor of the first

for(layer_index in 1:length(layers)) {
  layer <- layers[[layer_index]]
  medians <- c()
  
  for(i in 1:nrow(points)) {
    one_point <- points[i, ]
    one_point <- as.data.frame(one_point)
    
    # Create point as terra vector
    vect_point <- terra::vect(one_point, geom = c("longitude", "latitude"), crs = "EPSG:4326")
    
    # Buffer it
    buffered_point <- terra::buffer(vect_point, width = 10000)
    
    values <- terra::extract(layer, buffered_point)[,2] # [,2] to drop ID column
    
    if(names(layers)[layer_index] == "prop_bee_angios") {
      if(length(values) == 1 && values[1] == 0) {
        buffered_point <- terra::buffer(vect_point, width = 50000)
        values <- terra::extract(layer, buffered_point)[,2]
      }
      values <- values[values != 0]
    }
    
    values <- values[!is.na(values)]
    one_median <- median(values)
    medians <- c(medians, one_median)
  }
  
  coordinates <- cbind(coordinates, medians)
  colnames(coordinates)[2 + layer_index] <- names(layers)[layer_index]
}
all_surveys <- cbind(all_surveys,coordinates[,3:(2+length(layers))])

#---------------------------------
# Adding biome data 
#changed to convert st to sf
# Convert survey coordinates to sf
all_surveys_sf <- st_as_sf(all_surveys, coords = c("longitude", "latitude"), crs = 4326)
# Join with biome polygons to assign BIOME to each point
all_surveys_sf <- st_join(all_surveys_sf, merged_biomes, join = st_intersects)
# Drop rows with no matched biome (optional, depending on your data)
all_surveys_sf <- all_surveys_sf[!is.na(all_surveys_sf$BIOME), ]
# Convert back to data.frame and add XY columns if needed
all_surveys <- cbind(all_surveys, BIOME = all_surveys_sf$BIOME)

#---------------------------------
# Binarizing biome type:
names(all_surveys)#cheking
str(all_surveys$BIOME)#cheking
table(all_surveys$BIOME) #cheking
all_surveys$BIOME <- as.character(all_surveys$BIOME) #ensure biome is character 
closed_canopy <- c("Tropical & Subtropical Moist Broadleaf Forests", "Tropical & Subtropical Dry Broadleaf Forests", "Tropical & Subtropical Coniferous Forests", "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Boreal Forests/Taiga")
open_canopy <- c("Tropical & Subtropical Grasslands, Savannas & Shrubland", "Temperate Grasslands, Savannas & Shrublands", "Flooded Grasslands & Savannas", "Montane Grasslands & Shrublands", "Tundra","Deserts & Xeric Shrublands", "Mediterranean Forests, Woodlands & Scrub",  "Mangroves")
all_surveys$bin_biome <- ifelse(all_surveys$BIOME %in% closed_canopy, "closed", "open")
names(all_surveys)[names(all_surveys) == "BIOME"] <- "biome"

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
