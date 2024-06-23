

#---------------------------------
# Extracting climate and altitude medians per ecoregion
bio <- raster::getData('worldclim', var='bio', res=2.5) # 19 worldclim vars
alt <- raster::getData('worldclim', var='alt', res=2.5) # altitude
ai <- raster("layers/alt.bil")
npp <- raster("layers/MOD17A3H_Y_NPP_2023-01-01_rgb_720x360.TIFF")

points=coordinates
layers=c(bio[[1]],bio[[12]],alt, ai, npp)
names(layers) <- c("temperature", "precipitation","altitude","ai","npp")
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
all_surveys <- cbind(all_surveys,coordinates[,3:7])

#---------------------------------
# Subset dataset only for surveys with bees (TEMPORARY BEFORE WE STANDARTIZE DATASET)
subset_bees <- subset(all_surveys, !is.na(all_surveys$bee))