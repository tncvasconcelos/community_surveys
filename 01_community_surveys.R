# rm(list=ls())
library(sp)
library(raster)
library(maptools)
library(ggplot2)
library(rnaturalearth)
data("wrld_simpl")

path="TWDG/wgsrpd-master/level3/level3.shp"
#-----------------------------
twgd_data <- maptools::readShapeSpatial(path)
twgd_data01 <- sf::st_as_sf(twgd_data)
twgd_data01 <- subset(twgd_data01 , twgd_data01$LEVEL1_COD%in%c(7,8))

setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")

# Load data on community surveys:
all_surveys <- read.csv("data/community_studies.csv")
coordinates <- all_surveys[,c("latitude","longitude")]

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

#---------------------------------
# Plot map
map_surveys <- ggplot() +  geom_sf(data = twgd_data01, fill = "white", color = "black", lwd=0.1, alpha=0.5) +  # White map background with black outlines
  geom_point(data = subset_bees, aes(x = longitude, y = latitude, fill = bee), color = "black", alpha = 0.7, shape = 21, size = 4) +  # Color-code points based on population percentage
  scale_fill_gradient(low = "blue", high = "red", name="prop bee visited") +  # Gradient color scale
  labs(title = "", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  coord_sf(ylim = c(-60, 90), xlim = c(-170, 0), expand = FALSE)

#---------------------------------
# Plot linear regressions:

plot(subset_bees$precipitation, subset_bees$bee) 
model <- lm(subset_bees$bee~subset_bees$precipitation)
summary(model)
abline(model)

model <- lm(subset_bees$bee~subset_bees$altitude)

coordinates <- subset_bees



tropical <- subset(coordinates, coordinates$latitude > -23 & coordinates$latitude < 23)
nontropical <- subset(coordinates, coordinates$latitude < -23|coordinates$latitude > 23)

boxplot(tropical$bee, nontropical$bee)


# Load required packages
library(spdep)
library(MASS)

# Assuming your data is stored in a dataframe called 'data'
# 'proportion_var' is the name of the proportion variable

# Step 1: Create spatial weights matrix
coords <- subset_bees[, c("longitude", "latitude")]  # Replace with your actual coordinate columns
W <- dnearneigh(coords, 0, 0)  # Nearest neighbor weights

# Step 2: Create spatial lag of the proportion variable
subset_bees$lag_proportion <- lag.listw(W, subset_bees$bee)

?lag.listw

# Step 3: Fit spatial lag model
model <- lm(your_response_variable ~ proportion_var + lag_proportion + other_covariates, data = data)

# Step 4: Check for spatial autocorrelation in residuals (optional)
# You can use Moran's I test on residuals
moran.test(model$residuals, W)

# Step 5: If proportion variable transformation is needed
# data$transformed_proportion <- asin(sqrt(data$proportion_var))

# Step 6: Fit GLM with binomial distribution and logit link function
# glm_model <- glm(your_response_variable ~ transformed_proportion + other_covariates, data = data, family = binomial(link = "logit"))
