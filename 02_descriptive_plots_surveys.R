# rm(list=ls())
setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")
source("00_utility.R")

########################################################
# Plots to visually inspect data on community surveys #
########################################################
path="TWDG/wgsrpd-master/level3/level3.shp"
#-----------------------------
twgd_data <- maptools::readShapeSpatial(path)
twgd_data01 <- sf::st_as_sf(twgd_data)
twgd_data01 <- subset(twgd_data01 , twgd_data01$LEVEL1_COD%in%c(7,8))

# Load data on community surveys:
subset_bees <- read.csv("data/community_studies_w_habitat_categories_&_env_vars.csv")
subset_bees$bee <- as.numeric(subset_bees$bee)
coordinates <- subset_bees[,c("latitude","longitude")]

#---------------------------------
# Plot map of community surveys
pdf("plots/map_surveys.pdf")
map_surveys <- ggplot() +  geom_sf(data = twgd_data01, fill = "white", color = "black", lwd=0.1, alpha=0.5) +  # White map background with black outlines
  geom_point(data = subset_bees, aes(x = longitude, y = latitude, fill = bee), color = "black", alpha = 0.7, shape = 21, size = 4) +  # Color-code points based on population percentage
  scale_fill_gradient(low = "blue", high = "red", name="prop bee visited") +  # Gradient color scale
  labs(title = "", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  coord_sf(ylim = c(-60, 90), xlim = c(-170, 0), expand = FALSE)
map_surveys
dev.off()

#---------------------------------
# Boxplots
par(mfrow=c(1,4))
# Plot by type of vegetation
coordinates <- subset_bees[,c("latitude","longitude")]
biomes_for_points <- localityToBiome(coordinates, lat="latitude",lon="longitude")
subset_bees <- cbind(subset_bees, biomes_for_points)
subset_biome_comparison <- subset(subset_bees, subset_bees$biome %in% names(table(subset_bees$biome))[which(table(subset_bees$biome) > 2)])
#subset_biome_comparison <- subset_bees
#--------------------------------- 
boxplot(subset_biome_comparison$bee~subset_biome_comparison$biome, 
        xlab = "biome type", ylab = "proportion of bee flowers",
        names=c("desert (n=6)","mediterranean (n=5)","montane grasslands (n=5)",
                "tropical dry forest (n=4)","tropical savannas (n=16)","tropical moist forest (n=11)"), frame = FALSE)
abline(h=0.5, col="red", lty=3)

#---------------------------------
# Binarizing biome type:
closed_canopy <- c("Tropical & Subtropical Moist Broadleaf Forests", "Tropical & Subtropical Dry Broadleaf Forests", "Tropical & Subtropical Coniferous Forests", "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Boreal Forests/Taiga")
open_canopy <- c("Tropical & Subtropical Grasslands, Savannas & Shrubland", "Temperate Grasslands, Savannas & Shrublands", "Flooded Grasslands & Savannas", "Montane Grasslands & Shrublands", "Tundra","Deserts & Xeric Shrublands", "Mediterranean Forests, Woodlands & Scrub",  "Mangroves")
subset_bees$bin_biome <- unlist(ifelse(subset_bees$biome%in%closed_canopy, "closed","open"))

# any difference between open and closed canopy?
# table(subset_bees$bin_biome)
boxplot(subset_bees$bee~subset_bees$bin_biome, 
        xlab = "biome type", ylab = "proportion of bee flowers",
        col = c("white", "steelblue"), names=c("closed (n=19)","open (n=34)"), frame = FALSE)
abline(h=0.5, col="red", lty=3)
PMCMRplus::kwAllPairsConoverTest(x=subset_bees$bee,g=as.factor(subset_bees$bin_biome)) # non-significant difference


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

subset_bees$super_biome <- NA
for(i in 1:nrow(subset_bees)) {
  if(subset_bees$biome[i] %in% temperate) {
    subset_bees$super_biome[i] <- "temperate"
  } 
  if(subset_bees$biome[i] %in% tropical) {
    subset_bees$super_biome[i] <- "tropical"
  }
  if(subset_bees$biome[i] %in% arid) {
    subset_bees$super_biome[i] <- "arid"
  }
}
  
table(subset_bees$super_biome)
boxplot(subset_bees$bee~subset_bees$super_biome, 
        xlab = "biome type", ylab = "proportion of bee flowers",
        col = c("white","gray", "steelblue"), names=c("arid (n=11)","temperate (n=10)","tropical (32)"), frame = FALSE)
abline(h=0.5, col="red", lty=3)
PMCMRplus::kwAllPairsConoverTest(x=subset_bees$bee,g=as.factor(subset_bees$super_biome)) # non-significant difference

#--------------------------------- 
#---------------------------------
# Binarizing tropical vs. temperate:
tropical <- which(subset_bees$latitude > -23 & subset_bees$latitude < 23) 
temperate <- which(subset_bees$latitude <= -23 | subset_bees$latitude >= 23) 
subset_bees$tropical <- NA
subset_bees$tropical[tropical] <- "tropical"
subset_bees$tropical[temperate] <- "temperate"
#---------------------------------
# Comparing tropics vs. temperate zones
boxplot(subset_bees$bee~subset_bees$tropical,
        xlab = "latitude", ylab = "proportion of bee flowers",
        col = c("white", "steelblue"), names=c("temperate (n=16)","tropical (n=37)"), frame = FALSE)
#table(subset_bees$tropical)
abline(h=0.5, col="red", lty=3)
PMCMRplus::kwAllPairsConoverTest(x=subset_bees$bee,g=as.factor(subset_bees$tropical)) # non-significant difference

subset_bees <- subset(subset_bees, subset_bees$spatial_analyses=="keep")

one_model_cor <- gls(
  model = bee~bin_biome,
  data = subset_bees,
  correlation = corSpher(form = ~ longitude + latitude, nugget = F)
)

#--------------------------------- 
#--------------------------------- 
# By type of data (observation vs. syndrome)
boxplot(subset_bees$bee~subset_bees$data_type)
abline(h=0.5, col="red", lty=3)
PMCMRplus::kwAllPairsConoverTest(x=subset_bees$bee,g=as.factor(subset_bees$data_type))
#---------------------------------

write.csv(subset_bees, "data/community_studies_w_habitat_categories.csv", row.names=F)


#---------------------------------
# Extracting climate and altitude medians per ecoregion

# Predictors with spatial autocorrelation correction
bio <- raster::getData('worldclim', var='bio', res=2.5) # 19 worldclim vars
alt <- raster::getData('worldclim', var='alt', res=2.5) # altitude
ai <- raster("layers/ai_v3_yr.tif")
npp <- raster("layers/MOD17A3H_Y_NPP_2023-01-01_rgb_720x360.TIFF")
prop_bee_angios <- raster("layers/prop_raster.tif")
