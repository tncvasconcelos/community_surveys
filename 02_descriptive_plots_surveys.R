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
subset_bees <- read.csv("data/community_studies_5Jul2024.csv")
#---------------------------------
# Subset dataset for surveys with bees (before we standartize dataset?)
# subset_bees <- subset(all_surveys, all_surveys$bee!="")
# subset_bees <- subset(subset_bees, !grepl("did", subset_bees$bee))
# subset_bees$bee <- as.numeric(subset_bees$bee)

#---------------------------------
# Plot map
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
# Plot by type of vegetation
coordinates <- subset_bees[,c("latitude","longitude")]
biomes_for_points <- localityToBiome(coordinates, lat="latitude",lon="longitude")
# table(biomes_for_points$biome)
subset_bees <- cbind(subset_bees, biomes_for_points)
subset_bees <- subset(subset_bees, !is.na(subset_bees$biome))
biome_pal <- c("#FFFF99","#F5F5DC","#FF7F00","#AADC32FF","#33A02C","#B2DF8A","#7CAE00","#90EE90","#B15928","#FDBF6F","#006400")
#jitter_pal <- lapply(1:length(biome_pal), function(x) rep(biome_pal[x],times=as.numeric(table(subset_bees$biome))[x]))

pdf("plots/prop_by_biome.pdf", width=8, height=3)
ggplot(subset_bees, aes(x = biome, y = bee, fill = biome)) +
  geom_boxplot() +
  scale_fill_manual(values = biome_pal) +
  geom_jitter(color="black",fill="gray", shape=21, size=0.8, alpha=0.9) +
  theme_minimal() +  # Use a minimal theme with white background
  coord_flip() +   # Flip coordinates
  theme(legend.position="none",
    plot.title = element_text(size=13)) +
  ylab("proportion of species with bee flowers")
dev.off()

#--------------------------------- 
#---------------------------------
# Binarizing biome type:
closed_canopy <- c("Tropical & Subtropical Moist Broadleaf Forests", "Tropical & Subtropical Dry Broadleaf Forests", "Tropical & Subtropical Coniferous Forests", "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Boreal Forests/Taiga")
open_canopy <- c("Tropical & Subtropical Grasslands, Savannas & Shrubland", "Temperate Grasslands, Savannas & Shrublands", "Flooded Grasslands & Savannas", "Montane Grasslands & Shrublands", "Tundra","Deserts & Xeric Shrublands", "Mediterranean Forests, Woodlands & Scrub",  "Mangroves")
subset_bees$bin_biome <- unlist(ifelse(subset_bees$biome%in%closed_canopy, "closed","open"))

# any difference between open and closed canopy?
boxplot(subset_bees$bee~subset_bees$bin_biome)
table(subset_bees$bin_biome)
abline(h=0.5, col="red", lty=3)
PMCMRplus::kwAllPairsConoverTest(x=subset_bees$bee,g=as.factor(subset_bees$bin_biome)) # non-significant difference

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
boxplot(subset_bees$bee~subset_bees$tropical)
abline(h=0.5, col="red", lty=3)
PMCMRplus::kwAllPairsConoverTest(x=subset_bees$bee,g=as.factor(subset_bees$tropical)) # non-significant difference

#--------------------------------- 
#--------------------------------- 
# By type of data (observation vs. syndrome)
boxplot(subset_bees$bee~subset_bees$data_type)
abline(h=0.5, col="red", lty=3)
PMCMRplus::kwAllPairsConoverTest(x=subset_bees$bee,g=as.factor(subset_bees$data_type))
#---------------------------------

write.csv(subset_bees, "data/community_studies_w_habitat_categories.csv", row.names=F)
