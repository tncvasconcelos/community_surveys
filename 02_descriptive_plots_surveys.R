# rm(list=ls())
setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")
source("00_utility.R")
library(nlme)

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
subset_bees <- subset(subset_bees,subset_bees$spatial_analyses=="keep")
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
# By type of data (observation vs. syndrome)
medians <- boxplot(subset_bees$bee~subset_bees$data_type, plot=F)
abline(h=0.5, col="red", lty=3)
PMCMRplus::kwAllPairsConoverTest(x=subset_bees$bee,g=as.factor(subset_bees$data_type))
#---------------------------------
palette_name1= "Viridis"
pdf("plots/data_type.pdf", height=6, width=6)
boxplot(subset_bees$bee~subset_bees$data_type,
        xlab = "data type", ylab = "species with bee-flowers",
        col = hcl.colors(length(unique(subset_bees$data_type)), palette = palette_name1, alpha = 0.75),
        names=c("observation \n (n=4)","syndrome \n (n=19)","syndrome \n and observation \n (n=30)"), frame = T)
abline(h=0.5, col="red", lty=2, lwd=2)
dev.off()
