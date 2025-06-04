# rm(list=ls())
setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")
library(gridExtra)
source("00_utility.R")

########################################################
# Plots to visually inspect data on community surveys #
########################################################
# Load data on community surveys:
subset_bees <- read.csv("data/community_studies_w_habitat_categories_&_env_vars.csv")
subset_bees$bee <- as.numeric(subset_bees$bee)
coordinates <- subset_bees[,c("latitude","longitude")]
subset_bees <- subset(subset_bees,subset_bees$spatial_analyses=="keep")
subset_bees <- subset(subset_bees, subset_bees$bee!=0) # 52 studies

# After R1: excluding syndrome-only studies for sensitivity analyses
subset_bees_no_syndrome <- subset(subset_bees, subset_bees$data_type!="syndrome") # 33 studies

#-----------------------------
# Botanical countries
path="TWDG/wgsrpd-master/level3/level3.shp"
twgd_data <- st_read(path)
twgd_data01 <- sf::st_as_sf(twgd_data)
twgd_data01 <- subset(twgd_data01 , twgd_data01$LEVEL1_COD%in%c(7,8))

bee_range <- range(c(subset_bees$bee, subset_bees_no_syndrome$bee), na.rm = TRUE) # for plots

# Plot map of community surveys
pdf("plots/map_surveys.pdf")
map_surveys <- ggplot() +  geom_sf(data = twgd_data01, fill = "white", color = "black", lwd=0.1, alpha=0.5) +  # White map background with black outlines
  geom_point(data = subset_bees, aes(x = longitude, y = latitude, fill = bee), color = "black", alpha = 0.7, shape = 21, size = 4) +  # Color-code points based on population percentage
  scale_fill_gradient(low = "blue", high = "red", name="prop bee visited", limits=bee_range) +  # Gradient color scale
  labs(title = "Full dataset", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  coord_sf(ylim = c(-60, 90), xlim = c(-170, 0), expand = FALSE)
map_surveys
dev.off()


# Plot comparison between syndrome-only and full dataset for supplementary material:
pdf("plots/map_surveys_comparison_data_type.pdf", height=4, width=12)

map_surveys_no_syndrome <- ggplot() +  geom_sf(data = twgd_data01, fill = "white", color = "black", lwd=0.1, alpha=0.5) +  # White map background with black outlines
  geom_point(data = subset_bees_no_syndrome, aes(x = longitude, y = latitude, fill = bee), color = "black", alpha = 0.7, shape = 21, size = 4) +  # Color-code points based on population percentage
  scale_fill_gradient(low = "blue", high = "red", name="prop bee visited",limits=bee_range) +  # Gradient color scale
  labs(title = "Excluding syndrome-only studies", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  coord_sf(ylim = c(-60, 90), xlim = c(-170, 0), expand = FALSE)
grid.arrange(map_surveys,
             map_surveys_no_syndrome,
             ncol=2, nrow = 1)

dev.off()

#-----------------------------
# WWF biomes
biomes <- WWFload()
biomes <- sf::st_as_sf(biomes)

biomes <- subset(biomes, biomes$REALM%in%c("NA","NT"))

# Load required libraries
library(sf)
library(dplyr)

# Assuming `biomes` is already your sf object
# Merge geometries based on the 'Biome' column
biomes <- st_make_valid(biomes)

merged_biomes <- biomes %>%
  group_by(BIOME) %>%  # Group by the 'Biome' column
  summarise(geometry = st_union(geometry), .groups = "drop")  # Merge geometries

merged_biomes$BIOME[1:14] <- c("Tropical & Subtropical Moist Broadleaf Forests", 
                                         "Tropical & Subtropical Dry Broadleaf Forests", 
                                         "Tropical & Subtropical Coniferous Forests", 
                                         "Temperate Broadleaf & Mixed Forests", 
                                         "Temperate Conifer Forests", 
                                         "Boreal Forests/Taiga", 
                                         "Tropical & Subtropical Grasslands, Savannas & Shrubland", 
                                         "Temperate Grasslands, Savannas & Shrublands", 
                                         "Flooded Grasslands & Savannas", 
                                         "Montane Grasslands & Shrublands", 
                                         "Tundra", 
                                         "Mediterranean Forests, Woodlands & Scrub", 
                                         "Deserts & Xeric Shrublands", 
                                         "Mangroves")

save(merged_biomes, file="data/merged_biomes.Rsave")

# Plot map of community surveys
table(subset_bees$biome)

# Define a color palette for biomes
biome_colors <- c(
  "Tropical & Subtropical Moist Broadleaf Forests" = "#006400",  
  "Tropical & Subtropical Dry Broadleaf Forests" = "#8B4513",
  "Tropical & Subtropical Coniferous Forests" = "#228B22",
  "Temperate Broadleaf & Mixed Forests" = "#32CD32",
  "Temperate Conifer Forests" = "#2E8B57",
  "Boreal Forests/Taiga" = "white",
  "Tropical & Subtropical Grasslands, Savannas & Shrubland" = "#FFD700",
  "Temperate Grasslands, Savannas & Shrublands" = "#DAA520",
  "Flooded Grasslands & Savannas" = "white",
  "Montane Grasslands & Shrublands" = "#BDB76B",
  "Tundra" = "white",
  "Mediterranean Forests, Woodlands & Scrub" = "#FF8C00",
  "Deserts & Xeric Shrublands" = "#DEB887",
  "Mangroves" = "#4682B4"
)

biome_colors <- hcl.colors(14, palette = "Terrain", alpha = 0.5)
biome_colors <- c(
  "Boreal Forests/Taiga" = biome_colors[1],
  "Deserts & Xeric Shrublands" = biome_colors[2],
  "Flooded Grasslands & Savannas" = biome_colors[3],
  "Mangroves" = biome_colors[4],
  "Mediterranean Forests, Woodlands & Scrub" = biome_colors[5],
  "Montane Grasslands & Shrublands" = biome_colors[6],
  "Temperate Broadleaf & Mixed Forests" = biome_colors[7],
  "Temperate Conifer Forests" = biome_colors[8],
  "Temperate Grasslands, Savannas & Shrublands" = biome_colors[9],
  "Tropical & Subtropical Coniferous Forests" = biome_colors[10],
  "Tropical & Subtropical Dry Broadleaf Forests" = biome_colors[11],
  "Tropical & Subtropical Grasslands, Savannas & Shrubland" = biome_colors[12],
  "Tropical & Subtropical Moist Broadleaf Forests" = biome_colors[13],  
  "Tundra" = biome_colors[14]
)

# Convert subset_bees to sf object
subset_bees_sf <- st_as_sf(subset_bees, coords = c("longitude", "latitude"), crs = st_crs(merged_biomes))

# Perform spatial join to assign biomes to each point
subset_bees_sf <- st_join(subset_bees_sf, merged_biomes, join = st_intersects)

# Now 'subset_bees_sf' should have a 'BIOME' column from 'merged_biomes'

# Check for NA values in the BIOME column after join
subset_bees_sf <- subset_bees_sf[!is.na(subset_bees_sf$BIOME), ]

# Plot map with matching colors for biomes and points
map_biomes_points <- ggplot() +  
  geom_sf(data = merged_biomes, aes(fill = as.factor(BIOME)), color = "black", lwd = 0.05, alpha = 0.4) +  # Biome polygons
  geom_point(data = subset_bees_sf, aes(x = st_coordinates(subset_bees_sf)[,1], 
                                        y = st_coordinates(subset_bees_sf)[,2], 
                                        size = bee, fill = as.factor(BIOME)), 
             color = "black", alpha = 0.6, shape = 21) +  # Points with size and fill by biome
  scale_fill_manual(values = biome_colors, name = "Biome") +  # Matching colors
  scale_size_continuous(name = "Number of Bees", range = c(1, 8)) +  # Scale size of points
  labs(title = "", 
       x = "Longitude", y = "Latitude") +  
  theme_classic(base_size = 14) +  
  coord_sf(ylim = c(-60, 90), xlim = c(-170, 0), expand = FALSE) +
  theme(legend.position = "right", 
        legend.key.width = unit(1, "cm"), 
        plot.title = element_text(hjust = 0.5, face = "bold"))  # Center the title

# Save plot
ggsave("plots/map_biomes_bees2.pdf", plot = map_biomes_points, width = 12, height = 19)

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
