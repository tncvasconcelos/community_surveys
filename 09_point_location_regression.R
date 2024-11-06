# rm(list=ls())
setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")
library(sf) 
source("00_utility.R")

point_studies <- read.csv("data/bee_plant_inventories.csv")
point_studies$prop_b_a <- point_studies$bee_sp_rich / point_studies$fl_plant_sp_rich
layer <- raster("layers/prop_raster.tif")
points <- point_studies[,c("Latitude","Longitude")]

####
# If local
path="TWDG/wgsrpd-master/level3/level3.shp"
#-----------------------------
twgd_data <- maptools::readShapeSpatial(path)
twgd_data01 <- sf::st_as_sf(twgd_data)
organized_table_for_plot_total <- read.csv("files_for_maps/sp_rich_table_plants.csv")
twgd_data_plants <- merge(twgd_data01, organized_table_for_plot_total, by.x="LEVEL3_COD", by.y="one_area")
twgd_data_plants <- subset(twgd_data_plants , twgd_data_plants$LEVEL1_COD%in%c(7,8)) # only americas

####
bee_sp_rich <- maptools::readShapeSpatial("bee_rich/America_beerich.shp") # shapefile sent by Alice Hughes
twgd_data_bees <- sf::st_as_sf(bee_sp_rich)


point_studies$prop_from_layer <- NA
point_studies$bee_sp_rich_from_layer <- NA
point_studies$plant_sp_rich_from_layer <- NA
for(i in 1:nrow(points)) {
  one_point <- points[i,]
  one_point <- as.data.frame(one_point)
  sp::coordinates(one_point) <- ~ Longitude + Latitude
  crs(one_point) <- "+proj=longlat +datum=WGS84 +no_defs"
  buffered_point <- buffer(one_point, width = 10000)
  values <- raster::extract(layer, buffered_point)
  values[[1]] <- subset(values[[1]], values[[1]]!=0)
  values <- subset(values[[1]], !is.na(values[[1]]))
  point_studies$prop_from_layer[i] <- median(values)
  #------
  points_sf <- st_as_sf(one_point)  # Ensure points are of class 'sf'
  #Bees sp rich
  polygons_sf_bees <- st_as_sf(twgd_data_bees)  # Ensure polygons are of class 'sf'
  st_crs(points_sf) <- st_crs(polygons_sf_bees) 
  joined_data_bee <- st_join(points_sf, polygons_sf_bees)
  bee_sp_rich <- joined_data_bee$Total2
  point_studies$bee_sp_rich_from_layer[i] <- bee_sp_rich
  #Plant sp rich
  polygons_sf_plants <- st_as_sf(twgd_data_plants)  # Ensure polygons are of class 'sf'
  st_crs(points_sf) <- st_crs(polygons_sf_plants) 
  joined_data_plants <- st_join(points_sf, polygons_sf_plants)
  plant_sp_rich <- joined_data_plants$sp_rich
  point_studies$plant_sp_rich_from_layer[i] <- plant_sp_rich
}  

plot(point_studies$prop_from_layer~point_studies$prop_b_a)
a <- lm(point_studies$prop_from_layer~point_studies$prop_b_a)
summary(a)
abline(a)

plot(point_studies$bee_sp_rich~point_studies$bee_sp_rich_from_layer)
a <- lm(point_studies$bee_sp_rich~point_studies$bee_sp_rich_from_layer)
summary(a)
abline(a)

plot(point_studies$fl_plant_sp_rich~point_studies$plant_sp_rich_from_layer)
a <- lm(point_studies$fl_plant_sp_rich~point_studies$plant_sp_rich_from_layer)
summary(a)
abline(a)

#-----------------
# Load necessary libraries
library(ggplot2)
library(broom)

# Sample data: replace with your actual data
# point_studies <- data.frame(prop_b_a = ..., medians = ...)

# Fit the linear model
a <- lm(prop_from_layer ~ prop_b_a, data = point_studies)

# Extract statistics from the model
summary_stats <- glance(a)  # From broom package
r_squared <- round(summary_stats$r.squared, 3)
p_value <- signif(summary_stats$p.value, 3)


# Create the plot
pdf("point_study_shp_file_comparison.pdf")
ggplot(point_studies, aes(x = prop_b_a, y = prop_from_layer)) +
  geom_point(size = 2, color = "blue", alpha = 0.6) +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Regression line
  annotate("text", x = min(point_studies$prop_b_a), y = max(point_studies$prop_from_layer), 
           label = paste0("R² = ", r_squared, "\np = ", p_value),
           hjust = 0, vjust = 1, size = 5, color = "black") +
  labs(x = "bee:angiosperm (point studies)", y = "bee:angiosperm (shapefiles)", 
       title = "") +
  theme_minimal(base_size = 15)  # Clean theme
dev.off()



boxplot(point_studies$median_from_layer, point_studies$prop_b_a)
