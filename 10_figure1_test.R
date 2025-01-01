# rm(list=ls())
setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")
source("00_utility.R")

#bee <- read.csv("data/brazil_bee_sprich_state.csv")
#unique(bee$Species[bee$region=="southeast"])
#unique(bee$region)
#unique(bee$Species)

# If local
path="TWDG/wgsrpd-master/level3/level3.shp"
#-----------------------------
twgd_data <- maptools::readShapeSpatial(path)
twgd_data01 <- sf::st_as_sf(twgd_data)

#########################
# Making maps for plants
dist_sample <- read.table("WCVP/wcvp_distribution.txt", sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
names_sample <- read.table("WCVP/wcvp_names.txt", sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
all_vars <- merge(dist_sample, names_sample, by="plant_name_id")

# Excluding non-flowering plants
families_to_exclude <- as.character(read.csv("files_for_maps/families_to_exclude.txt")[,1])
all_vars <- subset(all_vars, !all_vars$family%in%families_to_exclude)

# reference table for taxized names
reference_table <- list.files("WCVP/taxized_reference_tables", full.names = T)
reference_table <- do.call(rbind, lapply(reference_table[grep("csv",reference_table)], read.csv))

sp_data <- reference_table
colnames(sp_data) <- c("species","")
organized_table_for_plot_total <- organize.bubble.plot(sp_data, reference_table, all_vars, twgd_data)
# write.csv(organized_table_for_plot_total, "sp_rich_table_plants.csv", row.names = F)

organized_table_for_plot_total <- read.csv("files_for_maps/sp_rich_table_plants.csv")
twgd_data_plants <- merge(twgd_data01, organized_table_for_plot_total, by.x="LEVEL3_COD", by.y="one_area")
twgd_data_plants <- subset(twgd_data_plants , twgd_data_plants$LEVEL1_COD%in%c(7,8)) # only americas
#twgd_data_plants <- subset(twgd_data_plants, twgd_data_plants$LEVEL3_NAM!="Greenland")

# Angiosperm plot
tmp_map_plants <- ggplot(data = twgd_data_plants) +
  geom_sf(aes(fill = sp_rich)) +
  scale_fill_viridis_c(option = "C", alpha=0.9, name="a species") +
  theme_classic() +
  coord_sf(ylim = c(-60, 90), xlim = c(-170, 0), expand = FALSE)

#-----------------------------
# BEES
# gbif_bees <- data.table::fread("OutputData/05_cleaned_database.csv") # from Dorey et al. 2023
# points <- gbif_bees[,c("scientificName","decimalLatitude","decimalLongitude")]
# colnames(points) <- c("species", "lat","lon")
# results <- organize.bubble.plot2(points, twgd_data)
# #write.csv(results, file="files_for_maps/sp_rich_table_bees.csv", row.names=F)
# 
# # Note: the following was editted to use numbers from melo2007 for the Neotropics
# results <- read.csv("files_for_maps/sp_rich_table_bees_twgd_edit_melo2007.csv")
# # results$area_name <- NA
# # for(i in 1:nrow(results)){
# #   results$area_name[i] <- as.character(twgd_data01$LEVEL3_NAM[as.character(twgd_data01$LEVEL3_COD)==results$one_area[i]])
# # }
# # write.csv(twgd_data_bees, file="files_for_maps/sp_rich_table_bees_twgd.csv", row.names=F)
# 
# twgd_data_bees <- merge(twgd_data01, results, by="LEVEL3_COD")
# twgd_data_bees <- twgd_data_bees[,c(1,8:12)]
# #twgd_data_bees <- subset(twgd_data_bees, twgd_data_bees$LEVEL3_NAM.x!="Greenland")
# 
# #sf::st_write(twgd_data_bees, "bee_sprich.shp")
# #save(twgd_data_bees, file="bee_sprich.Rsave")
# load("bee_sprich.Rsave")

bee_sp_rich <- maptools::readShapeSpatial("files_for_maps/bee_rich/America_beerich.shp") # shapefile sent by Alice Hughes
twgd_data_bees <- sf::st_as_sf(bee_sp_rich)

# df_bees <- as.data.frame(twgd_data_bees)
# df_bees <- df_bees[,c("LEVEL3_COD","Total2",
#                       "lon","lat","area_name")]
# write.csv(df_bees, file="files_for_maps/sp_rich_bee.csv", row.names = F)

# Bee plot
tmp_map_bees <- ggplot(data = twgd_data_bees) +
  geom_sf(aes(fill = Total2)) +
  scale_fill_viridis_c(option = "C",alpha=0.9, name="b species") +
  theme_classic() +
  coord_sf(ylim = c(-60, 90), xlim = c(-170, 0), expand = FALSE)


template <- readRDS("data/template.map.Rdata")
all_rasters <- list()
for(i in 1:nrow(bee_sp_rich)){
  one_raster <- template
  one_raster[] <- bee_sp_rich$Total2[i]
  r2 <- crop(one_raster, extent( bee_sp_rich[i,]))
  r3 <- mask(r2,  bee_sp_rich[i,])
  r3 <- raster::resample(r3, template)
  r3[is.na(r3)] <- 0
  all_rasters[[i]] <- raster::mask(r3, template)
  cat(i, "\r")
}
sp_rich_plot <- raster::calc(raster::stack(all_rasters), sum)
writeRaster(sp_rich_plot, file="layers/bee_rich.tif",overwrite=TRUE)


#-----------------------------
#-----------------------------
# Proportional map (bee:angiosperms)
merged <- subset(twgd_data_plants, twgd_data_plants$LEVEL3_COD%in%twgd_data_bees$LEVEL3_COD)
merged$prop <- NA
for(i in 1:nrow(twgd_data_plants)) {
  one_area <- as.character(twgd_data_plants$LEVEL3_COD[i])
  bee_sp_rich <- twgd_data_bees$n_points[twgd_data_bees$LEVEL3_COD==one_area]
  plant_sp_rich <- twgd_data_plants$sp_rich[twgd_data_plants$LEVEL3_COD==one_area]
  merged$prop[which(merged$LEVEL3_COD==one_area)] <-  bee_sp_rich / plant_sp_rich
}

tmp_map_prop <- ggplot(data = merged) +
  geom_sf(aes(fill = prop)) +
  scale_fill_viridis_c(option = "C",alpha=0.9,  name="b:a specis") +
  theme_classic() +
  coord_sf(ylim = c(-60, 90), xlim = c(-170, 0), expand = FALSE)

#merged$LEVEL3_NAM[which.max(merged$prop)]


#-------------------------------------
# Making a raster of proportion bee:angiosperms for correlation analyses
prop_merged <- merged[,-c(1:8)]
save(prop_merged, file="layers/prop_merged.Rsave")
template <- readRDS("data/template.map.Rdata")
#template <- aggregate(template, fact=6)
all_rasters <- list()
for(i in 1:nrow(prop_merged)){
  one_raster <- template
  one_raster[] <- prop_merged$prop[i]
  r2 <- crop(one_raster, extent( prop_merged[i,]))
  r3 <- mask(r2,  prop_merged[i,])
  r3 <- raster::resample(r3, template)
  r3[is.na(r3)] <- 0
  all_rasters[[i]] <- raster::mask(r3, template)
  cat(i, "\r")
}
sp_rich_plot <- raster::calc(raster::stack(all_rasters), sum)
writeRaster(sp_rich_plot, file="layers/prop_raster.tif",overwrite=TRUE)

# min(prop_merged$prop)
# arranged by proportion of species data deficient or unassessed
pdf("plots/spatial_mismatch.pdf", height=18, width=8)
grid.arrange(tmp_map_plants, 
             tmp_map_bees,
             tmp_map_prop,
             widths = 1,
             heights = c(1, 1, 1),
             nrow=3, ncol=1)
dev.off()





# rm(list=ls())
setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")
source("00_utility.R")

########################################################
# Plots to visually inspect data on community surveys #
########################################################
# Load data on community surveys:
subset_bees <- read.csv("data/community_studies_w_habitat_categories_&_env_vars.csv")
subset_bees$bee <- as.numeric(subset_bees$bee)
coordinates <- subset_bees[,c("latitude","longitude")]
subset_bees <- subset(subset_bees,subset_bees$spatial_analyses=="keep")
subset_bees <- subset(subset_bees, subset_bees$bee!=0)

#-----------------------------
# Botanical countries
path="TWDG/wgsrpd-master/level3/level3.shp"
twgd_data <- maptools::readShapeSpatial(path)
twgd_data01 <- sf::st_as_sf(twgd_data)
twgd_data01 <- subset(twgd_data01 , twgd_data01$LEVEL1_COD%in%c(7,8))

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