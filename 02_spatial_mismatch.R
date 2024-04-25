
# rm(list=ls())
library(sp)
library(sf)
library(raster)
library(ggplot2)
library(gridExtra)

setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")
# If local
path="TWDG/wgsrpd-master/level3/level3.shp"
#-----------------------------
twgd_data <- maptools::readShapeSpatial(path)
twgd_data01 <- sf::st_as_sf(twgd_data)

organize.bubble.plot <- function(trait_table, reference_table, all_vars, twgd_data) {
  tmp_reference_table <- subset(reference_table, reference_table$wcvp_name %in% unique(trait_table$species))
  wcvp_subset <- subset(all_vars, all_vars$taxon_name %in% tmp_reference_table$wcvp_name)
  wcvp_subset <- subset(wcvp_subset, wcvp_subset$introduced==0)
  wcvp_subset <- subset(wcvp_subset, wcvp_subset$extinct==0)
  wcvp_subset <- subset(wcvp_subset, wcvp_subset$location_doubtful==0)
  
  focal_areas <- unique(wcvp_subset$area_code_l3)
  results <- matrix(nrow=0, ncol=5)
  for(i in 1:length(focal_areas)) {
    one_area <- focal_areas[i]
    one_subset <- subset(wcvp_subset, wcvp_subset$area_code_l3==one_area)
    sp_rich <- length(unique(one_subset$taxon_name))
    family_rich <- length(unique(one_subset$family))
    area_plus_buffer <- twgd_data[which(as.character(twgd_data$LEVEL3_COD) %in% one_area),]
    if(nrow(area_plus_buffer)>0) {
      centroids <- rgeos::gCentroid(area_plus_buffer, byid=TRUE)
      lon <- extent(centroids)[1]
      lat <- extent(centroids)[3]
      results <- rbind(results, cbind(sp_rich, family_rich, one_area, lon, lat))
    }
    cat(i, "\r")
  }
  results <- as.data.frame(results)
  results$sp_rich <- as.numeric(results$sp_rich)
  results$family_rich <- as.numeric(results$family_rich)
  results$lon <- as.numeric(results$lon)
  results$lat <- as.numeric(results$lat)
  return(results)
}
#########################

organize.bubble.plot2 <- function(points, twgd_data) {
  focal_areas <- as.character(twgd_data$LEVEL3_COD)
  species <- points[,1]
  points <- points[,c(3,2)]
  sp::coordinates(points) <- ~ lon + lat
  results <- matrix(nrow=0, ncol=4)
  list_result1 <- list()
  for(i in 1:length(focal_areas)) {
    #i<-which(focal_areas=="FRG")
    one_area <- focal_areas[i]
    area_plus_buffer <- twgd_data[which(as.character(twgd_data$LEVEL3_COD) %in% one_area),]
    if(nrow(area_plus_buffer)>0) {
      res <- over(points, area_plus_buffer)
      if(any(!is.na(res$LEVEL1_COD))) {
        sp_rich <- unique(species[which(!is.na(res$LEVEL1_COD))])
        list_result1[[i]] <- sp_rich$species
        names(list_result1)[i] <- one_area
        n_points <- length(sp_rich$species)
        #centroids <- rgeos::gCentroid(area_plus_buffer, byid=TRUE)
        coords <- sp::coordinates(area_plus_buffer)
        centroid_x <- mean(coords[, 1])
        centroid_y <- mean(coords[, 2])
        centroids <- sp::SpatialPoints(matrix(c(centroid_x, centroid_y), ncol = 2))
        lon <- raster::extent(centroids)[1]
        lat <- raster::extent(centroids)[3]
        results <- rbind(results, cbind(n_points, one_area, lon, lat))
      }
      cat(i, "\r")
    }
  }
  results <- as.data.frame(results)
  results$n_points <- as.numeric(results$n_points)
  results$lon <- as.numeric(results$lon)
  results$lat <- as.numeric(results$lat)
  save(list_result1, file="list_result1.Rsave")
  return(results)
}

#########################
# Making maps for plants
dist_sample <- read.table("WCVP/wcvp_distribution.txt", sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
names_sample <- read.table("WCVP/wcvp_names.txt", sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
all_vars <- merge(dist_sample, names_sample, by="plant_name_id")

# reference table for taxized names
#-----------------------------
# If local
reference_table <- list.files("WCVP/taxized_reference_tables", full.names = T)
reference_table <- do.call(rbind, lapply(reference_table[grep("csv",reference_table)], read.csv))

#-----------------------------
# Looking at the WCVP table and TDWG to clean GBIF points
#-----------------------------

sp_data <- reference_table
colnames(sp_data) <- c("species","")
organized_table_for_plot_total <- organize.bubble.plot(sp_data, reference_table, all_vars, twgd_data)
# write.csv(organized_table_for_plot_total, "sp_rich_table_plants.csv", row.names = F)

organized_table_for_plot_total <- read.csv("files_for_maps/sp_rich_table_plants.csv")
twgd_data_plants <- merge(twgd_data01, organized_table_for_plot_total, by.x="LEVEL3_COD", by.y="one_area")
twgd_data_plants <- subset(twgd_data_plants , twgd_data_plants$LEVEL1_COD%in%c(7,8))

tmp_map_plants <- ggplot(data = twgd_data_plants) +
  geom_sf(aes(fill = sp_rich)) +
  scale_fill_viridis_c(option = "C", alpha=0.9, trans="log",name="angiosperm species") +
  theme_classic() +
  coord_sf(ylim = c(-60, 90), xlim = c(-170, 0), expand = FALSE)

#-----------------------------
#-----------------------------
# BEES
gbif_bees <- data.table::fread("OutputData/05_cleaned_database.csv")
#load("thinned_points_res1.Rsave")

#points <- thinned_points
points <- gbif_bees[,c("scientificName","decimalLatitude","decimalLongitude")]
colnames(points) <- c("species", "lat","lon")
results <- organize.bubble.plot2(points, twgd_data)
#write.csv(results, file="files_for_maps/sp_rich_table_bees.csv")

results <- read.csv("files_for_maps/sp_rich_table_bees.csv")
twgd_data_bees <- merge(twgd_data01, results, by.x="LEVEL3_COD", by.y="one_area")
twgd_data_bees <- subset(twgd_data_bees , twgd_data_bees$LEVEL1_COD%in%c(7,8))

tmp_map_bees <- ggplot(data = twgd_data_bees) +
  geom_sf(aes(fill = n_points)) +
  scale_fill_viridis_c(option = "C",alpha=0.9, trans="log", name="   bee species    ") +
  theme_classic() +
  coord_sf(ylim = c(-60, 90), xlim = c(-170, 0), expand = FALSE)

#-----------------------------
#-----------------------------
# Proportional map
merged <- subset(twgd_data_plants, twgd_data_plants$LEVEL3_COD%in%twgd_data_bees$LEVEL3_COD)
merged$prop <- NA
for(i in 1:nrow(twgd_data_plants)) {
  one_area <- as.character(twgd_data_plants$LEVEL3_COD[i])
  bee_sp_rich <- twgd_data_bees$n_points[twgd_data_bees$LEVEL3_COD==one_area]
  plant_sp_rich <- twgd_data_plants$sp_rich[twgd_data_plants$LEVEL3_COD==one_area]
  merged$prop[which(merged$LEVEL3_COD==one_area)] <- plant_sp_rich / bee_sp_rich
}

#merged <- subset(merged, merged$LEVEL3_COD!="FRG")

tmp_map_prop <- ggplot(data = merged) +
  geom_sf(aes(fill = prop)) +
  scale_fill_viridis_c(option = "A",alpha=0.9, trans="log", name="angio:bee species ") +
  theme_classic() +
  coord_sf(ylim = c(-60, 90), xlim = c(-170, 0), expand = FALSE)

# arranged by proportion of species data deficient or unassessed
pdf("spatial_mismatch.pdf", height=10, width=30)
grid.arrange(tmp_map_plants, 
             tmp_map_bees,
             tmp_map_prop,
             widths = c(1, 1, 1),
             heights = 1,
             nrow=1, ncol=3)
dev.off()
