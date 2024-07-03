# rm(list=ls())
setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")
source("00_utility.R")

#---------------------------------
# Extracting climate and altitude medians per ecoregion

# Predictors with spatial autocorrelation correction

bio <- raster::getData('worldclim', var='bio', res=2.5) # 19 worldclim vars
alt <- raster::getData('worldclim', var='alt', res=2.5) # altitude
ai <- raster("layers/ai_v3_yr.tif")
npp <- raster("layers/MOD17A3H_Y_NPP_2023-01-01_rgb_720x360.TIFF")
prop_bee_angios <- raster("layers/prop_raster.tif")

# Load data on community surveys:
all_surveys <- read.csv("data/community_studies_20Jun2024.csv")

#---------------------------------
# Subset dataset for surveys with bees (before we standartize dataset?)
subset_bees <- subset(all_surveys, all_surveys$bee!="")
subset_bees <- subset(subset_bees, !grepl("did", subset_bees$bee))
subset_bees$bee <- as.numeric(subset_bees$bee)
coordinates <- subset_bees[,c("latitude","longitude")]

points=coordinates
layers=c(bio[[1]],bio[[12]],alt, ai, npp, prop_bee_angios)
names(layers) <- c("temperature", "precipitation","altitude","ai","npp", "prop_bee_angios")
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
all_surveys <- cbind(all_surveys,coordinates[,3:8])

#---------------------------------
# Scatterplots:
results_div <- matrix(nrow=0, ncol=4)
for(i in names(layers)) {
  one_var <- all_surveys[,i]
  one_model_cor <- lm(all_surveys$bee~one_var)
  r.sq <- round(summary(one_model_cor)$r.squared,3)
  p <- round(unname(summary(one_model_cor)$coefficients[,4][2]),3)
  slope <- unname(summary(one_model_cor)$coefficients[,1][2])
  slope <- ifelse(slope>0,"+","-")
  n_points <- length(one_model_cor$residuals) 
  
  # Plots:
  pdf(paste0("plots/correlation_bee_prop_",i,".pdf"))
  par(mar=c(3,2,1,0.5))
  par(lwd=.3)
  plot(all_surveys$bee~one_var, bty="n", pch=21, bg="grey", xaxt="n", yaxt="n",xlab="", ylab="",  cex=1, lwd=0.5)
  # adding axes
  par(lwd=.6)
  axis(side=1, tick = TRUE, line = 0, lwd = .4, cex.axis=.6, tcl=NA, mgp=c(0,0.1,0),las=1, cex.lab=.4)
  axis(side=2, tick = TRUE, line = 0, lwd = .4, cex.axis=.6, tcl=NA, mgp=c(2.5,0.2,0),las=1)
  
  mtext(text=paste0(i), side=1, line=1, cex=.5)
  par(las=0)
  mtext(text="proportion of species with bee flowers", side=2, line=1.5, cex=.5)
  par(lwd=.5)
  abline(one_model_cor)
  dev.off()
  results_div <- rbind(results_div,c(i, r.sq, p, slope))
}
results_div <- as.data.frame(results_div)
colnames(results_div) <- c("variable","r^2","p","slope")
write.csv(results_div, "results_cor.csv", row.names = F)

