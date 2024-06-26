# rm(list=ls())
setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")

# Load data on community surveys:
all_surveys <- read.csv("data/community_studies_20Jun2024.csv")
subset_bees <- subset(all_surveys, all_surveys$bee!="")
subset_bees <- subset(subset_bees, !grepl("did", subset_bees$bee))
subset_bees$bee <- as.numeric(subset_bees$bee)

#---------------------------------
# Extracting climate and altitude medians for each community
bio <- raster::getData('worldclim', var='bio', res=2.5) # 19 worldclim vars
alt <- raster::getData('worldclim', var='alt', res=2.5) # altitude
ai <- raster("layers/ai_v3_yr.tif")
npp <- raster("layers/MOD17A3H_Y_NPP_2023-01-01_rgb_720x360.TIFF")

points <- subset_bees[,c("latitude","longitude")]
layers <- c(bio[[1]],bio[[12]], alt, ai, npp)
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
  points <- cbind(points, medians)
  colnames(points)[2+layer_index] <- names(layers)[layer_index]
}
subset_bees <- cbind(subset_bees,points[,3:7])

# if elevation is stated in study uses that; otherwise, takes data from altitude raster
subset_bees$elevation <- unlist(ifelse(is.na(subset_bees$elevation), subset_bees$altitude, all_surveys$elevation))

# Data types:
pdf("plots/survey_type.pdf",height=4,width=4)
pie(table(subset_bees$data_type))
dev.off()

subset_bees_synd <- subset(subset_bees, subset_bees$data_type=="syndrome")
subset_bees_obs <- subset(subset_bees, subset_bees$data_type!="syndrome")

# Some plots:
plot(subset_bees$temperature,subset_bees$bee) # all surveys
model <- lm(subset_bees$bee~subset_bees$temperature)
summary(model)
abline(model)
#
bee_temp <- ggplot(subset_bees, aes(V1, V2, color = V1)) +
  geom_point(shape = 19, size = 1, show.legend = FALSE, alpha=0.75) +
  theme_minimal() +
  scale_color_viridis(option = "B") +
  geom_abline(intercept=0, slope=pic_wo_cherries_misse$correlation$reg$coefficients, color="red",  linetype="dashed")  +
  geom_smooth(se=FALSE, formula=y~x-1, col="red", lwd=0.5) +
  ylim(-2, 2) +
  xlim(0,8.5) +
  xlab("PIC log(plant height) (m)") 

plot(subset_bees_synd$temperature, subset_bees_synd$bee) # syndrome only
model <- lm(subset_bees_synd$bee~subset_bees_synd$temperature)
summary(model)
abline(model)
#
plot(subset_bees_obs$temperature, subset_bees_obs$bee) # observation only
model <- lm(subset_bees_obs$bee~subset_bees_obs$temperature)
summary(model)
abline(model)
#

plot(subset_bees$bee, subset_bees$precipitation)
model <- lm(subset_bees$bee~subset_bees$precipitation)
summary(model)
abline(model)

plot(subset_bees$elevation, subset_bees$bee)
model <- lm(subset_bees$bee~subset_bees$elevation)
summary(model)
abline(model)

plot(subset_bees$npp, subset_bees$bee)
model <- lm(subset_bees$bee~subset_bees$npp)
summary(model)
abline(model)

plot( subset_bees$ai, subset_bees$bee)
model <- lm(subset_bees$bee~subset_bees$ai)
summary(model)
abline(model)


#

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






whittaker_base_plot()
?whittaker_base_plot

biome_colors <- c(
  "Tundra" = "#A6CEE3",          # Light Blue
  "Boreal Forest" = "#1F78B4",   # Dark Green
  "Temperate Deciduous Forest" = "#33A02C", # Forest Green
  "Temperate Grassland" = "#B2DF8A",        # Light Green
  "Desert" = "#FFFF99",          # Yellow
  "Savanna" = "#FDBF6F",         # Light Brown
  "Tropical Rainforest" = "#33A02C", # Dark Green
  "Tropical Seasonal Forest" = "#B15928",   # Olive Green
  "Mediterranean" = "#FF7F00",   # Orange
  "Subtropical Desert" = "#FDBF6F"  # Light Tan
)



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
