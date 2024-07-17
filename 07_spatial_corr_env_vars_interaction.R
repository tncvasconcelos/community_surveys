# rm(list=ls())
setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")
source("00_utility.R")
library(scatterplot3d)
library(nlme)

# Load data on community surveys:
subset_bees <- read.csv("data/community_studies_w_habitat_categories_&_env_vars.csv")
subset_bees$bee <- as.numeric(subset_bees$bee)
coordinates <- subset_bees[,c("latitude","longitude")]
subset_bees <- subset(subset_bees,subset_bees$spatial_analyses=="keep")

# CONTINUOUS VARS
predictors_cont <- c("temperature","temp_seasonality","precipitation","prec_seasonality","elevation","ai","npp")
pdf("plots/cont_vars_cor_int.pdf", height=12, width=8)
par(mar=c(0.5,0.5,0.5,0.5))
par(lwd=.3)
par(mfrow=c(4,2))
for(i in 1:length(predictors_cont)) {
  formula <- as.formula(paste("bee", "~", predictors_cont[i],"* prop_bee_angios"))
  one_model_cor <- gls(model = formula,data = subset_bees,correlation = corSpher(form = ~ longitude + latitude, nugget = TRUE),method = "ML")    
  summary(one_model_cor)
  #----------------
  coefs <- as.data.frame(coef(summary(one_model_cor)))
  coefs$`p-value` <- round(coefs$`p-value`,3)
  write.csv(coefs, paste0("spatial_regression_results/cont_interaction_",predictors_cont[i],".csv"))
  
  #----------------
  # Plots:
  # Create 3D scatter plot
  s3d <- scatterplot3d(subset_bees[,predictors_cont[i]], subset_bees$prop_bee_angios, subset_bees$bee, pch = 19, type = "h", color = "blue", 
                       main = "", xlab = paste0(predictors_cont[i]), ylab = "bee:angio ratio", zlab = "proportion of bee flowers")
  coefs <- coef(one_model_cor)
  intercept <- coefs[1]
  coef_x <- coefs[2]
  coef_z <- coefs[3]
  coef_xz <- coefs[4]
  
  # Generate points for the regression plane
  x_vals <- seq(min(subset_bees[,predictors_cont[i]]), max(subset_bees[,predictors_cont[i]]), length.out = 30)
  z_vals <- seq(min(subset_bees$prop_bee_angios), max(subset_bees$prop_bee_angios), length.out = 30)
  grid <- expand.grid(x = x_vals, z = z_vals)
  grid$y <- intercept + coef_x * grid$x + coef_z * grid$z + coef_xz * grid$x * grid$z
  s3d$points3d(grid$x, grid$z, grid$y, type = "p", col = "red", pch = ".")
  
}
dev.off()
