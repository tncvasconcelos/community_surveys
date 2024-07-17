# rm(list=ls())
setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")
source("00_utility.R")
library(nlme)
library(gridExtra)
library(ggplot2)

# Load data on community surveys:
subset_bees <- read.csv("data/community_studies_w_habitat_categories_&_env_vars.csv")
subset_bees$bee <- as.numeric(subset_bees$bee)
coordinates <- subset_bees[,c("latitude","longitude")]
subset_bees <- subset(subset_bees,subset_bees$spatial_analyses=="keep")
subset_biome_comparison <- subset(subset_bees, subset_bees$biome %in% names(table(subset_bees$biome))[which(table(subset_bees$biome) > 2)])

#-----------------------------------
# Fit the spatial autoregressive model (interaction between predictor and prop bee flower)
# DISCRETE VARS
predictors_discrete <- c("bin_biome","super_biome","tropical","biome") # discrete
results_discrete <- as.data.frame(matrix(ncol=4, nrow=length(predictors_discrete))) 
colnames(results_discrete) <- c("predictor","p-value","r-squared","slope")
plot_list <- list()
for(i in 1:length(predictors_discrete)) {
  formula <- as.formula(paste("bee", "~", predictors_discrete[i],"* prop_bee_angios"))
  if(predictors_discrete[i]=="biome") {
    one_model_cor <- gls(model = formula, data = subset_biome_comparison,correlation = corSpher(form = ~ longitude + latitude, nugget = TRUE),method = "ML")
  } else {
    one_model_cor <- gls(model = formula,data = subset_bees,correlation = corSpher(form = ~ longitude + latitude, nugget = TRUE), method = "ML")    
  }
  coefs <- as.data.frame(coef(summary(one_model_cor)))
  coefs$`p-value` <- round(coefs$`p-value`,3)
  write.csv(coefs, paste0("spatial_regression_results/",predictors_discrete[i],".csv"))

  # Create a grid of z values for prediction
  z_seq <- seq(min(subset_bees$prop_bee_angios), max(subset_bees$prop_bee_angios), length.out = 100)
  discrete_levels <- levels(as.factor(subset_bees[,predictors_discrete[i]]))
  prediction_grid <- expand.grid(prop_bee_angios = z_seq, discrete_var = discrete_levels)
  colnames(prediction_grid)[2] <- predictors_discrete[i]
  
  # Predict x values for the grid
  prediction_grid$x_pred <- predict(one_model_cor, newdata = prediction_grid)
  
  # Plot the original data and the interaction effect
  plot_list[[i]] <- ggplot(subset_biome_comparison, aes_string(x = "prop_bee_angios", y = "bee", color = predictors_discrete[i])) +
    geom_point() +
    geom_line(data = prediction_grid, aes(y = x_pred)) +
    labs(title = "",
         x = "number of bee per angiosperm",
         y = "proportion of bee flowers",
         color = paste0(predictors_discrete[i])) +
    theme_minimal()
}

pdf("plots/discrete_interactions.pdf", height=15, width=6)
grid.arrange(plot_list[[1]], 
             plot_list[[2]],
             plot_list[[3]],
             plot_list[[4]],
             widths = c(1),
             heights= c(1,1,1,1),
             nrow=4, ncol=1)
dev.off()
