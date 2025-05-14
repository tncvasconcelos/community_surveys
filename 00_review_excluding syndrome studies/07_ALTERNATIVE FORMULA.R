library(nlme)
library(scatterplot3d)
setwd("/Users/alinemartins/community_surveys/00_review_excluding syndrome studies")
# CONTINUOUS VARS## alternativee code to run the formula

for (i in seq_along(predictors_cont)) {
  var_name <- predictors_cont[i]
  formula <- as.formula(paste("bee", "~", var_name, "* prop_bee_angios"))
  
  # Filter data for NA values in only the required columns
  model_vars <- c("bee", var_name, "prop_bee_angios", "longitude", "latitude")
  model_data <- subset_bees[complete.cases(subset_bees[, model_vars]), ]
  
  # Fit model
  one_model_cor <- gls(
    model = formula,
    data = model_data,
    correlation = corSpher(form = ~ longitude + latitude, nugget = TRUE),
    method = "ML"
  )
  
  # Save coefficients
  coefs <- as.data.frame(coef(summary(one_model_cor)))
  coefs$`p-value` <- round(coefs$`p-value`, 3)
  write.csv(coefs, paste0("spatial_regression_results/cont_interaction_", var_name, ".csv"))
  
  # ----------------
  # Plot 3D regression
  s3d <- scatterplot3d(
    x = model_data[[var_name]],
    y = model_data$prop_bee_angios,
    z = model_data$bee,
    pch = 19, type = "h", color = "blue",
    xlab = var_name, ylab = "bee:angio ratio", zlab = "proportion of bee flowers"
  )
  
  # Extract coefficients
  beta <- coef(one_model_cor)
  intercept <- beta[1]
  coef_x <- beta[2]
  coef_z <- beta[3]
  coef_xz <- beta[4]
  
  # Generate prediction plane
  x_vals <- seq(min(model_data[[var_name]], na.rm = TRUE), max(model_data[[var_name]], na.rm = TRUE), length.out = 30)
  z_vals <- seq(min(model_data$prop_bee_angios, na.rm = TRUE), max(model_data$prop_bee_angios, na.rm = TRUE), length.out = 30)
  grid <- expand.grid(x = x_vals, z = z_vals)
  grid$y <- intercept + coef_x * grid$x + coef_z * grid$z + coef_xz * grid$x * grid$z
  
  # Overlay prediction plane
  s3d$points3d(grid$x, grid$z, grid$y, col = "red", pch = ".")
}
