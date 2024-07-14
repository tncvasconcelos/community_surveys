# rm(list=ls())
setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")
source("00_utility.R")
library(nlme)

# Load data on community surveys:
subset_bees <- read.csv("data/community_studies_w_habitat_categories_&_env_vars.csv")
subset_bees$bee <- as.numeric(subset_bees$bee)
coordinates <- subset_bees[,c("latitude","longitude")]
subset_bees <- subset(subset_bees,subset_bees$spatial_analyses=="keep")

#-----------------------------------------------
# Prep for plot
# subset_bees$pro_bee_cat <- NA
# for(i in 1:nrow(subset_bees)) {
#   if(subset_bees[,"prop_bee_angios"][i] < 0.035) {
#     subset_bees$pro_bee_cat[i] <- "low"
#   }
#   if(subset_bees[,"prop_bee_angios"][i] > 0.035 && subset_bees[,"prop_bee_angios"][i] < 0.045) {
#     subset_bees$pro_bee_cat[i] <- "medium"
#   }
#   if(subset_bees[,"prop_bee_angios"][i] > 0.045) {
#     subset_bees$pro_bee_cat[i] <- "high"
#   }
# }  

# CONTINUOUS VARS
predictors_cont <- c("temperature","temp_seasonality","precipitation","prec_seasonality","elevation","ai","npp")
results_cont <- as.data.frame(matrix(ncol=4, nrow=length(predictors_cont))) 
colnames(results_cont) <- c("predictor","p-value","r-squared","slope")
pal <- hcl.colors(7, palette = "Viridis", alpha = 0.7)
names(pal) <-  predictors_cont
pdf("plots/cont_vars_cor.pdf", height=7, width=5)
par(mar=c(2,4,1,1))
par(lwd=.3)
par(mfrow=c(4,2))
for(i in 1:length(predictors_cont)) {
  formula <- as.formula(paste("bee", "~", predictors_cont[i],"* pro_bee_cat"))
  one_model_cor <- gls(model = formula,data = subset_bees,correlation = corSpher(form = ~ longitude + latitude, nugget = TRUE),method = "ML")    
  #----------------
  results_cont[i,"predictor"] <- predictors_cont[i]
  results_cont[i,"p-value"] <- round(coef(summary(one_model_cor))[2, "p-value"],3)
  ss_total <- sum((subset_bees$bee - mean(subset_bees$bee))^2)
  ss_residual <- sum(residuals(one_model_cor)^2)
  results_cont[i,"r-squared"] <- round(1 - (ss_residual / ss_total),3)
  slope <- unname(summary(one_model_cor)$coefficients[2])
  results_cont[i,"slope"]  <- ifelse(slope>0,"+","-")  
  #----------------
  # Plots:
  plot(subset_bees$bee~subset_bees[,predictors_cont[i]], bty="n", pch=21, bg=pal[i], xaxt="n", yaxt="n",xlab="", ylab="", ylim=c(0,1),  cex=1, lwd=0.5)
  # adding axes
  par(lwd=.6)
  axis(side=1, tick = TRUE, line = 0, lwd = .4, cex.axis=.6, tcl=NA, mgp=c(0,0.1,0),las=1, cex.lab=.4)
  axis(side=2, tick = TRUE, line = 0, lwd = .4, cex.axis=.6, tcl=NA, mgp=c(2.5,0.2,0),las=1)
  mtext(text=paste0(names(pal[i])), side=1, line=1, cex=0.5)
  par(las=0)
  mtext(text="Proportion of bee flowers", side=2, line=1.5, cex=0.5)
  par(lwd=.5)
  abline(one_model_cor)
  
  #####
  
  # Create a grid of y and z values for prediction
  z_levels <- levels(factor(subset_bees$pro_bee_cat))
  prediction_grid <- expand.grid(temperature = subset_bees[,predictors_cont[i]], prop_bee_angios = subset_bees$bee)
  
  # Predict x values for the grid
  prediction_grid$x_pred <- predict(one_model_cor, newdata = prediction_grid)
  
  # Plot the original data and the interaction effect
  ggplot(subset_bees, aes(x = temperature, y = bee, color = prop_bee_angios)) +
    geom_point() +
    geom_line(data = prediction_grid, aes(y = x_pred), linewidth = 1) +
    labs(title = "Interaction Effect between y and z on x",
         x = "y",
         y = "Predicted x",
         color = "z") +
    theme_minimal()
}


dev.off()
all_results <- rbind(results_cont,results_discrete)
write.csv(all_results, "all_results_spatial_regression_comb.csv", row.names = F)
