# rm(list=ls())
setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")
source("00_utility.R")
library(nlme)

# Load data on community surveys:
subset_bees <- read.csv("data/community_studies_w_habitat_categories_&_env_vars.csv")
subset_bees$bee <- as.numeric(subset_bees$bee)
coordinates <- subset_bees[,c("latitude","longitude")]
subset_bees <- subset(subset_bees,subset_bees$spatial_analyses=="keep")

#---------------------------------
# INDIVIDUAL CORRELATION ANALYSES
# bee flower proportion vs. biome types
# Plot categorical (boxplots)
n_boxes <- c(5, 2.5, 3, 2.5)
total_boxes <- sum(n_boxes)
proportions <- n_boxes / total_boxes

pdf("plots/habitat_boxplots.pdf",height=4, width=17)
palette_name1 = "Viridis"
layout(matrix(1:4, nrow = 1), widths = proportions)
# for biome, exclude those with just one observation from ploting for main manuscript
subset_biome_comparison <- subset(subset_bees, subset_bees$biome %in% names(table(subset_bees$biome))[which(table(subset_bees$biome) > 2)])
#par(mar = c(6, 4, 4, 2) + 0.1)
boxplot(subset_biome_comparison$bee~subset_biome_comparison$biome, 
        xlab = "biome type", ylab = "proportion of bee flowers",
        col= hcl.colors(length(unique(subset_biome_comparison$biome)), palette = palette_name1, alpha = 0.75),
        names=c("desert \n (n=6)","mediterranean \n (n=5)","montane \n grasslands \n (n=5)",
                "tropical \n dry forest \n (n=4)","tropical \n savannas \n (n=16)","tropical \n rainforest \n (n=11)"), frame = T)
mtext("(A)", side = 2, line = 2, at = max(subset_biome_comparison$bee)+0.05, las = 1, cex = 1)
abline(h=0.5, col="darkorange", lty=2, lwd=2)
boxplot(subset_bees$bee~subset_bees$bin_biome, 
        xlab = "biome type (canopy)", ylab = "proportion of bee flowers",
        col= hcl.colors(length(unique(subset_bees$bin_biome)), palette = palette_name1, alpha = 0.75),
        names=c("closed (n=19)","open (n=34)"), frame = T)
mtext("(B)", side = 2, line = 2, at = max(subset_bees$bee)+0.05, las = 1, cex = 1)
abline(h=0.5, col="darkorange", lty=2, lwd=2)
boxplot(subset_bees$bee~subset_bees$super_biome, 
        xlab = "biome type (super-biome)", ylab = "proportion of bee flowers",
        col = hcl.colors(length(unique(subset_bees$super_biome)), palette = palette_name1, alpha = 0.75),
        names=c("arid (n=11)","temperate (n=10)","tropical (32)"), frame = T)
mtext("(C)", side = 2, line = 2, at = max(subset_bees$bee)+0.05, las = 1, cex = 1)
abline(h=0.5, col="darkorange", lty=2, lwd=2)
boxplot(subset_bees$bee~subset_bees$tropical,
        xlab = "latitudinal zone", ylab = "proportion of bee flowers",
        col = hcl.colors(length(unique(subset_bees$tropical)), palette = palette_name1, alpha = 0.75),
        names=c("temperate (n=16)","tropical (n=37)"), frame = T)
mtext("(D)", side = 2, line = 2, at = max(subset_bees$bee)+0.05, las = 1, cex = 1)
abline(h=0.5, col="darkorange", lty=2, lwd=2)
dev.off()

#-----------------------------------
# Fit the spatial autoregressive model (individual predictors)
predictors <- c("temperature","temp_seasonality","precipitation","prec_seasonality","elevation","ai","npp","prop_bee_angios", "bin_biome","super_biome","tropical","biome") 
results <- as.data.frame(matrix(ncol=4, nrow=length(predictors))) 
colnames(results) <- c("predictor","p-value","r-squared","slope")
for(i in 1:length(predictors)) {
  formula <- as.formula(paste("bee", "~", predictors[i]))
  if(predictors[i]=="biome") {
    one_model_cor <- gls(model = formula, data = subset_biome_comparison,correlation = corSpher(form = ~ longitude + latitude, nugget = TRUE), method = "ML")
  } else {
    one_model_cor <- gls( model = formula,data = subset_bees, correlation = corSpher(form = ~ longitude + latitude, nugget = TRUE),method = "ML" )    
  }
  results[i,"predictor"] <- predictors[i]
  results[i,"p-value"] <- round(coef(summary(one_model_cor))[2, "p-value"],3)
  ss_total <- sum((subset_bees$bee - mean(subset_bees$bee))^2)
  ss_residual <- sum(residuals(one_model_cor)^2)
  results[i,"r-squared"] <- round(1 - (ss_residual / ss_total),3)
  slope <- unname(summary(one_model_cor)$coefficients[2])
  results[i,"slope"]  <- ifelse(slope>0,"+","-")  
}

#-----------------------------------
# Fit the spatial autoregressive model (interaction between predictor and prop bee flower)
# DISCRETE VARS
predictors_discrete <- c("bin_biome","super_biome","tropical","biome") # discrete
results_discrete <- as.data.frame(matrix(ncol=4, nrow=length(predictors_discrete))) 
colnames(results_discrete) <- c("predictor","p-value","r-squared","slope")
for(i in 1:length(predictors_discrete)) {
  formula <- as.formula(paste("bee", "~", predictors_discrete[i],"* prop_bee_angios"))
  if(predictors_discrete[i]=="biome") {
    one_model_cor <- gls(model = formula, data = subset_biome_comparison,correlation = corSpher(form = ~ longitude + latitude, nugget = TRUE),method = "ML")
  } else {
    one_model_cor <- gls(model = formula,data = subset_bees,correlation = corSpher(form = ~ longitude + latitude, nugget = TRUE), method = "ML")    
  }
  results_discrete[i,"predictor"] <- predictors_discrete[i]
  results_discrete[i,"p-value"] <- round(coef(summary(one_model_cor))[2, "p-value"],3)
  ss_total <- sum((subset_bees$bee - mean(subset_bees$bee))^2)
  ss_residual <- sum(residuals(one_model_cor)^2)
  results_discrete[i,"r-squared"] <- round(1 - (ss_residual / ss_total),3)
  slope <- unname(summary(one_model_cor)$coefficients[2])
  results_discrete[i,"slope"]  <- ifelse(slope>0,"+","-")  
}

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
  formula <- as.formula(paste("bee", "~", predictors_cont[i],"* prop_bee_angios"))
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
}
dev.off()
all_results <- rbind(results_cont,results_discrete)
write.csv(all_results, "all_results_spatial_regression.csv", row.names = F)
