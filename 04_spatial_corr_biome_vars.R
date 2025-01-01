# rm(list=ls())
setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")
source("00_utility.R")
library(nlme)
library(vioplot)

# Load data on community surveys:
subset_bees <- read.csv("data/community_studies_w_habitat_categories_&_env_vars.csv")
subset_bees$bee <- as.numeric(subset_bees$bee)
coordinates <- subset_bees[,c("latitude","longitude")]
subset_bees <- subset(subset_bees,subset_bees$spatial_analyses=="keep")

subset_bees <- subset(subset_bees, subset_bees$bee!=0)
#---------------------------------
# INDIVIDUAL CORRELATION ANALYSES
# bee flower proportion vs. biome types
# Plot categorical (boxplots)
n_boxes <- c(5, 2.5, 3, 2.5)
total_boxes <- sum(n_boxes)
proportions <- n_boxes / total_boxes

pdf("plots/habitat_boxplots.pdf",height=3, width=12)
palette_name1 = "Viridis"
par(cex.lab=1) 
par(cex.axis=1) 
layout(matrix(1:4, nrow = 1), widths = proportions)
# for biome, exclude those with just one observation from ploting for main manuscript
subset_biome_comparison <- subset(subset_bees, subset_bees$biome %in% names(table(subset_bees$biome))[which(table(subset_bees$biome) > 1)])
vioplot(subset_biome_comparison$bee ~ subset_biome_comparison$biome,
        xlab = "biome type", ylab = "proportion of bee flowers", ylim=c(0,1),
        col = hcl.colors(length(unique(subset_biome_comparison$biome)), palette = palette_name1, alpha = 0.75),
        names=c("desert \n (n=6)","mediterranean \n (n=5)","montane \n grasslands \n (n=5)","temperate \n conifer forests \n (n=2)",
              "tropical \n dry forest \n (n=4)","tropical \n savannas \n (n=16)","tropical \n rainforest \n (n=11)"),)

mtext("(A)", side = 2, line = 2, at = 1 + 0.05, las = 1, cex = 0.8)
abline(h = 0.5, col = "darkorange", lty = 2, lwd = 2)

vioplot(subset_bees$bee~subset_bees$bin_biome, 
        xlab = "biome type (canopy)", ylab = "proportion of bee flowers", ylim=c(0,1),
        col= hcl.colors(length(unique(subset_bees$bin_biome)), palette = palette_name1, alpha = 0.75),
        names=c("closed (n=19)","open (n=34)"), frame = T)
mtext("(B)", side = 2, line = 2, at = 1+0.05, las = 1, cex = 0.8)
abline(h=0.5, col="darkorange", lty=2, lwd=2)

vioplot(subset_bees$bee~subset_bees$super_biome, 
        xlab = "biome type (super-biome)", ylab = "proportion of bee flowers", ylim=c(0,1),
        col = hcl.colors(length(unique(subset_bees$super_biome)), palette = palette_name1, alpha = 0.75),
        names=c("arid (n=11)","temperate (n=10)","tropical (32)"), frame = T)
mtext("(C)", side = 2, line = 2, at = 1+0.05, las = 1, cex = 0.8)
abline(h=0.5, col="darkorange", lty=2, lwd=2)
vioplot(subset_bees$bee~subset_bees$tropical,
        xlab = "latitudinal zone", ylab = "proportion of bee flowers", ylim=c(0,1),
        col = hcl.colors(length(unique(subset_bees$tropical)), palette = palette_name1, alpha = 0.75),
        names=c("temperate \n (n=16)","tropical \n (n=37)"), frame = T)
mtext("(D)", side = 2, line = 2, at = 1+0.05, las = 1, cex = 0.8)
abline(h=0.5, col="darkorange", lty=2, lwd=2)
dev.off()

#-------- regressions
predictors_discrete <- c("bin_biome","super_biome","tropical","biome") # discrete
results_discrete <- as.data.frame(matrix(ncol=4, nrow=length(predictors_discrete))) 
colnames(results_discrete) <- c("predictor","p-value","r-squared","slope")
for(i in 1:length(predictors_discrete)) {
  formula <- as.formula(paste("bee", "~", predictors_discrete[i]))
  if(predictors_discrete[i]=="biome") {
    one_model_cor <- gls(model = formula, data = subset_biome_comparison,correlation = corSpher(form = ~ longitude + latitude, nugget = TRUE),method = "ML")
  } else {
    one_model_cor <- gls(model = formula,data = subset_bees,correlation = corSpher(form = ~ longitude + latitude, nugget = TRUE), method = "ML")    
  }
  coefs <- as.data.frame(coef(summary(one_model_cor)))
  coefs$`p-value` <- round(coefs$`p-value`,3)
  write.csv(coefs, paste0("spatial_regression_results/individual_",predictors_discrete[i],".csv"))
}
