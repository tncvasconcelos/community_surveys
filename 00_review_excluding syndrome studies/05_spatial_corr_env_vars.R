# rm(list=ls())
setwd("/Users/alinemartins/community_surveys/00_review_excluding syndrome studies")
source("00_utility.R")
library(nlme)

# Load data on community surveys:
subset_bees <- read.csv("data/community_studies_w_habitat_categories_&_env_vars.csv")
subset_bees$bee <- as.numeric(subset_bees$bee)
coordinates <- subset_bees[,c("latitude","longitude")]
subset_bees <- subset(subset_bees,subset_bees$spatial_analyses=="keep")
subset_bees <- subset(subset_bees, data_type != "syndrome") #exclude syndrome only studies
subset_bees <- subset(subset_bees,!is.na(subset_bees$prop_bee_angios))

#-----------------------------------
# Fit the spatial autoregressive model (individual predictors, environmental variables)
predictors <- c("bio1","bio4","bio5","bio6","bio12","bio15","bio16","bio17","prop_bee_angios", "wind.1","srad","et0")
results <- as.data.frame(matrix(ncol=4, nrow=length(predictors))) 
colnames(results) <- c("predictor","p-value","r-squared","slope")
pal <- hcl.colors(length(predictors), palette = "Viridis", alpha = 0.7)
names(pal) <- predictors
pdf("plots/cont_vars_cor_indv_reg.pdf", height=7, width=5)
par(mar=c(2,4,1,1))
par(lwd=.3)
par(mfrow=c(4,2))
for(i in 1:length(predictors)) {
  formula <- as.formula(paste("bee", "~", predictors[i]))
  one_model_cor <- gls( model = formula,data = subset_bees, correlation = corSpher(form = ~ longitude + latitude, nugget = TRUE),method = "ML" )    
  results[i,"predictor"] <- predictors[i]
  results[i,"p-value"] <- round(coef(summary(one_model_cor))[2, "p-value"],3)
  ss_total <- sum((subset_bees$bee - mean(subset_bees$bee))^2)
  ss_residual <- sum(residuals(one_model_cor)^2)
  results[i,"r-squared"] <- round(1 - (ss_residual / ss_total),3)
  slope <- unname(summary(one_model_cor)$coefficients[2])
  results[i,"slope"]  <- ifelse(slope>0,"+","-")  
  #-------------------
  # Plots:
  plot(subset_bees$bee~subset_bees[,predictors[i]], bty="n", pch=21, bg=pal[i], xaxt="n", yaxt="n",xlab="", ylab="", ylim=c(0,1),  cex=1, lwd=0.5)
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
write.csv(results, "spatial_regression_results/individual_cont_vars_summary.csv", row.names=F)


#write.csv(results, "all_results_spatial_regression_indv.csv", row.names = F)