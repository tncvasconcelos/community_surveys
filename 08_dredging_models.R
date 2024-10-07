# Regression analyses 2: Dredging models
# rm(list=ls())
setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")

#################################################################################################
library(nlme)
library(car)
library(MuMIn)
library(ggplot2)
library(gridExtra)

# Functions to get r squares and to organize results


organize.table <- function(table_results, thrsh=T) {
  if(thrsh) {
    subset_best_fit <- subset(table_results, table_results$delta < 2)
  } else {
    subset_best_fit <- table_results
  }
  subset_best_fit <- subset_best_fit[,-1]
  vars <- setdiff(colnames(subset_best_fit),c("df","logLik","AICc","delta","weight"))
  only_var <- subset_best_fit[,vars]
  stats <- subset_best_fit[,c("df","logLik","AICc","delta","weight")]
  for(i in 1:nrow(only_var)) {
    #only_var[i,][which(!is.na(only_var[i,]))] <- "x"
    only_var[i,][which(is.na(only_var[i,]))] <- ""
    
    cat(i,"\r")
  }
  tmp_df <- cbind(only_var, stats)
  tmp_df$logLik <- round(tmp_df$logLik, 1)
  tmp_df$AICc <- round(tmp_df$AICc, 1)
  tmp_df$delta <- round(tmp_df$delta, 2)
  tmp_df$weight <- round(tmp_df$weight, 2)
  rownames(tmp_df) <- paste0("model ", 1:nrow(tmp_df))
  return(tmp_df)
}


# Master table
subset_bees <- readRDS("data/community_studies_w_habitat_categories_&_env_vars.Rdata") 
subset_bees$bee <- as.numeric(subset_bees$bee)
coordinates <- subset_bees[,c("latitude","longitude")]
subset_bees <- subset(subset_bees,subset_bees$spatial_analyses=="keep")
subset_bees <- subset(subset_bees,subset_bees$prop_bee_angios!=0)

#-----------------------------------
# Building global spatial model to identify correlates of proportion of bee-flowers in a community
# 
# # Excluding variables with colinearity problems
# # Selecting traits we would keep in the largest model
# var_to_keep <- c("bio1","bio2","bio3","bio4","bio5","bio6","bio7","bio8","bio9","bio10","bio11","bio12","bio13","bio14","bio15", "bio16","bio17", "bio18","bio19","prop_bee_angios","ai","npp","wind.1","srad","et0")
# subset_bees_test <- subset_bees[,c("bee",var_to_keep)]
# test_cor <- lm(bee~., data=subset_bees_test)
# vif(test_cor) # vif test
# 
# # testing removing vars
# div_model_vars_to_exclude <- names(vif(test_cor))[which(vif(test_cor)>100)]
# div_model_vars_to_include <- names(vif(test_cor))[which(vif(test_cor)<100)]

#div_model_vars_to_include <- c(div_model_vars_to_include, "bin_biome", "tropical", "prop_bee_angios")

div_model_vars_to_include <- c("bio1","bio4","bio5","bio6","bio12","bio15","bio16","bio17","prop_bee_angios", "bin_biome", "tropical", "wind.1","srad","et0")

full_model <- as.formula(paste("bee", "~", paste(div_model_vars_to_include,collapse="+")))

model_div_full <- gls(model = full_model,
                      data = subset_bees, 
                      correlation = corSpher(form = ~ longitude + latitude, nugget = TRUE),
                      method = "ML" )    


# Dredging full model for "best" combinations
  dredge_div <- dredge(model_div_full)
  save(dredge_div, file = "dredge_model.Rsave")


#----

load("dredge_model.Rsave")
# only models with a deltaAIC below 4 are included
mod_avg_res <- model.avg(dredge_div, subset = delta < 2)
# summarize the model averaged result to get an estimate of standard error and assess significance
summ_mod_avg_res <- summary(mod_avg_res)
# we treat variables as if they are always present in the model (if not in a model, it is set to 0)
param_table <- as.data.frame(summ_mod_avg_res$coefmat.subset)
varaible_importance <- c(summ_mod_avg_res$sw)

names(varaible_importance)[which(names(varaible_importance)=="tropical")] <- "tropicaltropical"
names(varaible_importance)[which(names(varaible_importance)=="bin_biome")] <- "bin_biomeopen"

param_table$sum_of_weight <- varaible_importance[match(rownames(param_table), names(varaible_importance))]
# param_table <- param_table[,c(1,2,5,4)]
# # remove the intercept term
param_table <- param_table[-1,]
# sort the parameters by importance (p-value)
param_table <- param_table[order(param_table[,3]),]
#param_table <- param_table[rev(rownames(param_table)),]

# nice table, wow
print(param_table)
param_table$Estimate <- round(param_table$Estimate, 3)
param_table$`Std. Error` <- round(param_table$`Std. Error`, 3)
param_table$sum_of_weight <- round(param_table$sum_of_weight, 3)
param_table$`Pr(>|z|)` <- round(param_table$`Pr(>|z|)`, 3)
write.csv(param_table, file="param_table_div.csv", row.names=T)

param_table$names <- rownames(param_table)
param_table$color_code <- NA
param_table$color_code[which(param_table$sum_of_weight>=0.5)] <- "red3" #"#d1495b"
#param_table$color_code[which(param_table$sum_of_weight<0.9)] <- #"#8d96a3"

param_table$names <- factor(param_table$names, levels = param_table$names)

p1 <- ggplot(param_table, aes(y=names, x=Estimate, 
                              xmin=Estimate-2*`Std. Error`,xmax=Estimate+2*`Std. Error`,color=color_code)) + 
  geom_pointrange()+
  theme_bw() +
  xlab("Estimate") +
  ylab("") +
  theme(legend.position = "none")

pdf("sum_of_weight.pdf")
barplot(rev(param_table$sum_of_weight), col=c("red3",rep("lightgray",11)))
dev.off()

pdf("plots/globalmodels.pdf" ,height=3.5,width=10)
p1
dev.off()
