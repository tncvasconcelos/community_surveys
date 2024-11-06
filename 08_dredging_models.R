# Regression analyses 2: Dredging models
# rm(list=ls())
setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")

get.rqrs <- function(organized_table, full_dataset, phy, dep.var="div_rate_eps0.9") {
  vars <- setdiff(colnames(organized_table),c("(Intercept)","df","logLik","AICc","delta","weight"))
  only_var <- organized_table[,vars]
  colnames(full_dataset)[which(colnames(full_dataset)==dep.var)] <- "focal_var"
  rqsrs <- c()
  for(i in 1:nrow(organized_table)) {
    to_include_in_model <- colnames(only_var)[which(only_var[i,] != "")]
    if(length(to_include_in_model)>0) {
      tmp_dataset <- full_dataset[c("focal_var", to_include_in_model,"latitude","longitude")]
      
      full_model <- as.formula(paste("focal_var", "~", paste(to_include_in_model,collapse="+")))
      model <- gls(full_model,data=tmp_dataset,correlation = corSpher(form = ~ longitude + latitude, nugget = TRUE),
                   method = "ML" )  
      fitted_values <- fitted(model)
      observed_values <- tmp_dataset$focal_var
      pseudo_r2 <- cor(fitted_values, observed_values)^2    
      rqsrs[i] <- round(pseudo_r2,3)
    }
    cat(i,"\r")
  }
  organized_table$rsqs <- rqsrs
  return(organized_table)
}

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

#################################################################################################
library(nlme)
library(car)
library(MuMIn)
library(ggplot2)
library(gridExtra)

# Master table
subset_bees <- readRDS("data/community_studies_w_habitat_categories_&_env_vars.Rdata") 
subset_bees$bee <- as.numeric(subset_bees$bee)
coordinates <- subset_bees[,c("latitude","longitude")]
subset_bees <- subset(subset_bees,subset_bees$spatial_analyses=="keep")
subset_bees <- subset(subset_bees,!is.na(subset_bees$prop_bee_angios))

#-----------------------------------
# Building global spatial model to identify correlates of proportion of bee-flowers in a community
model_vars_to_include <- c("bio1","bio4","bio5","bio6","bio12","bio15","bio16","bio17","prop_bee_angios", "wind.1","srad","et0")
full_model <- as.formula(paste("bee", "~", paste(model_vars_to_include,collapse="+")))
full_model <- gls(model = full_model,
                      data = subset_bees, 
                      correlation = corSpher(form = ~ longitude + latitude, nugget = TRUE),
                      method = "ML" )    


# Dredging full model for "best" combinations
  dredge_div <- dredge(full_model)
  save(dredge_div, file = "dredge_model.Rsave")

  dredge_div <- get.rqrs(organized_table=dredge_div, full_dataset=subset_bees, dep.var="bee")
  write.csv(as.data.frame(dredge_div), file = "dredge_model.csv")
  
  
  #----

load("dredge_model.Rsave")
  # only models with a deltaAIC below 4 are included
mod_avg_res <- model.avg(dredge_div, subset = delta < 2)
# summarize the model averaged result to get an estimate of standard error and assess significance
summ_mod_avg_res <- summary(mod_avg_res)
# we treat variables as if they are always present in the model (if not in a model, it is set to 0)
param_table <- as.data.frame(summ_mod_avg_res$coefmat.subset)
varaible_importance <- c(summ_mod_avg_res$sw)
# 
# names(varaible_importance)[which(names(varaible_importance)=="tropical")] <- "tropicaltropical"
# names(varaible_importance)[which(names(varaible_importance)=="bin_biome")] <- "bin_biomeopen"

param_table$sum_of_weight <- varaible_importance[match(rownames(param_table), names(varaible_importance))]
# param_table <- param_table[,c(1,2,5,4)]
# # remove the intercept term
param_table <- param_table[-1,]
# sort the parameters by importance (p-value)
param_table <- param_table[order(param_table[,"Pr(>|z|)"],decreasing = T),]
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
param_table$color_code[which(param_table$`Pr(>|z|)`<=0.05)] <- "red3" #"#d1495b"
#param_table$color_code[which(param_table$sum_of_weight<0.9)] <- #"#8d96a3"

param_table$names <- factor(param_table$names, levels = param_table$names)

p1 <- ggplot(param_table, aes(y=names, x=Estimate, 
                              xmin=Estimate-2*`Adjusted SE`,xmax=Estimate+2*`Adjusted SE`,color=color_code)) + 
  geom_pointrange()+
  theme_bw() +
  xlab("Estimate") +
  ylab("") +
  theme(legend.position = "none")

pdf("plots/sum_of_weight.pdf",height=3.5,width=4)
barplot(rev(param_table$sum_of_weight), col=c(rep("red",3),rep("lightgray",6)))
dev.off()

pdf("plots/globalmodels.pdf" ,height=3.5,width=4)
p1
dev.off()
