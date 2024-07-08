# Regression analyses 2: Dredging models
# rm(list=ls())
setwd("/Users/tvasc/Desktop/plant_pollinator_interactions")

#################################################################################################
library(MuMIn)
library(ggplot2)
library(gridExtra)

# Functions to get r squares and to organize results

get.rqrs <- function(organized_table, full_dataset, phy, dep.var="div_rate_eps0.9") {
  vars <- setdiff(colnames(organized_table),c("(Intercept)","df","logLik","AICc","delta","weight"))
  only_var <- organized_table[,vars]
  colnames(full_dataset)[which(colnames(full_dataset)==dep.var)] <- "focal_var"
  rqsrs <- c()
  for(i in 1:nrow(organized_table)) {
    to_include_in_model <- colnames(only_var)[which(only_var[i,] != "")]
    tmp_dataset <- full_dataset[c("focal_var", to_include_in_model)]
    model <- phylolm(focal_var~.,data=tmp_dataset, phy=tree)
    rqsrs[i] <- round(model$r.squared,3)
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


# Master table
master_table <- read.csv("data/community_studies_w_habitat_categories_&_env_vars.csv") 

############################################

############################################
# Building global model (prop of bee flowers as dependent variable)

master_table <- subset(master_table, master_table$spatial_analyses=="keep")

model_div_full <- gls(bee~ bin_biome+
                            tropical+
                            temperature+ 
                            precipitation+
                            altitude+
                            ai+
                            npp+
                            prop_bee_angios, 
                      data = master_table,
                          correlation = corSpher(form = ~ longitude + latitude, nugget = TRUE),
                      method = "ML")

# Dredging full model for "best" combinations
dredge_div <- dredge(model_div_full)
save(dredge_div, file = "dredge_full_model.Rsave")


#----

load("dredge_full_model.Rsave")
# only models with a deltaAIC below 4 are included
mod_avg_res <- model.avg(dredge_div, subset = delta < 4)
# summarize the model averaged result to get an estimate of standard error and assess significance
summ_mod_avg_res <- summary(mod_avg_res)
# we treat variables as if they are always present in the model (if not in a model, it is set to 0)
param_table <- as.data.frame(summ_mod_avg_res$coefmat.full)
varaible_importance <- c(summ_mod_avg_res$sw)

# names(varaible_importance)[which(names(varaible_importance)=="Most.Common.Life.Form")] <- "Most.Common.Life.Formwoody"
# names(varaible_importance)[which(names(varaible_importance)=="Dry.or.Fleshy.Fruit")] <- "Dry.or.Fleshy.FruitFleshy"
# names(varaible_importance)[which(names(varaible_importance)=="main_habitat")] <- "main_habitatopen"

param_table$sum_of_weight <- varaible_importance[match(rownames(param_table), names(varaible_importance))]
param_table <- param_table[,c(1,2,5,4)]
# remove the intercept term
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
write.csv(param_table, file="results/h2/param_table_div.csv", row.names=T)

param_table$names <- rownames(param_table)
param_table$names[which(param_table$names=="CHELSA_bio10_17")] <- "Precipitation of Driest Quarter (BIO17)"
param_table$names[which(param_table$names=="Most.Common.Life.Formwoody")] <- "Most common life form (woody)"
param_table$names[which(param_table$names=="main_habitatopen")] <- "Main habitat (open canopy)"
param_table$names[which(param_table$names=="CHELSA_bio10_02")] <- "Mean Diurnal Temperature Range (BIO2)"
param_table$names[which(param_table$names=="meanpH")] <- "Mean soil pH"
param_table$names[which(param_table$names=="depthtobedrock2")] <- "Depth to bedrock"
param_table$names[which(param_table$names=="GLOBAL_SLOPE_10MIN")] <- "Global slope (topography)"
param_table$names[which(param_table$names=="meancarbon")] <- "Mean soil carbon"
param_table$names[which(param_table$names=="Dry.or.Fleshy.FruitFleshy")] <- "Fruit type (fleshy)"
param_table$names[which(param_table$names=="meanwatercap")] <- "Mean water capacity"
param_table$names[which(param_table$names=="Mean.Seed.Number.per.Fruit")] <- "Seed number"
param_table$names[which(param_table$names=="Mean.Seed.Length")] <- "Seed length"
param_table$names[which(param_table$names=="CHELSA_bio10_11")] <- "Mean Temperature of Coldest Quarter (BIO11)"
param_table$names[which(param_table$names=="Mean.Corolla.Diameter")] <- "Flower diameter"
param_table$color_code <- NA
param_table$color_code[which(param_table$sum_of_weight>=0.9)] <- "red" #"#d1495b"
#param_table$color_code[which(param_table$sum_of_weight<0.9)] <- #"#8d96a3"

param_table$names <- factor(param_table$names, levels = param_table$names)

p1 <- ggplot(param_table, aes(y=names, x=Estimate, 
                              xmin=Estimate-2*`Std. Error`,xmax=Estimate+2*`Std. Error`,color=color_code)) + 
  geom_pointrange()+
  theme_bw() +
  xlab("Estimate") +
  ylab("") +
  theme(legend.position = "none")

pdf("plots/figure2_sum_of_weight_netdiv.pdf")
barplot(rev(param_table$sum_of_weight), col=c(rep("#d1495b",4),rep("lightgray",10)))
dev.off()

# model.avg(dredge_div, subset = cumsum(weight) <= .95) # get averaged coefficients
# summary(get.models(dredge_div, 1)[[1]])

# pdf(file = "h2-results.pdf", height = 20, width = 20)
# plot(dredge_div)
# dev.off()

#dredge_div <- organize.table(dredge_div, thrsh=F)
dredge_div <- get.rqrs(organized_table=dredge_div, full_dataset=master_table, phy=tree, dep.var="div_rate_eps0.9")
write.csv(dredge_div, file="results/h2/dredged_divrate_rsqr.csv")
# dredge_div[order(dredge_div$rsqs,decreasing = T),]

