
library(nlme)

#area_bee_diversity0 <- read.csv(file.choose())
area_bee_diversity <- area_bee_diversity0
area_bee_diversity <- subset(area_bee_diversity, area_bee_diversity$Area..km2.!="not included in the paper")
area_bee_diversity <- subset(area_bee_diversity, area_bee_diversity$Area..km2.!="more than one area")
area_bee_diversity$Area..km2. <- as.numeric(area_bee_diversity$Area..km2.)


pdf("area_vs_bee_proportion.pdf",height=6, width=12)
par(mfrow=c(1,2))
plot(area_bee_diversity$Proportion.Bee.Visited.Plants~as.numeric(area_bee_diversity$Area..km2.),
     xlab="Area (Km^2)",ylab="Proportion of bee flowers", pch=19)
#model <- lm(area_bee_diversity$Proportion.Bee.Visited.Plants~as.numeric(area_bee_diversity$Area..km2.))
model <- gls(model = Proportion.Bee.Visited.Plants~Area..km2.,
             data = area_bee_diversity, 
             correlation = corSpher(form = ~ Longitude + Latitude, nugget = TRUE),
             method = "ML" )    
abline(model, col="red")

area_bee_diversity <- subset(area_bee_diversity, area_bee_diversity$Area..km2.<50)
plot(area_bee_diversity$Proportion.Bee.Visited.Plants~as.numeric(area_bee_diversity$Area..km2.),
     xlab="Area (Km^2)",ylab="Proportion of bee flowers", pch=19)
model <- gls(model = Proportion.Bee.Visited.Plants~Area..km2.,
             data = area_bee_diversity, 
             correlation = corSpher(form = ~ Longitude + Latitude, nugget = TRUE),
             method = "ML" )    
summary(model)
abline(model, col="red")

dev.off()
