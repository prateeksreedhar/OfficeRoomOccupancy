#Author: Prateek Sreedhar Bharadwaj
#Date: Nov 10, 2016
#Purpose: Logistic Regression with two predictors

rm(list = ls())
library(boot)
library(e1071)
library(caret)
x <- read.table("datatraining.csv", header = TRUE, sep = ",")
y <- read.table("datatest.csv", header = TRUE, sep = ",")
View(x)
View(y)
attach(x)
#logistic regression with temperature and humidity as predictors
temp.humid <- glm(Occupancy ~ Temperature+Humidity, family = binomial)
summary(temp.humid)
#confusion matrix
pred.pro <- predict(temp.humid, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with temperature and humidity as a predictor
cv.error.10 <- cv.glm(y, temp.humid, K=10)
cv.error.10$delta

#logistic regression with temperature and light as predictors
temp.light <- glm(Occupancy ~ Temperature+Light, family = binomial, control = list(maxit = 500))
summary(temp.light)
#confusion matrix
pred.pro <- predict(temp.light, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with temperature and light as a predictor
cv.error.10 <- cv.glm(y, temp.light, K=10)
cv.error.10$delta

#logistic regression with temperature and CO2 as predictors
temp.carbon <- glm(Occupancy ~ Temperature+CO2, family = binomial)
summary(temp.carbon)
#confusion matrix
pred.pro <- predict(temp.carbon, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with temperature and CO2 as a predictor
cv.error.10 <- cv.glm(y, temp.carbon, K=10)
cv.error.10$delta

#logistic regression with temperature and Humidity Ratio as predictors
temp.humidratio <- glm(Occupancy ~ Temperature+HumidityRatio, family = binomial)
summary(temp.humidratio)
#confusion matrix
pred.pro <- predict(temp.humidratio, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with temperature and humidity ratio as a predictor
cv.error.10 <- cv.glm(y, temp.humidratio, K=10)
cv.error.10$delta

#logistic regression with Humidity and Light as predictors
humid.light <- glm(Occupancy ~ Humidity+Light, family = binomial)
summary(humid.light)
#confusion matrix
pred.pro <- predict(humid.light, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with humidity and light as a predictor
cv.error.10 <- cv.glm(y, humid.light, K=10)
cv.error.10$delta

#logistic regression with Humidity and CO2 as predictors
humid.carbon <- glm(Occupancy ~ Humidity+CO2, family = binomial)
summary(humid.carbon)
#confusion matrix
pred.pro <- predict(humid.carbon, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with humidity and CO2 as a predictor
cv.error.10 <- cv.glm(y, humid.carbon, K=10)
cv.error.10$delta

#logistic regression with Humidity and Humidity Ratio as predictors
humid.humidratio <- glm(Occupancy ~ Humidity+HumidityRatio, family = binomial)
summary(humid.humidratio)
#confusion matrix
pred.pro <- predict(humid.humidratio, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with humidity and humidity ratio as a predictor
cv.error.10 <- cv.glm(y, humid.humidratio, K=10)
cv.error.10$delta

#logistic regression with Light and CO2 as predictors
light.carbon <- glm(Occupancy ~ Light+CO2, family = binomial)
summary(light.carbon)
#confusion matrix
pred.pro <- predict(light.carbon, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with light and CO2 as a predictor
cv.error.10 <- cv.glm(y, light.carbon, K=10)
cv.error.10$delta

#logistic regression with Light and Humidity Ratio as predictors
light.humidratio <- glm(Occupancy ~ Light+HumidityRatio, family = binomial)
summary(light.humidratio)
#confusion matrix
pred.pro <- predict(light.humidratio, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with light and humidity ratio as a predictor
cv.error.10 <- cv.glm(y, light.humidratio, K=10)
cv.error.10$delta

#logistic regression with CO2 and Humidity Ratio as predictors
carbon.humidratio <- glm(Occupancy ~ CO2+HumidityRatio, family = binomial)
summary(carbon.humidratio)
#confusion matrix
pred.pro <- predict(carbon.humidratio, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with CO2 and humidity ratio as a predictor
cv.error.10 <- cv.glm(y, carbon.humidratio, K=10)
cv.error.10$delta