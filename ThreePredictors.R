#Author: Prateek Sreedhar Bharadwaj
#Date: Nov 16, 2016
#Purpose: Logistic Regression with three predictors

rm(list = ls())
library(boot)
library(e1071)
library(caret)
x <- read.table("datatraining.csv", header = TRUE, sep = ",")
y <- read.table("datatest.csv", header = TRUE, sep = ",")
View(x)
View(y)
attach(x)

#logistic regression with temperature, humidity and light as predictors
temp.humid.lig <- glm(Occupancy ~ Temperature+Humidity+Light, family = binomial)
summary(temp.humid.lig)
#confusion matrix
pred.pro <- predict(temp.humid.lig, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with temperature, humidity and light as a predictor
cv.error.10 <- cv.glm(y, temp.humid.lig, K=10)
cv.error.10$delta

#logistic regression with temperature, humidity and CO2 as predictors
temp.humid.carbon <- glm(Occupancy ~ Temperature+Humidity+CO2, family = binomial)
summary(temp.humid.carbon)
#confusion matrix
pred.pro <- predict(temp.humid.carbon, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with temperature, humidity and CO2 as a predictor
cv.error.10 <- cv.glm(y, temp.humid.carbon, K=10)
cv.error.10$delta

#logistic regression with temperature, humidity and humidity ratio as predictors
temp.humid.humidratio <- glm(Occupancy ~ Temperature+Humidity+HumidityRatio, family = binomial)
summary(temp.humid.humidratio)
#confusion matrix
pred.pro <- predict(temp.humid.humidratio, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with temperature, humidity and humidity ratio as a predictor
cv.error.10 <- cv.glm(y, temp.humid.humidratio, K=10)
cv.error.10$delta

#logistic regression with temperature, light and CO2 as predictors
temp.lig.carbon <- glm(Occupancy ~ Temperature+Light+CO2, family = binomial)
summary(temp.lig.carbon)
#confusion matrix
pred.pro <- predict(temp.lig.carbon, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with temperature, light and CO2 as a predictor
cv.error.10 <- cv.glm(y, temp.lig.carbon, K=10)
cv.error.10$delta

#logistic regression with temperature, light and humidity ratio as predictors
temp.lig.humidratio <- glm(Occupancy ~ Temperature+Light+HumidityRatio, family = binomial)
summary(temp.lig.humidratio)
#confusion matrix
pred.pro <- predict(temp.lig.humidratio, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with temperature, light and humidity ratio as a predictor
cv.error.10 <- cv.glm(y, temp.lig.humidratio, K=10)
cv.error.10$delta

#logistic regression with temperature, CO2 and humidity ratio as predictors
temp.carbon.humidratio <- glm(Occupancy ~ Temperature+CO2+HumidityRatio, family = binomial)
summary(temp.carbon.humidratio)
#confusion matrix
pred.pro <- predict(temp.carbon.humidratio, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with temperature, carbon and humidity ratio as a predictor
cv.error.10 <- cv.glm(y, temp.carbon.humidratio, K=10)
cv.error.10$delta

#logistic regression with humidity, light and carbon as predictors
humid.lig.carbon <- glm(Occupancy ~ Humidity+Light+CO2, family = binomial)
summary(humid.lig.carbon)
#confusion matrix
pred.pro <- predict(humid.lig.carbon, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with humidity, light and CO2 as a predictor
cv.error.10 <- cv.glm(y, humid.lig.carbon, K=10)
cv.error.10$delta

#logistic regression with humidity, light and humidity ratio as predictors
humid.lig.humidratio <- glm(Occupancy ~ Humidity+Light+HumidityRatio, family = binomial)
summary(humid.lig.humidratio)
#confusion matrix
pred.pro <- predict(humid.lig.humidratio, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with humidity, light and humidity ratio as a predictor
cv.error.10 <- cv.glm(y, humid.lig.humidratio, K=10)
cv.error.10$delta

#logistic regression with humidity, carbon and humidity ratio as predictors
humid.carbon.humidratio <- glm(Occupancy ~ Humidity+CO2+HumidityRatio, family = binomial)
summary(humid.carbon.humidratio)
#confusion matrix
pred.pro <- predict(humid.carbon.humidratio, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with humidity, CO2 and humidity ratio as a predictor
cv.error.10 <- cv.glm(y, humid.carbon.humidratio, K=10)
cv.error.10$delta

#logistic regression with light, carbon and humidity ratio as predictors
lig.carbon.humidratio <- glm(Occupancy ~ Light+CO2+HumidityRatio, family = binomial)
summary(lig.carbon.humidratio)
#confusion matrix
pred.pro <- predict(lig.carbon.humidratio, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with light, CO2 and humidity ratio as a predictor
cv.error.10 <- cv.glm(y, lig.carbon.humidratio, K=10)
cv.error.10$delta
