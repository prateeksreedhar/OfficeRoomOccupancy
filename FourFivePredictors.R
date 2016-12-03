#Author: Prateek Sreedhar Bharadwaj
#Date: Nov 16, 2016
#Purpose: Logistic Regression with four and five predictors

rm(list = ls())
library(boot)
library(e1071)
library(caret)
x <- read.table("datatraining.csv", header = TRUE, sep = ",")
y <- read.table("datatest.csv", header = TRUE, sep = ",")
View(x)
View(y)
attach(x)

#logistic regression with temperature, humidity, light and CO2 as predictors
temp.humid.lig.carbon <- glm(Occupancy ~ Temperature+Humidity+Light+CO2, family = binomial)
summary(temp.humid.lig.carbon)
#confusion matrix
pred.pro <- predict(temp.humid.lig.carbon, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with temperature, humidity, light and CO2 as a predictor
cv.error.10 <- cv.glm(y, temp.humid.lig.carbon, K=10)
cv.error.10$delta

#logistic regression with temperature, humidity, light and humidity ratio as predictors
temp.humid.lig.humidratio <- glm(Occupancy ~ Temperature+Humidity+Light+HumidityRatio, family = binomial)
summary(temp.humid.lig.humidratio)
#confusion matrix
pred.pro <- predict(temp.humid.lig.humidratio, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with temperature, humidity, light and humidity ratio as a predictor
cv.error.10 <- cv.glm(y, temp.humid.lig.humidratio, K=10)
cv.error.10$delta

#logistic regression with temperature, humidity, CO2 and humidity ratio as predictors
temp.humid.carbon.humidratio <- glm(Occupancy ~ Temperature+Humidity+CO2+HumidityRatio, family = binomial)
summary(temp.humid.carbon.humidratio)
#confusion matrix
pred.pro <- predict(temp.humid.carbon.humidratio, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with temperature, humidity, CO2 and humidity ratio as a predictor
cv.error.10 <- cv.glm(y, temp.humid.carbon.humidratio, K=10)
cv.error.10$delta

#logistic regression with temperature, light, CO2 and humidity ratio as predictors
temp.lig.carbon.humidratio <- glm(Occupancy ~ Temperature+Light+CO2+HumidityRatio, family = binomial)
summary(temp.lig.carbon.humidratio)
#confusion matrix
pred.pro <- predict(temp.lig.carbon.humidratio, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with temperature, light, CO2 and humidity ratio as a predictor
cv.error.10 <- cv.glm(y, temp.lig.carbon.humidratio, K=10)
cv.error.10$delta


#logistic regression with humidity, light, CO2 and humidity ratio as predictors
humid.lig.carbon.humidratio <- glm(Occupancy ~ Humidity+Light+CO2+HumidityRatio, family = binomial)
summary(humid.lig.carbon.humidratio)
#confusion matrix
pred.pro <- predict(humid.lig.carbon.humidratio, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with humidity, light, CO2 and humidity ratio as a predictor
cv.error.10 <- cv.glm(y, humid.lig.carbon.humidratio, K=10)
cv.error.10$delta

#logistic regression with temperature, humidity, light, CO2 and humidity ratio as predictors
multi <- glm(Occupancy ~ Temperature+Humidity+Light+CO2+HumidityRatio, family = binomial)
summary(multi)
#confusion matrix
pred.pro <- predict(multi, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
cv.error.10 <- cv.glm(y, multi, K=10)
cv.error.10$delta

#validation set approach
N <- nrow(x)
train <- sample(N, N/2)
glm.fit <- glm(Occupancy ~ Light+CO2, family = binomial, subset = train)
pred.probs <- predict(glm.fit, x[-train,], type = "response")
pred.occ <- rep("0", N/2)
pred.occ[pred.probs > 0.5] <- "1"
error.rate <- mean(pred.occ != x[-train])

