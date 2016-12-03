#Author: Prateek Sreedhar Bharadwaj
#Date: Nov 10, 2016
#Purpose: Logistic Regression with one predictor
rm(list = ls())
library(boot)
library(e1071)
library(caret)
x <- read.table("datatraining.csv", header = TRUE, sep = ",")
y <- read.table("datatest.csv", header = TRUE, sep = ",")
View(x)
View(y)
attach(x)
#histogram of the predictors- Exploratory data analysis
hist(Temperature)
hist(Humidity)
hist(Light)
hist(CO2)
hist(HumidityRatio)
#logistic regression with temperature as a predictor
temp <- glm(Occupancy ~ Temperature, family = binomial)
summary(temp)
#confusion matrix
pred.pro <- predict(temp, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with temperature as a predictor
set.seed(1000)
cv.error.10 <- cv.glm(y, temp, K=10)
cv.error.10$delta

#logistic regression with humitidity as a predictor-doubt
humid <- glm(Occupancy ~ Humidity, family = binomial)
summary(humid)
#confusion matrix
pred.pro <- predict(humid, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
u = union(pred.occ, Occupancy)
t = table(factor(pred.occ, u), factor(Occupancy, u))
confusionMatrix(t, positive = "1")
#k fold raw estimate for model with humidity as a predictor
set.seed(1000)
cv.error.10 <- cv.glm(y, humid, K=10)
cv.error.10$delta

#logistic regression with light as a predictor
lig <- glm(Occupancy ~ Light, family = binomial, control = list(maxit = 500))
summary(lig)
#confusion matrix
pred.pro <- predict(lig, type = "response")
pred.occ <- rep("0", nrow())
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with light as a predictor
cv.error.10 <- cv.glm(y, lig, K=10)
cv.error.10$delta

#logistic regression with CO2 as a predictor
carbon <- glm(Occupancy ~ CO2, family = binomial)
summary(carbon)
#confusion matrix
pred.pro <- predict(carbon, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with CO2 as a predictor
cv.error.10 <- cv.glm(y, carbon, K=10)
cv.error.10$delta

#logistic regression with humidity ratio as a predictor
humidratio <- glm(Occupancy ~ HumidityRatio, family = binomial)
summary(humidratio)
#confusion matrix
pred.pro <- predict(humidratio, type = "response")
pred.occ <- rep("0", nrow(y))
pred.occ[pred.pro > 0.5] <- "1"
confusionMatrix(table(pred.occ, Occupancy),positive="1")
#k fold raw estimate for model with humidity ratio as a predictor
cv.error.10 <- cv.glm(y, humidratio, K=10)
cv.error.10$delta