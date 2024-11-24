# ------------------------------------------------------------------------------
# loading libraries
# ------------------------------------------------------------------------------
library(dplyr)
library(boot)
library(ISLR2)
library(leaps)
library(Matrix)
library(glmnet)
library(car)
library(leaps)

# ------------------------------------------------------------------------------
# Git hub
# ------------------------------------------------------------------------------
library(usethis)
use_git()
# use_github()

# ------------------------------------------------------------------------------
# Setting workspace
# ------------------------------------------------------------------------------
# Delete all objects in the memory
rm(list=ls())

# Create results folder
setwd("~/Personale/UNINE/Master_Applied_Economics/Sem1/Econometrics/Case_study")

# ------------------------------------------------------------------------------
# Importing data
# ------------------------------------------------------------------------------
data <- read.csv("fredgraph.csv", sep = ",", header = T)
time <- data$Date
data <- data[, -1]
head(data)

# check na in data
sum(is.na(data))
# No NA
# ------------------------------------------------------------------------------
# Correlation problem
# ------------------------------------------------------------------------------
plot(data)
# Outlier for MABSE due to exchange rate intervention 2011

# ------------------------------------------------------------------------------
# Pooled model
# ------------------------------------------------------------------------------
model1 <- lm(CPI ~ ., data = data)
summary(model1)

# ------------------------------------------------------------------------------
# model selection
# ------------------------------------------------------------------------------
set.seed(5)
train <- sample(c(TRUE , FALSE), nrow(data), replace = TRUE)
test <- (!train)

# Find the best 6 models javing from 1 to 6 regressors
regfit_best <- regsubsets(CPI ~ ., data = data[train, ])
summary(regfit_best)

# create the X matrix 
test_matrix <- model.matrix(CPI ~ ., data = data[test, ])
View(test_matrix)
num_regr <- dim(test_matrix)[2] - 1

# create a data frame to store MSE
MSE <- rep(NA, times = num_regr)
for (i in 1:num_regr) {
  beta <- coef(regfit_best, id = i)
  pred <- test_matrix[, names(beta)] %*% beta
  MSE[i] <- mean((data$CPI[test] - pred)^2)
}
MSE
best_model <- which.min(MSE)
coef(regfit_best, id = best_model)
# Best model has IR (coef: 0.0322), OIL (coef: 0.00138), GDP (coef: 0.0619) and WUICHE (coef: 0.228)


