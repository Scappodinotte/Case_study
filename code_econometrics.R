library(dplyr)
library(boot)
library(ISLR2)
library(leaps)
library(Matrix)
library(glmnet)
library(car)


# Delete all objects in the memory
rm(list=ls())

# Create results folder
setwd("~/Personale/UNINE/Master_Applied_Economics/Sem1/Econometrics/Case_study")

data <- read.csv("fredgraph.csv", sep = ",", header = T)
time <- data$Date
data <- data[, -1]
head(data)

plot(data)
# Outlier for MABSE due to exchange rate intervention 2011

model1 <- lm(CPI ~ ., data = data)
summary(model1)

model2 <- lm(CPI ~ IR, data = data)
summary(model2)
