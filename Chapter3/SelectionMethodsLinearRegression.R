# Selection for Multiple Linear Regresion

library(MASS)
library(ISLR)
library(tidyverse)

Boston$lstat_pow5 = Boston$lstat^5
Boston$lstat_pow4 = Boston$lstat^4
Boston$lstat_pow3 = Boston$lstat^3
Boston$lstat_pow2 = Boston$lstat^2

mlr.fit  = lm(medv ~ .,data = Boston)

summary(mlr.fit)

par(mfrow=c(2,2));plot(mlr.fit)


# Forward Stepwise regression model
step.model <- stepAIC(mlr.fit , direction = "forward", 
                      trace = FALSE)
summary(step.model)


par(mfrow=c(2,2));plot(step.model)


# Backward Stepwise regression model
step.model <- stepAIC(mlr.fit , direction = "backward", 
                      trace = FALSE)
summary(step.model)

par(mfrow=c(2,2));plot(step.model)




