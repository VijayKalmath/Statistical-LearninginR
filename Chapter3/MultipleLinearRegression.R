# Convert R script to RmarkDown ->  Cmd + Shift + K
# Multiple Linear Regression - ISLR Lab Work 

library(MASS)
library(ISLR)
library(tidyverse)
library(ggcorrplot)


# Plotting Correlation Matrix 

corr <- round(cor(Boston), 1)
ggcorrplot(corr, hc.order = TRUE, type = "upper",lab=TRUE)

# Tax and Rad seem to be highly correlated -> 0.9
# dis and Zn seem to be highly correlated as well -> 0.7 

mlr.fit  = lm(medv~lstat + age,data = Boston)

summary(mlr.fit)

par(mfrow=c(2,2));plot(mlr.fit)


# Linear Regression with all terms 

mlr.fit1 = lm(medv~.,data=Boston)

summary(mlr.fit1)

par(mfrow=c(2,2));plot(mlr.fit1)

# Indus, age have huge P values for Beta=0 Hypothesis test, therefore they can be removed from the linear regression.


mlr.fit2 = lm(medv ~ . -indus -age,data=Boston)

summary(mlr.fit2)

par(mfrow=c(2,2));plot(mlr.fit2)



mlr.fit3 = lm(medv ~ . -indus -age -zn, - rad,data=Boston)

summary(mlr.fit3)

par(mfrow=c(2,2));plot(mlr.fit3)

# Since Residuals plot has a curve is Adding Non-Linear Transformations 

mlr.fit4 = lm(medv ~ . + I(lstat^3) -indus -age -zn, - rad ,data=Boston)

summary(mlr.fit4)

par(mfrow=c(2,2));plot(mlr.fit4)

# Using Poly 

mlr.fit5 = lm(medv ~ . + poly(lstat,5) -indus -age -zn, - rad ,data=Boston)

summary(mlr.fit5)

par(mfrow=c(2,2));plot(mlr.fit4)
