# Convert R script to RmarkDown ->  Cmd + Shift + K
# Simple Linear Regression - ISLR Lab Work 

library(MASS)
library(ISLR)
library(tidyverse)
library(ggcorrplot)


# Boston Data - Housing Data in suburbs of Boston - 500 Observations and 14 Variables 

colnames(Boston)

str(Boston)
# All are Numerical Values.

# medv is the Y - Variable that we need to fit the rest of the variables to. 

plot(medv~lstat,Boston)

plot(medv~chas,Boston)

# Plotting Correlation Matrix 

corr <- round(cor(Boston), 1)
ggcorrplot(corr, hc.order = TRUE, type = "upper",lab=TRUE)

# Tax and Rad seem to be highly correlated -> 0.9
# dis and Zn seem to be highly correlated as well -> 0.7 

lm.fit  = lm(medv~lstat,data = Boston)
lm.fit 

# Summary of lm.fit 

summary(lm.fit)

# Parts in lm.fit

names(lm.fit)

# Coeefficient of lm.fit
coef(lm.fit)

# Confidence Interval 
confint(lm.fit)

# Predict function 
predict(lm.fit,data.frame(lstat=c(1)))

plot(Boston$medv,Boston$lstat)
abline(lm.fit,col="red",lwd=3)


par(mfrow=c(2,2))
plot(lm.fit)

