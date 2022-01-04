# Convert R script to RmarkDown ->  Cmd + Shift + K
# Qualitative Linear Regression - ISLR Lab Work 


library(MASS)
library(ISLR)
library(tidyverse)
library(ggcorrplot)

# A data frame with 400 observations on the following 11 variables.

# Predicting Sales 

summary(Carseats)

# Urban,ShelveLoc,US are qualitative 

corr = round(cor(Carseats[c(-7,-10,-11)]),1)
ggcorrplot(corr, hc.order = TRUE, type = "upper",lab=TRUE)

# Linear Regression . 

qlm.fit = lm(Sales ~ .,data = Carseats)

summary(qlm.fit)

par(mfrow=c(2,2));plot(qlm.fit)


contrasts(Carseats$Urban)


contrasts(Carseats$ShelveLoc)


