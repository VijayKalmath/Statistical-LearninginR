# Principal Components Regression 

# PCR is part of the pls library

library(pls)
library(ISLR)
# Ridge Regression and Lasso Regression 

# GLMNET package is used for Lasso and Ridge Regression 

# set seed for reproducibility 
set.seed(1)

# Omitting NA
Hitters = na.omit(Hitters)

# Model Matrix produces matrix with 19 predictors but also transforms any qualitative variables into dummy variables
x = model.matrix(Salary~.,data=Hitters)[,-1]

y = Hitters$Salary


# Define Train and test set 
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)

set.seed(2)

pcr.fit = pcr(Salary~.,data=Hitters,scale=TRUE,validation = "CV")

summary(pcr.fit)

validationplot(pcr.fit,val.type="MSEP")


set.seed(1)

pcr.fit = pcr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")

validationplot(pcr.fit,val.type="MSEP")

pcr.pred = predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred - y[test])^2)


pcr.fit= pcr(y~x,scale=TRUE,ncomp=7)


summary(pcr.fit)
