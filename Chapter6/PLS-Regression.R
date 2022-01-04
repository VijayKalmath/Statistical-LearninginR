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


pls.fit = plsr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")

summary(pls.fit)

validationplot(pls.fit,val.type = "MSEP")

pls.pred = predict(pls.fit,x[test,],ncomp = 2)


mean((pls.pred - y[test])^2)

pls.fit = plsr(Salary~.,data=Hitters,scale=TRUE,ncomp=2)

summary(pls.fit)