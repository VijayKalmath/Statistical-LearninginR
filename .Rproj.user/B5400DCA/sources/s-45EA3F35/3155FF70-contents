# Ridge Regression and Lasso Regression 

# GLMNET package is used for Lasso and Ridge Regression 

# set seed for reproducibility 
set.seed(1)

library(ISLR)  # For the data 
library(glmnet)

# Omitting NA
Hitters = na.omit(Hitters)

# Model Matrix produces matrix with 19 predictors but also transforms any qualitative variables into dummy variables

x = model.matrix(Salary~.,data=Hitters)[,-1]

y = Hitters$Salary

# Creating Exponential space gridspace for lambda or regularization factor  
grid= 10^seq(10,-2,length=100)

# Define Train and test set 
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)

# Alpha = 0 -> Ridge Regression , Alpha = 1 -> Lasso Regression 
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
                    lambda = grid)

lasso.pred <- predict(lasso.mod, s = 4, newx = x[test, ])

plot(lasso.pred)

set.seed(1)

# Gridsearch for best lambda
cv.out  = cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)

bestlam = cv.out$lambda.min

lasso.pred = predict(lasso.mod,s=bestlam,newx = x[test,])

mean((lasso.pred = y[test])^2)

out  = glmnet(x,y,alpha=1,lambda = grid)

lasso.coef = predict(out,type="coefficients",s=bestlam)[1:20,]

lasso.coef

lasso.coef[lasso.coef != 0 ]
