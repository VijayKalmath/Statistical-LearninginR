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

# Alpha = 0 -> Ridge Regression , Alpha = 1 -> Lasso Regression 

ridge.mod = glmnet(x,y,alpha=0,lambda = grid)

# 100 Models with corresponding Coefficients 
dim(coef(ridge.mod))


# Define Train and test set 
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)


ridge.mod <- glmnet(x[train, ], y[train], alpha = 0,
                    lambda = grid, thresh = 1e-12)

ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])

mean((ridge.pred - y[test])^2)

# With a single dimensional predictor , intercept only , we get the MSE of the constant mean predictor to be .
mean((mean(y[train]) - y[test])^2)


# By making the lambda value very high , we can force the all the coeff to become closed to 0. 

ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test, ])

mean((ridge.pred - y[test])^2)

ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ],
                      exact = T, x = x[train, ], y = y[train])

mean((ridge.pred - y[test])^2)

lm(y ~ x, subset = train)

predict(ridge.mod, s = 0, exact = T, type = "coefficients",
        x = x[train, ], y = y[train])[1:20, ]


cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)

bestlam <- cv.out$lambda.min

bestlam

ridge.pred <- predict(ridge.mod, s = bestlam,
                      newx = x[test, ])

mean((ridge.pred - y[test])^2)

out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20, ]
