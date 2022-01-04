# Validation and Cross Validation  Approach

# We must use only the training examples to perform all aspects of the model-fitting including the variable selection.

# If the full data set is used to perform the best subset selection step , the validation set errors and cross-validation errors that we obtain will not be accurate estimates of the test error.


# set seed for reproducibility 
set.seed(1)

library(ISLR)
library(leaps)

Hitters = na.omit(Hitters)


train = sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)

test = (!train)

regfit.best = regsubsets(Salary~.,data=Hitters[train,],nvmax = 19 )

summary(regfit.best)


test.mat = model.matrix(Salary~.,data=Hitters[test,])

val.error  = rep(NA,19)

for (i in 1:19){
  coefi  = coef(regfit.best,id=i)
  
  pred <- test.mat[, names(coefi)] %*% coefi

  val.error[i]= mean((Hitters$Salary[test]-pred)^2)
}

val.error

# min value of val.error 

min(val.error)

# Associated number of variables count 

which.min(val.error)

# Get Coefficients of Best Validation error Model
coef(regfit.best,id=which.min(val.error))

# Cross Validation Approach 

###
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}



# Define number of K-Fold Cross Validation
k <- 10

n <- nrow(Hitters)

folds <- sample(rep(1:k, length = n))

cv.errors <- matrix(NA, k, 19,
                    dimnames = list(NULL, paste(1:19)))
for (j in 1:k) {
  best.fit <- regsubsets(Salary ~ .,
                         data = Hitters[folds != j, ],
                         nvmax = 19)
  for (i in 1:19) {
    pred <- predict(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] <-
      mean((Hitters$Salary[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)

mean.cv.errors

which.min(mean.cv.errors)

par(mfrow = c(1, 1))

plot(mean.cv.errors, type = "b") + points(which.min(mean.cv.errors),min(mean.cv.errors),col="red",pch=20,cex=1.75)

reg.best <- regsubsets(Salary ~ ., data = Hitters,
                       nvmax = 19)

coef(reg.best, which.min(mean.cv.errors))
