# K-Fold Cross Validation 

library(ISLR)
library(boot)
library(glmnet)

set.seed(17)

array_error = rep(0,10)


for (i in 1:10){
  
  glm.fit = glm(mpg ~ poly(horsepower,i) , data = Auto)
  
  array_error[i] = cv.glm(Auto,glm.fit,K=10)$delta[1] 
  
}

array_error
