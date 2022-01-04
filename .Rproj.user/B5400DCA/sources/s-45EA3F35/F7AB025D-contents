# Leave One Out Cross Validation 

# cv.glm does LOOCV  and is part of boot library

library(boot)
library(ISLR)

glm.fit = glm(mpg~horsepower,data=Auto)

summary(glm.fit)

# If K value is not given , cv.glm defaults to LOOOCV
cv.err = cv.glm(Auto,glm.fit)

cv.err$delta

cv_array = rep(0,5)

for (i in 1:10){
  glm.fit = glm(mpg~poly(horsepower,i),data = Auto)
  cv_array[i] = cv.glm(Auto,glm.fit)$delta[1]
}

cv_array

# We can see that apart from a sharp drop of Error from linear to quadratic model, there are no significant losses. Indicating that quadratic is the best model.


