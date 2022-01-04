library(tidyverse)

### R-Norm Function  is for random generation 
?rnorm

rnorm(10,mean=1,sd=1)

# dnorm(x, mean = 0, sd = 1, log = FALSE) -> Density
# pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) -> distribution function
# qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) -> quantile function
# rnorm(n, mean = 0, sd = 1) -> random deviates

#runif -> random variables

x = rnorm(100,mean = 1,sd = 1 )

y = rnorm(100,mean=1,sd = 1 )

plot(x,y)

mpg

# Summary Function 
summary(mpg)

