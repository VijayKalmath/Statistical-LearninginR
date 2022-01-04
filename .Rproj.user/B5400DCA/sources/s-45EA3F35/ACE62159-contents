# Subset Selection using Forward and Backware Stepwise Selection.

library(ISLR)

names(Hitters)


dim(Hitters)

# Removing NA from Hitters 

Hitters = na.omit(Hitters)

# Dimensions after removing NA.
dim(Hitters)

# the Regsubsets function is part of the leaps library 

library(leaps)

# calling regsubsets  with forward method

regfit.fwd = regsubsets(Salary~.,Hitters,nvmax=19,method="forward")


regfit.fwd.summary = summary(regfit.fwd)

regfit.fwd.summary$which 

regfit.fwd.summary$rss


plot(regfit.fwd.summary$rss, xlab = "Number of Variables used",ylab="RSS") + 
points(which.min(regfit.fwd.summary$rss),min(regfit.fwd.summary$rss),col="red",cex=1.5,pch=15)


plot(regfit.fwd.summary$adjr2, xlab = "Number of Variables used",ylab="Adjusted RSq") + 
points(which.max(regfit.fwd.summary$adjr2),max(regfit.fwd.summary$adjr2),col="red",cex=1.5,pch=15)


plot(regfit.fwd.summary$cp, xlab = "Number of Variables used",ylab="Cp") + 
points(which.min(regfit.fwd.summary$cp),min(regfit.fwd.summary$cp),col="red",cex=1.5,pch=15)

plot(regfit.fwd.summary$bic, xlab = "Number of Variables used",ylab="Cp") + points(which.min(regfit.fwd.summary$bic),min(regfit.fwd.summary$bic),col="red",cex=1.5,pch=15)


# Backward Selection 



regfit.bckwd = regsubsets(Salary~.,Hitters,nvmax=19,method="backward")


regfit.bckwd.summary = summary(regfit.bckwd)

regfit.bckwd.summary$which 

regfit.bckwd.summary$rss


plot(regfit.bckwd.summary$rss, xlab = "Number of Variables used",ylab="RSS") + 
  points(which.min(regfit.bckwd.summary$rss),min(regfit.bckwd.summary$rss),col="red",cex=1.5,pch=15)


plot(regfit.bckwd.summary$adjr2, xlab = "Number of Variables used",ylab="Adjusted RSq") + 
  points(which.max(regfit.bckwd.summary$adjr2),max(regfit.bckwd.summary$adjr2),col="red",cex=1.5,pch=15)


plot(regfit.bckwd.summary$cp, xlab = "Number of Variables used",ylab="Cp") + 
  points(which.min(regfit.bckwd.summary$cp),min(regfit.bckwd.summary$cp),col="red",cex=1.5,pch=15)

plot(regfit.bckwd.summary$bic, xlab = "Number of Variables used",ylab="Cp") + 
points(which.min(regfit.bckwd.summary$bic),min(regfit.bckwd.summary$bic),col="red",cex=1.5,pch=15)
