# K Nearest Neighbors for Smarket Data 


library(class)
library(ISLR)

subset_condition  = (Smarket$Year < 2005 )

Smarket.2005  = Smarket[!subset_condition,]

train.X = cbind(Smarket$Lag1,Smarket$Lag2)[subset_condition,]

test.X = cbind(Smarket$Lag1,Smarket$Lag2)[!subset_condition,]

train.Direction = Smarket$Direction[subset_condition]

set.seed(1)

knn.pred = knn(train.X,test.X,train.Direction,k=1)

table(knn.pred,Smarket.2005$Direction)


mean(knn.pred == Smarket.2005$Direction)


# KNN with different values 

knn_3.pred = knn(train.X,test.X,train.Direction,k=3)
mean(knn_3.pred == Smarket.2005$Direction)

knn_12.pred = knn(train.X,test.X,train.Direction,k=12)
mean(knn_12.pred == Smarket.2005$Direction)

knn_21.pred = knn(train.X,test.X,train.Direction,k=21)
mean(knn_21.pred == Smarket.2005$Direction)



