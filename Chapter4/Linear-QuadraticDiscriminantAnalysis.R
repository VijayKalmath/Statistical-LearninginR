# Linear Discriminant Analysis 

library(MASS)
library(ISLR)


# Subset Condition = Year < 2005 

subset_condition  = (Smarket$Year < 2005 )

Smarket.2005  = Smarket[!subset_condition,]

lda.fit = lda(Direction~Lag1+Lag2+Lag1:Lag2,data=Smarket,subset= subset_condition )

lda.fit

lda.pred = predict(lda.fit,Smarket.2005)

names(lda.pred)

lda.class = lda.pred$class

table(lda.class,Smarket.2005$Direction)

mean(lda.class == Smarket.2005$Direction)


# Quadratic Discriminant Analysis 


qda.fit = qda(Direction~Lag1+Lag2,data=Smarket,subset= subset_condition )

qda.fit

qda.pred = predict(qda.fit,Smarket.2005)

names(qda.pred)

qda.class = qda.pred$class

table(qda.class,Smarket.2005$Direction)

mean(qda.class == Smarket.2005$Direction)
