# Logistic Regression in R 

library(ISLR)
library(tidyverse)


# Using Smarket Data 

names(Smarket)


# Direction is the target class 

summary(Smarket)

# Correlation between columns 

cor(Smarket[,-9])

# We can see that Volume and Year have high correlation 

plot(Smarket$Volume)


# Lets fit Logistic Regression Model using glm func


glm.fit = glm(Direction ~ . -Today -Year ,data = Smarket,family = binomial )

summary(glm.fit)

# Using predict function 

glm.probs = predict(glm.fit,type="response")

glm.probs[1:10]

glm.predict = ifelse(glm.probs > 0.5 , "Up","Down")

# Create Table between glm.predict and Direction from Smarket

table(glm.predict,Smarket$Direction)

mean(glm.predict==Smarket$Direction)

# Lets train glm model with only a subset now .

# Subset Condition = Year < 2005 

subset_condition  = (Smarket$Year < 2005 )

Smarket_2005 = Smarket[!subset_condition,]

newglm.fit = glm(Direction ~ . -Today -Year ,data = Smarket,family = binomial,subset = subset_condition )


summary(newglm.fit)

newglm.probs = predict(newglm.fit,Smarket_2005,type="response")

newglm.probs[1:10]

newglm.predict = ifelse(newglm.probs > 0.5 , "Up","Down")

# Create Table between glm.predict and Direction from Smarket

table(newglm.predict,Smarket_2005$Direction)

mean(newglm.predict==Smarket_2005$Direction)

mean(newglm.predict!=Smarket_2005$Direction)


# Testing with only few columns 

customglm.fit = glm(Direction ~ Lag1 + Lag2 + Lag1:Lag2 ,data = Smarket,family = binomial,subset = subset_condition )


summary(customglm.fit)

customglm.probs = predict(customglm.fit,Smarket_2005,type="response")

customglm.probs[1:10]

customglm.predict = ifelse(customglm.probs > 0.5 , "Up","Down")

# Create Table between glm.predict and Direction from Smarket

table(customglm.predict,Smarket_2005$Direction)

mean(customglm.predict==Smarket_2005$Direction)

mean(customglm.predict!=Smarket_2005$Direction)


customglm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag1:Lag2 + Lag1:Lag3 + Lag2:Lag3 + Lag1:Lag2:Lag3,data = Smarket,family = binomial,subset = subset_condition )


summary(customglm.fit)

customglm.probs = predict(customglm.fit,Smarket_2005,type="response")

customglm.probs[1:10]

customglm.predict = ifelse(customglm.probs > 0.5 , "Up","Down")

# Create Table between glm.predict and Direction from Smarket

table(customglm.predict,Smarket_2005$Direction)

mean(customglm.predict==Smarket_2005$Direction)

mean(customglm.predict!=Smarket_2005$Direction)
