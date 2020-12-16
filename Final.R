library(caret)
library(glmnet)
library(tidyverse)

Wine <- read.csv("winequality.csv")
head(Wine)

#Partition the data
set.seed(3456)
trainIndex <- createDataPartition(Wine$colour, p = .8, list = FALSE, times = 1)

TrainWine <- Wine[ trainIndex,]
TestWine  <- Wine[-trainIndex,]
count(TrainWine, colour)
count(TestWine, colour)

#Explore with cv.glmnet

Winex <- TrainWine[,1:12]
class(Winex)
x=matrix(rnorm(100*5),100,5)
class(x)
Winex <- as.matrix(Winex)
class(Winex)

Winey <- as.factor(TrainWine[,13])
class(Winey)
y=rnorm(100)
class(y)
cvfitMisClass <- cv.glmnet(Winex,Winey,family = "binomial", type.measure = "class")
cvfitMSE <- cv.glmnet(Winex,Winey,family = "binomial", type.measure = "mse")
par(mfrow=c(1,2))
plot(cvfitMisClass)
plot(cvfitMSE)

par(mfrow=c(1,1))
fit <- glmnet(Winex,Winey,family = "binomial", type.measure = "class",lower=0)
plot(fit,"lambda")
plot(fit)
abline(v=log(cvfit1$lambda.min))

fit1 <- glmnet(Winex,Winey,family = "binomial", type.measure = "class",alpha = 0)
plot(fit1,"lambda")
plot(fit)
abline(v=log(cvfit1$lambda.min))

cv1=cv.glmnet(Winex,Winey,family = "binomial", type.measure = "class",alpha=1)
cv.5=cv.glmnet(Winex,Winey,family = "binomial", type.measure = "class",alpha=.5, lower = -10)
cv0=cv.glmnet(Winex,Winey,family = "binomial", type.measure = "class",alpha=0, lower = -10)

par(mfrow=c(2,2))
plot(cv1)
plot(cv.5)
plot(cv0)

par(mfrow=c(1,1))
plot(cv1$glmnet.fit, "lambda")
abline(v=log(cv1$lambda.min))
plot(cv.5$glmnet.fit, "lambda")
abline(v=log(cv.5$lambda.min))
plot(cv0$glmnet.fit, "lambda")
abline(v=log(cv0$lambda.min))


cv1=cv.glmnet(Winex,Winey,family = "binomial", type.measure = "class",alpha=1)
cv1$lambda.min
fit1 <- glmnet(Winex,Winey,family = "binomial", type.measure = "class",alpha = 1,lambda=0.1)
fit1$beta
names(TrainWine)

cv1$cvm

fit <- glmnet(Winex,Winey,family = "binomial", type.measure = "class",alpha = 1)
fit$beta


