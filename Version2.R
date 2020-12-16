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
Winex <- as.matrix(Winex)
class(Winex)

Winey <- as.factor(TrainWine[,13])
class(Winey)

linear <- glm(Winey~Winex, family = "binomial")
summary(linear)

#First choice ideal alpha value
#Preset folds so that same one used every analysis
folds <- sample(1:10,size=length(Winey),replace=TRUE)

alphamodels <- as.list(c(1:11))
alphatest <- seq(0,1,by=0.1)
for (a in 1:11){
  alphamodels[[a]] <- cv.glmnet(Winex,Winey,foldid = folds, family = "binomial", type.measure = "class", alpha = alphatest[a])
}

#Determine minimum misclassification error for each alph value
minMCE <- data.frame("alpha" = seq(0,1,by=0.1), "minMCE" = 1:11)
for(a in 1:11){
  minMCE[a,2] <- min(alphamodels[[a]]$cvm)
}
print(minMCE)

jpeg('alpha.jpg')
plot(minMCE[,1],minMCE[,2], ylim = c(0,0.015), ylab = "Minimum Misclassification Error", xlab = "alpha")
abline(h = min(minMCE[,2]))
dev.off()

#Will use alpha = 1 aka full lasso
#Lets fine ideal lambda

WineCV <- cv.glmnet(Winex,Winey, family = "binomial", type.measure = "class", alpha = 0.7)
plot(WineCV)

jpeg('Betadecay.jpg')
par(mfrow = c(2,1))
plot(WineCV$glmnet.fit, "lambda")
abline(v=log(WineCV$lambda.min))
abline(v=log(WineCV$lambda.1se))
plot(WineCV$glmnet.fit,"lambda", ylim=c(-20,10))
abline(v=log(WineCV$lambda.min))
abline(v=log(WineCV$lambda.1se))
dev.off()

par(mfrow = c(1,1))

#What features are being effected
WineNet <- glmnet(Winex,Winey, family = "binomial", type.measure = "class", alpha = 0.7, lambda = WineCV$lambda.1se)
WineNet$beta



#Predict/Confusion
class(TestWine)
head(TestWine)
TestWineX <- as.matrix(TestWine[,1:12])
TestWineLabel <- as.factor(TestWine[,13])

confusion.glmnet(WineCV, TestWineX, TestWineLabel, family = "binomial")
confusion.glmnet(WineNet, TestWineX, TestWineLabel, family = "binomial")


