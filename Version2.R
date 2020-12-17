library(caret)
library(glmnet)
library(tidyverse)
library(rms)

Wine <- read.csv("winequality.csv")
head(Wine)
count(Wine, colour)

#Partition the data, 3456
set.seed(3456)
trainIndex <- createDataPartition(Wine$colour, p = .8, list = FALSE, times = 1)

TrainWine <- Wine[ trainIndex,]
TestWine  <- Wine[-trainIndex,]
count(TrainWine, colour)

log(1)
count(TestWine, colour)

#Explore with cv.glmnet

Winex <- TrainWine[,1:12]
class(Winex)
Winex <- as.matrix(Winex)
class(Winex)

Winey <- as.factor(TrainWine[,13])
class(Winey)

binomial <- glm(Winey~Winex, family = "binomial")
summary(binomial)

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

jpeg('Figure1.jpg')
par(mfrow = c(1,1))
plot(minMCE[,1],minMCE[,2], ylim = c(0,0.015), ylab = "Minimum Misclassification Error", xlab = "Alpha")
abline(h = min(minMCE[,2]))
dev.off()

#Test with all four alphas: 0.7, 0.8, 0.9, 1.0

CV1.0 <- cv.glmnet(Winex,Winey, family = "binomial", foldid = folds, type.measure = "class", alpha = 1.0)
print(CV1.0$lambda.min)

#Visualize coefficients of this

jpeg('Figure2.jpg')
par(mfrow = c(1,1), mar = c(5, 4, 4, 4))
plot(CV1.0$glmnet.fit, "lambda", ylim=c(-10,5), yaxt = "n", ylab = " ")
abline(v=log(CV1.0$lambda.min), col = "black")
abline(v=log(CV1.0$lambda.1se), col = "blue")
vnat=coef(CV1.0$glmnet.fit)
vnat=vnat[-1,ncol(vnat)] # remove the intercept, and get the coefficients at the end of the path
vn = names(TrainWine[,1:12])
axis(2, at=vnat,line=-.5,label=vn,las=1,tick=FALSE, cex.axis=0.5)
axis(4, at = seq(-10,5,by = 5))
mtext("Coefficents", 4, 2)
dev.off()

#investigate remainign features
Neg8 <- glmnet(Winex,Winey, family = "binomial", alpha = 1, lambda = exp(-8))
Neg6 <- glmnet(Winex,Winey, family = "binomial", alpha = 1, lambda = exp(-6))
Neg5 <- glmnet(Winex,Winey, family = "binomial", alpha = 1, lambda = exp(-5))
cbind(Neg8$beta,Neg6$beta,Neg5$beta)

par(mfrow = c(1,1))

#What features are being effected
WineModel <- glmnet(Winex,Winey, family = "binomial", alpha = 1, lambda = CV1.0$lambda.min)
coef(WineModel)
#Predict/Confusion
class(TestWine)
head(TestWine)
TestWineX <- as.matrix(TestWine[,1:12])
TestWineLabel <- as.factor(TestWine[,13])

confusion.glmnet(WineModel, TestWineX, TestWineLabel, family = "binomial")

