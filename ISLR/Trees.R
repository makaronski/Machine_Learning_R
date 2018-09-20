getwd()
library(ISLR)
library(MASS)
library(tree)
library(randomForest)
attach(Boston)

######Exercise 8######
set.seed(1)
train <- sample(1:nrow(Boston),nrow(Boston)/2)
boston.test<-Boston[-train,'medv']
#p=13 => if m=13 same as bagging
bag.boston <- randomForest(medv~.,data=Boston,subset=train,mtry=6,ntree=25)
bag.boston

plot(bag.boston)
yhat.bag=predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)

mean((yhat.bag-boston.test)^2)
#For mtry=6 (6 predictors sampled on each tree) MSE = 11.753

rf1<-randomForest(medv~.,data=Boston,subset=train,mtry=4, ntree=100)
pred.rf1<-predict(rf1,newdata=Boston[-train,])
plot(pred.rf1,boston.test)
abline(0,1)
mean((pred.rf1-boston.test)^2)
#mtry=4, ntree=25 - 11.388 MSE

rf2<-randomForest(medv~.,data=Boston,subset=train,mtry=4, ntree=100)
pred.rf2<-predict(rf2,newdata=Boston[-train,])
mean((pred.rf2-boston.test)^2)
#mtry=4,ntree = 1000 - 11.132 MSE

rf3<-randomForest(medv~.,data=Boston,subset=train,mtry=3, ntree=100)
pred.rf3<-predict(rf3,newdata=Boston[-train,])
mean((pred.rf3-boston.test)^2)
#mtry=3, ntree = 100 - 12.818 MSE

rf4<-randomForest(medv~.,data=Boston,subset=train,mtry=7, ntree=100)
pred.rf4<-predict(rf4,newdata=Boston[-train,])
mean((pred.rf4-boston.test)^2)
#mtry=7,ntree=100, 11.642 MSE

rf5<-randomForest(medv~.,data=Boston,subset=train,mtry=5, ntree=100)
pred.rf5<-predict(rf5,newdata=Boston[-train,])
mean((pred.rf5-boston.test)^2)
#mtry=5, ntree=100 12.02 MSE

#The plot is exported as random_forests_comparison.png
plot(rf1,type="l",col="red",xlim=c(0,100),ylim=c(10,30))
lines(rf2$mse,col="green")
lines(rf3$mse,col="blue")
lines(rf4$mse,col="yellow")
lines(rf5$mse,col="brown")

plot(rf2$mse)
#Overall for each try, we can see that the MSE decreases marginally by reducing
#the ntrees for each mtry. The best results we got by using:
#mtry=4 and ntree = 1000. This only proves the rule, that the number
#of predictors, that need to be used on each split needs to be = all p/3
#But if we place the treshold for ntrees = 100, then the best performing model is:
#rf4 with mtry = 7, with lowest test MSE.

