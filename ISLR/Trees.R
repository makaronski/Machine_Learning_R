getwd()
library(ISLR)
library(MASS)
library(tree)
library(randomForest)
attach(Boston)

######Exercise 7######
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

######Exercise 8#######
set.seed(1)
train<-sample(1:nrow(Carseats),nrow(Carseats)/1.5)
train.carseats<-Carseats[train,]
test.carseats<-Carseats[-train,]
dim(test.carseats)

carseats.tree<-tree(Sales~.,data = Carseats,subset=train)
summary(carseats.tree)
plot(carseats.tree)
text(carseats.tree,pretty=0)

#Out of all the 10 variables (except Sales) that can be used to fit the model, the tree is really using only 7 of them.
#[1] "ShelveLoc"   "Price"       "Age"         "Advertising" "Population"  "Income"      "CompPrice"  
#The others were omitted. 
#The most important factor seems to be Shelve Location, 2nd most important - the Price

###Cross-validation to check which tree is selected:

cv.carseats<-cv.tree(carseats.tree)
plot(cv.carseats$size,cv.carseats$dev,type='b')
#Using the cv shows that the tree with the smallest deviation is the one with the largest number of terminal nodes
#(no pruning), so I continue to work with it.

#Predict the model on the test sample
pred.carseats<-predict(carseats.tree,newdata=test.carseats)
test.carseats.sales<-Carseats[-train,'Sales']
plot(pred.carseats,test.carseats.sales)
abline(0,1,col='red')
mean((pred.carseats-test.carseats.sales)^2)
#The mean squared error(MSE) = 5.395
sqrt(5.395316)
#The square root of the MSE is 2.322, which means, that the model leads to test predictions, which are within around 2.2$ 
#of the True median value for Carseat sales.


###Bagging on the Carseats

bag.carseats<-randomForest(Sales~.,data=Carseats,subset = train,mtry=10,importance=TRUE)
bag.carseats
summary(bag.carseats)
importance(bag.carseats)
varImpPlot(bag.carseats)
#We can clearly see, that Price and ShelveLoc are the two most important variables. They have the highest values in both
#of the relevant categories.

#Predict Bagging
carseats.bag.pred<-predict(bag.carseats,newdata=test.carseats)
plot(carseats.bag.pred,test.carseats.sales)
abline(0,1,col='red')
mean((carseats.bag.pred-test.carseats.sales)^2)
#The MSE here is 2.252, which is greatly (more than twice) improving the regression tree model built earlier!
#This is also noticeable on the plots of the models.


###Random Forest on the Carseats
rf.carseats<-randomForest(Sales~.,data=Carseats,subset=train,mtry=6, ntree=1000)
rf.carseats
importance(rf.carseats)
varImpPlot(rf.carseats)
#Again the same story here - most important are the Price and then Shelve loc with NodePurity index of 502 and 424 resp.

#Predict Random Forest
carseats.rf.pred<-predict(rf.carseats,newdata=test.carseats)
plot(carseats.rf.pred,test.carseats.sales)
abline(0,1,col='red')
mean((carseats.rf.pred-test.carseats.sales)^2)
#Here with m=3,ntree=100 we observe a bit higher test MSE = 2.714, also the plot seems worse.
#With m=5 MSE = 2.403, m=5, ntree = 1000 MSE = 2.323
###M=6, Ntree= 1000 - The smallest MSE OF ALL THE MODELS BUILT = 2.238
#Tried also other options but the last one mentioned leads to the lowest test MSE.


#####Exercise 9######

#a
attach(OJ)
traindata <- sample(dim(OJ)[1],800)
traindf<- OJ[traindata,]
testdf <- OJ[-traindata,]
dim(testdata)

#b
colnames(OJ)
model1 <- tree(Purchase~., data = traindf)
summary(model1)
#8 terminal nodes, 16% training error rate (130/800 wrong).
#Most important variables (actually used): "LoyalCH"     "SalePriceMM" "PriceDiff"  

#c
model1
#The Terminal Nodes are marked with a STAR (*) at the end of the row. For example we can reach node 10,
#which is also terminal node by starting from the root, then if LoyalCH is less than 0.48285, then if 
#LoyalCH is bigger than 0.0356, then SalePrice has to be lower than 2.04
#There are 142 points below this node in the tree
#The deviance for all the points beneath this node in the tree = 135.2
#The prediction is, that an observation that reaches this node, will be classified as MM
#18% of the training observations in this node have CH, 81% - MM

#So in order to classify an observation there, the LoyalCH has to be between 0.48285 and 0.035, AND 
#SalePriceMM < 2.04

#d
plot(model1)
text(model1,pretty=0)
#We can clearly see that the most important variable is LoyalCH, which stays not only on the first level
#of the tree, but also on the second.


#e
set.seed(1)
model1.pred<-predict(model1,newdata=testdf,type='class')
#oj.pred.check <- testdf$Purchase
#oj.pred.matrix<-rep("MM",length(oj.pred.check))
#oj.pred.matrix<-oj.pred.matrix[model1.pred>0.5,'CH']

table(testdf$Purchase,model1.pred)
#   model1.pred
#    CH  MM
#CH 154  15
#MM  31  70
mean(model1.pred==testdf$Purchase)
# 0.8296296
#Almost 83% accuracy on the test set.
#~17% Error rate

#f
set.seed(2)
cvcheck<-cv.tree(model1,FUN=prune.misclass)
cvcheck

#g
#plot(cvcheck)
plot(cvcheck$size,cvcheck$dev,type='b')

#h
#With tree size of 7(7 terminal nodes) the MSE is least. 

#i
pruned <- prune.tree(model1,best=7)

#j
summary(pruned)
#Misclassification errors of both pruned and non-pruned trees are the equal. The only difference we can 
#observe is in the residual mean deviance.

#k
unpruned.err<-mean((model1.pred!=testdf$Purchase)^2)
unpruned.err
#0.1703704

pruned.pred<-predict(pruned,newdata = testdf,type='class')
pruned.err<-mean((pruned.pred!=testdf$Purchase)^2)
pruned.err
#0.1703704
#No difference again on the MSE of test data between pruned and unpruned trees
