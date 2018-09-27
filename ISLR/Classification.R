setwd("D:/Documents/WU/R Projects/ISLR")
library(MASS)
library(ISLR)
library(class)
library(gpairs)
library(corrplot)

attach(Smarket)
train = (Year<2005)
market.2005<- Smarket[!train,]

qda.fit <- lda(Direction~Lag1+Lag2, data = Smarket, subset = train)
qda.fit
plot(qda.fit)

################Exercise 10################


#Logistic Regression
attach(Weekly)
colnames(Weekly)
pairs(Weekly)
cor(Weekly[,c(1:8)])
#We can see, that a positive correlation between Year and Volume exists.

logreg<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, family=binomial)
summary(logreg)
#Only Lag2 variable appears to be statisticaly significant. 

pred<-predict(logreg,type = 'response')
contrasts(Direction)

dim(Weekly)
logcheck<- rep("Down",1089)
logcheck[pred>0.5]<-"Up"

table(logcheck,Direction)
(557+54)/1089
mean(logcheck==Direction)
## 56% correct, false positive - 48, false negative - 430

trainingdataa<-(Year<2009)
testdata<-Weekly[!trainingdataa,]
trainingdata<-Weekly[trainingdataa,]
test.direction<-Direction[!trainingdataa]

dim(test.direction)
dim(testdata)

logreg2<-glm(Direction~Lag2,data=trainingdata,family = binomial)
?predict
pred2<- predict(logreg2,testdata,type='response')

logcheck2<-rep("Down",104)
logcheck2[pred2>0.5]<-"Up"

table(logcheck2,test.direction)
mean(logcheck2==test.direction)
65/104
##62.5% accurate predictions => 37.5% error rate.



###LDA
lineard<-lda(Direction~Lag2,data = trainingdata)
lineard
#Prior probabilities of groups:
#  Down        Up 
#0.4477157 0.5522843 

pred3<-predict(lineard,testdata)

lda.class<-pred3$class
table(lda.class,test.direction)
###Same results as with logistic regression appear - 62.5% accuracy



###QDA
quadrd<-qda(Direction~Lag2,data = trainingdata)

pred4<-predict(quadrd,testdata)
quadrd.class<-pred4$class
table(quadrd.class,test.direction)
#        test.direction
#quadrd.class Down Up
#Down            0  0
#Up             43 61

#We can see here, that this model is not working really good, because it tends to qualify everything as going UP, no
#output for Down at all. We have to change the probability treshold  in order to fix this issue, but in general, 
#the other 2 models gave a better explanation of the real data.

#all are less than .5
sum(pred4$posterior[,2]>0.5)
#only 18 >.6
sum(pred4$posterior[,2]>0.6)
mean(quadrd.class==test.direction)
#59% correct predictions



###KNN
train.direction<-Weekly[Year<2009,9]
train.pred<-matrix(trainingdata[,2])
test.pred<-matrix(testdata[,2])

set.seed(1)
knn1<-knn(train.pred,test.pred,train.direction,k=1)
table(knn1,test.direction)
mean(knn1==test.direction)
##here we get the worst predictions with  45% accuracy. I tried to improve it, by changing the number of k (the neighbors)
##but we dont get a significant improvement.

table(Weekly$Direction)
605/1089

##Overall, if we didn't use any model and put everything on Up by default, we would get 55% correct assumptions, so
#everything below that is unacceptable. But we did have 2 models with over 62% accuracy and correct predictions
#so I would choose one of them. Of course we could improve them and try to make them a bit more accurate, but they seem
#to be with the highest potential.




#########Exercise 11##############
##Descriptive Analysis
attach(Auto)

colnames(Auto)
median(mpg)

mpg.01<-matrix(Auto[,1])
mpg.01[median(mpg)>mpg]=0
mpg.01[median(mpg)<mpg]=1
mpg.01
mpg

df<-data.frame(Auto,mpg.01)
summary(df)
attach(df)

gpairs(df)
pairs(df[,-9])
cor(df[,-c(9)])
colnames(df)
#Most of the variables show good levels of correlation between mpg and themselves, most notably and with strongest corr
#are the cylinders, displacement, horsepower and weight, the other 3 are around or below 0.5 corr.

boxplot(horsepower~mpg.01)
#Here we can see, that most of the vehicles with high hp have low mileage(below the median)
boxplot(year~mpg.01)
#The newer the car is, better the mpg is.
boxplot(weight~mpg.01)
#The heavier the vehicle, the less mpg it can run.
boxplot(jitter(cylinders)~mpg.01)
#The fewer cylinders, more mpg. The majority of cars wit 4 cilynders belong to the group with higher MPG.

train<-(year%%2==0)
trainauto<-df[train,]
testauto<-df[!train,]
testmpg<-testauto[,"mpg.01"]



###LDA###
ldauto<-lda(mpg.01~cylinders+displacement+horsepower+weight,data = trainauto)
ldauto

autopredld<-predict(ldauto,testauto)
dim(testauto)

table(autopredld$class,testmpg)
mean(autopredld$class==testmpg)
##We receive 0.8736264 correct predictions!! It is a good result, so it means the model is working.
##The model outputs only 14 false positives and only 9 false negatives.
mean(autopredld$class!=testmpg)
#The test error is 12,6%



###QDA###
qdauto<-qda(mpg.01~cylinders+displacement+horsepower+weight,data = trainauto)
qdauto
autopredqd<-predict(qdauto,testauto)
table(autopredqd$class,testmpg)
mean(autopredqd$class==testmpg)
#We receive a little bit worse result: 0.8681319
#But it is still a very good model, which predicts the data well and is close to the real decision boundary.



###Logistic Regression###
lrauto<-glm(mpg.01~cylinders+displacement+horsepower+weight,data = trainauto,family=binomial)
summary(lrauto)
#Not all variables seem to be statisticaly significant, we will have to adjust and play a little bit with them in order to
#make them suitable for use in the log regression model. However this is not defined as an objective of this exercise, so
#I will move on.

autopredlr<-predict(lrauto,testauto,type='response')

##Create the Confusion Matrix
dim(testauto)
autocheck<-rep(0,182)
autocheck[autopredlr>0.5]<-1

table(autocheck,testauto$mpg.01)
mean(autocheck==testauto$mpg.01)
##Logistic regression seems to produce the best results for the moment - almost 88% prediction accuracy.



###KNN###
colnames(df)
test.mpg01<-df$mpg.01[!train]
train.mpg01<-as.matrix(df$mpg.01[train])

train.knn<-as.matrix(trainauto[,c(2,3,4,5)])
test.knn<-as.matrix(testauto[,c(2,3,4,5)])

set.seed(1)
autoknn<- knn(train.knn,test.knn,train.mpg01,k=100)
table(autoknn,test.mpg01)
mean(autoknn==test.mpg01)

#For k=1 the accuracy is: ~84.6%
#For k=3 the accuracy is: ~86.2%
#For k=10 the accuracy  is: ~82.9%
#For k=100 the accuracy is: ~85.7%
#The best model is the one where k=3, because it has the smallest error compared to all others. 


##############Exercise 13###############
attach(Boston)
colnames(Boston)
pairs(Boston)
crimedf<-Boston

#Create a dummy column for the classes - over and under the median:
crimerate.med<-matrix(Boston[,'crim'])
crimerate.med[Boston$crim>median(Boston$crim)]=1
crimerate.med[Boston$crim<median(Boston$crim)]=0
crimedf<-data.frame(crimedf,crimerate.med)
colnames(crimedf)

#Divide to test and training set:
crimetrain<-sample(dim(crimedf)[1],350)
crimetest<-crimedf[-crimetrain,]

#Descriptive Analysis
attach(crimedf)
corr<-cor(crimedf)
corrplot.mixed(corr,lower='ellipse')

boxplot(age~crimerate.med)
#When the proportion of owner-occupied units built prior to 1940 grows, the crime rate also grows.

boxplot(lstat~crimerate.med)
#The bigger the % of population with low status the crime rate is higher.

boxplot(indus~crimerate.med)
#The higher the proportion of non-retail business acres in town, the higher the crime rate.

boxplot(nox~crimerate.med)
#The higher the nitric oxides concentration, the higher the crime rate.

boxplot(dis~crimerate.med)
#The lower the distance to the nearest employment centres, the higher the crime rate.

#And I can continue with these, but it seems, that the pattern of areas with high area looks like to 
#have the properties of the suburban and rural areas. There the houses are old, the population tends to have
#lower status, there are less offices and working places. Also the employment centres tend to be also in the 
#outskirts of the towns, not in city centres.


#Logistic Regression
crime.lr <- glm(crimerate.med~age+black+dis+tax, data=crimedf,subset=crimetrain,family=binomial)
summary(crime.lr)
#Looks like this is one of the better combinations of predictor variables. I have tried also other combinations,
#but this one looks most promising.
#in progress
