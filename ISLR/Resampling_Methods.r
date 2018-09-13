setwd("D:/Documents/WU/R Projects/ISLR")

library(MASS)
library(ISLR)
library(class)
library(boot)

############Exercise 5##############
attach(Default)

m1<-glm(default~balance+income,data=Default,family=binomial)
summary(m1)
set.seed(1)

plot(default,balance)
plot(traindata)
traindata<-sample(10000,6000)
?sample

m2<-glm(default~balance+income,data=Default,subset=traindata,family=binomial)

m2.pred=rep("No", 4000)
m2.probs=predict(m2,Default[-traindata,],type="response")
m2.pred[m2.probs>0.5]<-"Yes"
mean(m2.pred!=Default[-traindata,]$default)

ssd<-Default[-traindata,]
dim(ssd)
x <- 1:12
# a random permutation
sample(x,6)


Default$student<-factor(Default$student)

m3<-glm(default~balance+income+student,data=Default,subset=traindata,family = binomial)
m3.probs<-predict(m3,Default[-traindata,],type="response")
m3.pred<-rep("No",4000)
m3.pred[m3.probs>0.5]<-"Yes"
mean(m3.pred!=Default[-traindata,]$default)

###VALIDATION SET ERROR = FRACTION OF MISSCLASSIFIED OBSERVATIONS
###THE == APPROACH CALCULATES THE FRACTION OF CORRECT ONES!!!
###Therefore - here the lower the value the better; 
###So, including the student improves the model by reducing the error

defaultdefault<-Default[-traindata,]$default
table(m3.pred,defaultdefault)

#####Exercise 6######
set.seed(1)
summary(m2)
##balance std error = 3.046e-04
##income std error = 6.89e-06

###In order to use the boot() function, we must first create another func
###and then insert it into the boot func as a parameter
boot.fn<-function(dataa=Default,index){
  m2<-glm(default~balance+income,data=dataa,family=binomial,subset=index)
  return(m2$coefficients)
}
boot.fn(index=sample(10000:6000))

boot(data=Default,boot.fn,50)
?boot

#The std errors are a bit better when using the bootstrap, 
#than when using only the glm() function


#####Exercise 7#####

###MANUAL LOOCV###

attach(Weekly)
logreg<-glm(Direction~Lag1+Lag2,family = binomial)
logreg<-glm(Direction~Lag1+Lag2,data=Weekly[-1,],family = binomial)
dim(Weekly)
direction1<-"Down"
prediction<-predict.glm(logreg,newdata=Weekly[1,],type="response")
direction1[prediction>0.5]<-"Up"
direction1
Weekly$Direction[1]
###Here the prediction is wrong!

count<-rep(1,dim(Weekly)[1])
for(i in 1:(dim(Weekly)[1])){
  logreg<-glm(Direction~Lag1+Lag2,data=Weekly[-i,],family = binomial)
  prediction<-predict.glm(logreg,newdata=Weekly[i,],type="response")
  direction1<-"Down"
  direction1[prediction>0.5]<-"Up"
  real<-Weekly$Direction[i]
  if(direction1==real)
    count[i]=0
}
sum(count)
### 599 True Predictions out of 1089
### 490 Wrong Predictions

sum(count)/(dim(Weekly)[1])
###Error rate ~45%