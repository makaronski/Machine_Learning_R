setwd("D:/Documents/WU/R Projects/ISLR")

library(MASS)
library(ISLR)
library(class)

############Exercise 5##############
attach(Default)

m1<-glm(default~balance+income,data=Default,family=binomial)
summary(m1)
set.seed(1)

plot(default,balance)
plot(traindata)
traindata<-sample(10000,6000)
str(Default)
?sample
m2<-glm(default~balance+income,data=Default,subset=traindata,family=binomial)
?subset
testdefault<-subset(Default$default,!traindata)
colnames(Default)
length(testdefault)
p1<-predict(m2,Default)[-traindata]
summary(p1)
mean((default-p1)^2)
mean((default-predict(m2,Default))[-traindata]^2)
table(p1,)

x <- 1:12
# a random permutation
sample(x,6)
