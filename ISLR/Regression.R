getwd()
setwd("D:/Documents/WU/R Projects/ISLR")
library(MASS)
library(ISLR)

##############8
fix(Boston)
attach(Auto)

lm.fit <- lm(medv~lstat)

summary(lm.fit)
plot(lstat,medv)
abline(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)
plot(rstudent(lm.fit))

model<-lm(mpg~horsepower)
summary(model)
#There is a significant relationship btw the predictor and response
#Very strong
#Negative relationship, when hp go up, mpg go down

predict(model,data.frame(horsepower=98), interval = 'prediction')
#Confidence
#      lwr      upr
#1 24.46708 23.97308 24.96108

#Prediction
#       fit     lwr      upr
#1 24.46708 14.8094 34.12476

plot(horsepower,mpg)
par(mfrow=c(1,1))
abline(model,col = "blue")
?abline

par(mfrow=c(2,2))
plot(model)
#The data is not following a linear pattern


####################9

pairs(Auto)
cor(Auto[,names(Auto)!='name'])
colnames(Auto)

dfquant<- Auto[,names(Auto)!='name']
model2 <- lm(mpg~.,data=dfquant)
summary(model2)
#There is a relationship between some of the predictors
#and the outcome, some are not significant and have
#high p-values.
#displacement,weight,year,origin have sign. rel
#The coefficient in the year variable means that the 
#newer the car is, the more the mpg it can run.

plot(model2)
#We can observe a slight suggestion of a non-linear rel
#And also see, that observance with index 14 has really
#high leverage and is a potential outlier

plot(predict(model2), rstudent(lm.fit1))
## Studentized residuals - the values outside of the range
##[-3:3] are outliers

model2.5<-lm(mpg~cylinders+weight+year+origin+cylinders*horsepower)
summary(model2.5)
# Now with the interaction between horsepower and cilinders
#the model is better and also the variables are
#significant simultaneously and also on their own



###########10
attach(Carseats)
fix(Carseats)

model3 <- lm(Sales~Price+Urban+US,data = Carseats)
summary(model3)

##The intercept means that the sales start from 13.04
#without the interference of the other variables

#When Price increases by 1 unit, Sales decreases by
#0.05...
#If the house is in a Urban Area, the Sales decrease by
#0,02... = 13.02...This parameter has a rather high p-value, so
#it is insignifficant
#If the house is in the US, the sales grow by 1.2...
#= 14.24...

#Sales = 13.04-0.5*Price+-0.02IfURban+-1.2IfUsYes

#Reject the Ho for price,USYes Accept Ho for UrbanYes

model3.5<- lm(Sales~Price+US,data=Carseats)
summary(model3.5)

#Model 3.5 fits the data a bit better (ADJ Rsq = 0.2354)
#than model 3 (0.2335). But still both of them are not
#good enough and explain only 23% of the variance.

confint(model3.5)
plot(model3.5)

#I don't believe, that there are any outliers or high
#leverage observations, based on the confintervals and
#the residual plots.


############14

set.seed(1)
x1<-runif(100)
x2<-0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)

#The regression coefficients are:
#Intercept = 2
#b1 = 2
#b2 = 0.3
#irreducible error= rnorm(100)

cor(x1,x2)
#> cor(x1,x2)
#[1] 0.8351212

plot(x1,x2)

#Linear relationship, with high correlation, suggests
#Collinearity

model4<- lm(y~x1+x2)
summary(model4)

#Both parameters are not really significant(x1 is a bit)
#b0 2.13 b1= 1.4 b2 = 1.01
#I can accept both hypothesis that b1 and b2 are = 0
#But b1 can also be different than 0, needs more investigation
#The values of both b1 and b2 are far from the real ones
#the intercept(b0) is ok

model4.5<-lm(y~x1)
summary(model4.5)

#This model has a better fit for the x1 the Adj R2 is better
#Also the p-value of the F-statistic.
#The value of b1 is really close to the real value of b1
#Also the intercept value
#The null hypothesis about b1 is rejected as well.

model4.6<- lm(y~x2)
summary(model4.6)
#This model is worse than the previous two, the R2 is less
#b1 is now significant and can explain the model, as well the 
#f-statistic is also with good value.<1

#They don't contradict, because of the collinearity property
#Both  the variables can be useful to build a model
#But they also are with high correlation and this messes
#up the entire model, when using both of them

#Adding the *mismeasured* observation
x1=c(x1,0.1)
x2=c(x2,0.8)
y=c(y,6)

plot(model4)
#no new outliers, the point 101, the one we just added
#has high leverage and needs to be removed

plot(model4.5)
#no outliers, good distribution and no high leverage points

plot(model4.6)
#One high leverage point- 101, the one we just added,
#no outliers

############15 COME BACK AND DO IT
