getwd()
library(MASS)
library(ISLR)
library(class)
library(gpairs)
library(corrplot)
library(randomForest)

df<-read.csv('train.csv')
fix(df)
sum(is.na(df$Age))
#177 na in Age (and overall)
colnames(df)

df$Sex<- factor(df$Sex)
sum(df$Survived)
dim(df)
siblings<-rep(0,dim(df)[1])
siblings[df$SibSp>0]<-1
#If a person has siblings - 1, otherwise - 0

df['siblings']<-siblings
sum(df$siblings)

contrasts(df$Sex)
#female=0;male=1    

df$Name
str(df$Name)
namelength<-lapply(df$Name,as.character)
df$namelength<-namelength
df$namelengthh<-lapply(df$namelength,nchar)
df$namelength<-df$namelengthh
df<-df[,-15]


df2<-df[,-c(1,4,9,11,13)]
colnames(df2)
#Fill NA values in age column with average for age.
df2[which(is.na(df2$Age)),4]<-mean(na.omit(df2$Age))
sum(is.na(df2$Age))

str(df2)
df2[] <- lapply(df2,as.integer)
corr<-cor(df2)
corrplot.mixed(corr,use="pairwise.complete.obs",lower = 'ellipse',upper='ellipse')
gpairs(df2[,])


boxplot(jitter(df2$Sex)~df2$Survived)
boxplot(jitter(df2$Pclass)~df2$Survived)

#split the data to training/test

trainingdata<- sample(dim(df2)[1],600)
testdata<-df2[-trainingdata,]

#Logistic Regression:
titanic.lr<-glm(df2$Survived~.-Fare,data=df2,subset=trainingdata,family='binomial')
summary(titanic.lr)

lr.pred<-predict(titanic.lr,newdata=testdata,type='response')
checklr<-rep(0,dim(testdata)[1])
checklr[lr.pred>0.5]=1
table(checklr,testdata$Survived)
mean(checklr==testdata$Survived)
#77% correctness, can be improved by selecting different variables and trying different methods.

#Random Forest
set.seed(1)
titanic.rf<-randomForest(df2$Survived~.,data=df2,subset = trainingdata,mtry=3,ntree=100)
importance(titanic.rf)
varImpPlot(titanic.rf)
summary(titanic.rf)
titanic.rf.pred<-predict(titanic.rf,newdata=testdata)
mean((titanic.rf.pred-testdata$Survived)^2)
#0.139 MSE
checklr2<-rep(0,dim(testdata)[1])
checklr2[titanic.rf.pred>0.5]=1

table(checklr2,testdata$Survived)
mean(checklr2==testdata$Survived)
#82.1% correctness
#In progress