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



#Exclude non-relevant columns (id,cabin nr,ticket nr, name)
df2<-df[,-c(1,4,9,11,13)]
colnames(df2)
#Fill NA values in age column with average for age.
df2[which(is.na(df2$Age)),4]<-mean(na.omit(df2$Age))
sum(is.na(df2$Age))


#create 3 age groups:

for( i in 1:length(df2$Age) ) {
  if(is.na(df2$Age[i])){
    df2$AgeGroup[i] <- NA
  } else if (df2$Age[i]<16){
    df2$AgeGroup[i] <- 1
  } else if(df2$Age[i] >15 & df2$Age[i]<60) {
    df2$AgeGroup[i] <- 2
  } else if (df2$Age[i]>59 & df$Age[i]<75){
    df2$AgeGroup[i] <- 3
  } else if (df2$Age[i]>74){
    df2$AgeGroup[i] <- 4
  }
}

boxplot(jitter(df$Parch)~df$Survived)
barplot(counts,beside = TRUE)
counts <- table(df2$AgeGroup,df2$Survived)


#Create two groups from the Parch feature
family<-rep(0,dim(df)[1])
family[df2$Parch>1]<-1
#If a person has more than 2 children - 1, otherwise - 0

df2['family']<-family
sum(df2$family)
df2['siblings']<-siblings

str(df2)
df2[] <- lapply(df2,as.integer)
corr<-cor(df2)
corrplot.mixed(corr,use="pairwise.complete.obs",lower = 'ellipse',upper='ellipse')
gpairs(df2)


boxplot(jitter(df2$Sex)~df2$Survived)
boxplot(jitter(df2$Pclass)~df2$Survived)


#####Create testdf
testdf<-read.csv('test.csv')
colnames(testdf)
namelength<-lapply(testdf$Name,as.character)
testdf$namelength<-namelength
testdf$namelengthh<-lapply(testdf$namelength,nchar)
testdf$namelength<-testdf$namelengthh
testdf$namelength<-as.integer(testdf$namelength)
testdf<-testdf[,-c(1,3,8,10,13)]
testdf$Sex<-as.integer(testdf$Sex)

siblings2<-rep(0,dim(testdf)[1])
siblings2[testdf$SibSp>0]<-1
testdf['siblings']<-siblings2

family2<-rep(0,dim(testdf)[1])
family2[testdf$Parch>1]<-1
testdf['family']<-family2


testdf[which(is.na(testdf$Age)),3]<-mean(na.omit(testdf$Age))
sum(is.na(testdf$Age))

for( i in 1:length(testdf$Age) ) {
  if(is.na(testdf$Age[i])){
    testdf$AgeGroup[i] <- NA
  } else if (testdf$Age[i]<16){
    testdf$AgeGroup[i] <- 1
  } else if(testdf$Age[i] >15 & df2$Age[i]<60) {
    testdf$AgeGroup[i] <- 2
  } else if (testdf$Age[i]>59 & df$Age[i]<75){
    testdf$AgeGroup[i] <- 3
  } else if (testdf$Age[i]>74){
    testdf$AgeGroup[i] <- 4
  }
}
which(is.na(testdf))
colnames(testdf)
testdf<-testdf[,-c(7)]
summary(testdf)
testdf$namelength
str(testdf)
str(df2)
sum(is.na(df$Embarked))


#split the data to training/test

trainingdata<- sample(dim(df2)[1],600)
testdata<-df2[-trainingdata,]

#Logistic Regression:
titanic.lr<-glm(df2$Survived~.,data=df2,subset=trainingdata,family='binomial')
summary(titanic.lr)
##df2<-df2[,-8]
##colnames(df2)
###df2$Embarked<-factor(df2$Embarked)

lr.pred<-predict(titanic.lr,newdata=testdata,type='response')
checklr<-rep(0,dim(testdata)[1])
checklr[lr.pred>0.5]=1
table(checklr,testdata$Survived)
mean(checklr==testdata$Survived)
#77% correctness, can be improved by selecting different variables and trying different methods.




#Random Forest
set.seed(1)
df2$Survived<-factor(df2$Survived)
titanic.rf<-randomForest(df2$Survived~.,data=df2,mtry=4,ntree=100)
importance(titanic.rf)
varImpPlot(titanic.rf)
summary(titanic.rf)
titanic.rf.pred<-predict(titanic.rf,newdata=testdf)
mean(titanic.rf.pred==testdata$Survived)
#In progress

write.csv(titanic.rf.pred,file='results.xlsx')

#SVM
cor(df$Parch,df$Survived)

testdf[153,6]<-mean(na.omit(testdf$Fare))
