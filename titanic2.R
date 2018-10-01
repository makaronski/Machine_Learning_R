getwd()
library(MASS)
library(ISLR)
library(class)
library(gpairs)
library(corrplot)
library(randomForest)

#####Data Manipulation#####

df<-read.csv('train.csv')
testdf<-read.csv('test.csv')

df$Sex<- factor(df$Sex)
testdf$Sex<-factor(testdf$Sex)

#Create a variable for the family size
familysize<-df$SibSp+df$Parch
df['familysize']<-familysize
familysize2<-testdf$SibSp+testdf$Parch
testdf['familysize']<-familysize2

#Deal with 4 levels of Embarked factor
str(df)
which(df$Embarked=="S")
df[62,12]<-'S'
df[830,12]<-'C'
df$Embarked[df$Embarked %in% c('', 'C')] <- 'C'
df$Embarked <- droplevels(df$Embarked)

#Add mean to all missing Age values
df[which(is.na(df$Age)),6]<-mean(na.omit(df$Age))
sum(is.na(df$Age))

testdf[which(is.na(testdf$Age)),5]<-mean(na.omit(testdf$Age))
sum(is.na(df$Age))

#Create a variable for Age groups (Kids)

for( i in 1:length(df$Age) ) {
  if(is.na(df$Age[i])){
    df$AgeGroup[i] <- NA
  } else if (df$Age[i]<16){
    df$AgeGroup[i] <- 1
  } else if(df$Age[i] >15 & df$Age[i]<60) {
    df$AgeGroup[i] <- 2
  } else if (df$Age[i]>59){
    df$AgeGroup[i] <- 3
  }
}

for( i in 1:length(testdf$Age) ) {
  if(is.na(testdf$Age[i])){
    testdf$AgeGroup[i] <- NA
  } else if (testdf$Age[i]<16){
    testdf$AgeGroup[i] <- 1
  } else if(testdf$Age[i] >15 & testdf$Age[i]<60) {
    testdf$AgeGroup[i] <- 2
  } else if (testdf$Age[i]>59){
    testdf$AgeGroup[i] <- 3
  }
}


#Try to use the names somehow, 1st try with the length
namelength<-lapply(df$Name,as.character)
df$namelength<-namelength
df$namelengthh<-lapply(df$namelength,nchar)
df$namelength<-df$namelengthh
df$namelength<-as.integer(df$namelength)

namelength2<-lapply(testdf$Name,as.character)
testdf$namelength<-namelength2
testdf$namelengthh<-lapply(testdf$namelength,nchar)
testdf$namelength<- testdf$namelengthh
testdf$namelength<-as.integer(testdf$namelength)

#Drop unnecessary columns
colnames(df)
df<-df[,-c(1,4,7,8,9,11,16)]
colnames(testdf)
testdf<-testdf[,-c(1,3,6,7,8,10,15)]

#####Descriptive Analysis#####
barplot(table(df$AgeGroup,df$Survived),beside = TRUE)
#Kids had best survival rate(only positive)

barplot(table(df$Pclass,df$Survived),beside=TRUE)
#Persons from higher class had higher survival rate

barplot(table(df$Embarked,df$Survived),beside = TRUE)
contrasts(df$Embarked)
#Persons, who embarked from Cherbourg had best survival rate (positive)
#Greatest number of ppl embarked from Southampton, but with the worst survival rate.

barplot(table(df$familysize,df$Survived),beside = TRUE)
#Persons who were alone on the ship (no other relatives) had worst survival rate
#Persons who had 1 relative had best survival rate (positive)
#Persons with family size larger than 7 didn't survive

boxplot(df$namelength~df$Survived)
#Persons with longer names tend to have better survival rate (probably some titles?)

#####MODELS#####
#Split into test and train to compare the fit:
trainingdata<- sample(dim(df)[1],600)
testdata<-df[-trainingdata,]

#Random Forest
df$Survived<-as.factor(df$Survived)
set.seed(1)
titanic.rf<-randomForest(df$Survived~.-Age,data=df,subset = trainingdata ,mtry=3,ntree=1000)
varImpPlot(titanic.rf)
sqrt(dim(df)[2])
titanic.rf.pred<-predict(titanic.rf,newdata=testdata)
mean(titanic.rf.pred==testdata$Survived)
colnames(testdata)
#82% correctness


#Logistic Regression
titanic.lr<-glm(df$Survived~.-Age,data=df,subset=trainingdata,family='binomial')
summary(titanic.lr)
titanic.lr.pred<-predict(titanic.lr,newdata=testdata,type='response')
checklr<-rep(0,dim(testdata)[1])
checklr[lr.pred>0.5]=1
table(checklr,testdata$Survived)
mean(checklr==testdata$Survived)
#53% correctness - very low.