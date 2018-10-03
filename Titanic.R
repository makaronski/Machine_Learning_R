getwd()
library(MASS)
library(ISLR)
library(class)
library(gpairs)
library(corrplot)
library(randomForest)
library(e1071)

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
#df[62,12]<-'S'
#df[830,12]<-'C'
df$Embarked[df$Embarked %in% c('')] <- 'S'
df$Embarked <- droplevels(df$Embarked)

#Add mean to all missing Age values
df[which(is.na(df$Age)),6]<-mean(na.omit(df$Age))
sum(is.na(df$Age))

testdf[which(is.na(testdf$Age)),5]<-mean(na.omit(testdf$Age))
sum(is.na(df$Age))
#Too many missing values and it is not appropriate to add the mean only. Will change the column based on correlation 
#with other variables.



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
#EDIT - Doesn't work well, it has too much weight on model, without any worth.
#namelength<-lapply(df$Name,as.character)
#df$namelength<-namelength
#df$namelengthh<-lapply(df$namelength,nchar)
#df$namelength<-df$namelengthh
#df$namelength<-as.integer(df$namelength)

#namelength2<-lapply(testdf$Name,as.character)
#testdf$namelength<-namelength2
#testdf$namelengthh<-lapply(testdf$namelength,nchar)
#testdf$namelength<- testdf$namelengthh
#testdf$namelength<-as.integer(testdf$namelength)

#Extract title from name variable (Miss/Mrs/Mr/etc.)
#proba<-substring(df$Name, regexpr(".", df$Name,perl=TRUE))
regex<-regexpr("([A-Za-z]+)\\.",df$Name,perl=TRUE)
df['titles']<-regmatches(df$Name,regex)
df['titles']<-as.factor(df$titles)
levels(df$titles)[18]<-'Rare'
df$titles[df$titles %in% c('Lady.', 'Countess.','Capt.', 'Col.',
                           'Don.', 'Dr.', 'Major.', 'Rev.', 'Sir.', 'Jonkheer.', 'Dona.')] <- 'Rare'
df$titles[df$titles=='Mlle.']<-'Miss.'
df$titles[df$titles=='Ms.']<-'Miss.'
df$titles[df$titles=='Mme.']<-'Mrs.'
df$titles <- droplevels(df$titles)


regex2<-regexpr("([A-Za-z]+)\\.",testdf$Name,perl=TRUE)
testdf['titles']<-regmatches(testdf$Name,regex2)
testdf['titles']<-as.factor(testdf$titles)
levels(testdf$titles)[18]<-'Rare'
testdf$titles[testdf$titles %in% c('Lady.', 'Countess.','Capt.', 'Col.',
                           'Don.', 'Dr.', 'Major.', 'Rev.', 'Sir.', 'Jonkheer.', 'Dona.')] <- 'Rare'
testdf$titles[testdf$titles=='Mlle.']<-'Miss.'
testdf$titles[testdf$titles=='Ms.']<-'Miss.'
testdf$titles[testdf$titles=='Mme.']<-'Mrs.'
testdf$titles <- droplevels(testdf$titles)


#There are some missing values from the testdf and we cant predict properly
apply(is.na(testdf),2,sum)
which(is.na(testdf$Fare))
testdf[153,9]<-mean(na.omit(testdf$Fare))

#Add a range for Fare
df <- within(df, FareRange <- as.integer(cut(df$Fare, quantile(df$Fare, probs=0:4/4), include.lowest=TRUE)))
testdf<-within(testdf, FareRange <- as.integer(cut(testdf$Fare, quantile(testdf$Fare, probs=0:4/4), include.lowest=TRUE)))

#Add a HasFamily variable - The persons who were alone had least chance to survive.
HasFamily<-rep(1,dim(df)[1])
HasFamily[df$familysize<1]<-0
df['HasFamily']<-HasFamily

HasFamily<-rep(1,dim(testdf)[1])
HasFamily[testdf$familysize<1]<-0
testdf['HasFamily']<-HasFamily

#Drop unnecessary columns
colnames(df)
df<-df[,-c(1,4,6,7,8,9,10,11)]
colnames(testdf)
testdf<-testdf[,-c(1,3,5,6,7,8,9,10)]





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

#boxplot(df$namelength~df$Survived)
#Persons with longer names tend to have better survival rate (probably some titles?)

boxplot(jitter(df$HasFamily)~df$Survived)

#####MODELS#####
#Split into test and train to compare the fit:
trainingdata<- sample(dim(df)[1],600)
testdata<-df[-trainingdata,]

###Random Forest
df$Survived<-as.factor(df$Survived)
set.seed(1)
titanic.rf<-randomForest(Survived~.,data=df,subset = trainingdata,mtry=3,ntree=1000)
varImpPlot(titanic.rf)
sqrt(dim(df)[2])
titanic.rf.pred<-predict(titanic.rf,newdata=testdata)
mean(titanic.rf.pred==testdata$Survived)
colnames(testdata)
#82% correctness
titanic.rf<-randomForest(Survived~.,data=df,mtry=3,ntree=1000)
titanic.test.pred<-predict(titanic.rf,newdata=testdf)
write.csv(titanic.test.pred,file='resultsrandf2.csv')

###Logistic Regression
titanic.lr<-glm(df$Survived~.,data=df,subset=trainingdata,family='binomial')
summary(titanic.lr)
titanic.lr.pred<-predict(titanic.lr,newdata=testdata,type='response')
checklr<-rep(0,dim(testdata)[1])
checklr[titanic.lr.pred>0.5]=1
table(checklr,testdata$Survived)
mean(checklr==testdata$Survived)
#81% correctness

#titanic.test.pred<-predict(titanic.rf,newdata=testdf)
#write.csv(checklr,file='resultslogr.csv')

###SVM
crossvalSVM=tune(svm ,Survived~.,data=df ,kernel ="linear",
              ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))
summary(crossvalSVM)
#Best with cost=1
titanic.svm<-svm(Survived~.,data=df,subset=trainingdata,kernel='linear',cost=1,scale=FALSE)
titanic.svm.pred<-predict(titanic.svm,newdata = testdata)
mean(titanic.svm.pred==testdata$Survived)
#For Linear SVM - 81.09% accuracy

crossvalSVM2=tune(svm ,Survived~.,data=df ,kernel ="radial",
                 ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100),gamma=c(0.5,1,2,3,4) ) )
summary(crossvalSVM2)
titanic.svm2<-svm(Survived~.,data=df,subset=trainingdata,kernel='radial',cost=0.1,gamma=0.05,scale=TRUE)
titanic.svm.pred2<-predict(titanic.svm2,newdata = testdata)
mean(titanic.svm.pred2==testdata$Survived)
#Radial SVM - Slightly worse 80.7%

crossvalSVM3=tune(svm ,Survived~.,data=df ,kernel ="polynomial",
                  ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100),degree=c(0.5,1,2,3,4) ) )
summary(crossvalSVM3)
titanic.svm3<-svm(Survived~.,data=df,subset=trainingdata,kernel='radial',cost=1,degree=2,scale=TRUE)
titanic.svm.pred3<-predict(titanic.svm2,newdata = testdata)
mean(titanic.svm.pred3==testdata$Survived)
#Same as Linear method.

#So I assume the relationship is almost linear, because we don't see improvement with the non-linear SVMs and 
#also they get really close with small values for their parameters.

#Try to submit with SVM on Kaggle to check if some improvement is made.
titanic.svm.test<-svm(Survived~.,data=df,kernel='linear',cost=1,scale=FALSE)
titanic.svm.test<-predict(titanic.svm.test,newdata = testdf)
write.csv(titanic.svm.test,file='resultssvm.csv')
