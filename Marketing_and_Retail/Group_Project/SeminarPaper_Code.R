############################################################################################
###
###                 Seminar Paper - Evaluation 
###
###           Names: 
###
############################################################################################

### 1. Step: Read in data
getwd()
setwd("/Users/daniel/Dropbox/Master/Marketing and Retail/GW") # set working directory
setwd("C:/Users/Administrator/Downloads/")
setwd("~/Documents/Uni/Master/Courses/SS2018/MarketingAndRetail_sp/SeminarPaper")
market_old.df <- readxl::read_excel("Data.xlsx")
summary(market_old.df)
str(market_old.df)
market.df <- market_old.df

### delete non-meaningful variables ####
market.df$ID <- NULL
market.df$version <- NULL
market.df$interviewer <- NULL
market.df$Informatio_00043 <- NULL
market.df$Informatio_00062 <- NULL
########################################

### Variable 1
colnames(market.df)[colnames(market.df)=="Lebensmitt_00010/Yes"] <- "Lebensmitt_00010"
market.df$`Lebensmitt_00010/No` <- NULL

### Variable 2
colnames(market.df)[colnames(market.df)=="Lebensmitt_00011/Yes"] <- "Lebensmitt_00011"
market.df$`Lebensmitt_00011/No` <- NULL

### Variable 3
colnames(market.df)[colnames(market.df)=="Lebensmitt_00012/Yes"] <- "Lebensmitt_00012"
market.df$`Lebensmitt_00012/No` <- NULL

### Variable 4
colnames(market.df)[colnames(market.df)=="Lebensmitt_00013/Yes"] <- "Lebensmitt_00013"
market.df$`Lebensmitt_00013/No` <- NULL

### Variable 5
colnames(market.df)[colnames(market.df)=="Lebensmitt_00014/Yes"] <- "Lebensmitt_00014"
market.df$`Lebensmitt_00014/No` <- NULL

### Variable 6
colnames(market.df)[colnames(market.df)=="Lebensmitt_00015/Yes"] <- "Lebensmitt_00015"
market.df$`Lebensmitt_00015/No` <- NULL
market.df$Lebensmitt_00015

### Variable 7 pay attention doesn't determine amount of months
market.df$Lebensmitt_00016 <- market.df$`Lebensmitt_00016/I never purchase groceries`

for( i in 1:length(market.df$`Lebensmitt_00016/I never purchase groceries`) ) {
    if( market.df$`Lebensmitt_00016/I never purchase groceries`[i] == "1") {
        market.df$Lebensmitt_00016[i] <- 1 
    } else if (market.df$`Lebensmitt_00016/Less than once a month`[i]=="1"){
        market.df$Lebensmitt_00016[i] <- 2 
    } else if (market.df$`Lebensmitt_00016/Once a month`[i]=="1"){
      market.df$Lebensmitt_00016[i] <- 3 
    } else if (market.df$`Lebensmitt_00016/Twice a month`[i]=="1"){
      market.df$Lebensmitt_00016[i] <- 4 
    } else if (market.df$`Lebensmitt_00016/Three times a month`[i]=="1"){
      market.df$Lebensmitt_00016[i] <- 5 
    } else if (market.df$`Lebensmitt_00016/Once in a week`[i]=="1"){
      market.df$Lebensmitt_00016[i] <- 6
    } else {
      market.df$Lebensmitt_00016[i] <- 7 #more than once in a week
    }
}

market.df$`Lebensmitt_00016/I never purchase groceries` <- NULL
market.df$`Lebensmitt_00016/Less than once a month` <- NULL
market.df$`Lebensmitt_00016/Once a month` <- NULL
market.df$`Lebensmitt_00016/Twice a month` <- NULL
market.df$`Lebensmitt_00016/Three times a month` <- NULL
market.df$`Lebensmitt_00016/Once in a week` <- NULL
market.df$`Lebensmitt_00016/More than once a week` <- NULL

table(market.df$Lebensmitt_00016)

### Variable 8 pay attention doesn't determine amount of months
market.df$Lebensmitt_00017 <- market.df$`Lebensmitt_00017/1`

for( i in 1:length(market.df$`Lebensmitt_00017/1`) ) {
  if (is.na(market.df$`Lebensmitt_00017/1`[i])){
    market.df$Lebensmitt_00017[i] <- NA
  } else if( market.df$`Lebensmitt_00017/1`[i] == "1") {
    market.df$Lebensmitt_00017[i] <- 1 
  } else if (market.df$`Lebensmitt_00017/2`[i]=="1"){
    market.df$Lebensmitt_00017[i] <- 2 
  } else if (market.df$`Lebensmitt_00017/3`[i]=="1"){
    market.df$Lebensmitt_00017[i] <- 3 
  } else if (market.df$`Lebensmitt_00017/4`[i]=="1"){
    market.df$Lebensmitt_00017[i] <- 4 
  } else if (market.df$`Lebensmitt_00017/5`[i]=="1"){
    market.df$Lebensmitt_00017[i] <- 5 
  } else if (market.df$`Lebensmitt_00017/6`[i]=="1"){
    market.df$Lebensmitt_00017[i] <- 6
  } else {
    market.df$Lebensmitt_00017[i] <- 7
  }
}
market.df$Lebensmitt_00017

market.df$`Lebensmitt_00017/1` <- NULL
market.df$`Lebensmitt_00017/2` <- NULL
market.df$`Lebensmitt_00017/3` <- NULL
market.df$`Lebensmitt_00017/4` <- NULL
market.df$`Lebensmitt_00017/5` <- NULL
market.df$`Lebensmitt_00017/6`<- NULL
market.df$`Lebensmitt_00017/7` <- NULL

table(market.df$Lebensmitt_00017)


### Variable 8 pay attention doesn't determine amount of months
market.df$Lebensmitt_00025 <- market.df$`Lebensmitt_00025/less than 5 Minutes`

for( i in 1:length(market.df$`Lebensmitt_00025/less than 5 Minutes`) ) {
  if(is.na(market.df$`Lebensmitt_00025/less than 5 Minutes`[i])){
    market.df$Lebensmitt_00025[i] <- NA
  } else if (market.df$`Lebensmitt_00025/less than 5 Minutes`[i]){
    market.df$Lebensmitt_00025[i] <- 1
  } else if( market.df$`Lebensmitt_00025/6-10 Minutes`[i] == "1") {
    market.df$Lebensmitt_00025[i] <- 2 
  } else if (market.df$`Lebensmitt_00025/11-20 Minutes`[i]=="1"){
    market.df$Lebensmitt_00025[i] <- 3 
  } else if (market.df$`Lebensmitt_00025/21-30 Minutes`[i]=="1"){
    market.df$Lebensmitt_00025[i] <- 4 
  } else {
    market.df$Lebensmitt_00025[i] <- 5 # more than 30minutes
  }
}
market.df$Lebensmitt_00025

market.df$`Lebensmitt_00025/less than 5 Minutes` <- NULL
market.df$`Lebensmitt_00025/6-10 Minutes` <- NULL
market.df$`Lebensmitt_00025/11-20 Minutes` <- NULL
market.df$`Lebensmitt_00025/21-30 Minutes` <- NULL
market.df$`Lebensmitt_00025/more than 30 Minutes` <- NULL

table(market.df$Lebensmitt_00025)

### Variable 9 pay attention doesn't determine amount of months
market.df$Online_00049 <- market.df$`Online_00049/Never`

for( i in 1:length(market.df$`Online_00049/Never`) ) {
  if(is.na(market.df$`Online_00049/Never`[i])){
    market.df$Online_00049[i] <- NA
  } else if (market.df$`Online_00049/Never`[i]){
    market.df$Online_00049[i] <- 1
  } else if( market.df$`Online_00049/Less often`[i] == "1") {
    market.df$Online_00049[i] <- 2 
  } else if (market.df$`Online_00049/2-3 times a year`[i]=="1"){
    market.df$Online_00049[i] <- 3 
  } else if (market.df$`Online_00049/Every 2-3 months`[i]=="1"){
    market.df$Online_00049[i] <- 4 
  } else if (market.df$`Online_00049/Monthly`[i]=="1"){
    market.df$Online_00049[i] <- 5 
  } else if (market.df$`Online_00049/Several times a month`[i]=="1"){
    market.df$Online_00049[i] <- 6 
  } else if (market.df$`Online_00049/Weekly`[i]=="1"){
    market.df$Online_00049[i] <- 7 
  }else {
    market.df$Online_00049[i] <- 8 #several times a week
  }
}
market.df$Online_00049

market.df$`Online_00049/Never` <- NULL
market.df$`Online_00049/Less often` <- NULL
market.df$`Online_00049/2-3 times a year` <- NULL
market.df$`Online_00049/Every 2-3 months` <- NULL
market.df$`Online_00049/Monthly` <- NULL
market.df$`Online_00049/Several times a month` <- NULL
market.df$`Online_00049/Weekly` <- NULL
market.df$`Online_00049/Several times a week` <- NULL
table(market.df$Lebensmitt_00049)

### Variable 10 
market.df$`Online_00074/yes`
colnames(market.df)[colnames(market.df)=="Online_00074/yes"] <- "Online_00074"
market.df$`Online_00074/no` <- NULL

### Variable 11 
market.df$`Demographi_00036/female`
colnames(market.df)[colnames(market.df)=="Demographi_00036/female"] <- "gender"
market.df$gender <- ifelse(market.df$gender==1, "female", "male")
market.df$gender
market.df$`Demographi_00036/male` <- NULL

### Variable 12 
market.df$Demographi_00051 <- market.df$`Demographi_00051/full-time employed`

for( i in 1:length(market.df$`Demographi_00051/full-time employed`) ) {
  if(is.na(market.df$`Demographi_00051/full-time employed`[i])){
    market.df$Demographi_00051[i] <- NA
  } else if (market.df$`Demographi_00051/full-time employed`[i]){
    market.df$Demographi_00051[i] <- 1
  } else if( market.df$`Demographi_00051/homekeeper`[i] == "1") {
    market.df$Demographi_00051[i] <- 2 
  } else if (market.df$`Demographi_00051/not working`[i]=="1"){
    market.df$Demographi_00051[i] <- 3 
  } else if (market.df$`Demographi_00051/part-time employed`[i]=="1"){
    market.df$Demographi_00051[i] <- 4 
  } else if (market.df$`Demographi_00051/retired`[i]=="1"){
    market.df$Demographi_00051[i] <- 5 
  } else if (market.df$`Demographi_00051/self-employed`[i]=="1"){
    market.df$Demographi_00051[i] <- 6 
  } else {
    market.df$Demographi_00051[i] <- 7 #student
  }
}
market.df$Demographi_00051

market.df$`Demographi_00051/full-time employed` <- NULL
market.df$`Demographi_00051/homekeeper` <- NULL
market.df$`Demographi_00051/not working` <- NULL
market.df$`Demographi_00051/part-time employed` <- NULL
market.df$`Demographi_00051/retired` <- NULL
market.df$`Demographi_00051/self-employed` <- NULL
market.df$`Demographi_00051/student` <- NULL

### Variable 13
market.df$Demographi_00053 <- market.df$`Demographi_00053/I have enough time for myself`

for( i in 1:length(market.df$`Demographi_00053/I have enough time for myself`) ) {
  if(is.na(market.df$`Demographi_00053/I have enough time for myself`[i])){
    market.df$Demographi_00053[i] <- NA
  } else if (market.df$`Demographi_00053/I have enough time for myself`[i]){
    market.df$Demographi_00053[i] <- 3
  } else if( market.df$`Demographi_00053/I have hardly any time for myself`[i] == "1") {
    market.df$Demographi_00053[i] <- 4
  } else if (market.df$`Demographi_00053/I have much time for myself`[i]=="1"){
    market.df$Demographi_00053[i] <- 2
  } else if (market.df$`Demographi_00053/I have no time for myself`[i]=="1"){
    market.df$Demographi_00053[i] <- 5
  } else {
    market.df$Demographi_00053[i] <- 1 # I have very much time for myself
  }
}
market.df$Demographi_00053

market.df$`Demographi_00053/I have enough time for myself` <- NULL
market.df$`Demographi_00053/I have hardly any time for myself` <- NULL
market.df$`Demographi_00053/I have much time for myself` <- NULL
market.df$`Demographi_00053/I have no time for myself` <- NULL
market.df$`Demographi_00053/I have very much time for myself` <- NULL

### Variable 14
market.df$Demographi_00039 <- market.df$`Demographi_00039/getting along`

for( i in 1:length(market.df$`Demographi_00039/getting along`) ) {
  if(is.na(market.df$`Demographi_00039/getting along`[i])){
    market.df$Demographi_00039[i] <- NA
  } else if (market.df$`Demographi_00039/getting along`[i]){
    market.df$Demographi_00039[i] <- 3
  } else if( market.df$`Demographi_00039/getting along hardly`[i] == "1") {
    market.df$Demographi_00039[i] <- 4 
  } else if (market.df$`Demographi_00039/living comfortably`[i]=="1"){
    market.df$Demographi_00039[i] <- 2
  } else if (market.df$`Demographi_00039/living very comfortably`[i]=="1"){
    market.df$Demographi_00039[i] <- 1
  } else {
    market.df$Demographi_00039[i] <- 5 #not getting along
  }
}
market.df$Demographi_00039

market.df$`Demographi_00039/getting along` <- NULL
market.df$`Demographi_00039/getting along hardly` <- NULL
market.df$`Demographi_00039/living comfortably`<- NULL
market.df$`Demographi_00039/living very comfortably` <- NULL
market.df$`Demographi_00039/not getting along` <- NULL


### Variable 15
market.df$Demographi_00052 <- market.df$`Demographi_00052/apprenticeship`

for( i in 1:length(market.df$`Demographi_00052/apprenticeship`) ) {
  if(is.na(market.df$`Demographi_00052/apprenticeship`[i])){
    market.df$Demographi_00052[i] <- NA
  } else if (market.df$`Demographi_00052/apprenticeship`[i]){
    market.df$Demographi_00052[i] <- 3
  } else if( market.df$`Demographi_00052/compulsory school`[i] == "1") {
    market.df$Demographi_00052[i] <- 4
  } else if (market.df$`Demographi_00052/graduate`[i]=="1"){
    market.df$Demographi_00052[i] <- 1
  } else if (market.df$`Demographi_00052/matriculation`[i]=="1"){
    market.df$Demographi_00052[i] <- 2
  } else {
    market.df$Demographi_00052[i] <- 5 #none
  }
}
table(market.df$Demographi_00052)

market.df$`Demographi_00052/apprenticeship` <- NULL
market.df$`Demographi_00052/compulsory school` <- NULL
market.df$`Demographi_00052/graduate`<- NULL
market.df$`Demographi_00052/matriculation` <- NULL
market.df$`Demographi_00052/none` <- NULL


### Variable 16 -> Transforming zip codes into states
str(market.df$Demographi_00050)

market.df$Demographi_plz <- market.df$Demographi_00050

market.df$Demographi_plz[market.df$Demographi_plz < 2000] <- 'Vienna'
market.df$Demographi_plz[market.df$Demographi_plz > 1999 & market.df$Demographi_plz < 3000] <- 'Lower Austria'
market.df$Demographi_plz[market.df$Demographi_plz > 2999 & market.df$Demographi_plz < 4000] <- 'Lower Austria'
market.df$Demographi_plz[market.df$Demographi_plz > 3999 & market.df$Demographi_plz < 5000] <- 'Upper Austria'
market.df$Demographi_plz[market.df$Demographi_plz > 4999 & market.df$Demographi_plz < 6000] <- 'Salzburg'
market.df$Demographi_plz[market.df$Demographi_plz > 5999 & market.df$Demographi_plz < 7000] <- 'Tyrol and Vorarlberg'
market.df$Demographi_plz[market.df$Demographi_plz > 6999 & market.df$Demographi_plz < 8000] <- 'Burgenland'
market.df$Demographi_plz[market.df$Demographi_plz > 7999 & market.df$Demographi_plz < 9000] <- 'Styria'
market.df$Demographi_plz[market.df$Demographi_plz > 8999 & market.df$Demographi_plz <= 9999] <- 'Carinthia and East-Tyrol'

str(market.df$Demographi_plz)
market.df$Demographi_plz

### Variable 17 -> removing Outlier for Lebensmitt_00019
market.df$Lebensmitt_00019[market.df$Lebensmitt_00019 > 50] <- NA
market.df$Lebensmitt_00019

descriptive.df <- market.df

########## Descriptive Analysis - Univariate ###########

### Do people have heard prior this study about the possibility of buying groceries online?
descriptive.df$Lebensmitt_00010 <- factor(descriptive.df$Lebensmitt_00010, labels = c("No","Yes"))
table(descriptive.df$Lebensmitt_00010) #yes = 522, no = 16

### Are people interested in buying groceries online?
descriptive.df$Lebensmitt_00011 <- factor(descriptive.df$Lebensmitt_00011, labels = c("No","Yes"))
table(descriptive.df$Lebensmitt_00011) #yes = 322, no = 216

### Did people inform themselve about online buying groceries online prior this study?
descriptive.df$Lebensmitt_00012 <- factor(descriptive.df$Lebensmitt_00012, labels = c("No","Yes"))
table(descriptive.df$Lebensmitt_00012) #yes = 326, no = 212

### Do people plan to buy groceries online within 2 months?
descriptive.df$Lebensmitt_00013 <- factor(descriptive.df$Lebensmitt_00013, labels = c("No","Yes"))
table(descriptive.df$Lebensmitt_00013) #yes = 203, no = 335

### Did people already buy groceries online?
descriptive.df$Lebensmitt_00014 <- factor(descriptive.df$Lebensmitt_00014, labels = c("No","Yes"))
table(descriptive.df$Lebensmitt_00014) #yes = 345, no = 193

### Where people satisfied with their last online purchase of groceries?
descriptive.df$Lebensmitt_00015 <- factor(descriptive.df$Lebensmitt_00015, labels = c("No","Yes"))
table(descriptive.df$Lebensmitt_00015) #yes = 179, no = 14

### How often do people buy groceries personally?
descriptive.df$Lebensmitt_00016 <- factor(descriptive.df$Lebensmitt_00016, labels=c("Never", "Less than once a month","Once a month","Twice a month","3 times a month","Once a week","More than once a week")) #notch -> huge gap = will be statistically significant)
plot(descriptive.df$Lebensmitt_00016, 
     ylab="Count",
     main="How often do you buy groceries personally?")

### On how many days of a week do you buy groceries personally 
descriptive.df$Lebensmitt_00017 <- factor(descriptive.df$Lebensmitt_00017, labels=c(1,2,3,4,5,6,7))
plot(descriptive.df$Lebensmitt_00017, 
     ylab="Count",
     main="On how many days of a week do you buy groceries personally?",
     xlab="Days")
table(descriptive.df$Lebensmitt_00017)

### On average, how much do you spend per week on groceries (in EURO)?
boxplot(descriptive.df$Lebensmitt_00018,
        main="Average amount spent on groceries/week",
        ylab="Amount")
mean(descriptive.df$Lebensmitt_00018)
median(descriptive.df$Lebensmitt_00018)

### For how many persons do you typically purchase groceries (including you)?
boxplot(descriptive.df$Lebensmitt_00019, 
        main="Amount of people groceries are bought for",
        ylab="Amount")
mean(na.omit(descriptive.df$Lebensmitt_00019)) # there is at least one outlier
median(na.omit(descriptive.df$Lebensmitt_00019))

### Imagine a typical purchase of groceries. How often are the following products part of your purchase?(e.g., 1 = 10% of purchases)
boxplot(descriptive.df$`Lebensmitt_00020/Meat (incl. fish and poultry)`, 
        descriptive.df$`Lebensmitt_00020/Dairy products (incl. Eggs and cheese)`,
        descriptive.df$`Lebensmitt_00020/frozen food`,
        descriptive.df$`Lebensmitt_00020/fresh fruit`,
        descriptive.df$`Lebensmitt_00020/fresh vegetables`,
        descriptive.df$`Lebensmitt_00020/bread and pastries`,
        descriptive.df$`Lebensmitt_00020/beverages`,
        descriptive.df$`Lebensmitt_00020/convenience food`,
        descriptive.df$`Lebensmitt_00020/sausage and ham`,
        descriptive.df$`Lebensmitt_00020/tinned food`,
        main="How often are the following products part of your purchase?",
        ylab="Count",
        xaxt="n")
axis(side=1, at=c(1:10), labels=c("Meat", "Dairy products","Frozen food","Fresh fruit","fresh vegetables",
                                  "bread and pastries","beverages",
                                  "convenience food", "sausage and ham", "tinned food"))

### How much time do you require to your next possibility to purchase groceries?
descriptive.df$Lebensmitt_00025 <- factor(descriptive.df$Lebensmitt_00025, labels=c("Less than 5", "6-10","11-20","21-30","More than 30"))
plot(descriptive.df$Lebensmitt_00025, col="lightblue", main="Required time (in minutes) to next possibility to purchase groceries")
table(descriptive.df$Lebensmitt_00025)

### How do you typically reach this location
descriptive.df$`Lebensmitt_00031/by foot` <- factor(descriptive.df$`Lebensmitt_00031/by foot`, labels=c("No","Yes"))
descriptive.df$`Lebensmitt_00031/bicylce` <- factor(descriptive.df$`Lebensmitt_00031/bicylce`, labels=c("No","Yes"))
descriptive.df$`Lebensmitt_00031/public transport` <- factor(descriptive.df$`Lebensmitt_00031/public transport`, labels=c("No","Yes"))
descriptive.df$`Lebensmitt_00031/car` <- factor(descriptive.df$`Lebensmitt_00031/car`, labels=c("No","Yes"))
descriptive.df$`Lebensmitt_00031/taxi` <- factor(descriptive.df$`Lebensmitt_00031/taxi`, labels=c("No","Yes"))
descriptive.df$`Lebensmitt_00031/motorcycle` <- factor(descriptive.df$`Lebensmitt_00031/motorcycle``, labels=c("No","Yes"))


boxplot(jitter(market.df$`Lebensmitt_00031/by foot`),
        jitter(market.df$`Lebensmitt_00031/bicylce`),
        jitter(market.df$`Lebensmitt_00031/public transport`),
        jitter(market.df$`Lebensmitt_00031/car`),
        jitter(market.df$`Lebensmitt_00031/taxi`),
        jitter(market.df$`Lebensmitt_00031/motorcycle`))

table(descriptive.df$`Lebensmitt_00031/by foot`,
      descriptive.df$`Lebensmitt_00031/bicylce`,
      descriptive.df$`Lebensmitt_00031/public transport`,
      descriptive.df$`Lebensmitt_00031/car`,
      descriptive.df$`Lebensmitt_00031/taxi`,
      descriptive.df$`Lebensmitt_00031/motorcycle`)

### At which of these grocery stores did you purchase at least once within the last year?
table(descriptive.df$`Lebensmitt_00063/Spar / Eurospar`)
str(descriptive.df[26])

### Which shops do you prefer? Please rank.
boxplot(descriptive.df$`Lebensmitt_00021/Hofer`,
        descriptive.df$`Lebensmitt_00021/Billa`,
        descriptive.df$`Lebensmitt_00021/Adeg`,
        descriptive.df$`Lebensmitt_00021/Interspar`,
        descriptive.df$`Lebensmitt_00021/Lidl`,
        descriptive.df$`Lebensmitt_00021/Merkur`,
        descriptive.df$`Lebensmitt_00021/Mpreis`,
        descriptive.df$`Lebensmitt_00021/Nah&Frisch`,
        descriptive.df$`Lebensmitt_00021/Penny`,
        descriptive.df$`Lebensmitt_00021/Spar / Eurospar`,
        descriptive.df$`Lebensmitt_00021/Sutterlüty`,
        descriptive.df$`Lebensmitt_00021/Unimarkt`,
     main="Which shops do you prefer?",
     ylab="Ranking",
     xaxt="n")
axis(side=1, at=c(1:12), labels=c("Hofer", "Billa","Adeg","Interspar","Lidl", "Merkur",
                                  "Mpreis", "Nah & Frisch", "Penny", "Spar/Eurospar", "Sutterlüty", "Unimarkt"))

### Please rank the following attributes describing an online purchase according to your personal preference
boxplot(descriptive.df$`Attributreihung/Delay of delivery`,
        descriptive.df$`Attributreihung/Delivery day`,
        descriptive.df$`Attributreihung/Delivery fee`,
        descriptive.df$`Attributreihung/Discount`,
        descriptive.df$`Attributreihung/Remaining shelf life`,
        descriptive.df$`Attributreihung/Time window width`,
        main="Ranking attributes describing an online purchase",
        ylab="Ranking",
        xaxt="n")
axis(side=1, at=c(1:6), labels=c("Delay of delivery", "Delivery Day","Delivery Fee","Discount","Remaining shelf life", "Time window width"))

### Imagine you could delegate your purchase of groceries to a third person. 
### This person will purchase the items according to you shopping list at your 
### favorite store and delivers the product to your home. Your attendance is not 
### required. What are you willing to pay for such a service (in Euro)?
boxplot(descriptive.df$Online_00064, main="Willignness to pay", ylab= "in Euro")
plot(descriptive.df$Online_00064) # one outlier

### For which time-windows are you able to receive your goods at home?
boxplot(jitter(descriptive.df$`Online_00047/00:00-02:00`),
        jitter(descriptive.df$`Online_00047/02:00-04:00`),
      jitter(descriptive.df$`Online_00047/04:00-06:00`),
      jitter(descriptive.df$`Online_00047/06:00-08:00`),
      jitter(descriptive.df$`Online_00047/08:00-10:00`),
      jitter(descriptive.df$`Online_00047/10:00-12:00`),
      jitter(descriptive.df$`Online_00047/12:00-14:00`))

### How often do you purchase online in general (not only groceries)?
descriptive.df$Online_00049 <- factor(descriptive.df$Online_00049, labels=c("Never", "Less often","2-3 times a year","Every 2-3 months","Monthly", "Several times a month", "Weekly","Several times a week"))
plot(descriptive.df$Online_00049, col="lightblue", ylab="Amount", main="How often do you buy online in general?")

### Do you do those purchases/actions more often online (=1) or offline (=5)?
boxplot(descriptive.df$`Online_00048/Bank transactions`,
        descriptive.df$`Online_00048/Books / DVDs / eGames`,
        descriptive.df$`Online_00048/Electronics (TV / PC / ...)`,
        descriptive.df$`Online_00048/Food (Delivery service / Take-away)`,
        descriptive.df$`Online_00048/Gambling & Betting`,
        descriptive.df$`Online_00048/Groceries`,
        descriptive.df$`Online_00048/Hotels / Flights`,
        descriptive.df$`Online_00048/Software / Hardware`,
        descriptive.df$`Online_00048/Sporting goods`,
        descriptive.df$`Online_00048/Textiles`,
        descriptive.df$`Online_00048/Tickets for events`,
        ylab="Preference",
        main="What do people buy more offline(=5) or online (=1)",
        xaxt="n")
axis(side=1, at=c(1:11), labels=c("Bank transactions", "Books, DVDs, Games","Electronics",
"Food", "Gambling/Betting", "Groceries", "Hotels/Flights", "Software/Hardware", 
"Sporting goods", "Textiles", "Tickets for events"))

### Do you prefer to be at home at the time of the delivery?
descriptive.df$Online_00074 <- factor(descriptive.df$Online_00074, labels=c("No","Yes"))
table(descriptive.df$Online_00074) ### yes=437, no=98

### Age
boxplot(market.df$Demographi_00035, main="Age of the respondents",ylab="Age")
mean(market.df$Demographi_00035)
median(market.df$Demographi_00035)
hist(market.df$Demographi_00035) ### slightly left-skewed

### Gender
table(market.df$gender)

### Number of people in your household
table(market.df$Demographi_00037)
hist(market.df$Demographi_00037,breaks=9)

### What describes your financial situation best?
descriptive.df$Demographi_00039 <- factor(descriptive.df$Demographi_00039, labels=c("living very comfortably", "living comfortably","Getting along", "Getting along hardly", "not getting along"))
plot(descriptive.df$Demographi_00039, col="lightblue",main="What describes your financial situation best?")

### What describes your available spare time best? 
descriptive.df$Demographi_00053 <- factor(descriptive.df$Demographi_00053, labels=c("very much", "much","enough", "hardly any", "no"))
plot(descriptive.df$Demographi_00053, col="lightblue",main="What describes your available spare time best? I have ... time for myself")

### Zip code
table(descriptive.df$Demographi_plz)

### Employment status
descriptive.df$Demographi_00051 <- factor(descriptive.df$Demographi_00051, labels=c("Full-time employed", "homekeeper","not working", "part-time employed", "retired", "self-employed","student"))
plot(descriptive.df$Demographi_00051, col="lightblue",main="Employment Status")

### Highest education
descriptive.df$Demographi_00052 <- factor(descriptive.df$Demographi_00052, labels=c("Apprenticeship", "compulsory school","graduate","matriculation", "none"))
plot(descriptive.df$Demographi_00052, col="lightblue",main="Highest Education")

########## Descriptive Analysis - Bivariate ###########

### Does the interest in purchasing groceries online depend on the age?
boxplot(descriptive.df$Demographi_00035~ descriptive.df$Lebensmitt_00011, ylab = "Age", 
        main = "Does the interest in purchasing groceries online depend on the age?")
cor(market.df$Demographi_00035, market.df$Lebensmitt_00011)

### Does the interest in purchasing groceries online depend on the location?
descriptive.df$Demographi_plz <- factor(descriptive.df$Demographi_plz)
plot(descriptive.df$Demographi_plz)
boxplot(jitter(market.df$Lebensmitt_00011)~ market.df$Demographi_plz, ylab = "Zip Code", 
        main = "Does the interest in purchasing groceries online depend on the plz?")

### Does the age influence on how often do people buy groceries personally
boxplot(market.df$Demographi_00035~ descriptive.df$Lebensmitt_00016, ylab ="Age", main = "Influence of age on number of buying groceries personally")

### Does the age influence the amount spent on groceries?
plot(market.df$Demographi_00035~ market.df$Lebensmitt_00018, ylab ="Age", xlab = "Amount spent on groceries")
      
### Does the financial situation influence the willingness to buy groceries online within the next two months?
boxplot(jitter(market.df$Lebensmitt_00013)~descriptive.df$Demographi_00039)

### Does the financial situation have an influence on the preference of the grocery store?
boxplot(market.df$`Lebensmitt_00021/Billa`~descriptive.df$Demographi_00039)
boxplot(market.df$`Lebensmitt_00021/Hofer`~descriptive.df$Demographi_00039)
boxplot(market.df$`Lebensmitt_00021/Sutterlüty`~descriptive.df$Demographi_00039)

###Does the distance have an influence on the willingness to buy stuff online? ### should be rechecked
boxplot(market.df$Lebensmitt_00025 ~ descriptive.df$Lebensmitt_00012, main="Test")
table(descriptive.df$Lebensmitt_00012)
table(descriptive.df$Lebensmitt_00011)
table(descriptive.df$`Attributreihung/Time window width`)

library(gpairs)
gpairs(market.df[ , c(5:5)])

### Available spare time compared to whether people already bought groceries online
boxplot(jitter(market.df$Lebensmitt_00014)~ descriptive.df$Demographi_00053)

### Education level compared to whether people buy stuff online (not only groceries)
boxplot(market.df$Online_00049~descriptive.df$Demographi_00052) # the higher the education lvl, the more the ppl buy stuff online

### Employment status compared whether people buy stuff online (not only groceries)
boxplot(market.df$Online_00049~descriptive.df$Demographi_00051) # not correlated

### Number of ppl in Household compared whether people buy stuff online (not only groceries)

#Money spent compared to Number of HH members
boxplot(market.df$Lebensmitt_00018~market.df$Demographi_00037)
which(market.df$Demographi_00037==8)

#Money spent compared to the financial situation of the household
boxplot(market.df$Lebensmitt_00018~descriptive.df$Demographi_00039) #not dependent, basically spending the same amount on average
#no matter how much they earn.

#### SEGMENTATION #### 
## HIERARCHICAL CLUSTERING - WITH First 6 vars
library(cluster)

colnames(market.df)
seg.dist3<-daisy(market.df[,c(1:6)])
seg.hc3<-hclust(seg.dist3,method="ward.D2")
plot(seg.hc3)
plot(seg.hc3$height[520:540])
seg.hc.segment3<-cutree(seg.hc3, k=3)
table(seg.hc.segment3)

### based on product group
boxplot(market.df$`Lebensmitt_00020/beverages`~seg.hc.segment3, main = "Beverages",ylab = "Importance for purchase", xlab = "Cluster", notch = FALSE) 
boxplot(market.df$`Lebensmitt_00020/fresh fruit`~seg.hc.segment3, main = "Fresh Fruit",ylab = "Importance for purchase", xlab = "Cluster", notch = FALSE) 
boxplot(market.df$`Lebensmitt_00020/bread and pastries` ~seg.hc.segment3, main = "Bread and pastries",ylab = "Importance for purchase", xlab = "Cluster", notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/convenience food` ~seg.hc.segment3, main = "Convenience food",ylab = "Importance for purchase", xlab = "Cluster", notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/fresh vegetables` ~seg.hc.segment3, main = "Fresh vegetables",ylab = "Importance for purchase", xlab = "Cluster", notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/sausage and ham` ~seg.hc.segment3, main = "Sausage and ham",ylab = "Importance for purchase", xlab = "Cluster", notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/tinned food` ~seg.hc.segment3, main = "Tinned food",ylab = "Importance for purchase", xlab = "Cluster", notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/frozen food` ~seg.hc.segment3, main = "Frozen food",ylab = "Importance for purchase", xlab = "Cluster", notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/Meat (incl. fish and poultry)` ~seg.hc.segment3, main = "Meat",ylab = "Importance for purchase", xlab = "Cluster", notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/Dairy products (incl. Eggs and cheese)` ~seg.hc.segment3, main = "Dairy products",ylab = "Importance for purchase", xlab = "Cluster", notch = FALSE)

### more often online or offline
boxplot(jitter(market.df$`Online_00048/Bank transactions`) ~seg.hc.segment3, main = "Bank transactions", ylab = "Online - Offline", xlab = "Cluster", notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Books / DVDs / eGames`) ~seg.hc.segment3, main = "Books", ylab = "Online - Offline", xlab = "Cluster", notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Electronics (TV / PC / ...)`)~ seg.hc.segment3, main = "Elecronics", ylab = "Online - Offline", xlab = "Cluster", notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Food (Delivery service / Take-away)`)~ seg.hc.segment3, main = "Delivery service", ylab = "Online - Offline", xlab = "Cluster", notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Gambling & Betting`)~ seg.hc.segment3, main = "Gambling & Betting", ylab = "Online - Offline", xlab = "Cluster", notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Groceries`)~ seg.hc.segment3, main = "Groceries", ylab = "Online - Offline", xlab = "Cluster", notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Hotels / Flights`)~ seg.hc.segment3, main = "Hotel / Flights", ylab = "Online - Offline", xlab = "Cluster", notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Software / Hardware`)~ seg.hc.segment3, main = "Software / Hardware", ylab = "Online - Offline", xlab = "Cluster", notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Sporting goods`)~ seg.hc.segment3, main = "Sporting goods", ylab = "Online - Offline", xlab = "Cluster", notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Textiles`)~ seg.hc.segment3, main = "Textiles", ylab = "Online - Offline", xlab = "Cluster", notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Tickets for events`)~ seg.hc.segment3, main = "Tickets for events", ylab = "Online - Offline", xlab = "Cluster", notch = FALSE)

######DEMOGRAPHICS
boxplot(jitter(market.df$Demographi_00039)~seg.hc.segment3, main = "Financial Situation", ylab = "Online - Offline", xlab = "Cluster", notch = FALSE) #FIN SITUATION
boxplot(jitter(market.df$Demographi_00035)~seg.hc.segment3, main = "Age per cluster", ylab = "Age", xlab = "Cluster", notch = FALSE) #AGE
boxplot(jitter(market.df$Demographi_00037)~seg.hc.segment3, main = "Number of people in household", ylab = "Amount", xlab = "Cluster", notch = FALSE) #NR OF PPL IN HH
boxplot(jitter(market.df$Demographi_00053)~seg.hc.segment3, main = "Available Spare Time", ylab = "much time - no time", xlab = "Cluster", notch = FALSE) #SPARE TIME
#boxplot(jitter(market.df$Demographi_00051)~seg.hc.segment3, main = "", ylab = "", xlab = "Cluster", notch = FALSE) #EMPL STATUS
boxplot(jitter(market.df$Demographi_00052)~seg.hc.segment3, main = "Educational level per cluster", ylab = "none - graduate", xlab = "Cluster", notch = FALSE) #EDUCATION

boxplot(jitter(market.df$Lebensmitt_00011)~seg.hc.segment3, main = "Interested in purchasing groceries online?", ylab = "no - yes", xlab = "Cluster",notch = FALSE) # interested in purch. online
boxplot(jitter(market.df$Lebensmitt_00013)~seg.hc.segment3, main = "Planning to buy groceries online within 2 months", ylab = "no - yes", xlab = "Cluster", notch = FALSE) # planning in next 2 months
boxplot(jitter(market.df$Lebensmitt_00014)~seg.hc.segment3, main = "Did people buy already groceries online?", ylab = "no - yes", xlab = "Cluster",notch = FALSE) # purchased already
#boxplot(jitter(market.df$Lebensmitt_00015)~seg.hc.segment3, notch = FALSE) # satisfied already
boxplot(jitter(market.df$Lebensmitt_00016)~seg.hc.segment3, main = "Frequency of purchasing groceries personally", ylab = "Never - More than once a week", xlab = "Cluster", notch = FALSE) # how often personally
boxplot(jitter(market.df$Lebensmitt_00018)~seg.hc.segment3, main = "Amount spent for groceries per week (in Euro)", ylab = "Amount", xlab = "Cluster", notch = FALSE) #AMOUNT SPENT

boxplot(jitter(market.df$Lebensmitt_00025)~seg.hc.segment3, main = "Required time to next store", ylab = "less than 5 minutes - more than 30 minutes", xlab = "Cluster", notch = FALSE) # Time to the shop
boxplot(jitter(market.df$`Lebensmitt_00031/by foot`)~seg.hc.segment3, main = "Reach the store on foot", ylab = "no - yes", xlab = "Cluster", notch = FALSE) # HOW DO YOU REACH - #2 interesting, 
#probably dont have a car, that is why they go by foot, not able to take a lot of groceries with them
boxplot(jitter(market.df$`Lebensmitt_00031/car`)~seg.hc.segment3, main = "Reach the store by car", ylab = "no - yes", xlab = "Cluster", notch = FALSE) # HOW DO YOU REACH - #2 interesting
boxplot(jitter(market.df$`Lebensmitt_00021/Merkur`)~seg.hc.segment3, notch = FALSE) # HOW DO YOU REACH - #2 interesting
boxplot(jitter(market.df$Online_00049)~seg.hc.segment3, main = "Amount of shopping online in general", ylab = "never - several times a week", xlab = "Cluster", notch = FALSE) # How often online - in general #2 slightly better
boxplot(jitter(market.df$`Online_00065/In…% of my purchases`)~seg.hc.segment3, main = "Willingness to delegate online purchases to third persons", ylab = "low - high", xlab = "Cluster", notch = FALSE) #Delegate to another person; #2 slightly better

### reporting means per group
seg.summ(market.df[], seg.hc.segment3)


##################### Cemetery ###########################

# base variables
market.df$Lebensmitt_00011 #2
market.df$Lebensmitt_00013 #4
market.df$Lebensmitt_00014 #5
market.df$Lebensmitt_00015 #6
market.df$Lebensmitt_00018 #8
market.df$Lebensmitt_00019 #9
market.df$`Lebensmitt_00020/Meat (incl. fish and poultry)` #10
market.df$`Lebensmitt_00020/Dairy products (incl. Eggs and cheese)`
market.df$`Lebensmitt_00020/frozen food`
market.df$`Lebensmitt_00020/fresh fruit`
market.df$`Lebensmitt_00020/fresh vegetables`
market.df$`Lebensmitt_00020/bread and pastries`
market.df$`Lebensmitt_00020/beverages`
market.df$`Lebensmitt_00020/convenience food`
market.df$`Lebensmitt_00020/sausage and ham`
market.df$`Lebensmitt_00020/tinned food` #19
market.df$Lebensmitt_00025 #97
market.df$`Attributreihung/Delay of delivery` #53
market.df$`Attributreihung/Delivery day` #56
market.df$`Attributreihung/Delivery fee` #57
market.df$`Attributreihung/Discount` #55 
market.df$`Attributreihung/Remaining shelf life` #52
market.df$`Attributreihung/Time window width` #54

market.df$Online_00064 # 58
market.df$`Online_00065/In % of my purchases` #59
market.df$Online_00049 #98

library(cluster)                  
#seg.dist <- daisy(market.df[,c(2:5, 7:8, 10:19, 97, 58:59, 79:89, 98)])
#seg.dist <- daisy(market.df[,c(2:5, 7:8, 10:19, 97, 53:59, 79:89, 98)])
seg.dist <- daisy(market.df[,c(2,4:6,8:19, 53:59, 79:89, 98)])
#seg.dist <- daisy(market.df[,c(1:])
colnames(market.df)

### KMEANS

market.df.num <- market.df[,c(2:19, 97, 53:59, 98)]


market.k <- kmeans(na.omit(market.df.num), centers=4, nstart = 50)
market.k$size
sum(market.k$withinss)

boxplot(market.df.num$Online_00064 ~ market.k$cluster, ylab="Income", xlab="Cluster")

### ALL VARIABLES #########
colnames(market.df)
seg.dist2<-daisy(market.df[,c(1:90,95:98,101)])
seg.hc2<-hclust(seg.dist2,method="ward.D2")
plot(seg.hc2)
plot(seg.hc2$height[520:540])
seg.hc.segment2 <- cutree(seg.hc2, k=7)
table(seg.hc.segment2) 


#AMOUNT SPENT - not significant differences
boxplot(market.df$Lebensmitt_00018~seg.hc.segment2, notch = FALSE) 

### based on product group
boxplot(market.df$`Lebensmitt_00020/beverages`~seg.hc.segment2, notch = FALSE) 
boxplot(market.df$`Lebensmitt_00020/fresh fruit`~seg.hc.segment2, notch = FALSE) 
boxplot(market.df$`Lebensmitt_00020/bread and pastries` ~seg.hc.segment2, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/convenience food` ~seg.hc.segment2, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/fresh vegetables` ~seg.hc.segment2, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/sausage and ham` ~seg.hc.segment2, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/tinned food` ~seg.hc.segment2, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/frozen food` ~seg.hc.segment2, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/Meat (incl. fish and poultry)` ~seg.hc.segment2, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/Dairy products (incl. Eggs and cheese)` ~seg.hc.segment2, notch = FALSE)

### more often online or offline - SEGMENT 3 LOOKS PROMISING?
boxplot(jitter(market.df$`Online_00048/Bank transactions`) ~seg.hc.segment2, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Books / DVDs / eGames`) ~seg.hc.segment2, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Electronics (TV / PC / ...)`)~ seg.hc.segment2, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Food (Delivery service / Take-away)`)~ seg.hc.segment2, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Gambling & Betting`)~ seg.hc.segment2, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Groceries`)~ seg.hc.segment2, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Hotels / Flights`)~ seg.hc.segment2, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Software / Hardware`)~ seg.hc.segment2, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Sporting goods`)~ seg.hc.segment2, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Textiles`)~ seg.hc.segment2, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Tickets for events`)~ seg.hc.segment2, notch = FALSE)

### based on spare time
boxplot(market.df$Demographi_00039 ~seg.hc.segment2, notch = FALSE)
summary(market.df)


### interested in purch. online
boxplot(jitter(market.df$Lebensmitt_00011)~seg.hc.segment2, notch = FALSE) 
### planning to do groc. online in the next two month
boxplot(jitter(market.df$Lebensmitt_00013)~seg.hc.segment2, notch = FALSE) # Cluster 2 
boxplot(jitter(market.df$Lebensmitt_00014)~seg.hc.segment2, notch = FALSE) # Cluster 2 
table(seg.hc.segment2)


boxplot(jitter(market.df$Demographi_00035)~seg.hc.segment2, notch = FALSE) 
boxplot(jitter(market.df$Online_00049)~seg.hc.segment2, notch = FALSE) 
boxplot(jitter(market.df$Online_00074) ~seg.hc.segment2, notch = FALSE) 


### WITHOUT THE SHOP VARIABLES
seg.dist3<-daisy(market.df[,c(1:26,50:90,95:98)])
seg.hc3<-hclust(seg.dist3,method="ward.D2")
plot(seg.hc3)
plot(seg.hc3$height[520:540])
seg.hc.segment3<-cutree(seg.hc3, k=6)
table(seg.hc.segment3)

#AMOUNT SPENT - not significant differences
boxplot(market.df$Lebensmitt_00018~seg.hc.segment3, notch = FALSE) 

### based on product group
boxplot(market.df$`Lebensmitt_00020/beverages`~seg.hc.segment3, notch = FALSE) 
boxplot(market.df$`Lebensmitt_00020/fresh fruit`~seg.hc.segment3, notch = FALSE) 
boxplot(market.df$`Lebensmitt_00020/bread and pastries` ~seg.hc.segment3, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/convenience food` ~seg.hc.segment3, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/fresh vegetables` ~seg.hc.segment3, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/sausage and ham` ~seg.hc.segment3, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/tinned food` ~seg.hc.segment3, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/frozen food` ~seg.hc.segment3, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/Meat (incl. fish and poultry)` ~seg.hc.segment3, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/Dairy products (incl. Eggs and cheese)` ~seg.hc.segment3, notch = FALSE)

### more often online or offline
boxplot(jitter(market.df$`Online_00048/Bank transactions`) ~seg.hc.segment3, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Books / DVDs / eGames`) ~seg.hc.segment3, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Electronics (TV / PC / ...)`)~ seg.hc.segment3, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Food (Delivery service / Take-away)`)~ seg.hc.segment3, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Gambling & Betting`)~ seg.hc.segment3, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Groceries`)~ seg.hc.segment3, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Hotels / Flights`)~ seg.hc.segment3, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Software / Hardware`)~ seg.hc.segment3, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Sporting goods`)~ seg.hc.segment3, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Textiles`)~ seg.hc.segment3, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Tickets for events`)~ seg.hc.segment3, notch = FALSE)

### based on spare time
boxplot(market.df$Demographi_00039 ~seg.hc.segment3, notch = FALSE)
summary(market.df)

boxplot(market.df$`Online_00048/Groceries`~ seg.hc.segment3, notch = FALSE)
boxplot(market.df$`Online_00048/Electronics (TV / PC / ...)`~ seg.hc.segment3, notch = FALSE)


### interested in purch. online
boxplot(jitter(market.df$Lebensmitt_00011)~seg.hc.segment3, notch = FALSE) 
### planning to do groc. online in the next two month
boxplot(jitter(market.df$Lebensmitt_00013)~seg.hc.segment3, notch = FALSE) # Cluster 2 
boxplot(jitter(market.df$Lebensmitt_00014)~seg.hc.segment3, notch = FALSE) # Cluster 2 
table(seg.hc.segment3)


boxplot(jitter(market.df$Demographi_00035)~seg.hc.segment3, notch = FALSE) 
boxplot(jitter(market.df$Online_00049)~seg.hc.segment3, notch = FALSE) 
boxplot(jitter(market.df$Online_00074) ~seg.hc.segment3, notch = FALSE) 

###  2nd try

seg.dist <- daisy(market.df[,c(2:9)])

### hierarchical clustering

seg.hc <- hclust(seg.dist,method="ward.D2") 
plot(seg.hc)
plot(seg.hc$height[525:540]) # 4 clusters


plot(seg.hc)
rect.hclust(seg.hc, k=5, border="red")

# actually get 6 groups
seg.hc.segment <- cutree(seg.hc, k=5)   #cuts the dendogram into 6 parts and is assigning each branch a number
seg.hc.segment


table(seg.hc.segment) #e.g. segment 5 seems to be pretty small
seg.summ(market.df, seg.hc.segment) 

### hierarchical clustering

seg.hc <- hclust(seg.dist,method="ward.D2") 
plot(seg.hc)
plot(seg.hc$height[525:540]) # 4 clusters
cor(cophenetic(seg.hc), seg.dist)

plot(seg.hc)
rect.hclust(seg.hc, k=5, border="red")

# actually get 5 groups
seg.hc.segment <- cutree(seg.hc, k=5)   
seg.hc.segment


table(seg.hc.segment)
seg.summ(market.df, seg.hc.segment) 


### depending on the amount spent on groceries
boxplot(market.df$Lebensmitt_00018~seg.hc.segment, notch = FALSE) 

### based on product group
boxplot(market.df$`Lebensmitt_00020/beverages`~seg.hc.segment, notch = FALSE) 
boxplot(market.df$`Lebensmitt_00020/fresh fruit`~seg.hc.segment, notch = FALSE) 
boxplot(market.df$`Lebensmitt_00020/bread and pastries` ~seg.hc.segment, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/convenience food` ~seg.hc.segment, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/fresh vegetables` ~seg.hc.segment, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/sausage and ham` ~seg.hc.segment, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/tinned food` ~seg.hc.segment, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/frozen food` ~seg.hc.segment, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/Meat (incl. fish and poultry)` ~seg.hc.segment, notch = FALSE)
boxplot(market.df$`Lebensmitt_00020/Dairy products (incl. Eggs and cheese)` ~seg.hc.segment, notch = FALSE)

### more often online or offline
boxplot(jitter(market.df$`Online_00048/Bank transactions`) ~seg.hc.segment, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Books / DVDs / eGames`) ~seg.hc.segment, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Electronics (TV / PC / ...)`)~ seg.hc.segment, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Food (Delivery service / Take-away)`)~ seg.hc.segment, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Gambling & Betting`)~ seg.hc.segment, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Groceries`)~ seg.hc.segment, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Hotels / Flights`)~ seg.hc.segment, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Software / Hardware`)~ seg.hc.segment, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Sporting goods`)~ seg.hc.segment, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Textiles`)~ seg.hc.segment, notch = FALSE)
boxplot(jitter(market.df$`Online_00048/Tickets for events`)~ seg.hc.segment, notch = FALSE)

### based on spare time
boxplot(market.df$Demographi_00039 ~seg.hc.segment, notch = FALSE)

### interested in purch. online
boxplot(jitter(market.df$Lebensmitt_00011)~seg.hc.segment, notch = FALSE) 
### planning to do groc. online in the next two month
boxplot(jitter(market.df$Lebensmitt_00013)~seg.hc.segment, notch = FALSE) # Cluster 2 
boxplot(jitter(market.df$Lebensmitt_00014)~seg.hc.segment, notch = FALSE) # Cluster 2 
table(seg.hc.segment)


boxplot(jitter(market.df$Demographi_00035)~seg.hc.segment, notch = FALSE) 
boxplot(jitter(market.df$Online_00049)~seg.hc.segment, notch = FALSE) 
boxplot(jitter(market.df$Online_00074) ~seg.hc.segment, notch = FALSE) 


###ALL ONLINE_ VARIABLES###

colnames(market.df)
seg.dist3<-daisy(market.df[,c(60:78)])
seg.hc3<-hclust(seg.dist3,method="ward.D2")
plot(seg.hc3)
plot(seg.hc3$height)
seg.hc.segment3<-cutree(seg.hc3, k=5)
table(seg.hc.segment3)

###ALL LEBENSMITT_ VARIABLES###
colnames(market.df)
seg.dist3<-daisy(market.df[,c(79:89,98)])

seg.hc3<-hclust(seg.dist3,method="ward.D2")
plot(seg.hc3)
plot(seg.hc3$height[520:540])
seg.hc.segment3<-cutree(seg.hc3, k=3)
table(seg.hc.segment3)

refactorVariable <- function(df,start,end, label) {
  
  for( i in 1:length(market.df[,start]) ) {
    if( market.df[i,start] == "1") {
      market.df[label] <- 1 
    } else if (market.df[i,start+1] =="1"){
      market.df[label] <- 2 
    } else if (market.df[i,start+2] =="1"){
      market.df[label] <- 3 
    } else if (market.df[i,start+3] =="1"){
      market.df[label] <- 4 
    } else if (market.df[i,start+4]=="1"){
      market.df[label] <- 5 
    } else if (market.df[i,start+5]=="1"){
      market.df[label] <- 6
    } else {
      market.df[label] <- 7
    }
  }
}

str(market.df)

