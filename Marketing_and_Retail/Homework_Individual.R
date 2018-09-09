library(gpairs)
library(corrplot)

####REGRESSION#####################
df<-read.csv("regression.csv")
summary(df)
str(df)
table(df$Distribution)
gpairs(df)
corrplot.mixed(cor(df),upper="ellipse")


df$saleslog<- log(df$Sales)

#df<-df[,-5]

m5<-lm(saleslog~price,data=df)
m1<-lm(Sales~price,data=df)
summary(m5)
confint(m5)
m11<-lm(Sales~Advertising,data=df)
df$pricelog <- log(df$price)
df$saleslog <- log(df$Sales)
m11<-lm(Sales~pricelog,data=df)
plot(Sales~price,data=df)
abline(m1,col="blue")

plot(Sales~Advertising,data=df)
par(mfrow=c(2,2))
plot(m5)
df[c(2,16,58),]

#58 is outlier 
df[58,]
dftest<-df[-58,]
#summary(dftest)
#mtest<-lm(Sales~price,data=dftest)
#mtest
##par(mfrow=c(2,2))
#plot(mtest)
#plot(Sales~price,data = dftest)
#abline(mtest,col="red")
mx<-lm(saleslog~Distribution,data=df)
summary(mx)
plot(mx)


m2<-lm(saleslog~price+Distribution+Advertising,data = df)
summary(m2)
plot(m2)

m3<-lm(saleslog~price+Distribution,data=df)
summary(m3)
plot(m3)

library(coefplot)
coefplot(m2, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Feature",
         xlab="Association with Sales")
summary(m1)$adj.r.squared
summary(m3)$adj.r.squared #model 3 explains more of the variation in Sales than model 1
par(mfrow=c(1,1))
plot(df$saleslog, fitted(m5), col="red",
     xlim=c(7,10), ylim=c(7,10),
     xlab="Sales Log", ylab="Fitted Sales Log")
points(df$saleslog, fitted(m3), col="blue")
legend("topleft", legend=c("model 1", "model 2"),
         col=c("red", "blue"), pch=1)

df.std<-scale(df)
summary(df.std)
head(df.std)


##############SEGMENTATION##################

dfsegm<- read.csv2("data_segmentation.csv")
dfsegm
str(dfsegm)
dfsegm$sex<-factor(dfsegm$sex) #0=female; 1=male
summary(dfsegm)
summary(dfsegm$price)
plot(dfsegm$price)
plot(dfsegm$age)

plot(dfsegm$A_weight)
table(dfsegm$A_weight)

which(dfsegm$age > 80)
dfsegm[c(43,226,345),]


library(cluster)
seg.dist <- daisy(dfsegm)
as.matrix(seg.dist)[1:5,1:5]
seg.dist
seg.hc <- hclust(seg.dist, method="complete")
plot(seg.hc)

nrow(dfsegm)
dfsegm[complete.cases(dfsegm),]

plot(cut(as.dendrogram(seg.hc), h=0.2)$lower[[1]])

plot(seg.hc)
rect.hclust(seg.hc, k=4, border="red")
seg.hc.segment <- cutree(seg.hc, k=4)
table(seg.hc.segment)


seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

seg.summ(dfsegm[,c(1:5)],seg.hc.segment)

dfsegm2<-dfsegm
dfsegm2$Segment<-seg.hc.segment
   
summary(dfsegm2)
dfsegm2[1:5,]
which(dfsegm2$rebuy_24months==1)
length(which(dfsegm2$rebuy_24months==1)) #just a check for the count of the fields if the mean is correct
length(which(dfsegm2$Segment==2)) # looks like the option to buy new thing is the most influential in this grouping
#recreate model for 5 clusters, because this one is misleding (based on next buy)


plot(seg.hc$height)# look for appropriate nr of clusters
plot(seg.hc$height[350:359])
plot(seg.hc)
rect.hclust(seg.hc, k=5, border="red")
seg.hc.segment2<- cutree(seg.hc,k=5)
table(seg.hc.segment2)
seg.summ(dfsegm, seg.hc.segment2)

seg.summ(dfsegm[,c(1:5)],seg.hc.segment2)
seg.summ(dfsegm[,c(19:20)],seg.hc.segment2)
#K-means

seg.df.num <- dfsegm
seg.df.num$sex    <- ifelse(dfsegm$sex==1, 0, 1)
seg.summ(seg.df.num$sex, seg.hc.segment2)

kmeanssegm <- kmeans(seg.df.num, centers=5, nstart = 50)
sum(kmeanssegm$withinss)

seg.summ(dfsegm, kmeanssegm$cluster)

length(which(kmeanssegm$cluster==1))
length(which(kmeanssegm$cluster==2))
length(which(kmeanssegm$cluster==3))
length(which(kmeanssegm$cluster==4))
length(which(kmeanssegm$cluster==5))

library(cluster)
clusplot(dfsegm, kmeanssegm$cluster, color=TRUE, shade=TRUE, 
         labels=5, lines=0, main="K-means cluster plot")

par(mfrow=c(1,1))
boxplot(seg.df.num$age ~ kmeanssegm$cluster, ylab="Age", xlab="Cluster")
boxplot(seg.df.num$price ~ kmeanssegm$cluster, ylab="Price", xlab="Cluster")
boxplot(seg.df.num$A_CPUpower ~ kmeanssegm$cluster, ylab="CPU power", xlab="Cluster")
boxplot(seg.df.num$A_brand ~ kmeanssegm$cluster, ylab="Brand", xlab="Cluster")

#####REWORK####
segmdata<-dfsegm[,c(7:18)]
str(segmdata)

seg.dist2 <- daisy(segmdata)
as.matrix(seg.dist2)[1:5,1:5]
hcl2 <- hclust(seg.dist2, method="complete")
plot(hcl2)
plot(hcl2$height)
plot(hcl2$height[330:359])
seg.hc.segment.new<- cutree(hcl2,k=12)
table(seg.hc.segment.new)
seg.summ(segmdata, seg.hc.segment.new)

kmeanssegm2 <- kmeans(seg.hc.segment.new, centers=12, nstart = 50)
sum(kmeanssegm2$withinss)

boxplot(seg.df.num$age ~ kmeanssegm2$cluster, ylab="Age", xlab="Cluster")
boxplot(seg.df.num$price ~ kmeanssegm2$cluster, ylab="Price", xlab="Cluster")

clusplot(dfsegm, kmeanssegm2$cluster, color=TRUE, shade=TRUE, 
         labels=5, lines=0, main="K-means cluster plot")
