library(readr)
airline<- read.csv(file.choose())
View(airline)
boxplot(airline$Balance)
#The data is no normal.there are many outliers.also the data is positiveky skewed
boxplot(airline$Qual_miles)
#data is not normal it is positively skewed and there are so many outliers. 
boxplot(airline$cc1_miles)
#tha data is positively skewed there are no outliers.
boxplot(airline$cc2_miles)
#data is positively skewed and there are two outliers
boxplot(airline$cc3_miles)
#data is positively skewed and there are some outliers
boxplot(airline$Bonus_miles)
#positively skewed data and there are plentyof outliers
boxplot(airline$Bonus_trans)
#there are many outliers and data is positively skewed
boxplot(airline$Flight_miles_12mo)
#data is positively skewed and there are many outliers
boxplot(airline$Flight_trans_12)
#posotively skewed data and there are many outliers
boxplot(airline$Days_since_enroll)
#the data is normal and there are no outliers
boxplot(airline$Award.)

#now we nrmalize the entire data set
normdata<-scale(airline[,2:(ncol(airline))]) 
View(normdata)
d<-dist(normdata,method="euclidean")
fit<-hclust(d,method="complete")
plot(fit)
plot(fit,hang=-1)
rect.hclust(fit,k=10,border="red")
groups<-cutree(fit,k=10)
membership<-as.matrix(groups)
final<-data.frame(airline,membership)
View(final)
final1<-final[,c(ncol(final),2:(ncol(final)-1))]
View(final1)
write.csv(final1, file="final airline.csv",row.names = F)
getwd()
aggregate(airline,by=list(final1$membership),mean)
#the table of which member belongs to which cluster is created using complete linkage.

#using single linkage
fit1<-hclust(d,method="single")
plot(fit1)
plot(fit1,hang=-1)
rect.hclust(fit1,k=10,border="red")
groups1<-cutree(fit1,k=10)
membership1<-as.matrix(groups1)
final1<-data.frame(airline,membership1)
View(final1)
final1single<-final1[,c(ncol(final1),2:(ncol(final1)-1))]
View(final1single)
write.csv(final1single, file="final airline single.csv",row.names = F)
getwd()
aggregate(airline,by=list(final1single$membership1),mean)

#using average linkage
fit2<-hclust(d,method="average")
plot(fit2,hang=-1)
rect.hclust(fit2,k=10,border="red")
groups2<-cutree(fit2,k=10)
membership2<-as.matrix(groups2)
final2<-data.frame(airline,membership2)
View(final2)
final1avg<-final2[,c(ncol(final2),2:(ncol(final2)-1))]
View(final1avg)
write.csv(final1avg, file="final airline average.csv",row.names = F)
getwd()
aggregate(airline,by=list(final1avg$membership2),mean)


#using centroid linkage
fit3<-hclust(d,method="centroid")
plot(fit3,hang=-1)
rect.hclust(fit3,k=10,border="red")
groups3<-cutree(fit3,k=10)
membership3<-as.matrix(groups3)
final3<-data.frame(airline,membership3)
View(final3)
final1centroid<-final3[,c(ncol(final3),2:(ncol(final3)-1))]
View(final1centroid)
write.csv(final1centroid, file="final airline centroid.csv",row.names = F)
getwd()
aggregate(airline,by=list(final1centroid$membership3),mean)

#after performing clustering using different linkages we have formed tables which shows which member is in which cluser
# we can see that size of cluster and members in the cluster changing while using different linkages.

#since there are 3999 observations 
#it is not appropriate to perform hierarchieal clustering
#because the dendrogram representation is not clear 
#so we go for k means clustering
wss = NULL

kmean1<- kmeans(normdata,10)
str(kmean1)
library(animation)
kmeans.ani(airline,10)
wss = (nrow(normdata)-1)*sum(apply(normdata, 2, var))
for (i in 2:10) wss[i] = sum(kmeans(normdata, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")


twss <- NULL
for (i in 2:15){
  twss <- c(twss,kmeans(normdata,i)$tot.withinss)
}

twss
plot(2:15,twss,type="o")

kmean6 <- kmeans(normdata,6)
str(kmean6)
kmean11<-kmeans(normdata,11)
str(kmean11)
kmean13<-kmeans(normdata,13)
str(kmean13)
kmean8<-kmeans(normdata,8)
str(kmean8)
#using elbow curve we get that optimum no. of cluster is 13 which has minimum twss 
final2<- data.frame(airline, kmean13$cluster)
aggregate(airline[,2:11],by=list(kmean13$cluster), FUN=mean)
final1<-final[,c(ncol(final2),1:(ncol(final2)-1))]
View(final1)
write.csv(final1,file="airlines kmean.csv",row.names= F)
getwd()

#here total within sum of squares is minimum for 13 clusters
#so we choose it as optimum no. of clusters
#the table representing which member belongs to which cluster is created