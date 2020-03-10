glass <-read.csv(file.choose())
class(glass)
View(glass)
str(glass)
table(glass$type)
summary(glass)
boxplot(glass)
#data contains so many outliers
glass$Type <- as.factor(glass$Type) # Factorize the Type in Glass dataset
glassnew<-(glass[,-c(10)])
View(glassnew)
#now to normalize he data

norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

glass_norm <- as.data.frame(lapply(glassnew, norm)) 
View(glass_norm)
summary(glass_norm)

glass$Type <- factor(glass$Type, levels = c("1","2","3","4","5","6","7"),
                     labels = c("type1","type2","type3","type4","type5","type6","type7"))
# Partitioning the data based on the Glass Type
library(caret)
Index <- createDataPartition(glass$Type, p=0.7, list=FALSE) 
#spliting the data into train and test using the ratio 7:3
glass_train <- glass_norm[ Index,] 
glass_test <- glass_norm[-Index,] 
glass_train_lbs <- glass[ Index,10] 
glass_test_lbs <- glass[-Index,10] 

table(glass_train_lbs)
table(glass_test_lbs) 


# Build a KNN model on training dataset
library("class")
library(caret)
#to find the optimum k value we use thumb rule as sqrt(number of rows)
sqrt_n = ceiling(round(sqrt(nrow(glass_train))))
sqrt_n
#=12
# we have to build the model
glass_pred <- knn(train = glass_train , test = glass_test, cl = glass_train_lbs, k=12)
## Now evualuating the model performance
library("gmodels")

# Create cross table of predicted and actual
CrossTable( x =  glass_test_lbs, y = glass_pred)       
#here the model accuracy is 43/61=0.704 that is the model is 70.4 accurate
#by creating a loop we can come to kknow which is the best k value
i=1
k.optm=1
for(i in 1:30){
  knn.mod<-knn(train=glass_train,test=glass_test,cl=glass_train_lbs,k=i)
  k.optm[i]<-100*sum(glass_test_lbs==knn.mod)/NROW(glass_test_lbs)
  k=i
  cat(k,'=',k.optm[i],'\n')
}
plot(k.optm,type="b",xlab="k-va;ue",ylab="accuracy")
#we can find that the best accuracy is given by k=4,17,18,19 from the elbow curve 
#and the accuracy is 67.21%