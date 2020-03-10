toyota <- read.csv(file.choose())
View(toyota)
attach(toyota)

#here the columns 1,2,5,6,8,10,11,12,15 ad the columns from 19 to 38 are not necessary for our analysis
#so we can remove those columns
toyota.1 <- toyota[,-c(1,2,5,6,8,10,11,12,15,19:38)]
View(toyota.1)
attach(toyota.1)

boxplot(toyota.1$Price)
library(moments)
skewness(Price)
kurtosis(Price)
hist(Price)

boxplot(toyota.1$Age_08_04)
skewness(Age_08_04)
kurtosis(Age_08_04)
hist(Age_08_04)

boxplot(toyota.1$KM)
skewness(KM)
kurtosis(KM)
hist(KM)

boxplot(toyota.1$HP)
skewness(HP)
kurtosis(HP)
hist(HP)

boxplot(toyota.1$cc)
skewness(cc)
kurtosis(cc)
hist(cc)

boxplot(toyota.1$Doors)
skewness(Doors)
kurtosis(Doors)
hist(Doors)

boxplot(toyota.1$Gears)
skewness(Gears)
kurtosis(Gears)
hist(Gears)

boxplot(toyota.1$Quarterly_Tax)
skewness(Quarterly_Tax)
kurtosis(Quarterly_Tax)
hist(Quarterly_Tax)

cor(toyota.1)
#here the age and price are highly negatively correlated.
#age and km and price and km are having  moderately negative correlation
#weight and price are having moderate positive correlation
#all othe variables are very less dependency on each other

pairs(toyota.1)

library(corpcor)
#we may not get dependancy of the variables by using correlation function 
#since it may give the influence of the the other variables on the correlation of the two variables
#so we use the partial correlation function
cor2pcor(cor(toyota.1))
#here we can see that the price is decreasing for the vehicle when age increases
#price decreases when km increases


model.toy <- lm(Price~.,data=toyota.1)
summary(model.toy)
#here p value is insignificant for cc and doors
#we create model using that variables separately
model.toycc<-lm(Price~cc,data=toyota.1)
summary(model.toycc)
model.toyDoors<-lm(Price~Doors,data=toyota.1)
summary(model.toyDoors)

#in both the cases p values are significant 
#now we build model using bith variables
model.toyDC<-lm(Price~Doors+cc,data=toyota.1)
summary(model.toyDC)
#here too the p value is significant 
#so we go for variance inflation factor

library(car)
vif(model.toy)

#here variance inflation factor is less than ten 
#now we go for influence index plot and av plot
avPlots(model.toy)
#here we can see that from the av plots 
#the variable door is contributing very less influence on the price 
#all other variables are influencing the model
#so we can build model by dropping the variable
model.toy1 <- lm(Price~.-Doors,data=toyota.1)
summary(model.toy1)
#even if the coefficient of determination is increased the p value is not significant for the cc
#now we go for influe ce index plot
influenceIndexPlot(model.toy)
influence.measures(model.toy)


finalmodelt2 <- lm(Price~.-Doors,data=toyota.1[-c(81,222)])
summary(finalmodelt2) 
#still the p value is not significant 
#so we go for transformations
finalmodelt3 <- lm(Price~Age_08_04+KM+HP+log(cc)+Gears+Quarterly_Tax+Weight,data=toyota.1[-c(81,222)])
summary(finalmodelt3)
#here the p value became significant
#coefficient of determination is also good#86.6%
confint(finalmodelt3,level = 0.95)
predict(finalmodelt3,interval = "predict")

plot(finalmodelt3)  


hist(residuals(finalmodelt2))  

#the final model is
#Price= 8.288e+03-(1.211e+02*Age_08_04) - (1.928e-02*KM) + (3.677e+01*HP)
#        -(2.261e+03*log(cc)) + (5.582e+02*Gears) +(6.545e+00*Quarterly_Tax)
#            (1.870e+01*Weight)

