library(readr)
data<-read_csv("C://Users//ADMIN//Desktop//data sets//dat sets ba//calories_consumed.csv")
View(data)
hist(data$`Weight gained (grams)`)
hist(data$`Calories Consumed`)
boxplot(data$`Weight gained (grams)`)
boxplot(data$`Calories Consumed`)
library(moments)
kurtosis(data$`Weight gained (grams)`)
#since kurtosis is 2.89 and it is less than 3 the data is platy kurtic or having lesser peak compared to normal curve
kurtosis(data$`Calories Consumed`)
#since kurtosis =2.40 and it is lessthan 3 the data have lesser peak when compared to normal data
skewness(data$`Weight gained (grams)`)
#since skewness=1.117 and the mass of the data is on the left side and tail is in the right side the data is positively skewed
skewness(data$`Calories Consumed`)
#since skewness is 0.58  the data mass is on the left side the data is positively skewed
summary(data)
#mean of weight gained is 357.7 and median is 200. therefore mean>median the is positively skewed
#mean of calories consumed> median agian the data is positively skewed.
plot(data$`Weight gained (grams)`,data$`Calories Consumed`)
attach(data)
cor(`Calories Consumed`,`Weight gained (grams)`)
#the correlation between weight gained and calories consumed is 0.94 which indicates
# they are highly positively correlted
reg1<-lm(`Weight gained (grams)`~`Calories Consumed`)
summary(reg1)
predicted1<-predict(reg1)
predicted1
reg1$residuals
error<-sum(reg1$residuals)
error
rmse<-sqrt((sum(error)^2)/nrow(data))
rmse
confint(reg1,level=.95)
predict(reg1,interval="predict")


#the p-values are significant and multiple r^2 values are better but 
#high standard error for the y intercept 
#rmse value is low
#the prediction intervals are very much wider. 
#for example,first value  is 62 and its predicted as lying between -258.20569 and 267.1709

library(ggplot2)
ggplot(data = data, aes(x =`Calories Consumed` , y = `Weight gained (grams)`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = data, aes(x=`Calories Consumed`, y=predicted1))


#taking logarithemiv tansformation

reg2<-lm(`Weight gained (grams)`~log(`Calories Consumed`))
summary(reg2)
predicted2<-predict(reg2)
predicted2
reg2$residuals
error<-sum(reg2$residuals)
error
rmse<-sqrt((sum(error)^2)/nrow(data))
rmse
confint(reg2,level=.95)
predict(reg2,interval="predict")
#p values are significant but R^2 value is reduced 
# rmse valuenis low
#standard error value is high
# here too the confidence interval is wider

#now taking log for both the dependent and independent variable

logy<-log(data$`Weight gained (grams)`)
View(logy)
reg3<-lm(logy~log(`Calories Consumed`))
summary(reg3)
predicted3<-predict(reg3)
predicted3
reg3$residuals
error<-sum(reg3$residuals)
error
rmse<-sqrt((sum(error)^2)/nrow(data))
rmse
confint(reg3,level=.95)
predict(reg3,interval="predict")
logx<-log(`Calories Consumed`)
data2<-data.frame(logy,logx)
View(data2)
library(ggplot2)
ggplot(data = data2, aes(x =data2$logx , y = data2$logy)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = data2, aes(x=data2$logx, y=predicted3))
#the p-values are significant and multiple r^2 values are better 
#less standard error for the y intercept 
#rmse value is low
#the prediction intevals are small
#so we can conclude that this model is the best fit model for the above data. 
#we canget the best result from taking the logarithemic transformation.


#therefore the model is


#           log(weight gained) = -14.9275 + 2.6479 log(calories consumed)

#the confidence interval for the y intercept is (-20.400993 , -9.454036)
#the confidence interval for coefficient of calories consumed is (1.938717 ,  3.357173)