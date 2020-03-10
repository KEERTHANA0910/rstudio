library(readr)
Computer_Data <- read_csv(file.choose())
View(Computer_Data)
comp_data <- Computer_Data[-1]
View(comp_data)
attach(comp_data)
library(plyr)
comp_data$cd <- as.factor(revalue(Computer_Data$cd, c("yes" = 1, "no" = 0)))
comp_data$multi <- as.factor(revalue(Computer_Data$multi,c("yes" = 1, "no" = 0)))
comp_data$premium<- as.factor(revalue(Computer_Data$premium,c("yes" = 1, "no" = 0)))
View(comp_data)
hist(comp_data$price)
library(moments)
skewness(comp_data$price)
kurtosis(comp_data$price)
boxplot(price)
#the data is posirively skewed and lepto kurtic or having higher peak than the normal
#there are some outliers
hist(comp_data$speed)
skewness(comp_data$speed)
kurtosis(comp_data$speed)
boxplot(speed)
#data is positively skewed and havimg lesser peak than the normal data
hist(comp_data$hd)
skewness(hd)
kurtosis(hd)
boxplot(hd)
#data is positively skewed and high peak compared to the normal data
#there are some outliers in the data
hist(comp_data$ram)
skewness(ram)
kurtosis(ram)
boxplot(ram)
#here the data is positively skewed and having higher peak there are some outliers which shows 
#there are some computers with having high ram like 16 gb, 24 gb and 32 gb
#most common rab capacity is 8 gb which is represented by median

norm <- function(x)
  
{
  return((x-min(x))/(max(x)-min(x)))
}

normdata <- as.data.frame(lapply(comp_data[2:10],norm))
View(normdata)
data <- cbind(price,normdata)
data <- data.frame(data)
View(data)
attach(data)
summary(data)
pairs(data)
cor(data)
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(data))
#here we can see that the price is depending mainly on the ram size
#second on the speed 
#from the analysis we can see that ram and hard disk are depending each other
#that is it shows presence of multicollinearity 
model <- lm(price~.,data=data)
summary(model)
#here all the p values are significant
#coefficient of determination is also high

influence.measures(model)
library(car)
influenceIndexPlot(model,id.n=3)
influencePlot(model,id.n=3)
#here we can see that the 1441 and 1701 are the most influencing measures 
#so we can build model by removing them
models1 <- lm(log(price)~.,data = data[-c(1441,1701),])
summary(models1)
#here we can see that the value of coefficient of determination is increased
#all the p values are significant
vif(models1)
#now there is no multicollinearity 
avPlots(models1,id.n=2,id.cex=0.7)
#from the avplot we can see that all the  variables are influencing the price
#so we cant remove a variable
confint(models1,level = 0.95)
predict(models1,interval = "predict")
plot(models1)
res<-models1$residuals
var(res)
#model validation
#here we can see that the model is valid since the residuals or the error is normally distributed 
#variance of the errors are almost constant

#Therefore the final model by omitting the 1441th and 1701th observation is 



#log(Price)= 7.756542+(0.318432*speed) + (0.684078*hd) + (0.627729*ram) 
#        + (0.161184*screen) + (0.049371*cd) + (0.047493*multi)
#         -(0.227230*premium) + (0.080870*ads) - (0.800745*trend)
