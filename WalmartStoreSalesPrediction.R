library(zoo)
library(tseries)
library(forecast)
library(urca)
library(vars)
rm(list = ls())

?Box.test

#Loading the source datasets
train<-read.csv('/Users/vyas/Dropbox/Berkeley MIDS/Advanced Regression/W271 Lab 3/train.csv')
features<-read.csv('/Users/vyas/Dropbox/Berkeley MIDS/Advanced Regression/W271 Lab 3/features.csv')
train <- train[train['Dept']==12,]
train<-train[,c(1,2,3,4)]
head(train)

#Merging the datasets so that the relevant features are captured
input<-merge(train,features,by  =c("Store","Date"))
input<-input[,c(1,2,3,4,5,6,12,13)]
input<-input[(input["Store"]==4)|(input["Store"]==14)|(input["Store"]==15),]
head(input)

#The data we are going to analyze (for store 4 and department 12 are in the dataframe below)
store4_data<-input[input["Store"]==4,]
store4_data$Date


#Analyze cointegrations

po.test(cbind(store4_temp.ts,store4_sales.ts))

po.test(cbind(diff(store4_cpi.ts),store4_sales.ts))

po.test(cbind(store4_ue.ts,store4_sales.ts))

po.test(cbind(store4_fp.ts,store4_sales.ts))

#Based on this analysis, it appears as if sales at store 4 are cointegrated with temperature


#EDA
#1. Let's take a look at the time series for sales at store 4 and department 12
#Store4

store4_sales.ts<-as.ts(x=store4_data$Weekly_Sales,start = c(2010,5),frequency=52)
p1<-plot.ts(store4_sales.ts)
acf(store4_sales.ts)
pacf(store4_sales.ts)
adf.test(store4_sales.ts) #- non stationary series

store4_sales.d.ts<-diff(store4_sales.ts,differences=1)
adf.test(store4_sales.d.ts)
p1<-plot.ts(store4_sales.d.ts)
acf(store4_sales.d.ts)
pacf(store4_sales.d.ts)


#2. Let's look at the  time series for temperature
store4_temp.ts<-as.ts(x=store4_data$Temperature,start = c(2010,5),frequency=52)
p2<-plot.ts(store4_temp.ts)
acf(store4_temp.ts)
pacf(store4_temp.ts)
adf.test(store4_temp.ts)

store4_temp.d.ts<-diff(store4_temp.ts,differences=1)
adf.test(store4_temp.d.ts)
plot.ts(store4_temp.d.ts)
acf(store4_temp.d.ts)
pacf(store4_temp.d.ts)



#3. Let's look at the  time series for CPI
store4_cpi.ts<-as.ts(x=store4_data$CPI,start = c(2010,5),frequency=52)
p2<-plot.ts(store4_cpi.ts)
acf(store4_cpi.ts)
pacf(store4_cpi.ts)
adf.test(store4_cpi.ts)

store4_cpi.d.ts<-diff(store4_cpi.ts,differences=1)
adf.test(store4_cpi.d.ts)
plot.ts(store4_cpi.d.ts)
acf(store4_cpi.d.ts)
pacf(store4_cpi.d.ts)

#4. Let's look at the  time series for Fuel Price
store4_fp.ts<-as.ts(x=store4_data$Fuel_Price,start = c(2010,5),frequency=52)
p2<-plot.ts(store4_fp.ts)
acf(store4_fp.ts)
pacf(store4_fp.ts)
adf.test(store4_fp.ts)

store4_fp.d.ts<-diff(store4_fp.ts,differences=1)
adf.test(store4_fp.d.ts)
plot.ts(store4_fp.d.ts)
acf(store4_fp.d.ts)
pacf(store4_fp.d.ts)

#4. Let's look at the  time series for Unemployment
store4_ue.ts<-as.ts(x=store4_data$Unemployment,start = c(2010,5),frequency=52)
p2<-plot.ts(store4_ue.ts)
acf(store4_ue.ts)
pacf(store4_ue.ts)
adf.test(store4_ue.ts)

store4_ue.d.ts<-diff(store4_ue.ts,differences=1)
adf.test(store4_ue.d.ts)
plot.ts(store4_ue.d.ts)
acf(store4_ue.d.ts)
pacf(store4_ue.d.ts)


#Based on the plot, this series is clearly non stationary
#This is further validated by the adf test which returns a non-significant p-value
#Let's take the first difference of this plot

oos=cbind(store4_sales.d.ts)[137:142]
#Model1 - Just based on sales

modelinput = cbind(store4_sales.d.ts)[1:136]

model1<-ar(modelinput,method = 'ols',dmean=T,intercept=F)
model1$ar
model1$aic
model1$order

p1<-predict(model1,n.ahead=6)

p1<-predict(model1,n.ahead=6)
rmse(p1$pred,oos)

#Model2 - Based on both sales and temperature

modelinput = cbind(store4_sales.d.ts,store4_temp.d.ts)[1:136,]

model2<-ar(modelinput,method = 'ols',dmean=T,intercept=F)
model2$ar
model2$aic

p2<-predict(model2,n.ahead=6)

p2<-predict(model2,n.ahead=6)
rmse(p2$pred[,c(1)],oos)

#Model3 - Based on both sales and temperature and unemployment

modelinput = cbind(store4_sales.d.ts,store4_temp.d.ts,store4_ue.d.ts)[1:136,]

model3<-ar(modelinput,method = 'ols',dmean=T,intercept=F)
model3$ar
model3$aic
model3$order

p3<-predict(model3,n.ahead=6)

p3<-predict(model3,n.ahead=6)
rmse(p3$pred[,c(1)],oos)



#Model3

modelinput = cbind(store4_sales.d.ts,store4_temp.d.ts,store4_ue.d.ts,store4_fp.d.ts,store4_cpi.d.ts)[2:135,]
modeloos=cbind(store4_sales.d.ts,store4_temp.d.ts,store4_ue.d.ts,store4_fp.d.ts,store4_cpi.d.ts)[136:141,]


modelinput = cbind(store4_sales.d.ts,store4_temp.d.ts)[2:135,]
modeloos=cbind(store4_sales.d.ts,store4_temp.d.ts)[136:141,]


model1<-ar(modelinput,method = 'ols',dmean=T,intercept=F)
model1$aic

modelvar

?VAR

modelvar<-VAR(modelinput,p=1,ic=c("AIC"))



plot(modelvar)

a<-summary(modelvar)

str(a)

sales.ar$aic

p<-predict(modelvar,n.ahead=6)

p$fcst$store4_sales.d.ts[,c(1)]

oos<-store4_sales.d.ts[136:141]
?predict



rmse<-function(t1,t2){
  sqrt(sum((t1-t2)^2))
}

?pairs

help(pairs)

?Pacf


par('mar')