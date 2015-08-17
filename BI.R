## Loading useful packages
library(ggplot2)
library(sqldf)
library(forecast)
library(tseries)
library(vars)
library(lmtest)
library(knitr)

## Import data from a csv file
data= read.csv("~/Desktop/Training data set.csv")
## Attach data
attach(data)

## Create a new data.frame of first 696 observations of v1 and date
v1.date= sqldf("SELECT v1,date FROM data LIMIT 696")
## Scatter plot of v1
ggplot(v1.date,aes(x=date,y=v1))+geom_point()

## Transform v1 to a time series and set frequency equal to 24 (hourly)
v1timeseries=ts(v1[1:696],frequency=24)
v1timeseries
## Plot time series v1 and use adf test to check stationarity
par(mfrow=c(1,1))
plot.ts(v1timeseries)
adf.test(v1timeseries)
## Plot shows that v1 time series is stationary and seasonal with multiple peaks
## ADF test also accepts alternative hypothesis: stationary

## Use HoltWinters seasonal methods
## Unknown parameters are determined by minimizing the squared prediction error.
v1.hw=HoltWinters(v1timeseries,seasonal="additive")
## Plot holtwinters
plot(v1.hw)
## Forecast next 35 values
v1.forecast=forecast.HoltWinters(v1.hw,h=35)
## Check the accuracy of forecasting
accuracy(v1.forecast)
## Plot forecast
plot.forecast(v1.forecast)
## Print forecast
v1.forecast

## Second method is using ARIMA
## Find the best ARIMA first
v1.arima=auto.arima(v1timeseries)
acf(v1.arima$residuals)
pacf(v1.arima$residuals)

## Forecast next 35 values
v1.arima.forecast=forecast(v1.arima,h=35)
## Check the accuracy of forecasting
accuracy(v1.arima.forecast)
## Plot forecast
plot(v1.arima.forecast)
## Print forecast 
v1.arima.forecast


#######################################################
## Question 2
#######################################################
## Check seasonality and stationarity by plot and ADF test
adf.plot=function(x){
  plot.ts(ts(x,frequency=24))
  adf.test(ts(x,frequency=24))
}

adf.plot(v1[1:696])
adf.plot(v2)
adf.plot(v3)
adf.plot(v4)
adf.plot(v5)
adf.plot(v6)
adf.plot(v7)
adf.plot(v8)
adf.plot(v9)
adf.plot(v10)
adf.plot(v11)
adf.plot(v12)
adf.plot(v13)
adf.plot(v14)

## ADF tests show that all time series are stationary, in which case we cannot use
## vector autoregression. 

## Then, one can use granger test to check if one time series is likely to forecast future
## values of another

grangertest(v2 ~ v1, order = 1, data =data)
grangertest(v3 ~ v1, order = 1, data =data)
grangertest(v4 ~ v1, order = 1, data =data)
grangertest(v5 ~ v1, order = 1, data =data)
grangertest(v6 ~ v1, order = 1, data =data)
grangertest(v7 ~ v1, order = 1, data =data)
grangertest(v8 ~ v1, order = 1, data =data)
grangertest(v9 ~ v1, order = 1, data =data)
grangertest(v10 ~ v1, order = 1, data =data)
grangertest(v11 ~ v1, order = 1, data =data)
grangertest(v12 ~ v1, order = 1, data =data)
grangertest(v13 ~ v1, order = 1, data =data)
grangertest(v14 ~ v1, order = 1, data =data)

## Results show that p value is not significant in the granger test involving v3
## Therefore, v3 can be used to predict future values of v1

## Plot v1,v3 time series
par(mfrow=c(2,1))
plot.ts(ts(v1,frequency=24))
plot.ts(ts(v3,frequency=24))

## Linear regression 
model.1.3=lm(v1~v3,data=data)
summary(model.1.3) 

## Plot v1 and v3
par(mfrow=c(1,1))
plot(v3,v1)
abline(model.1.3,col='red')

## In the summary, Adjusted R-squared is 0.9235, which is very close to 1, so fit is
## excellent.

## Residual plot
plot(resid(model.1.3))

## Predict v1 using v3 and confidence intervals
newdata = data.frame(data$v3)
predict.1.3=predict(model.1.3,newdata=newdata,interval="predict")
predict.1.3[697:731,]


### Second approach

### We can also consider multiple linear regression model
observed.data<- sqldf("SELECT * FROM data LIMIT 696")
full.model=lm(v1~v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12+v13+v14,data=observed.data)
summary(full.model)
plot(resid(full.model))

## Use the step function based on AIC in the package MASS
library(MASS)
step=step(full.model,direction="both")
## Show steps it took to the best model
step$anova
## Delete v12 to get the best model
best.model=update(full.model, ~.-v12)
summary(best.model)

## Plot residuals
plot(resid(best.model))

## Residuals are dense at 0, and symmetric around 0, which indicates good fit.

## Make predictions
predict.data=sqldf("SELECT v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v13,v14 FROM data")
predict.bestmodel=predict(best.model,newdata=predict.data,interval="predict")
predict.bestmodel[697:731,]

################



