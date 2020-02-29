
# Reading the store data

superstore <- read.csv("F:/DSP Projects/Project 5/Data Set/Super Store.csv")
summary(superstore)
#View(superstore)
str(superstore)

head(superstore$Order.Date)
tail(superstore$Order.Date)

s <- superstore['Sales']
s1 <- s$Sales[1:530]
s2 <- s$Sales[531:1060]
s3 <- s$Sales[1061:1590]
s4 <- s$Sales[1591:2121]
s5 <- s$Sales[1591:1855]
s6 <- s$Sales[1591:1723]

s_timeseries <- ts(s)
s1_timeseries <- ts(s1)
s2_timeseries <- ts(s2)
s3_timeseries <- ts(s3)
s4_timeseries <- ts(s4)
s5_timeseries <- ts(s5)
s6_timeseries <- ts(s6)

s_timeseries
#View(superstore_timeseries)
colSums(is.na(s_timeseries))
summary(s_timeseries)


plot.ts(s_timeseries, col='green')
plot.ts(s1_timeseries, col='red')
plot.ts(s2_timeseries, col='red')
plot.ts(s3_timeseries, col='red')
plot.ts(s4_timeseries, col='red')
plot.ts(s5_timeseries, col='blue')
plot.ts(s6_timeseries, col='blue')

# Applying Moving Average

library(TTR)

s_timeseriesSMA3 <- SMA(s_timeseries,n=3)
plot.ts(s_timeseriesSMA3, col='red')

s_timeseriesSMA8 <- SMA(s_timeseries,n=8)
plot.ts(s_timeseriesSMA8, col='red')

e3=s_timeseries-s_timeseriesSMA3
mse3=sum(e3^2, na.rm=T)/2121
mse3  #167919.2

e8=s_timeseries-s_timeseriesSMA8
mse8=sum(e8^2, na.rm=T)/2121
mse8  #219359.7

s_forecasts <- HoltWinters(s_timeseries, beta = FALSE, gamma = FALSE)
s_forecasts
s_forecasts$SSE
s_forecasts$fitted

s_forecasts2 <- forecast:::forecast.HoltWinters(s_forecasts, h=8)
s_forecasts2
forecast:::plot.forecast(s_forecasts2, col='purple', lwd=4)

s_forecasts2$residuals
s_forecasts2$fitted

# Checking autocorrelation

acf(s_forecasts2$residuals, lag.max=10, na.action = na.pass)
Box.test(s_forecasts2$residuals, lag=20, type='Ljung-Box')
# H0 : There is NO non zero significant autocorrelation
# As p-value>0.5, H0 is accepted
plot.ts(s_forecasts2$residuals, col='purple', lwd=4)

library(psych)
describe(s_forecasts2$residuals)
# As skewness and kurtosis are outside (-2,2), it is not normally distributed.
hist(s_forecasts2$residuals, col='purple')

# Differencing

s_diff1 <- diff(s_timeseries, differences = 1)
plot.ts(s_diff1, col='purple', lwd=2)

s_diff2 <- diff(s_timeseries, differences = 2)
plot.ts(s_diff2, col='purple', lwd=2)

# AutoCorrelation Factor : Differenced

acf(s_diff1, lag.max=20, col='red', lwd=4)
acf(s_diff1, lag.max=20, plot=FALSE)

acf(s_diff2, lag.max=20, col='red', lwd=4)
acf(s_diff2, lag.max=20, plot = FALSE)

# Partial AutoCorrelation Factor : Differenced

pacf(s_diff1, lag.max=20, col='red', lwd=4)
pacf(s_diff1, lag.max=20, plot=FALSE)

pacf(s_diff2, lag.max=20, col='red', lwd=4)
pacf(s_diff2, lag.max=20, plot = FALSE)

install.packages("tseries")
library(tseries)

# Augmented Dickey-Fuller Test

adf.test(s_timeseries) # As p-value<0.05, it's stationary

adf.test(s_diff1) # As p-value<0.05, it's stationary

adf.test(s_diff2) # As p-value<0.05, it's stationary

# Auto Regressive Integrated Moving Averages(ARIMA)

s_ARIMA1 <- arima(s_timeseries, order=c(1,1,2))
s_ARIMA2 <- arima(s_timeseries, order=c(1,2,1))
s_ARIMA3 <- arima(s_timeseries, order=c(2,1,1))

s_ARIMA1
s_ARIMA2
s_ARIMA3

# ARIMA : Forecasts

s_fcasts <- forecast:::forecast(s_ARIMA3, h=530)
head(s_fcasts)
plot(s_fcasts, col='red')

# Writing csv file
write.csv(s_fcasts,"F:/DSP Projects/Project 5/Sales Forecast.csv")

# Plotting residuals

plot.ts(residuals(s_ARIMA3), main="(2,1,1) Model Residuals", col='green', lwd=4)
hist(s_ARIMA3$residuals, col='red', main = "Histogram of residuals(2,1,1)")

library(psych)
describe(s_ARIMA3$residuals)
# As skewness and kurtosis are outside (-2,2), it doesnot confirm to normality.

#Checking autocorrelation

Box.test(s_ARIMA3$residuals, lag=20, type="Ljung-Box")
# H0 : There is NO non zero autocorrelation
# As p-value>0.05, H0 is accepted.

# Checking accuracy
install.packages("forecast")
library(forecast)
accuracy(s_ARIMA3)  #RMSE=502.47
