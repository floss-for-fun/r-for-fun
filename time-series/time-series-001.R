##########################
# Creating a time series #
##########################

# Reference : http://www.statmethods.net/advstats/timeseries.html

# generate random observations
myvector <- runif(48, 1.9, 10.0)

# Creating a time series
########################
# save a numeric vector containing 48 monthly observations
# from Jan 2009 to Dec 2014 as a time series object
myts <- ts(myvector, start=c(2009, 1), end=c(2014, 12), frequency=12)

# subset the time series (June 2014 to December 2014)
myts2 <- window(myts, start=c(2014, 6), end=c(2014, 12))

# plot series
plot(myts)

# Seasonal decomposition
########################
fit <- stl(myts, s.window="period")
plot(fit)

# additional plots
monthplot(myts)
library(forecast)
seasonplot(myts) 

# Exponential Models
####################
# simple exponential - models level
fit <- HoltWinters(myts, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fit <- HoltWinters(myts, gamma=FALSE)
# triple exponential - models level, trend, and seasonal components
fit <- HoltWinters(myts)

# predictive accuracy
library(forecast)
accuracy(fit)

# predict next three future values
library(forecast)
forecast(fit, 3)
plot(forecast(fit, 3))

# Arima models
##############
# fit an ARIMA model of order P, D, Q
fit <- arima(myts, order=c(p, d, q)

# predictive accuracy
library(forecast)
accuracy(fit)

# predict next 5 observations
library(forecast)
forecast(fit, 5)
plot(forecast(fit, 5))

# Automated Forecasting
#######################
library(forecast)
# Automated forecasting using an exponential model
fit <- ets(myts)

# Automated forecasting using an ARIMA model
fit <- auto.arima(myts) 
