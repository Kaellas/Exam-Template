#
# ──────────────────────────────────────────────────────────────────────────────
#
# SECTION 5: TIME SERIES MODELS
#
# ──────────────────────────────────────────────────────────────────────────────
#
# Libraries used:

library(dplyr)
library(forecast)
library(ARDL)
library(vars)
library(tseries)

# SECTION TABLE OF CONTENTS
#
# 
# 5.  Time Series Models
#     -   Simple Forecasts
#         -   Previous observed value
#         -   Previous value + proportion of previously observed change
#         -   Lagged Forecast
#         -   Averages
#         -   Weighed Forecasts
#     -   Moving Averages
#         -   Single
#         -   Double
#     -   Exponential Smoothening
#         -   Normal
#         -   Holt's
#         -   Winter's
#     -   Linear Regression
#     -   Time Series Decomposition
#     -   ARIMA
#     -   ADL Models
#     -   VAR Models
#     -   IRF Cholesky Models
#     -   Non-stationary tests
#         -   ADF
#         -   EG
#     -   Combining forecasts
#         -   Simple Averaging
#         -   Weighted Averaging
#         -   Nelson Combination
#         -   Granger-Ramanathan Combination Method
#     -   Model Evaluation
#         -   Accuracy
#         -   Diebold-Mariano
# 
# ──────────────────────────────────────────────────────────────────────────────
# INTRODUCTION
# ──────────────────────────────────────────────────────────────────────────────

# Before we begin, the dataset used in this section will be AirPassengers

air <- AirPassengers

# We will later use Seatbelts

seat <- Seatbelts

# and to split a time series object into training and testing, we can use:
train <- window(Seatbelts, end = c(1977, 9))
test <- window(Seatbelts, start = c(1977, 9))

# If you need to split a time series object along a specific fraction (e.g. 70%)
start_time <- start(seat) # January 1969
frequency <- frequency(seat) # Monthly data

# Calculate the index for the 70% mark
total_length <- length(seat)
split_index <- round(total_length * 0.7)

# Convert this index back to a date
years_passed <- (split_index - 1) / frequency / dim(seat)[2]
months_passed <- (split_index - 1) %% frequency
start_year <- start_time[1] + years_passed
start_month <- start_time[2] + months_passed

# Use window() to get the subset
train <- window(Seatbelts, end = c(start_year, start_month))
test <- window(Seatbelts, start = c(start_year, start_month))

# ──────────────────────────────────────────────────────────────────────────────
# SIMPLE FORECASTS - PREVIOUS OBSERVED VALUE
# ──────────────────────────────────────────────────────────────────────────────

naive_forecast <- function(ts_data) {
  forecast <- ts_data[length(ts_data)]
  # fetch most recent datapoint
  return(forecast)
}

naive_forecast(air)

# ──────────────────────────────────────────────────────────────────────────────
# SIMPLE FORECASTS - PREVIOUS VALUE + PROPORTION OF PREVIOUSLY OBSERVED CHANGE
# ──────────────────────────────────────────────────────────────────────────────

forecast_with_change <- function(ts_data, proportion) {
  change <- ts_data[length(ts_data)] - ts_data[length(ts_data) - 1]
  # change between t-1 and t-2
  forecast <- ts_data[length(ts_data)] + proportion * change
  # change plus proportion of change
  return(forecast)
}

forecast_with_change(air, 0.5)
# Example with 50% of the change

# ──────────────────────────────────────────────────────────────────────────────
# SIMPLES FORECASTS - LAGGED FORECAST
# ──────────────────────────────────────────────────────────────────────────────

lagged_forecast <- function(ts_data, lag) {
  forecast <- ts_data[length(ts_data) - lag + 1]
  # Fetch lagged variables
  return(forecast)
}

lagged_forecast(air, 1)
# Lag of 1 for previous value, higher for older values

# ──────────────────────────────────────────────────────────────────────────────
# SIMPLE FORECASTS - AVERAGES
# ──────────────────────────────────────────────────────────────────────────────

# can be calculated as simply as:
mean(air)

# and from library(dplyr) for a cumulativve average:
cummean(air)

# ──────────────────────────────────────────────────────────────────────────────
# SIMPLE FORECASTS - WEIGHED FORECASTS
# ──────────────────────────────────────────────────────────────────────────────

weighted_forecast <- function(ts_data, weights) {
  forecast <- sum(tail(ts_data, length(weights)) * weights)
  # fetch tail according to the amount of weights and multiply
  return(forecast)
}

weighted_forecast(air, c(0.1, 0.3, 0.6))
# there can be any number of weights

# ──────────────────────────────────────────────────────────────────────────────
# MOVING AVERAGES - SINGLE
# ──────────────────────────────────────────────────────────────────────────────

single_ma <- function(ts_data, n) {
  forecast <- mean(tail(ts_data, n))
  # fetch tail according to the amount of weights and multiply
  return(forecast)
}

single_ma(air, 4)
# Moving average based on last n entries

# ──────────────────────────────────────────────────────────────────────────────
# MOVING AVERAGES - DOUBLE
# ──────────────────────────────────────────────────────────────────────────────

double_ma <- function(ts_data, n) {
  # First moving average
  first_ma <- single_ma(ts_data, n)
  
  # Second moving average applied to the first
  second_ma <- single_ma(first_ma, n)
  
  return(second_ma)
}

double_ma(air, 4)
# Double moving average based on last n entries

# ──────────────────────────────────────────────────────────────────────────────
# EXPONENTIAL SMOOTHENING - NORMAL
# ──────────────────────────────────────────────────────────────────────────────

# A.K.A. Simple exponential smoothening

# simple function from library(forecast), alpha is the smoothening parameter
ses(air, alpha = 0.2)

# ──────────────────────────────────────────────────────────────────────────────
# EXPONENTIAL SMOOTHENING - HOLT'S
# ──────────────────────────────────────────────────────────────────────────────

# another function from library(forecast), 'h' is the forecast horizon
holt(air, h = 10)

# ──────────────────────────────────────────────────────────────────────────────
# EXPONENTIAL SMOOTHENING - WINTER'S
# ──────────────────────────────────────────────────────────────────────────────

# one more function from library(forecast), can be "additive" or "multiplicative"
hw(air, seasonal = "additive", h = 10)

# Or an in built function to create a model, you can even specify the parameters
# otherwise it will fit them automatically
hw <- HoltWinters(air)

hw <- HoltWinters(air, alpha = NULL, beta = NULL, gamma = NULL)
# (Don't run this, just an example)

# and then to forecast
forecast(hw)

# ──────────────────────────────────────────────────────────────────────────────
# LINEAR REGRESSION
# ──────────────────────────────────────────────────────────────────────────────

# AirPassengers is only the predictor, but we can plot it against time like
# so:

time <- seq_along(air)
# generate time variable

linear <- lm(air ~ time)
# fit model

summary(linear)

# We need a more complex dataset for analyses from now on, we can use:
fit <- tslm(drivers ~ ., train)
summary(fit)

# To obtain a forecast, use:
forecast(fit, as.data.frame(test))

# It's also possible to pass trend as season to the formula
fit2 <- tslm(drivers ~ trend + season, train)
summary(fit2)
forecast(fit2)

# ──────────────────────────────────────────────────────────────────────────────
# TIME SERIES DECOMPOSITION
# ──────────────────────────────────────────────────────────────────────────────

# We can use the in-built function, but it can only take a single column
decomposed <- decompose(seat[,"drivers"], type = "multiplicative")
plot(decomposed)

# You can extract the individual components like so:
decomposed$trend
decomposed$seasonal
decomposed$random

# ──────────────────────────────────────────────────────────────────────────────
# ARIMA
# ──────────────────────────────────────────────────────────────────────────────

# You can use a function from library(forecasts) to fit an ARIMA model 
# automatically, but it can also be used only for univariate time series 
# (only one variable)
arima <- auto.arima(seat[,"drivers"])
summary(arima)

# if you need to specify the order, you can use:
arima2 <- Arima(seat[,"drivers"],
                order = c(1,0,1),
                seasonal = c(0,1,1))
summary(arima2)

# and to forecast
forecast(arima)

# ──────────────────────────────────────────────────────────────────────────────
# ADL MODELS
# ──────────────────────────────────────────────────────────────────────────────

# This function from library(ARDL) will fit an ARDL model automatically
Auto_ADL <- auto_ardl(drivers ~ PetrolPrice + kms, train, max_order = 5)

# If you need to specify the order, you can use:
ADL <- ARDL::ardl(drivers ~ PetrolPrice + kms, train, order = c(5,5,5))

# ──────────────────────────────────────────────────────────────────────────────
# VAR MODELS
# ──────────────────────────────────────────────────────────────────────────────

# You can use library(vars) 'p' is the order of the VAR model
fit_var <- VAR(seat, p = 2)
summary(fit_var)

# to choose the right 'p', use:
VARselect(seat, lag.max = 10, type="const")

# ──────────────────────────────────────────────────────────────────────────────
# IRF CHOLESKY MODELS
# ──────────────────────────────────────────────────────────────────────────────

# Another function from library(vars) to forecast with a var model
irf_var <- irf(fit_var, impulse = "drivers", response = "PetrolPrice", n.ahead = 10)
plot(irf_var)

# ──────────────────────────────────────────────────────────────────────────────
# NON-STATIONARY TESTS - ADF
# ──────────────────────────────────────────────────────────────────────────────

# I'll be using library(tseries)
adf.test(seat[,"drivers"])

# ──────────────────────────────────────────────────────────────────────────────
# NON-STATIONARY TESTS - EG
# ──────────────────────────────────────────────────────────────────────────────

# Another function from library(tseries) which performs a two step EG test
po.test(seat[, c("drivers", "PetrolPrice")])

# ──────────────────────────────────────────────────────────────────────────────
# COMBINING FORECASTS - SIMPLE AVERAGING
# ──────────────────────────────────────────────────────────────────────────────

mean(naive_forecast(air), forecast_with_change(air, 0.5))

# ──────────────────────────────────────────────────────────────────────────────
# COMBINING FORECASTS - WEIGHTED AVERAGING
# ──────────────────────────────────────────────────────────────────────────────

weighted_average <- function(forecasts, weights) {
  sum(forecasts * weights) / sum(weights)
}

weighted_average(
  forecasts = c(naive_forecast(air), forecast_with_change(air, 0.5)),
  weights = c(0.6, 0.4)
)

# ──────────────────────────────────────────────────────────────────────────────
# COMBINING FORECASTS - NELSON COMBINATION
# ──────────────────────────────────────────────────────────────────────────────

# the method below only works for UNIVARIATE time series

combine_forecasts_nelson <- function(outsamp, fcast1, fcast2) {
  # Combine forecasts using linear regression and correct for bias
  combfitN <- lm(outsamp ~ fcast1 + fcast2)
  outsampcor <- outsamp - combfitN$coefficients[1]
  
  # Apply linear regression without intercept, enforcing weight constraint
  fitW <- lm(outsampcor ~ 0 + offset(fcast1) + I(fcast2 - fcast1))
  coef_2 <- coef(fitW)
  
  # Calculate the weights
  beta_1 <- 1 - coef_2
  beta_2 <- coef_2
  
  # Calculate the combined forecast
  combfcastN <- beta_1 * fcast1 + beta_2 * fcast2
  
  # Accuracy of the combined forecast
  forecast_accuracy <- forecast::accuracy(combfcastN, outsamp)
  
  # Return results as a list
  return(list(combined_forecast = combfcastN, 
              weights = c(beta_1, beta_2), 
              accuracy = forecast_accuracy))
}

# make two models
hw <- HoltWinters(train[,"drivers"])
arima <- auto.arima(train[,"drivers"])

# make two forecasts
forecast1 <- forecast(hw, h = dim(test)[1])
forecast2 <- forecast(arima, h = dim(test)[1])

# combine 
result <- combine_forecasts_nelson(test[,"drivers"],
                                   forecast1$mean,
                                   forecast2$mean)

result

# ──────────────────────────────────────────────────────────────────────────────
# COMBINING FORECASTS - GRANGER-RAMANATHAN COMBINATION METHOD
# ──────────────────────────────────────────────────────────────────────────────

# GR is essentially a simpler version of Nelson, also only works with
# Univariate time series

combine_forecasts_GR <- function(outsamp, fcast1, fcast2) {
  # Combine forecasts using linear regression
  combfit <- lm(outsamp ~ fcast1 + fcast2)
  
  # Print summary of the linear model
  model_summary <- summary(combfit)
  
  # Create a time series object for the combined forecast
  combfcast <- ts(combfit$fitted.values,
                  start = start(outsamp),
                  frequency = frequency(outsamp))
  
  # Calculate accuracy of the combined forecast
  forecast_accuracy <- forecast::accuracy(combfcast, outsamp)
  
  # Return results as a list
  return(list(combined_forecast = combfcast, 
              model_summary = model_summary, 
              accuracy = forecast_accuracy))
}

# combine two previously made forecasts
result <- combine_forecasts_GR(test[,"drivers"],
                                   forecast1$mean,
                                   forecast2$mean)

result

# ──────────────────────────────────────────────────────────────────────────────
# MODEL EVALUATION - ACCURACY
# ──────────────────────────────────────────────────────────────────────────────

# You can get the vast majority of required accuracy parameters with:

accuracy(arima)
# called on a model object (for insample/training errors)

accuracy(forecast1, test[,"drivers"])
# called on a forecast object and actual values (for outsample/testing errors)

# ──────────────────────────────────────────────────────────────────────────────
# MODEL EVALUATION - DIEBOLD-MARIANO
# ──────────────────────────────────────────────────────────────────────────────

# This test is included in library(forecast)
dm.test(forecast1$residuals, forecast2$residuals, h = dim(test)[1])
