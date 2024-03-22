#
# ──────────────────────────────────────────────────────────────────────────────
#
# SECTION 5: TIME SERIES MODELS
#
# ──────────────────────────────────────────────────────────────────────────────
#
# Libraries used:

library(GGally)
library(dplyr)
library(forecast)
library(ARDL)
library(vars)
library(tseries)
library(glmnet)

# SECTION TABLE OF CONTENTS
#
# 
# 5.  Time Series Models
#     -   Loading and Splitting TS Objects
#     -   Analysing Dynamic Properties
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
#     -   Tests
#         -   ADF
#         -   EG
#         -   BP
#         -   JB
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
# LOADING AND SPLITTING TS OBJECTS
# ──────────────────────────────────────────────────────────────────────────────

# to load from an external source, use
data <- readxl::read_excel("filepath.xlsx")

# to define it as a ts object, use
data <- as.data.frame(ts(data, frequency = 7))
# REMEMBER to change frequency!

# Before we begin, the dataset used in this section will be AirPassengers

air <- AirPassengers

# We will later use Seatbelts
seat <- Seatbelts

# and to split a time series object into training and testing, we can use:
train <- window(Seatbelts, end = c(1977, 9))
test <- window(Seatbelts, start = c(1977, 9))


# If you need to split a time series object along a specific fraction (e.g. 70%)

split_time_series <- function(time_series, split_ratio = 0.7) {

  start_time <- start(time_series)
  frequency <- frequency(time_series)
  total_length <- nrow(time_series)
  
  # Calculate the index for the split
  split_index <- round(total_length * split_ratio)
  
  # Calculate the year and month for the split
  split_year <- start_time[1] + (split_index - 1) %/% frequency
  split_month <- start_time[2] + (split_index - 1) %% frequency
  
  # Split the data using window()
  train <- window(time_series, end = c(split_year, split_month))
  test <- window(time_series, start = c(split_year, split_month+1))
  
  return(list(train = train, test = test))
}

split <- split_time_series(Seatbelts, 0.7)
train <- split$train
test <- split$test

# ──────────────────────────────────────────────────────────────────────────────
# ANALYSING DYNAMIC PROPERTIES
# ──────────────────────────────────────────────────────────────────────────────

plot(air)
# generate a plot and insoect the highs and lows of the dataset

acf(air)
# generate a plot of the autocorrelation function, check whether current values
# are influenced by current values

pacf(air)
# generate a plot of the partial ACF, 

tsdisplay(air)
# To plot the data, ACF and PACF all together

frequency(air)
# check the frequency of the dataset (only ts objects)

# from library(GGally):

ggpairs(air)
ggpairs(seat[,"drivers"])
# correlation plot of all variables in a univariate time series

ggpairs(as.data.frame(seat))
# same for multivariate time series

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
  first_result <- c(ts_data, first_ma)
  
  # Second moving average applied to the first
  second_ma <- single_ma(first_result, n)
  
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
forecast::forecast(hw)

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

# For Seatbelts, static regression looks like this:
fit <- tslm(drivers ~ ., train)
summary(fit)

# To obtain a forecast, use:
forecast::forecast(fit, as.data.frame(test))

# It's also possible to pass trend as season to the formula to make a dynamic
# regression:
fit2 <- tslm(drivers ~ trend + season, train)
summary(fit2)
forecast::forecast(fit2)

plot(forecast::forecast(fit2))
# you can also plot forecast objects

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

# you can also plot them
plot(decomposed$trend)

# Additive Model: Use this when the seasonal variations are roughly constant 
# over time. In an additive model, the components are simply added together.

# Multiplicative Model: Use this when the seasonal variations are changing
# proportionally with the level of the time series. In a multiplicative model, 
# the components are multiplied together.

# Plot your time series data and look at the pattern. If the amplitude (height)
# of the seasonal fluctuations or the variability of the series appears to be 
# increasing with the level of the time series, a multiplicative model might be
# more appropriate. Conversely, if the seasonal fluctuations or variability 
# appear to be stable over time, regardless of the level of the series, an 
# additive model might be more suitable.

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
forecast::forecast(arima)

# ──────────────────────────────────────────────────────────────────────────────
# ADL MODELS
# ──────────────────────────────────────────────────────────────────────────────

# This function from library(ARDL) will fit an ARDL model automatically
Auto_ADL <- auto_ardl(drivers ~ PetrolPrice + kms, train, max_order = 5)

# If you need to specify the order, you can use:
ADL <- ARDL::ardl(drivers ~ PetrolPrice + kms, train, order = c(5,5,5))


# to generate a model that can be predicted:

# prepare data for the model
model_data <- as.data.frame(train[, c("drivers", "kms", "PetrolPrice")])

# Fit the model
dLagARDL <- dLagM::ardlDlm(formula = drivers ~ kms + PetrolPrice, 
                           data = model_data, 
                           p = 5, q = 5)

# Prepare the test data
test_data <- as.data.frame(test[, c("kms", "PetrolPrice")])
test_matrix <- t(as.matrix(test_data))

# Forecast
forecast <- dLagM::forecast(dLagARDL, x = test_matrix, h = dim(test_matrix)[2])

# Calculaing the accuracy of the forecast
forecast::accuracy(as.vector(forecast$forecasts), test[,"drivers"])

# ──────────────────────────────────────────────────────────────────────────────
# VAR MODELS
# ──────────────────────────────────────────────────────────────────────────────

# You can use library(vars) 'p' is the order of the VAR model
fit_var <- VAR(seat, p = 2)
summary(fit_var)

# to choose the right 'p', use:
VARselect(seat, lag.max = 10, type="const")

# forecast
forecast::forecast(fit_var)

# ──────────────────────────────────────────────────────────────────────────────
# IRF CHOLESKY MODELS
# ──────────────────────────────────────────────────────────────────────────────

# Another function from library(vars) to predict the impact of shocks to a 
# single variable according to a VAR model
irf_var <- irf(fit_var, impulse = "drivers", response = "PetrolPrice", n.ahead = 10)
plot(irf_var)

# ──────────────────────────────────────────────────────────────────────────────
# TESTS - ADF
# ──────────────────────────────────────────────────────────────────────────────

# I'll be using library(tseries)
adf.test(seat[,"drivers"])

# perform and Augmented Dickey Fuller test to determine whether the time series
# is stationary or not. 

# If p-value is below the significance level, it is stationary

# ──────────────────────────────────────────────────────────────────────────────
# TESTS - EG
# ──────────────────────────────────────────────────────────────────────────────

# Another function from library(tseries) which performs a two step EG test
po.test(seat[, c("drivers", "PetrolPrice")])

# ──────────────────────────────────────────────────────────────────────────────
# TESTS - BP
# ──────────────────────────────────────────────────────────────────────────────

# The Breusch-Pagan test is used to detect the presence of heteroskedasticity 
# in a regression model
lmtest::bptest(fit)

# ──────────────────────────────────────────────────────────────────────────────
# TESTS - JB
# ──────────────────────────────────────────────────────────────────────────────

# The Jarque-Bera is used to check for normality
tseries::jarque.bera.test(residuals(fit))

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

# make example models
hw <- HoltWinters(train[,"drivers"])
arima <- auto.arima(train[,"drivers"])
tslm1 <- tslm(drivers ~ ., train)
tslm2 <- tslm(drivers ~ kms + PetrolPrice, train)

# example forecasts
forecast1 <- forecast::forecast(tslm1, as.data.frame(test))
forecast2 <- forecast::forecast(tslm2, as.data.frame(test))
forecast3 <- forecast(hw, h = dim(test)[1])
forecast4 <- forecast(arima, h = dim(test)[1])

# combine 
result <- combine_forecasts_nelson(test[,"drivers"],
                                   forecast1$mean,
                                   forecast4$mean)

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
                                   forecast4$mean,
                                   forecast2$mean)

result

# ──────────────────────────────────────────────────────────────────────────────
# MODEL EVALUATION - ACCURACY
# ──────────────────────────────────────────────────────────────────────────────

# You can get the vast majority of required accuracy parameters with:

forecast::accuracy(arima)
# called on a model object (for insample/training errors)

forecast::accuracy(forecast4, test[,"drivers"])
# called on a forecast object and actual values (for outsample/testing errors)

# ──────────────────────────────────────────────────────────────────────────────
# MODEL EVALUATION - DIEBOLD-MARIANO
# ──────────────────────────────────────────────────────────────────────────────

# This test is included in library(forecast), it is ued for checking which
# model is better
dm.test(forecast1$residuals, forecast2$residuals, h = dim(test)[1])
