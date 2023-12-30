#
# ──────────────────────────────────────────────────────────────────────────────
#
# SECTION 2: REGRESSION MODELS
#
# ──────────────────────────────────────────────────────────────────────────────
#
# Libraries used:

library(caret)
library(stats)
library(GGally)
library(glmnet)
library(Metrics)

# SECTION TABLE OF CONTENTS
#
# 2. Regression Models
#     - K Nearest Neighbors (KNN)
#     - Ordinary Linear Regression (OLS)
#     - Resampling
#         - LOOCV
#         - k-fold Cross Validation
#         - Bootstrap
#     - Principal Component Analysis (PCA)
#     - Shrinkage (Regularisation)
#         - Ridge Regression
#         - Lasso
#     - Subset Selection
#         - Backwards Stepwise
#         - Forwards Stepwise
#     - Model Evaluation
#         - R2 & adjusted R2
#         - MSE & MAE
#         - Mallow's Cp
#         - Akaike Information Criterion
#         - Bayesian Information Criterion
#
#
# ──────────────────────────────────────────────────────────────────────────────
# K NEAREST NEIGHBOURS (KNN)
# ──────────────────────────────────────────────────────────────────────────────

# After loading, preparing and splitting the data, you can go ahead with
# regressing the models. The easiest one is KNN, usually performed from
# library(caret)

# Note: it's also a good idea to standardise the data before KNN

# First, train the model - note that it cannot have NAs anywhere

set.seed(123)
KNN_train <- train(High.Price ~ Low.Price, data = train, method = 'knn',
                     preProcess = c("center", "scale", "knnImpute")
                     # the function can do some preprocessing for you!
                   )

# you can inspect the KNN model by calling it

KNN_train

# You can insert a dot "." instead of Low.Price to get all predictors
# You can also include multiple predictors with "+" (~ Low.Price + Mostly.High)

# Then predict it on the test data

KNN_predict <- predict(KNN_train, test)

# This will return a vector of what KNN thinks the dependent in the test should
# be based on the independents

# ──────────────────────────────────────────────────────────────────────────────
# ORDINARY LINEAR REGRESSION (OLS)
# ──────────────────────────────────────────────────────────────────────────────

# Regressing basic linear models can be done using the aptly named lm()
# function from library(stats)

lm_model <- lm(High.Price ~ Low.Price, data = train)

# you should always see the summary of the lm model to check on coefficients, 
# p-values and R squared

summary(lm_model)

# use this code to visually check for assumptions

par(mfrow = c(2,2))
plot(lm_model)
par(mfrow = c(1,1))

# You'll get four plots, here is what they mean:
#
# 1. Residuals vs Fitted Plot
# 
# Purpose: Checks for non-linearity and homoscedasticity (constant variance of the residuals).
# 
# Ideal Scenario:
#   
# - Residuals are randomly dispersed around the horizontal axis (0 line).
# - No clear pattern or systematic structure in the residuals.
# - If you see a funnel shape (residuals spread out as the fitted values increase or decrease), it suggests heteroscedasticity.
# 
# 2. Normal Q-Q Plot
# 
# Purpose: Checks if residuals are normally distributed.
# 
# Ideal Scenario:
#   
# - The points should fall approximately along the reference line.
# - Deviations from the line at the ends suggest potential outliers or a long-tailed distribution.
# 
# 3. Scale-Location (or Spread-Location) Plot
# 
# Purpose: Another check for homoscedasticity.
# 
# Ideal Scenario:
#   
# - Points are spread evenly along the y-axis as you move along the x-axis.
# - A pattern or a funnel shape indicates that residuals have non-constant variance.
# 
# 4. Residuals vs Leverage Plot
# 
# Purpose: Identifies influential observations that have an undue influence on the regression model (a.k.a. outliers)
# 
# Ideal Scenario:
#   
# - Most data points should have low leverage and small residuals.
# - Points outside the dashed Cook’s Distance lines might be influential to the regression results.
# - High-leverage points (far to the right of the plot) that also have high residuals (far from the horizontal line at 0) are of particular concern.

# Independence - plot residuals vs. all variables, look for high correlation

train[,ncol(train)+1] <- residuals(lm_model)
# add new column with residuals from the model - function is from

numeric_train <- sapply(train, is.numeric)
# here I extract indices of numeric columns since train is not prepared

# library(GGally)
ggpairs(train[, numeric_train])

# ──────────────────────────────────────────────────────────────────────────────
# RESAMPLING - LOOCV
# ──────────────────────────────────────────────────────────────────────────────

# On its own, LOOCV code does nothing. You have to include it when building a
# model. The simplest way to do that is to use library(caret)

LOOCV_model <- train(High.Price ~ Low.Price, 
                     data = train, 
                     method = "lm", 
                     trControl = trainControl(method = "LOOCV"))

# it will be a fairly large output (a few MB) so it may take a while

# remember to inspect the result with both a regular call and summary()

LOOCV_model
summary(LOOCV_model)

# ──────────────────────────────────────────────────────────────────────────────
# RESAMPLING - K-FOLD CROSS VALIDATION
# ──────────────────────────────────────────────────────────────────────────────

# The method here is nearly identical to LOOCV

kfold_model <- train(High.Price ~ Low.Price, 
                     data = train, 
                     method = "lm", 
                     trControl = trainControl(method = "cv", number = 10))
# "number" lest you select the number of folds

# also remember to inspect the result

kfold_model
summary(kfold_model)

# ──────────────────────────────────────────────────────────────────────────────
# RESAMPLING - BOOTSTRAP
# ──────────────────────────────────────────────────────────────────────────────

# At this point, you know the drill

boot_model <- train(High.Price ~ Low.Price, 
                     data = train, 
                     method = "lm", 
                     trControl = trainControl(method = "boot", number = 100))
# here, "number" lest you select the number of resamples

boot_model
summary(boot_model)

# ──────────────────────────────────────────────────────────────────────────────
# PRINCIPAL COMPONENT ANALYSIS (PCA)
# ──────────────────────────────────────────────────────────────────────────────

# This will theoretically allow us to "collapse" highly correlated predictors
# into one variable (or just choose the most significant one and ditch the rest)

# eliminating non-numeric columns and NAs to prepare for PCA:
pca_train <- train[, sapply(train, is.numeric)]
x <- pca_train$Mostly.High
y <- pca_train$Mostly.Low
pca_train$Mostly.High[is.na(x)] <- mean(x, na.rm = TRUE)
pca_train$Mostly.Low[is.na(y)] <- mean(y, na.rm = TRUE)

# Then we can use the base function prcomp()
pca_result <- prcomp(pca_train, scale = TRUE)

# Then extract the result
summary(pca_result)
plot(pca_result)

# based on that, we can decide which components to keep

# ──────────────────────────────────────────────────────────────────────────────
# SHRINKAGE (REGULARISATION) - RIDGE REGRESSION
# ──────────────────────────────────────────────────────────────────────────────

# Performing ridge requires transforming the predictors as a matrix
# makeX from library(glmnet) takes care of that
x <- makeX(train[, c("Low.Price", "Mostly.Low", "Mostly.High")], na.impute = TRUE)

# and the predicted as a vector
y <- as.vector(train$High.Price)

# and then using the main function

set.seed(123)
cv_ridge_model <- cv.glmnet(x, y, alpha = 0)

# Note that there cannot be any NAs in x and y!

coef(cv_ridge_model, s = cv_ridge_model$lambda.min)
# extract the coefficients of the lambda which minimizes MSE

#It's also possible to extract the coefficients of all possible lambdas
ridge_model <- glmnet(x, y, alpha = 0)
plot(ridge_model, xvar = "lambda", label = TRUE) # to plot them
coef(ridge_model, s = ridge_model$lambda.min) # to extarct them

# and it's possible to use to model to predict

test_x <- makeX(test[, c("Low.Price", "Mostly.Low", "Mostly.High")], na.impute = TRUE)
predict(cv_ridge_model, test_x)

# ──────────────────────────────────────────────────────────────────────────────
# SHRINKAGE (REGULARISATION) - LASSO
# ──────────────────────────────────────────────────────────────────────────────

# Lasso can be carried out identically, with the only difference being the alpha
# is equal to 1, not 0

cv_lasso_model <- cv.glmnet(x, y, alpha = 1)

# ──────────────────────────────────────────────────────────────────────────────
# SUBSET SELECTION - BACKWARDS STEPWISE
# ──────────────────────────────────────────────────────────────────────────────

# stepwise selection can be easily performed on a lm() model

step_model <- lm(High.Price ~ Low.Price + Mostly.High + Mostly.Low, data = train)

# then use step() from library(stats)
backward_model <- step(step_model, direction = "backward")
summary(backward_model)

# You'll recieve a model with only the relevant predictors

# ──────────────────────────────────────────────────────────────────────────────
# SUBSET SELECTION - FORWARDS STEPWISE
# ──────────────────────────────────────────────────────────────────────────────

# The same procedure as backwards, just in reverse
# Start with a model with no predictors
null_model <- lm(High.Price ~ 1, data = train)

# Full model to compare
full_model <- lm(High.Price ~ Low.Price + Mostly.High + Mostly.Low, data = train)

# Quick NA imputation (just because pumpkin wasn't prepared)
x <- train$Mostly.High
y <- train$Mostly.Low
train$Mostly.High[is.na(x)] <- mean(x, na.rm = TRUE)
train$Mostly.Low[is.na(y)] <- mean(y, na.rm = TRUE)

# Perform forward stepwise selection
forward_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
summary(forward_model)

# ──────────────────────────────────────────────────────────────────────────────
# MODEL EVALUATION - R2 & ADJUSTED R2
# ──────────────────────────────────────────────────────────────────────────────

# These are usually given by calling the model and/or its summary

model <- lm(High.Price ~ Low.Price, data = train) # or other models
model
summary(model)

# they can also be directly extracted from the model object (at least lm())
summary(model)$r.squared
summary(model)$adj.r.squared

# ──────────────────────────────────────────────────────────────────────────────
# MODEL EVALUATION - MSE & MAE
# ──────────────────────────────────────────────────────────────────────────────

# you can get the Mean Squared Error and the Mean Absolute Error 
# with library(Metrics)

predictions <- predict(model, newdata = test)
mse(test$High.Price, predictions) # first the actual, then the predicted
mae(test$High.Price, predictions)

# it can also be done manually
mean((test$High.Price - predictions)^2) # mse
mean(abs(test$High.Price - predictions)) # mae

# ──────────────────────────────────────────────────────────────────────────────
# MODEL EVALUATION - MALLOW'S CP
# ──────────────────────────────────────────────────────────────────────────────

# can be calculated manually from the output of the AIC function from
# library(stats)

p <- length(coef(model)) # Fetch number of predictors (incl. intercept)
AIC(model) - (2 * p) + (2 * length(train$High.Price))

# ──────────────────────────────────────────────────────────────────────────────
# MODEL EVALUATION - AKAIKE INFORMATION CRITERION
# ──────────────────────────────────────────────────────────────────────────────

# from the aforementioned function
AIC(model)

# can be done manually too
n <- nrow(train) # Number of observations
p <- length(coef(model)) # the same p as in the above section
residuals_sq <- sum((test$High.Price - predictions)^2)
log_likelihood <- -n/2 * (log(2 * pi) + log(residuals_sq / n) + 1)

aic <- 2*p - 2*log_likelihood
aic

# but the manual method is mathematically complex and will often return
# wrong results for different model types (and often different than just
# using AIC() )

# ──────────────────────────────────────────────────────────────────────────────
# MODEL EVALUATION - BAYESIAN INFORMATION CRITERION
# ──────────────────────────────────────────────────────────────────────────────

# similarly simple
BIC(model)

# using the code above, it is also possible to get BIC manually
bic <- log(n)*p - 2*log_likelihood
bic

# but it also has similiar accuracy problems
