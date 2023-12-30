# ──────────────────────────────────────────────────────────────────────────────
#
# APPENDIX: APPLIED DATA WORK EXERCISES
#
# ──────────────────────────────────────────────────────────────────────────────
#
# Libraries used:

library(skimr)
library(ggplot2)
library(rsample)
library(recipes)
library(caret)
library(boot)

# SECTION TABLE OF CONTENTS
#
# 6.  Appendix: Applied Data Work Exercises
#     -   Ana's Set
#     -   Ben's Set
#
# ──────────────────────────────────────────────────────────────────────────────
# ANA'S SET
# ──────────────────────────────────────────────────────────────────────────────

# --- EXERCISE A ---------------------------------------------------------------

pumpkin <- read.csv("US-pumpkins.csv", stringsAsFactors = T)
# Upload data

# --- EXERCISE B ---------------------------------------------------------------

skim(pumpkin) # check basic properties

dim(pumpkin) # check dimensions
class(pumpkin) # check variable type
summary(pumpkin) # check column variable type
head(pumpkin) # print a few rows
View(pumpkin) # view entire dataset

# --- EXERCISE C ---------------------------------------------------------------

pumpkin <- subset(pumpkin, select = -c(X, X.1))
# delete unknown columns

# --- EXERCISE D ---------------------------------------------------------------

pumpkin$Date <- as.Date(pumpkin$Date, format = "%m/%d/%y")
# make a new date column

# OR, for seperate month and year
pumpkin$Month <- format(pumpkin$Date, "%m")
pumpkin$Year <- format(pumpkin$Date, "%Y")

# OR, for column with only month and year
pumpkin$MonthYear <- format(pumpkin$Date, "%m-%Y")

# --- EXERCISE E ---------------------------------------------------------------

checkNA <- function(x) {
  # Create an empty data frame to store results
  results <- data.frame(ColumnIndex = integer(),
                        ColumnName = character(),
                        ColumnClass = character(),
                        NA_Count = integer(),
                        NA_Percentage = numeric(),
                        stringsAsFactors = FALSE)
  
  for (i in 1:length(x)) {
    NA_Count <- 0
    
    # Check for NA values based on the type of the column
    if (is.factor(x[[i]])) {
      NA_Count <- sum(x[[i]] == "")
    }
    else {
      NA_Count <- sum(is.na(x[[i]]))
    }
    
    # Calculate the percentage of NA values
    NA_Percentage <- NA_Count / length(x[[i]])
    
    # Append the results to the data frame
    results <- rbind(results, data.frame(ColumnIndex = i,
                                         ColumnName = names(x)[i],
                                         ColumnClass = class(x[[i]]),
                                         NA_Count = NA_Count,
                                         NA_Percentage = round(NA_Percentage, 2),
                                         stringsAsFactors = FALSE))
  }
  
  # Return the results data frame
  return(results)
}

checkNA(pumpkin)

check <- checkNA(pumpkin)

pumpkin <- pumpkin[, -c(check$ColumnIndex[check$NA_Percentage>0.15])]
# delete all columns with high amount of NAs

x <- pumpkin$Mostly.High
pumpkin$Mostly.High[is.na(x)] <- mean(x, na.rm = TRUE)

x <- pumpkin$Mostly.Low
pumpkin$Mostly.Low[is.na(x)] <- mean(x, na.rm = TRUE)
# the remaining NAs can me imputed to avoid losing additional data

# --- EXERCISE F ---------------------------------------------------------------

pumpkin$AvgPrice <- (pumpkin$Low.Price+pumpkin$High.Price)/2
# create new column

pumpkin <- subset(pumpkin, select = -c(Low.Price, High.Price))
# drop old columns

hist_all_trans <- function(x) {
  par(mfrow = c(2,2))
  hist(x, main = "Original Data")
  hist(log(x), main = "Log")
  hist(forecast::BoxCox(x, lambda = "auto"), main = "BoxCox")
  hist(car::yjPower(x, 0), main = "Yeo Johnson")
  par(mfrow = c(1,1))
}

boxplot_all_trans <- function(x) {
  par(mfrow = c(2,2))
  boxplot(x, main = "Original Data")
  boxplot(log(x), main = "Log")
  boxplot(forecast::BoxCox(x, lambda = "auto"), main = "BoxCox")
  boxplot(car::yjPower(x, 0), main = "Yeo Johnson")
  par(mfrow = c(1,1))
}

hist_all_trans(pumpkin$AvgPrice)
boxplot_all_trans(pumpkin$AvgPrice)
# BoxCox seems to be the most reliable transformation when it comes to normalising
# the outlier count is roughly the same regardless of transformations

# --- EXERCISE G ---------------------------------------------------------------

checkVAR <- function(x) {
  # Create an empty data frame to store results
  results <- data.frame(ColumnIndex = integer(),
                        ColumnName = character(),
                        ColumnClass = character(),
                        Variance = integer(),
                        stringsAsFactors = FALSE)
  
  for (i in 1:length(x)) {
    Variance <- 0
    
    # Check for variance in numeric columns only
    if (is.numeric(x[[i]])) {
      Variance <- var(x[[i]], na.rm = TRUE)
    }
    else {
      next
    }
    
    # Append the results to the data frame
    results <- rbind(results, data.frame(ColumnIndex = i,
                                         ColumnName = names(x)[i],
                                         Variance = Variance,
                                         stringsAsFactors = FALSE))
  }
  
  # Return the results data frame
  return(results)
}

checkVAR(pumpkin)
# Can't see any columns with zero or near zero variance

# --- EXERCISE H ---------------------------------------------------------------

numeric_columns <- pumpkin[, sapply(pumpkin, is.numeric)]

par(mfrow = c(1,2))
for(i in 1:length(numeric_columns)) {
  hist(numeric_columns[[i]], main = names(numeric_columns)[i])
  boxplot(numeric_columns[[i]], main = names(numeric_columns)[i])
}
par(mfrow = c(1,1))

# Scales are similiar - no need to standardise
# Since AvgPrice will be transformed to BoxCox, we can do the same to all
# the numeric columns
# There are some outliers, should be watched for later impact
# NAs will be imputed as mentioned previously

# --- EXERCISE I ---------------------------------------------------------------

factor_columns <- pumpkin[, sapply(pumpkin, is.factor)]

# Loop through factor columns and create bar plots and inspect unique counts
# of each factor level
for(col_name in names(factor_columns)) {
  print(ggplot(factor_columns, aes_string(x = col_name)) + 
          geom_bar() + 
          ggtitle(col_name))
  print(col_name)
  print(sort(tapply(factor_columns[[col_name]], factor_columns[[col_name]], length)))
}

# a few categories can be lumped in a few columns
# Repack is 99% one variable, it can probably be disregarded unless the few apperances
# of the single variable prove to be very significant

# --- EXERCISE J ---------------------------------------------------------------

pumpkin_blueprint <- read.csv("US-pumpkins.csv", stringsAsFactors = T)
# Load a new dataframe of pumpkin for the blueprint

pumpkin_blueprint$AvgPrice <- (pumpkin_blueprint$Low.Price+pumpkin_blueprint$High.Price)/2
# create AvgPrice (can't be done as a step with recipes since it goes in the
# main function)

check <- checkNA(pumpkin_blueprint)
cols_to_remove <- check$ColumnName[check$NA_Percentage > 0.15]
# Get list of columns with too many NAs to delete

set.seed(123)
split <- initial_split(pumpkin_blueprint, prop = 0.7)
train <- training(split)
test <- testing(split)
# Split into training and testing dataset

rec <- recipe(AvgPrice ~ ., data = pumpkin_blueprint) %>%
  step_mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>% # transform Date
  step_rm(Low.Price, High.Price) %>% # remove AvgPrice components
  step_rm(all_of(cols_to_remove)) %>% # remove high NA count columns (including X and X.1)
  step_impute_knn(Mostly.High, Mostly.Low) %>% # impute remaining NAs with KNN
  step_BoxCox(all_numeric()) %>% # transform numeric columns with BoxCox
  step_other(all_factor(), threshold = 0.1) # lump factor variables

prep <- prep(rec, training = train)

train <- bake(prep, new_data = train)
test <- bake(prep, new_data = test)

# --- EXERCISE K ---------------------------------------------------------------

trControl = trainControl(method = "cv", number = 10)

LM_model <- train(AvgPrice ~ ., 
                     data = train, 
                     method = "lm", 
                     trControl = trControl)

KNN_model <- train(AvgPrice ~ ., 
                  data = train, 
                  method = "knn", 
                  trControl = trControl)

GLM_model <- train(AvgPrice ~ ., 
                   data = train, 
                   method = "glm", 
                   trControl = trControl)

# --- EXERCISE L ---------------------------------------------------------------

# To inspect individually:
summary(LM_model)

KNN_model

summary(GLM_model)
GLM_model

# To check side by side:
results <- resamples(list(LM_model, KNN_model, GLM_model))
summary(results)

# The best model seems to be LM

# --- EXERCISE M ---------------------------------------------------------------

predictions <- predict(LM_model, newdata = test)
# Generate predictions

postResample(predictions, test$AvgPrice)
# Check parameters
# RMSE and MAE are low
# R Squared is excellent

# --- EXERCISE N ---------------------------------------------------------------

summary(LM_model)
# Inspect the coefficients

# The intercept is the highest coefficient, but it is not significant.
# All other coefficients are quite low, but a few are significant.

# The significant factor predictors are not siginifcant in their entirety,
# e.g. one category is significant, but the others are not.

# Mostly.High and Mostly.Low are both very significant, but it is hard to say
# what is their relation to AvgPrice. It can be checked with cor()

cor(train$Mostly.Low, train$AvgPrice)
cor(train$Mostly.High, train$AvgPrice)

# They both appear to be very correlated, so they should be deleted to eliminate
# autocorrelation. We can see if the other predictors and the entire model 
# are more or less significant afterwards

# --- EXERCISE O ---------------------------------------------------------------

# Run the same code as in the previous exercises, but with the change of 
# deleting the Mostly.High and Mostly.Low variables

# prepare the data
pumpkin_blueprint <- read.csv("US-pumpkins.csv", stringsAsFactors = T)

pumpkin_blueprint$AvgPrice <- (pumpkin_blueprint$Low.Price+pumpkin_blueprint$High.Price)/2

check <- checkNA(pumpkin_blueprint)
cols_to_remove <- check$ColumnName[check$NA_Percentage > 0.15]

set.seed(123)
split <- initial_split(pumpkin_blueprint, prop = 0.7)
train <- training(split)
test <- testing(split)

rec <- recipe(AvgPrice ~ ., data = pumpkin_blueprint) %>%
  step_mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>% 
  step_rm(Low.Price, High.Price) %>% 
  step_rm(all_of(cols_to_remove)) %>% 
  step_rm(Mostly.High, Mostly.Low) %>% #here's the change
  step_BoxCox(all_numeric()) %>%
  step_other(all_factor(), threshold = 0.1)

prep <- prep(rec, training = train)

train <- bake(prep, new_data = train)
test <- bake(prep, new_data = test)

# run the models

trControl = trainControl(method = "cv", number = 10)

LM_model <- train(AvgPrice ~ ., 
                  data = train, 
                  method = "lm", 
                  trControl = trControl)

KNN_model <- train(AvgPrice ~ ., 
                   data = train, 
                   method = "knn", 
                   trControl = trControl)

GLM_model <- train(AvgPrice ~ ., 
                   data = train, 
                   method = "glm", 
                   trControl = trControl)

# compare the models

summary(LM_model)

KNN_model

summary(GLM_model)
GLM_model

results <- resamples(list(LM_model, KNN_model, GLM_model))
summary(results)

# Despite all the models having worse stats overall, LM remains the best model
# with the lowest RMSE and MAE and highest R2

# generate predictions and check for errors

predictions <- predict(LM_model, newdata = test)

postResample(predictions, test$AvgPrice)

summary(LM_model)

# The errors have increased and R2 has been slightly reduced, however the new
# model has revealed multiple of the factor columns and intercept to be
# significant (including the model itself)

# --- EXERCISE P ---------------------------------------------------------------

# Let me hear your thoughts and comments!

# ──────────────────────────────────────────────────────────────────────────────
# BEN'S SET
# ──────────────────────────────────────────────────────────────────────────────

# --- EXERCISE A ---------------------------------------------------------------

pumpkin <- read.csv("US-pumpkins.csv", stringsAsFactors = T)
# load data

pumpkin <- pumpkin[, c("City.Name", "Variety", "Date", "Low.Price", "High.Price", "Item.Size")]
# Keep only given variables, delete others

# --- EXERCISE B ---------------------------------------------------------------

pumpkin$Price <- (pumpkin$Low.Price + pumpkin$High.Price)/2
pumpkin$Spread <- 5+(pumpkin$High.Price-pumpkin$Low.Price)
# create new columns

pumpkin <- subset(pumpkin, select = -c(Low.Price, High.Price))
# remove useless variables

ggplot(pumpkin, aes(x = Price)) + 
  geom_histogram() +
  labs(title = "Price Histogram")

ggplot(pumpkin, aes(x = Price)) + 
  geom_density() +
  labs(title = "Price Density")

# Fairly left skewed, but the middle looks normalised. The mean is around 200.

ggplot(pumpkin, aes(x = Spread)) + 
  geom_histogram() +
  labs(title = "Spread Histogram")

ggplot(pumpkin, aes(x = Spread)) + 
  geom_density() +
  labs(title = "Spread Density")

# Extremely left skewed, with the majority of the datapoints being close to 0

# --- EXERCISE C ---------------------------------------------------------------

pumpkin$Date <- as.Date(pumpkin$Date, format = "%m/%d/%y")

pumpkin$Month <- as.numeric(format(pumpkin$Date, "%m"))
pumpkin$Year <- as.numeric(format(pumpkin$Date, "%Y"))
# Create month and year columns

pumpkin <- subset(pumpkin, select = -Date)
# Delete Date column

# --- EXERCISE D ---------------------------------------------------------------

pumpkin <- pumpkin[-which(pumpkin$Variety == ""), ]
pumpkin <- pumpkin[-which(pumpkin$Item.Size == ""), ]
# delete rows with NAs

# --- EXERCISE E ---------------------------------------------------------------

# It would be fairly easy to provide a point estimate for the AdjPrice

AdjPrice <- median(pumpkin$Price*pumpkin$Spread)/mean(pumpkin$Spread)

# It gets a bit more tricky with the standard error, since it is usually
# calculated with the standard deviation of the whole population. We could 
# use the standard deviation of the sample (dataset) as an estimate, but
# AdjPrice is made out of two columns, which makes it very hard to calculate

# It could be done with a resampling method like bootstrap, but it is not 
# allowed in this exercise - i'll do it in the next one

# --- EXERCISE F ---------------------------------------------------------------

set.seed(123)
# seed for reproductibility

# The first step is declaring the AdjPrice function
f_adjprice <- function(data, indices) {
  data_sample <- data[indices, ] # Store data from resample with replacement
  # from the original dataset
  median_adjprice <- median(data_sample$Price * data_sample$Spread) # Calculate median
  mean_spread <- mean(data_sample$Spread) # Calculate mean
  return(median_adjprice / mean_spread) # return Adj Price
}

results <- boot(data = pumpkin, statistic = f_adjprice, R = 1000)
# the boot function then performs the calculation 1000 times after sampling
# with replacement from the pumpkin dataset n times (where n = size of the 
# dataset)

# the point estimate is the mean of all the calculated formula results
mean(results$t)

# the standard error is also calculated from them
sd(results$t)

# and the confidence interval is:
boot.ci(results, type = "perc")$percent[4:5]

# --- EXERCISE G ---------------------------------------------------------------

train <- pumpkin[pumpkin$Year<2017,]
test <- pumpkin[pumpkin$Year==2017,]
# set training and testing sets

# --- EXERCISE H ---------------------------------------------------------------

# Before constructing the models, we need to delete the Year variable, as it
# contains only four instances of "2014" among over 1000 instances of "2016"
# which causes the lasso function to have problems with generating the model

sort(tapply(train$Year, train$Year, length))
# amount of each unique entry in Year

train <- subset(train, select = -Year)
test <- subset(test, select = -Year)
# Year removed from both columns

OLS_model <- lm(Price ~ ., data = train)
# Create OLS model

Ridge_model <- train(Price ~ .,
                     data = train,
                     method = "ridge",
                     trControl = trainControl(method = "cv", number = 5))
# Create ridge model with 5 fold cv

Lasso_model <- train(Price ~ .,
                     data = train,
                     method = "lasso",
                     trControl = trainControl(method = "LOOCV"))
# Create lasso model with LOOCV

# --- EXERCISE I ---------------------------------------------------------------

Intercept_model <- lm(Price ~ NULL, data = train)
# Generate an intercept only model

City_model <- lm(Price ~ City.Name, data = train)
# Generate a model with only the City.Name

Month_model <- lm(Price ~ Month, data = train)
# Generate a model with only the month

City_Month_model <- lm(Price ~ City.Name + Month, data = train)
# Generate a model with both the month and the City.Name

# --- EXERCISE J ---------------------------------------------------------------

# Make predictions based on all the models

# OLS Model
predictions_ols <- predict(OLS_model, newdata = test)

# Ridge Model
predictions_ridge <- predict(Ridge_model, newdata = test)

# Lasso Model
predictions_lasso <- predict(Lasso_model, newdata = test)

# Intercept Only Model
predictions_intercept <- predict(Intercept_model, newdata = test)

# City Only Model
predictions_city <- predict(City_model, newdata = test)

# Month Only Model
predictions_month <- predict(Month_model, newdata = test)

# City and Month Model
predictions_city_month <- predict(City_Month_model, newdata = test)


# Create mse function to avoid rewriting the code multiple times
mse <- function(predictions, actual) {
  mean((predictions - actual)^2)
}

# Calculate test MSE for all models (immediately inserted into a data frame for
# readability)
mse_results <- data.frame(OLS = mse(predictions_ols, test$Price), 
                          Ridge = mse(predictions_ridge, test$Price), 
                          Lasso = mse(predictions_lasso, test$Price), 
                          Intercept = mse(predictions_intercept, test$Price), 
                          City = mse(predictions_city, test$Price), 
                          Month = mse(predictions_month, test$Price), 
                          City_Month = mse(predictions_city_month, test$Price))

mse_results
# Lasso is the model with the lowest test MSE, followed closely by OLS and Ridge

# --- EXERCISE K ---------------------------------------------------------------

# Let me hear your thoughts and comments!
