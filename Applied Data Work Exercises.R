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