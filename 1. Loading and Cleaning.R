#
# ──────────────────────────────────────────────────────────────────────────────
#
# SECTION 1: LOADING AND CLEANING
#
# ──────────────────────────────────────────────────────────────────────────────
#
# Libraries used:

library(skimr)
library(ggplot2)
library(readxl)
library(jsonlite)
library(stringr)
library(scales)
library(forecast)
library(car)
library(DataExplorer)
library(forcats)
library(writexl)
library(rsample)
library(Hmisc)

# SECTION TABLE OF CONTENTS
#
# 
# 1. Loading and Cleaning
#     - Extraction
#     - Exploration
#     - Manipulations (incl. Common Problems)
#     - Outliers
#         - Detecting
#         - Handling
#     - Normalisation (Transformations)
#     - Standardisation
#     - Centralisation
#     - Missing Values
#         - Detection
#         - Imputation
#         - Deletion
#     - Lumping
#     - Dummy Encoding
#     - Writing Files
#     - Sampling
#
# ──────────────────────────────────────────────────────────────────────────────
# EXTRACTION
# ──────────────────────────────────────────────────────────────────────────────
#
# Extracting US-pumpkins is simple can be done in one line:

pumpkin <- read.csv("US-pumpkins.csv")

# If the need arises, you can also read files using:

data <- readxl::read_excel("filepath.xlsx")
# for Excel files (.xlsx and others)

data <- read.table("filepath.txt", header = TRUE, sep = "\t")
data <- read.delim("filepath.txt", header = TRUE, sep = "\t")
# for .txt files

data <- jsonlite::fromJSON("filepath.json")
# for .json files

# ──────────────────────────────────────────────────────────────────────────────
# EXPLORATION
# ──────────────────────────────────────────────────────────────────────────────
#
# The first step to inspecting the data is always viewing it

View(pumpkin)

# After that, I recommend using library(skimr), as it 
# returns the most important informations in one go

skim(pumpkin)

# Other useful functions here include:

class(pumpkin)
#returns the class of the object

dim(pumpkin)
#returns the dimensions of the object

head(pumpkin)
#returns the headers and first few rows

summary(pumpkin)
#returns summary dataframe statistics

str(pumpkin)
#displays the structure of an object

unique(pumpkin$Package)
#display all unique column elements

# ──────────────────────────────────────────────────────────────────────────────
# MANIPULATION (INCL. COMMON PROBLEMS)
# ──────────────────────────────────────────────────────────────────────────────

subset(pumpkin, High.Price > 100)
# Use to select relevant columns

pumpkin$Repack <- factor(pumpkin$Repack)
# turn characters into numerical factors that models understand

cols <- c("City.Name", "Variety", "Origin")
for (col in cols) {
  pumpkin[[col]] <- factor(pumpkin[[col]])
}
# you can also do a few at a time

colnames(pumpkin) <- tolower(colnames(pumpkin))
# turn column names to lowercase

tapply(pumpkin$High.Price, pumpkin$City.Name, mean)
# apply functions to grouped columns for quick analysis

colSums(pumpkin[,8:9])
rowSums(pumpkin[1:100,8:9])
# Get sums of rows and columns, respecitvely

colnames(pumpkin)[1] <- "city.name"
#change names of columns

# DELETING COLUMNS
# There are multiple ways to achieve this

pumpkin$Crop <- NULL
pumpkin <- pumpkin[, -23]
pumpkin <- pumpkin[, -c(21:22)]
pumpkin <- subset(pumpkin, select = -c(Quality, Condition))

# You can also select a given group that will be retained, while all
# else will be deleted
pumpkin <- pumpkin[, c("City.Name", "Variety", "Origin")]

#

attach(pumpkin)
detach(pumpkin)
# access columns without 'pumpkin$' (and cancel)

# STANDARDISING FORMATS

# In

pumpkin$Package

# there are multiple unique entries that have different measurements

unique(pumpkin$Package)

# It is crucial to standardise it to kilograms

pumpkin$Package_Numeric <- NA #make a new column
lb_to_kg <- 0.453592
inch_to_kg <- 20 #just a guess, inches aren't a measure of weight
bushel_to_kg <- 10 #another guess
bins_to_kg <- 5 #more guessing
each_to_kg <- 1 #you guessed it

for (i in 1:length(pumpkin$Package)) {
  
  if (str_detect(pumpkin$Package[i], "lb")) { #check if the value is "lb"
    temp <- as.numeric(substr(pumpkin$Package[i], start = 1, stop = 2))
    #extract numeric value
    pumpkin$Package_Numeric[i] <- temp * lb_to_kg
    #assign to new column
  }
  
  else if (str_detect(pumpkin$Package[i], "inch")) { 
    temp <- as.numeric(substr(pumpkin$Package[i], start = 1, stop = 2))
    pumpkin$Package_Numeric[i] <- temp * inch_to_kg
  }
  
  else if (str_detect(pumpkin$Package[i], "bushel")) {
    if (str_detect(pumpkin$Package[i], "1 1/9")) {
      pumpkin$Package_Numeric[i] <- (1+1/9) * bushel_to_kg
    }
    else if (str_detect(pumpkin$Package[i], "1/2")) {
      pumpkin$Package_Numeric[i] <- 1/2 * bushel_to_kg
    }
    else {
      pumpkin$Package_Numeric[i] <- bushel_to_kg
    }
  }
  
  else if (str_detect(pumpkin$Package[i], "each")) { 
    pumpkin$Package_Numeric[i] <- each_to_kg
  }
  
  else if (str_detect(pumpkin$Package[i], "bins")) { 
    pumpkin$Package_Numeric[i] <- bins_to_kg
  }
  
  else {
    pumpkin$Package_Numeric[i] <- NULL
  }
  
}

# After that, run skim() one more time to check if there are NAs, and then you
# can safely delete the original pumpkin$Package column

# STANDARDISING DATES
# Another problem is the date format

class(pumpkin$Date)

# It needs to be a Date class, not a character

pumpkin$Date <- as.Date(pumpkin$Date, format = "%m/%d/%y")
# we specify the format for the as.Date() function

# ──────────────────────────────────────────────────────────────────────────────
# OUTLIERS - DETECTING
# ──────────────────────────────────────────────────────────────────────────────

boxplot(pumpkin$High.Price) # visualises outliers

# you can also try:

ggplot(pumpkin, aes(x = High.Price, y = City.Name)) + 
  geom_boxplot()

# if you want to see outliers per group

z_scores <- scale(pumpkin$High.Price)  # scale() computes the Z-score
outliers <- pumpkin$High.Price[abs(z_scores) > 3]  # try 2 or 3
outlier_indices <- which(abs(z_scores) > 3)  # which() gets the row indices

# You can also try
max(pumpkin$High.Price)
which(pumpkin$High.Price == max(pumpkin$High.Price))

min(pumpkin$High.Price)
which(pumpkin$High.Price == min(pumpkin$High.Price))

# ──────────────────────────────────────────────────────────────────────────────
# OUTLIERS - HANDLING
# ──────────────────────────────────────────────────────────────────────────────

# The easiest thing to do is to delete outliers - you need to delete the
# entire row at once

pumpkin <- pumpkin[-3, ] #delete the outlier row by indice (after finding it)
pumpkin <- pumpkin[pumpkin$High.Price<=100, ] 
# delete outlier row by condition (or rather, keep those that match condition)

# You can also transform the whole column to normalise it which will limit
# the impact of the dataset - look to Normalisation (Transformations) Section

# Another option is imputing them like you would with NAs - look to the
# Missing Values - Imputation Section

# ──────────────────────────────────────────────────────────────────────────────
# NORMALISATION (TRANSFORMATIONS)
# ──────────────────────────────────────────────────────────────────────────────

# This is the process of bringing the dataset closer to a normal distribution
# It can be achieved with transformations

# The Log transformation is included in R (since it means you just take a
# logarithm)

pumpkin_log <- log(pumpkin$High.Price)

# The BoxCox transformation is from library(forecast)

pumpkin_bc <- BoxCox(pumpkin$High.Price, lambda = "auto")

# The YeoJohnson transformation is from library(car)

pumpkin_yj <- yjPower(pumpkin$High.Price, 0)

# After you carry out all the transformations, remember to
# plot them and choose the best one!

par(mfrow = c(2,2))
hist(pumpkin$High.Price)
hist(pumpkin_log)
hist(pumpkin_bc)
hist(pumpkin_yj)
par(mfrow = c(1,1)) #remember to always reset the plot display

# Use this handy function to instantly compare transformations

plot_all_trans <- function(x) {
  x_plots <- NULL
  x_plots[1] <- hist(x, main = "Original Data")
  x_plots[2] <- hist(log(x), main = "Log")
  x_plots[3] <- hist(forecast::BoxCox(x, lambda = "auto"), main = "BoxCox")
  x_plots[4] <- hist(car::yjPower(x, 0), main = "Yeo Johnson")
  return(x_plots)
}

par(mfrow = c(2,2))
plot_all_trans(pumpkin$High.Price)
par(mfrow = c(1,1))

# ──────────────────────────────────────────────────────────────────────────────
# STANDARDISATION
# ──────────────────────────────────────────────────────────────────────────────

# You can standardise (scale the data to values between -1 and 1) in two ways

# the first one is your own function

standardise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

pumpkin$High.Price <- standardise(pumpkin$High.Price)

# the second is an external function from library(scales)

pumpkin$High.Price <- rescale(pumpkin$High.Price, to = c(0, 1))

# You can also 

# ──────────────────────────────────────────────────────────────────────────────
# CENTRALISATION
# ──────────────────────────────────────────────────────────────────────────────

# Centralisation means bringing your data to mean 0 and standard dev 1
# There are also two ways here

# own function

centralise <- function(x) {
  return((x - mean(x)) / sd(x))
}

pumpkin$High.Price <- centralise(pumpkin$High.Price)

# another function from library(scales)

pumpkin$High.Price <- scale(pumpkin$High.Price)

# ──────────────────────────────────────────────────────────────────────────────
# MISSING VALUES - DETECTION
# ──────────────────────────────────────────────────────────────────────────────

# The presence and amount of missing values will already be included in
# the output of skim(), but there are other ways

sum(is.na(pumpkin$Mostly.High)) #returns amount of NAs
which(is.na(pumpkin$Mostly.High)) #returns location of NAs
# is.na returns TRUE whenever there is no value

# You can also plot NAs with library(DataExplorer)
plot_missing(pumpkin)

# ──────────────────────────────────────────────────────────────────────────────
# MISSING VALUES - IMPUTATION
# ──────────────────────────────────────────────────────────────────────────────

# Imputation can be carried out in a few ways

# The first one is replacing it with mean or median

x <- pumpkin$Mostly.High
#I'm using x here as a shorthand to make the code more readable
pumpkin$Mostly.High[is.na(x)] <- mean(x, na.rm = TRUE)
pumpkin$Mostly.High[is.na(x)] <- median(x, na.rm = TRUE)

# The second is just choosing a random value

x <- pumpkin$Mostly.High
pumpkin$Mostly.High[is.na(x)] <- sample(x[!is.na(x)], sum(is.na(x)), replace = TRUE)
# The line above chooses a random value from non-na entries for each na entry
# with replacement

# The third one takes advantage of library(Hmisc), but it requires the data
# to be a factor

pumpkin$Mostly.High <- impute(as.factor(pumpkin$Mostly.High))

# The fourth is using K-Nearest Neighbours - check out the section on the 
# Recipes Package for more details

# ──────────────────────────────────────────────────────────────────────────────
# MISSING VALUES - DELETION
# ──────────────────────────────────────────────────────────────────────────────

# Deleting NAs is as simple as deleting the outliers in the previous section,
# but it can be done a bit differently

clean_pumpkin <- na.omit(pumpkin) # deletes all rows with NAs
# IN CASE OF PUMPKIN, THIS WILL DELETE ALL ROWS UNLESS YOU REMOVE THE EMPTY
# COLUMNS FIRST

# before running na.omit(), make sure to check for the amount of complete rows
sum(complete.cases(pumpkin))

# ──────────────────────────────────────────────────────────────────────────────
# LUMPING
# ──────────────────────────────────────────────────────────────────────────────

# Lumping refers to grouping the rarest levels of a categorical variable into
# a single "other" group

# First, check the amount of entries for each group
sort(tapply(pumpkin$City.Name, pumpkin$City.Name, length))

# Then, you can use library(forcats) for lumping (forcats, not forecasts)

pumpkin$City.Name <- fct_lump(pumpkin$City.Name, n = 5)
# Keeps the top 'n' levels, lumps the rest into 'Other'

# ──────────────────────────────────────────────────────────────────────────────
# DUMMY ENCODING
# ──────────────────────────────────────────────────────────────────────────────

# This allows you to transform a column of categorical vars into several binary
# columns. It can be useful when analysing the impact of only a single level
# or when the model is purely numeric. model.matrix() is included in base R

dummies <- model.matrix(~ Repack - 1, data = pumpkin)
dummy_pumpkin <- cbind(pumpkin, dummies)

# ──────────────────────────────────────────────────────────────────────────────
# WRITING FILES
# ──────────────────────────────────────────────────────────────────────────────

# Write a dataframe to a CSV file
write.csv(pumpkin, "pumpkin.csv", row.names = FALSE)

# Write a dataframe to a text file
write.table(pumpkin, "pumpkin.txt", row.names = FALSE, sep = "\t")

# Writing to excel with library(writexl)
write_xlsx(pumpkin, "pumpkin.xlsx")

# Writing to JSON with library(jsonlite)
write_json(pumpkin, "pumpkin.json")

# ──────────────────────────────────────────────────────────────────────────────
# SAMPLING
# ──────────────────────────────────────────────────────────────────────────────

# In the context of this class sampling refers to splitting the sample
# into training and testing sets. This can be achieved in two main ways

# Both will require you to set a seed, which ensures that despite the
# presence of random functions, the outcome can be reproduced

set.seed(123) # it doesn't really matter what number you choose here

# the first method can be done in base R

n <- nrow(pumpkin)
shuffle <- sample(n) # generate a vector of shuffled row indices

# Splitting into 70% training and 30% testing

splitIndex <- round(n * 0.7) # rounding to ensure accuracy
train <- data[shuffle[1:splitIndex], ]
test <- data[shuffle[(splitIndex + 1):n], ]


# the second method involves library(rsample)

# Create a 70/30 split
split <- initial_split(pumpkin, prop = 0.7)
train <- training(split)
test <- testing(split)

