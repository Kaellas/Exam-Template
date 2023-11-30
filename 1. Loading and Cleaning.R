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

# SECTION TABLE OF CONTENTS
#
# 
# 1. Loading and Cleaning
#     - Extraction
#     - Exploration
#     - Manipulations (incl. Common Problems)
#     - Outliers
#         - Detecing
#         - Handling
#     - Normalisation/Centralisation
#     - Standardisation
#     - Missing Values
#         - Detection
#         - Imputation
#         - Deletion
#     - Lumping
#     - Dummy encoding
#     - Label encoding
#     - Writing Files
# 
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

# After that, I recommend the skimr library, as it 
# returns the most important informations in one go

skimr::skim(pumpkin)

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

unique(pumpkin$Repack)
#display all unique column elements

# ──────────────────────────────────────────────────────────────────────────────
# MANIPULATION (INCL. COMMON PROBLEMS)
# ──────────────────────────────────────────────────────────────────────────────

subset(pumpkin, High.Price > 100)
# Use to select relevant columns

pumpkin$Repack <- factor(pumpkin$Repack)
# turn characters into numerical factors that models understand

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

# You can also try
max(pumpkin$High.Price)
min(pumpkin$High.Price)
