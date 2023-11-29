#
# ──────────────────────────────────────────────────────────────────────────────
# SECTION 2: LOADING AND CLEANING
# ──────────────────────────────────────────────────────────────────────────────
#
# Libraries used:

library(skimr)
library(readxl)
library(jsonlite)

# SECTION TABLE OF CONTENTS
#
# 2. Loading and Cleaning
#     - Extraction
#     - Exploration
#     - Manipulations (incl. Common Problems)
#     - Outliers
#         - Detecing
#         - Handling 
#     - Normalisation/Centralisation
#     - Standardisation
#     - Missing Values
#         - Detecting
#         - Imputation
#         - Deletion
#     - Lumping
#     - Dummy encoding
#     - Label encoding
#     - Writing Files
#
# ──────────────────────────────────────────────────────────────────────────────
# EXTRACTION
# ──────────────────────────────────────────────────────────────────────────────
#
# As shown in Section 1, extracting US-pumpkins can be done in one line:

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

# After that, like in the first Section, I recommend the skimr library
# It returns the most important informations in one go

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

# ──────────────────────────────────────────────────────────────────────────────
# MANIPULATION
# ──────────────────────────────────────────────────────────────────────────────
#
#