# 
# MACHINE LEARNING EXAM TEMPLATE
# Version 0.1
# 2023-11-29
# 
# Made by Paweł Gach with contributions from others
# 
# ──────────────────────────────────────────────────────────────────────────────
# SECTION 1: START HERE
# ──────────────────────────────────────────────────────────────────────────────
# 
# SCROLL DOWN FOR TABLE OF CONTENTS
# 
# This template is meant to be used as a hub of building blocks
# 
# When the exam is underway, read the problem, make a plan of the relevant
# actions your code will need to perform, then go to the respective sections
# (.R files) and copy the relevant code - do this a few times,
# and you will have created a code that should be up and running
# 
# The dataset used during the exam will be US-pumpkins and the code will refer
# to simply it as "pumpkin":

pumpkin <- read.csv("US-pumpkins.csv")

# with its characteristics being:

skimr::skim(pumpkin)

# ── Data Summary ────────────────────────
# Values
# Name                       pumpkin
# Number of rows             1757
# Number of columns          26
# _______________________
# Column type frequency:
# character                13
# logical                  9
# numeric                  4
# ________________________
# Group variables            None

# ── Variable type: character ──────────────────────────────────────────────────
# skim_variable     n_missing complete_rate min max empty n_unique whitespace
# 1 City.Name               0             1   5  13     0       13          0
# 2 Type                    0             1   0   7  1712        2          0
# 3 Package                 0             1   4  20     0       15          0
# 4 Variety                 0             1   0  24     5       11          0
# 5 Sub.Variety             0             1   0  10  1461        3          0
# 6 Date                    0             1   6   8     0       57          0
# 7 Origin                  0             1   0  14     3       25          0
# 8 Origin.District         0             1   0  37  1626        6          0
# 9 Item.Size               0             1   0   7   279        8          0
# 10 Color                  0             1   0   7   616        4          0
# 11 Unit.of.Sale           0             1   0  10  1595        5          0
# 12 Repack                 0             1   1   1     0        2          0
# 13 X.1                    0             1   0  31  1654        5          0
#
# ── Variable type: logical ────────────────────────────────────────────────────
# skim_variable   n_missing complete_rate mean count
# 1 Grade              1757             0  NaN ": "
# 2 Environment        1757             0  NaN ": "
# 3 Quality            1757             0  NaN ": "
# 4 Condition          1757             0  NaN ": "
# 5 Appearance         1757             0  NaN ": "
# 6 Storage            1757             0  NaN ": "
# 7 Crop               1757             0  NaN ": "
# 8 Trans.Mode         1757             0  NaN ": "
# 9 X                  1757             0  NaN ": "
#
# ── Variable type: numeric ────────────────────────────────────────────────────
# skim_variable   n_missing complete_rate mean  sd   p0  p25 p50 p75 p100 hist
# 1 Low.Price             0         1     125. 83.9 0.24 24   140 180  480 ▆▇▃▁▁
# 2 High.Price            0         1     133. 89.5 0.24 24.5 150 200  480 ▇▇▅▁▁
# 3 Mostly.Low          103         0.941 128. 86.5 0.24 24.6 147 185  480 ▆▇▅▁▁
# 4 Mostly.High         103         0.941 132. 88.4 0.24 26.1 150 200  480 ▆▇▅▁▁
# 
# 
# 
TABLE OF CONTENTS
As this is Version 0.1, this section is subject to change

1. Start Here

2. Loading and Cleaning
    - Extraction
    - Exploration
    - Manipulations (incl. Common Problems)
    - Outliers
        - Detecing
        - Handling
    - Normalisation/Centralisation
    - Standardisation
    - Missing Values
        - Detecting
        - Imputation
        - Deletion
    - Lumping
    - Dummy encoding
    - Label encoding
    - Writing Files

3. Preparing the Parameters
    - Subset Selection
        - Mallow's Cp
        - Akaike Information Criterion
        - Bayesian Information Criterion
        - Backwards Stepwise
        - Forwards Stepwise
    - Shrinkage (Regularisation)
        - Ridge Regression
        - Lasso
    - Sampling
    - Resampling
        - LOOCV
        - k-fold Cross Validation
        - Bootstrap

4. Numeric Models
    - K Nearest Neighbors (KNN)
    - Ordinary Linear Regression (OLS)

5. Categorical Models
    - Principal Component Analysis (PCA)
    - Logistic Regression
    - Linear Discriminant Analysis (LDA)
    - Quadratic Discriminant Analysis (QDA)
    - Lift Chart
    - Bayes Classifiers
        - Exact
        - Naive
#