# Exam Template

Made by **Paweł Gach** with contributions from others

1st semester Business Intelligence at AU BSS

Version 1.4

2024-01-14

------------------------------------------------------------------------

This template is meant to be used as a hub of building blocks

When the exam is underway, read the problem, make a plan of the relevant actions your code will need to perform, then go to the respective sections (.R files) and copy the relevant code - do this a few times, and you will have created a code that should be up and running and (reasonably) bug-free

The dataset used during the Machine Learning exam will be US-pumpkins.csv and the code will refer to simply it as "pumpkin". Sections relevant to ML are 1 through 4.

The code also includes two sections, 1 and 5, relevant for Business Forecasting, as well as subsections helpful for the R elements of the DMDV exam in Section 4.

## Need some more theoretical notes?

Is so, check out:

[Machine Learning for Business Intelligence - Paweł Gach's Notion](https://www.notion.so/pawelgach/Aarhus-Uni-Home-91aeb930754247b098340fbc6616b777?pvs=4)

## Table Of Contents

As this is not the final version, this section is subject to change

1.  Loading and Cleaning
    -   Extraction
    -   Exploration
    -   Manipulations (incl. Common Problems)
    -   Outliers
        -   Detecting
        -   Handling
    -   Normalisation (Transformations)
    -   Standardisation
    -   Centralisation
    -   Missing Values
        -   Detection
        -   Imputation
        -   Deletion
    -   Lumping
    -   Dummy Encoding
    -   Writing Files
    -   Sampling
2.  Regression Models
    -   K Nearest Neighbors (KNN)
    -   Ordinary Linear Regression (OLS)
    -   Resampling
        -   LOOCV
        -   k-fold Cross Validation
        -   Bootstrap
    -   Principal Component Analysis (PCA)
    -   Shrinkage (Regularisation)
        -   Ridge Regression
        -   Lasso
    -   Subset Selection
        -   Backwards Stepwise
        -   Forwards Stepwise
    -   Model Evaluation
        -   R2 & adjusted R2
        -   MSE & MAE
        -   Mallow's Cp
        -   Akaike Information Criterion
        -   Bayesian Information Criterion
        -   Log Likelihood
3.  Categorical Models
    -   Discretization
    -   Logistic Regression
    -   Linear Discriminant Analysis (LDA)
    -   Quadratic Discriminant Analysis (QDA)
    -   Naive Bayes Classifier
    -   Model Evaluation
        -   Accuracy, Precision, Recall
        -   F1-Score
        -   ROC and AUC
        -   Lift Chart
4.  Data Visualisation & Automation
    -   ggplot2 Package
    -   recipes Package
    -   shiny Package
    -   cronR Package
    -   httr2 Package (API calls)
    -   DBI and RPostgres Packages (Database Integration)
5.  Time Series Models
    -   Loading and Splitting TS Objects
    -   Analysing Dynamic Properties
    -   Simple Forecasts
        -   Previous observed value
        -   Previous value + proportion of previously observed change
        -   Lagged Forecast
        -   Averages
        -   Weighed Forecasts
    -   Moving Averages
        -   Single
        -   Double
    -   Exponential Smoothening
        -   Normal
        -   Holt's
        -   Winter's
    -   Linear Regression
    -   Time Series Decomposition
    -   ARIMA
    -   ADL Models
    -   VAR Models
    -   IRF Cholesky Models
    -   Non-stationary tests
        -   ADF
        -   EG
        -   BP
        -   JB
    -   Combining forecasts
        -   Simple Averaging
        -   Weighted Averaging
        -   Nelson Combination
        -   Granger-Ramanathan Combination Method
    -   Model Evaluation
        -   Accuracy
        -   Diebold-Mariano
6.  Appendix: Applied Data Work Exercises
    -   Ana's Set
    -   Ben's Set
