#
# ──────────────────────────────────────────────────────────────────────────────
#
# SECTION 3: CATEGORICAL MODELS
#
# ──────────────────────────────────────────────────────────────────────────────
#
# Libraries used:



# SECTION TABLE OF CONTENTS
#
# 
# 3. Categorical Models
#     - Logistic Regression
#     - Linear Discriminant Analysis (LDA)
#     - Quadratic Discriminant Analysis (QDA)
#     - Lift Chart
#     - Bayes Classifiers
#         - Exact
#         - Naive
#     - Model Evaluation
#         - Accuracy, Precision, Recall
#         - F1-Score
#         - ROC and AUC
#
# ──────────────────────────────────────────────────────────────────────────────
# LOGISTIC REGRESSION
# ──────────────────────────────────────────────────────────────────────────────

# First, let's factorise a few columns of train (you would usually just do this
# on pumpkin beforehand)
cols <- c("City.Name", "Variety", "Origin")
for (col in cols) {
  train[[col]] <- factor(train[[col]])
}

# and we will also only retain the categorical columns

train <- train[, cols]
