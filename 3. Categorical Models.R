#
# ──────────────────────────────────────────────────────────────────────────────
#
# SECTION 3: CATEGORICAL MODELS
#
# ──────────────────────────────────────────────────────────────────────────────
#
# Libraries used:

library(stats)
library(forcats)
library(rsample)
library(MASS)
library(e1071)
library(caret)
library(pROC)

# SECTION TABLE OF CONTENTS
#
# 
# 3. Categorical Models
#     - Logistic Regression
#     - Linear Discriminant Analysis (LDA)
#     - Quadratic Discriminant Analysis (QDA)
#     - Naive Bayes Classifier
#     - Model Evaluation
#         - Accuracy, Precision, Recall
#         - F1-Score
#         - ROC and AUC
#         - Lift Chart
#
# ──────────────────────────────────────────────────────────────────────────────
# LOGISTIC REGRESSION
# ──────────────────────────────────────────────────────────────────────────────

# First, let's factorise and lump a few columns of pumpkin (you would usually 
# just do this in preprocessing beforehand)
cols <- c("City.Name", "Variety", "Origin", "Repack")
for (col in cols) {
  pumpkin[[col]] <- factor(pumpkin[[col]])
  pumpkin[[col]] <- fct_lump(pumpkin[[col]], n = 5)
}

pumpkin <- pumpkin[, cols]

split <- initial_split(pumpkin, prop = 0.7)
train <- training(split)
test <- testing(split)

# and we will also only retain the categorical columns

# Logistic regression can be easily performed with a function library(stats)
lr_model <- glm(Repack ~ ., data = train, family = "binomial")

# extracting the results:
summary(lr_model)

# and for making predictions:
probabilities <- predict(lr_model, newdata = test, type = "response")

# decide which class is most likely
lr_pred <- ifelse(probabilities > 0.5, 1, 0)

# display the total amount of each class
table(test$Repack, lr_pred)

# ──────────────────────────────────────────────────────────────────────────────
# LINEAR DISCRIMINANT ANALYSIS (LDA)
# ──────────────────────────────────────────────────────────────────────────────

# a simple function from library(MASS)
lda_model <- lda(Repack ~ ., data = train)

# in this case you should just call the model without summary()
lda_model

# the base plot() function also works well
plot(lda_model)

# prediction works similiar to LR. You display it as a confusion matrix
# it tells you when the model predicted the class accurately (e.g. E as E)
# and when it made a blunder (e.g. identified E as N)
lda_pred <- predict(lda_model, newdata = test)
table(test$Repack, lda_pred$class)

# ──────────────────────────────────────────────────────────────────────────────
# QUADRATIC DISCRIMINANT ANALYSIS (QDA)
# ──────────────────────────────────────────────────────────────────────────────

# The groups in categorical predictors in pumpkin often have rank deficiency
# they are either too small or too multicorrelated to be used for QDA
# we will therefore have to use a different dataset for explanatory purposes

data(iris)
split <- initial_split(iris, prop = 0.7)
i_train <- training(split)
i_test <- testing(split)

# generate a model using another function from library(MASS)
qda_model <- qda(Species ~ ., data = i_train)

# explore the results
qda_model

# plot() doesn't work with QDA objects, unfortunately

# prediction is similiarily easy
qda_pred <- predict(qda_model, newdata = i_test)
table(i_test$Species, qda_pred$class)

# ──────────────────────────────────────────────────────────────────────────────
# NAIVE BAYES CLASSIFIER
# ──────────────────────────────────────────────────────────────────────────────

# We can use a function from library(e1071)
nb_model <- naiveBayes(Repack ~ ., data = train)

# call to inspect
nb_model

# predict and make a confusion matrix
nb_pred <- predict(nb_model, newdata = test)
table(test$Repack, nb_pred)

# ──────────────────────────────────────────────────────────────────────────────
# MODEL EVALUATION - ACCURACY, PRECISION, RECALL
# ──────────────────────────────────────────────────────────────────────────────

# These three metrics are all calculated from the confusion matrix
# Theoretically, you could do it by hand just from the table() output of all the
# previous sections - refer to equations in the theoretical notes

# Here we cover the more automatic approach

# We can use the function from library(caret) on predicted and actual
# values from any model
conf_matrix <- confusionMatrix(as.factor(lda_pred$class), as.factor(test$Repack))

# extract the values from the object
accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Precision']
recall <- conf_matrix$byClass['Sensitivity']

# print values
accuracy
precision
recall

# ──────────────────────────────────────────────────────────────────────────────
# MODEL EVALUATION - F1-SCORE
# ──────────────────────────────────────────────────────────────────────────────

# following the code from the previous section, the F1-Score can be calculated
# as such:

f1_score <- 2 * (precision * recall) / (precision + recall)

# call to print
f1_score

# note that in this example, the score is zero due to the specifics of pumpkin

# ──────────────────────────────────────────────────────────────────────────────
# MODEL EVALUATION - ROC AND AUC
# ──────────────────────────────────────────────────────────────────────────────

# Here, we use the main function of the library(pROC) package
# you need to pass the actual values and posterior probabilities
# (not the predicted values) of the classes
roc_obj <- roc(test$Repack, lda_pred$posterior[,2])

# then plot as normal
plot(roc_obj, main="ROC Curve")

# AUC can be obtained with another pROC function
auc(roc_obj)

# ──────────────────────────────────────────────────────────────────────────────
# MODEL EVALUATION - LIFT CHART
# ──────────────────────────────────────────────────────────────────────────────

# First, make a dataframe with the actual classes in one column, and the 
# predictedd probabilities in another
lift_frame <- data.frame(actual = test$Repack, predicted = lda_pred$posterior[,2])

# Then, use the lift() function from library(caret)
lift_obj <- lift(actual ~ predicted, data = lift_frame)
plot(lift_obj, plot = "gain")
