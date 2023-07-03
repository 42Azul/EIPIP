library(testthat)
source("../R/EIPIP.R")
library(caret)
library(MLmetrics)
library(mlbench)

# Define helper functions for testing

# Helper function to check if two data frames are equal
assert_df_equal <- function(actual, expected) {
  # Check column names
  expect_equal(colnames(actual), colnames(expected))

  # Check data
  for (col in colnames(actual)) {
    expect_identical(actual[[col]], expected[[col]])
  }
}


library(testthat)
source("../R/EIPIP.R")
library(caret)
library(MLmetrics)
library(mlbench)

# Define helper functions for testing

# Helper function to check if two data frames are equal
assert_df_equal <- function(actual, expected) {
  # Check column names
  expect_equal(colnames(actual), colnames(expected))

  # Check data
  for (col in colnames(actual)) {
    expect_identical(actual[[col]], expected[[col]])
  }
}



# Test predict function
test_that("predict function works correctly", {
  # Load BreastCancer data
  data(BreastCancer)
  data <- BreastCancer
  data <- data[, -c(1)]
  data$Class <- factor(data$Class)
  data <- na.omit(data)

  # Split into train and test set
  indices <- createDataPartition(data$Class, p = 0.3, list = FALSE)
  train_set <- data[indices, ]
  test_set <- data[-indices, ]

  learner <- function(df.train, metrics, OUTPUT, metric_optimize) {
    tC <- trainControl(
      summaryFunction = metrics,
      allowParallel = TRUE,
      classProbs = TRUE
    )
  rlog <- train(as.formula(sprintf("%s ~.", OUTPUT)),
                data = df.train,
                method = "glmnet",
                family = 'binomial',
                metric = metric_optimize,
                maximize = TRUE,
                trControl = tC
  )

  return(rlog)
  }

  metrics <- function(data, lev = levels(as.factor(data$obs)), model = NULL){
    sensi = sensitivity(data[, "pred"],data[, "obs"],positive="malignant",negative="benign")
    met <- c(
    KAPPA = psych::cohen.kappa(cbind(data[, "obs"],data[, "pred"]))$kappa,
    SENS = sensi,
    BAL_ACC = ( sensi + specificity(data[, "pred"], data[, "obs"],positive="malignant",negative="benign"))/2
  )

  return(met)}


  # Train the model
  EIPIP_instance <- EIPIP$new(
    OUTPUT_VAR = "Class",
    OUTPUT_MIN = "malignant",
    OUTPUT_MAJ = "benign",
    conf = list(learner, learner, learner, learner, learner, learner, learner),
    metrics = metrics,
    TRAIN_METRIC = "BAL_ACC",
    exhaustive = FALSE
  )
  EIPIP_instance$train(train_set, NULL)

  #Make predictions
  predictions <- EIPIP_instance$predict(test_set)

  #Check if predictions have the expected number of rows
  expect_equal(nrow(predictions), nrow(test_set))

  #Check if predictions have the expected column names
  expected_cols <- c("pred", "probs")
  expect_equal(colnames(predictions), expected_cols)

  #Check if predictions have the expected class labels
  expected_labels <- levels(test_set$Class)
  expect_equal(levels(predictions$pred), expected_labels)

  #Check if predictions have probabilities within the valid range [0, 1]
  expect_true(all(predictions$probs >= 0 & predictions$probs <= 1))
})
