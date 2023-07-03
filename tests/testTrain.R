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


# Test train function
test_that("train function works correctly", {

  data(BreastCancer)
  data <- BreastCancer
  data <- data[, -c(1)]
  data$Class <- factor(data$Class)
  data <- na.omit(data)

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


  indices <- createDataPartition(train_set$Class, p = 0.8, list = FALSE)
  val_set <- train_set[-indices, ]
  train_set <- train_set[indices, ]

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
  EIPIP_instance$train(train_set, val_set)

  # Check if the model is trained
  expect_true(EIPIP_instance$ensemble_model != NULL)

  # Check if the model has the expected configuration
  expected_conf <- list(learner, learner, learner, learner, learner, learner, learner)
  assert_df_equal(EIPIP_instance$conf, expected_conf)

  # Check if the model has the expected training data
  assert_df_equal(EIPIP_instance$train_data, train_set)

  # Check if the model has the expected validation data
  assert_df_equal(EIPIP_instance$val_data, val_set)
})
