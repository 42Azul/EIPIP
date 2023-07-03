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


# Test metricTest function
test_that("metricTest function works correctly", {
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

  # Evaluate model performance
  performance <- EIPIP_instance$metricTest(test_set)

  #Check if the performance metrics have the expected names
  expected_metrics <- c("KAPPA", "SENS", "BAL_ACC")
  expect_equal(names(performance), expected_metrics)

  #Check if the performance metrics have valid values
  for (metric in expected_metrics) {
    expect_true(!is.na(performance[[metric]]))
  }
})

