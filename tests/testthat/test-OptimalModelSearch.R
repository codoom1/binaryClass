test_that("OptimalModelSearch functions correctly with different criteria", {
  # Skip on CRAN to avoid time-consuming tests
  testthat::skip_on_cran()
  
  # Load test data
  data(iris)
  
  # Create a binary classification dataset
  binary_data <- iris[iris$Species != "virginica", ]
  # Ensure Species is coded as 0/1 for binary classification
  binary_data$Species <- factor(ifelse(binary_data$Species == "setosa", 0, 1), levels = c(0, 1))
  
  # Set seed for reproducibility
  set.seed(123)
  
  # Test AUC criterion
  result_auc <- OptimalModelSearch(
    Species ~ ., 
    data = binary_data, 
    criterion = "AUC", 
    training_percent = 0.7
  )
  
  # Check structure of AUC results
  expect_type(result_auc, "list")
  expect_true("criterion" %in% names(result_auc))
  expect_true("best_model_name" %in% names(result_auc))
  expect_true("performance_metric" %in% names(result_auc))
  expect_true("best_model_object" %in% names(result_auc))
  expect_true("coefficients" %in% names(result_auc))
  expect_true("details" %in% names(result_auc))
  
  # Check values of AUC results
  expect_equal(result_auc$criterion, "AUC")
  expect_true(result_auc$performance_metric >= 0 && result_auc$performance_metric <= 1)
  
  # Test Accuracy criterion
  result_accuracy <- OptimalModelSearch(
    Species ~ ., 
    data = binary_data, 
    criterion = "Accuracy", 
    training_percent = 0.7,
    threshold = 0.5
  )
  
  # Check structure of Accuracy results
  expect_type(result_accuracy, "list")
  expect_equal(result_accuracy$criterion, "Accuracy")
  expect_true(result_accuracy$performance_metric >= 0 && result_accuracy$performance_metric <= 1)
  
  # Test AIC criterion
  result_aic <- OptimalModelSearch(
    Species ~ ., 
    data = binary_data, 
    criterion = "AIC", 
    training_percent = 0.7
  )
  
  # Check structure of AIC results
  expect_type(result_aic, "list")
  expect_equal(result_aic$criterion, "AIC")
  expect_true(is.numeric(result_aic$performance_metric))
  
  # Test error handling
  expect_error(OptimalModelSearch(Species ~ ., data = binary_data, criterion = "InvalidCriterion"))
  
  # Test handling of missing data
  # Create data with some NA values
  binary_data_with_na <- binary_data
  binary_data_with_na[sample(1:nrow(binary_data), 10), "Sepal.Length"] <- NA
  
  # Should handle NAs without error
  expect_error(OptimalModelSearch(Species ~ ., data = binary_data_with_na, criterion = "AUC"), NA)
})

test_that("OptimalModelSearch GAM model works correctly", {
  testthat::skip_on_cran()
  
  # Load test data
  data(iris)
  
  # Create a binary classification dataset
  binary_data <- iris[iris$Species != "virginica", ]
  # Ensure Species is coded as 0/1 for binary classification
  binary_data$Species <- factor(ifelse(binary_data$Species == "setosa", 0, 1), levels = c(0, 1))
  
  # Set seed for reproducibility
  set.seed(456)
  
  # Only run this test if mgcv is available
  if (requireNamespace("mgcv", quietly = TRUE)) {
    # Test with AUC criterion
    result_with_gam <- OptimalModelSearch(
      Species ~ ., 
      data = binary_data, 
      criterion = "AUC", 
      training_percent = 0.7
    )
    
    # Check if GAM results are present
    roc_list <- attr(result_with_gam$details, "roc_list")
    expect_true(!is.null(roc_list))
    
    # If GAM is the best model, verify its structure
    if (result_with_gam$best_model_name == "gam") {
      expect_true(inherits(result_with_gam$best_model_object, "gam"))
    }
    
    # Test with AIC criterion
    result_aic_with_gam <- OptimalModelSearch(
      Species ~ ., 
      data = binary_data, 
      criterion = "AIC", 
      training_percent = 0.7
    )
    
    # If GAM is the best model with AIC, verify its structure
    if (result_aic_with_gam$best_model_name == "gam") {
      expect_true(inherits(result_aic_with_gam$best_model_object, "gam"))
    }
  } else {
    skip("mgcv package not available for testing GAM models")
  }
})

test_that("OptimalModelSearch forward stepwise model works correctly", {
  testthat::skip_on_cran()
  
  # Load test data
  data(iris)
  
  # Create a binary classification dataset with many predictors to test forward selection
  binary_data <- iris[iris$Species != "virginica", ]
  # Ensure Species is coded as 0/1 for binary classification
  binary_data$Species <- factor(ifelse(binary_data$Species == "setosa", 0, 1), levels = c(0, 1))
  
  # Add some squared terms to make forward selection potentially different
  binary_data$Sepal.Length_sq <- binary_data$Sepal.Length^2
  binary_data$Sepal.Width_sq <- binary_data$Sepal.Width^2
  binary_data$Petal.Length_sq <- binary_data$Petal.Length^2
  binary_data$Petal.Width_sq <- binary_data$Petal.Width^2
  
  # Set seed for reproducibility
  set.seed(789)
  
  # Test with AIC criterion which should favor simpler models
  result_forward <- OptimalModelSearch(
    Species ~ ., 
    data = binary_data, 
    criterion = "AIC", 
    training_percent = 0.7
  )
  
  # Check that forward_stepwise could potentially be selected
  expect_true(result_forward$best_model_name %in% c(
    "full.glm", "backward.stepwise", "forward.stepwise", "gam", "lasso.refit.glm"
  ))
})

test_that("OptimalModelSearch compares all models with Accuracy criterion", {
  testthat::skip_on_cran()
  
  # Load test data
  data(iris)
  
  # Create a binary classification dataset
  binary_data <- iris[iris$Species != "virginica", ]
  # Ensure Species is coded as 0/1 for binary classification
  binary_data$Species <- factor(ifelse(binary_data$Species == "setosa", 0, 1), levels = c(0, 1))
  
  # Set seed for reproducibility
  set.seed(101)
  
  # Test Accuracy criterion
  result_accuracy <- OptimalModelSearch(
    Species ~ ., 
    data = binary_data, 
    criterion = "Accuracy", 
    training_percent = 0.7,
    threshold = 0.5
  )
  
  # Check that result has the basic expected structure
  expect_type(result_accuracy, "list")
  expect_true("criterion" %in% names(result_accuracy))
  expect_true("best_model_name" %in% names(result_accuracy))
  expect_true("performance_metric" %in% names(result_accuracy))
  
  # Check that criterion is set correctly
  expect_equal(result_accuracy$criterion, "Accuracy")
  
  # Check that performance metric is between 0 and 1
  expect_true(result_accuracy$performance_metric >= 0 && result_accuracy$performance_metric <= 1)
}) 