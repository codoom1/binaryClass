test_that("compare_model_rocs functions correctly", {
  # Skip on CRAN due to time-consuming computations
  testthat::skip_on_cran()
  
  # Load test data and prepare
  data(iris)
  
  # Create a binary classification dataset
  binary_data <- iris[iris$Species != "virginica", ]
  binary_data$Species <- factor(as.character(binary_data$Species))
  
  # Basic test of the function
  result <- compare_model_rocs(Species ~ ., data = binary_data, training_percent = 0.7)
  
  # Check structure of results
  expect_type(result, "list")
  expect_true("roc_list" %in% names(result))
  expect_true("auc_values" %in% names(result))
  expect_true("best_model" %in% names(result))
  
  # Check that roc_list is a list containing ROC objects
  expect_type(result$roc_list, "list")
  expect_gt(length(result$roc_list), 0)
  
  # Check for expected models in the results
  model_names <- names(result$roc_list)
  expected_models <- c("full.glm", "backward.stepwise", "forward.stepwise", "lasso", "ridge")
  expect_true(any(expected_models %in% model_names))
  
  # Check AUC values are numeric and between 0 and 1
  expect_true(all(result$auc_values >= 0 & result$auc_values <= 1))
  
  # Test with missing data
  binary_data_with_na <- binary_data
  binary_data_with_na[sample(1:nrow(binary_data), 10), "Sepal.Length"] <- NA
  result_with_na <- compare_model_rocs(Species ~ ., data = binary_data_with_na)
  expect_true(all(result_with_na$auc_values >= 0 & result_with_na$auc_values <= 1))
  
  # Test with saving plot
  temp_file <- tempfile(fileext = ".pdf")
  result_save <- compare_model_rocs(Species ~ ., data = binary_data, 
                                   save_plot = TRUE, 
                                   pdf_filename = temp_file)
  expect_true(file.exists(temp_file))
  file.remove(temp_file)
  
  # Test suppress_warnings parameter
  result_suppress <- compare_model_rocs(Species ~ ., data = binary_data, 
                                       suppress_warnings = TRUE)
  expect_type(result_suppress, "list")
  
  # Test error handling
  expect_error(compare_model_rocs(NonExistentVar ~ ., data = binary_data))
}) 