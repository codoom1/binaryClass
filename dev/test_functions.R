library(binaryClass)
library(mlbench)

# Redirect output to a file
sink("test_results.txt")

# Set seed for reproducibility
set.seed(123)

cat("\n=== Testing binaryClass functions ===\n\n")

# Create a synthetic dataset for testing
cat("Creating a synthetic dataset...\n")
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
prob <- 1/(1 + exp(-(1 + 0.8*x1 - 0.5*x2 + 0.3*x3)))
y <- rbinom(n, 1, prob)
test_data <- data.frame(y=y, x1=x1, x2=x2, x3=x3)
cat("Dataset created with", nrow(test_data), "observations and", 
    ncol(test_data), "variables\n\n")

# Test 1: OptimalModelSearch with Accuracy criterion
cat("Test 1: OptimalModelSearch with Accuracy criterion\n")
tryCatch({
  result_acc <- OptimalModelSearch(formula=y~., data=test_data, 
                                 criterion="Accuracy", 
                                 training_percent=0.8,
                                 suppress_warnings=TRUE,
                                 plot_cm=TRUE)  # Enable CM plotting
  cat("SUCCESS: Best model:", result_acc$best_model_name, 
      "with accuracy:", result_acc$performance_metric, "\n\n")
  
  # Test 2: extract_best_model
  cat("Test 2: extract_best_model\n")
  best_model <- extract_best_model(result_acc, test_data)
  cat("SUCCESS: Extracted model of class:", class(best_model)[1], "\n\n")
  
}, error=function(e) {
  cat("ERROR in OptimalModelSearch or extract_best_model:", e$message, "\n\n")
})

# Test 3: OptimalModelSearch with AUC criterion
cat("Test 3: OptimalModelSearch with AUC criterion\n")
tryCatch({
  result_auc <- OptimalModelSearch(formula=y~., data=test_data, 
                                criterion="AUC", 
                                training_percent=0.8,
                                suppress_warnings=TRUE,
                                plot_roc=TRUE)  # Enable ROC plotting
  cat("SUCCESS: Best model:", result_auc$best_model_name, 
      "with AUC:", result_auc$performance_metric, "\n\n")
  
  # Test 4: plot_model_rocs
  cat("Test 4: plot_model_rocs\n")
  tryCatch({
    # Plot ROC in a null device to avoid display issues
    pdf(NULL)
    plot_model_rocs(result_auc, comparison=TRUE)
    dev.off()
    cat("SUCCESS: plot_model_rocs executed without errors\n\n")
  }, error=function(e) {
    cat("ERROR in plot_model_rocs:", e$message, "\n\n")
  })
  
}, error=function(e) {
  cat("ERROR in OptimalModelSearch (AUC):", e$message, "\n\n")
})

# Test 5: OptimalModelSearch with AIC criterion
cat("Test 5: OptimalModelSearch with AIC criterion\n")
tryCatch({
  result_aic <- OptimalModelSearch(formula=y~., data=test_data, 
                                criterion="AIC", 
                                training_percent=0.8,
                                suppress_warnings=TRUE)
  cat("SUCCESS: Best model:", result_aic$best_model_name, 
      "with AIC:", result_aic$performance_metric, "\n\n")
}, error=function(e) {
  cat("ERROR in OptimalModelSearch (AIC):", e$message, "\n\n")
})

# Test 6: compare_model_rocs using correct signature
cat("Test 6: compare_model_rocs\n")
tryCatch({
  # Use the function with its proper signature
  pdf(NULL)
  compare_model_rocs(formula=y~., data=test_data, training_percent=0.8, 
                   suppress_warnings=TRUE)
  dev.off()
  cat("SUCCESS: compare_model_rocs executed without errors\n\n")
}, error=function(e) {
  cat("ERROR in compare_model_rocs:", e$message, "\n\n")
})

# Test 7: plot_model_cm
cat("Test 7: plot_model_cm\n")
tryCatch({
  if(exists("result_acc")) {
    # Plot in a null device
    pdf(NULL)
    plot_model_cm(result_acc)
    dev.off()
    cat("SUCCESS: plot_model_cm executed without errors\n\n")
  } else {
    cat("SKIPPED: plot_model_cm test skipped because result_acc is not available\n\n")
  }
}, error=function(e) {
  cat("ERROR in plot_model_cm:", e$message, "\n\n")
})

cat("=== All tests completed ===\n")

# Close the output file
sink() 