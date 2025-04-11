# Minimal example to debug confusion matrix in OptimalModelSearch
library(binaryClass)
library(mlbench)

# Create a simple dataset
set.seed(123)
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- as.numeric(x1 + x2 + rnorm(n) > 0)
df <- data.frame(y = y, x1 = x1, x2 = x2)

# Run OptimalModelSearch with Accuracy criterion
cat("Running OptimalModelSearch...\n")
result <- OptimalModelSearch(formula = y ~ ., data = df, criterion = "Accuracy")

# Print structure of result
cat("\nResult names:", names(result), "\n")
cat("Result criterion:", result$criterion, "\n")
cat("Result best_model_name:", result$best_model_name, "\n")
cat("Has details field:", "details" %in% names(result), "\n")

# Try to modify plot_model_cm to work with any confusion matrix
plot_cm <- function(result) {
  # Extract confusion matrix from the result
  cm <- result$details
  
  # If it's not a valid confusion matrix, try to create one
  if (is.null(cm) || !is.list(cm) || !all(c("table", "byClass", "overall") %in% names(cm))) {
    cat("No valid confusion matrix found in results, stopping\n")
    return(NULL)
  }
  
  # Basic visualization
  print(cm$table)
  cat("\nAccuracy:", cm$overall["Accuracy"], "\n")
}

# Try to plot the confusion matrix
cat("\nAttempting to plot confusion matrix...\n")
plot_cm(result) 