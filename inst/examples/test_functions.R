# Load the package
library(devtools)
load_all()

cat("===== TEST 1: Simple random data =====\n")
# Create a simple test dataset
test_df <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100),
  y = sample(c(0, 1), 100, replace = TRUE)
)

# Test plot_descrip
cat("Testing plot_descrip...\n")
plot_descrip(test_df, type = "ind", ppv = 1)
cat("Successfully tested plot_descrip with individual plots\n")

# Test OptimalModelSearch
cat("\nTesting OptimalModelSearch...\n")
try({
  result <- OptimalModelSearch(y ~ x1 + x2, data = test_df, criterion = "Accuracy")
  cat("Best model:", result$best_model_name, "\n")
  cat("Accuracy:", result$performance_metric, "\n")
})

cat("\n\n===== TEST 2: Dataset with categorical features =====\n")
# Create dataset with mixed feature types
mixed_df <- data.frame(
  num1 = rnorm(150),
  num2 = rnorm(150),
  cat1 = sample(c("A", "B", "C"), 150, replace = TRUE),
  cat2 = sample(c("X", "Y"), 150, replace = TRUE)
)
mixed_df$response <- ifelse(mixed_df$num1 > 0 & mixed_df$cat1 == "A", 1, 
                      ifelse(mixed_df$num2 < -0.5 & mixed_df$cat2 == "X", 1, 0))

# Convert categorical to factors
mixed_df$cat1 <- as.factor(mixed_df$cat1)
mixed_df$cat2 <- as.factor(mixed_df$cat2)

# View data structure
cat("Data structure:\n")
str(mixed_df)

# Test OptimalModelSearch with categorical features
cat("\nTesting OptimalModelSearch with mixed feature types...\n")
try({
  result_mixed <- OptimalModelSearch(response ~ ., data = mixed_df, criterion = "AUC")
  cat("Best model:", result_mixed$best_model_name, "\n")
  cat("AUC:", result_mixed$performance_metric, "\n")
})

cat("\n\n===== TEST 3: Boston Housing data (classification task) =====\n")
# Use Boston Housing data (convert to binary classification)
library(MASS)
data(Boston)

# Create binary outcome: 1 if median value > 21, 0 otherwise
boston_df <- Boston
boston_df$high_value <- ifelse(boston_df$medv > 21, 1, 0)

# Remove the original continuous outcome
boston_df$medv <- NULL

# Check data
cat("Boston data structure:\n")
str(boston_df)
cat("Binary outcome distribution:\n")
table(boston_df$high_value)

# Test OptimalModelSearch with Boston data
cat("\nTesting OptimalModelSearch with Boston housing data...\n")
try({
  result_boston <- OptimalModelSearch(high_value ~ ., data = boston_df, criterion = "AIC")
  cat("Best model:", result_boston$best_model_name, "\n")
  cat("AIC:", result_boston$performance_metric, "\n")
  
  # Print selected variables
  if ("selected_vars" %in% names(result_boston$details)) {
    cat("Selected variables:", paste(result_boston$details$selected_vars, collapse=", "), "\n")
  }
})

cat("\n\n===== TESTS COMPLETED =====\n")
