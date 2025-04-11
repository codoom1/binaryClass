# Load the package
library(devtools)
load_all()

cat("===== TEST: compare_model_rocs function =====\n")

# Create a test dataset with a clear signal
set.seed(123)
n <- 200 # Number of observations
p <- 5   # Number of predictors

# Create predictors with some correlation
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
x4 <- 0.7*x1 + 0.3*rnorm(n)  # x4 correlated with x1
x5 <- 0.6*x2 + 0.4*rnorm(n)  # x5 correlated with x2

# Create a binary outcome with a nonlinear pattern
# This pattern should favor different model types
z <- 1.5*x1 - 0.8*x2 + 0.5*x3 - 0.2*x1*x2 + 0.1*x2^2 + rnorm(n, 0, 0.8)
y <- ifelse(z > 0, 1, 0)

# Combine into a data frame
test_df <- data.frame(x1, x2, x3, x4, x5, y)

cat("Data summary:\n")
cat("Number of observations:", n, "\n")
cat("Number of predictors:", p, "\n")
cat("Class distribution:\n")
table(test_df$y)

# Test the compare_model_rocs function
cat("\nRunning compare_model_rocs...\n")
result <- compare_model_rocs(y ~ ., data = test_df, save_plot = TRUE, 
                             pdf_filename = "model_comparison_test.pdf")

# Print the results
cat("\nResults:\n")
cat("AUC values:\n")
print(round(result$auc_values, 4))
cat("\nBest model:", result$best_model, "\n")

# Test with a different subset of predictors
cat("\nRunning compare_model_rocs with a subset of predictors...\n")
result2 <- compare_model_rocs(y ~ x1 + x2 + x3, data = test_df, 
                             plot_title = "Comparison with Subset of Predictors")

# Print the results
cat("\nResults with subset of predictors:\n")
cat("AUC values:\n")
print(round(result2$auc_values, 4))
cat("\nBest model:", result2$best_model, "\n")

cat("\n===== TEST COMPLETED =====\n")
cat("ROC curves comparison saved to 'model_comparison_test.pdf'\n") 