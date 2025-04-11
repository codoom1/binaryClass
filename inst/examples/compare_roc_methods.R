# Comparing Different Methods for ROC Curve Analysis
# This script demonstrates the two main approaches for plotting ROC curves in the binaryClass package:
# 1. Using compare_model_rocs function (standalone approach)
# 2. Using OptimalModelSearch with plot_roc=TRUE (integrated approach)

# Load required packages
library(binaryClass)
library(mlbench)

# ----- Prepare data -----
# Load Sonar dataset
data(Sonar)
dat <- Sonar
dat$Class <- ifelse(dat$Class=="R", 0, 1)

# Set seed for reproducibility
set.seed(123)

# ----- Method 1: Using compare_model_rocs -----
cat("\n======= Method 1: Dedicated compare_model_rocs Function =======\n")
cat("This function specifically compares stepwise, lasso, and ridge models\n")
cat("and automatically generates a comparison ROC plot\n\n")

# Run compare_model_rocs
cat("Running: compare_model_rocs(formula=Class~., data=dat)\n")
roc_result <- compare_model_rocs(formula=Class~., data=dat)

# Print results
cat("\nResults from compare_model_rocs:\n")
cat("Best model:", roc_result$best_model, "with AUC:", 
    round(roc_result$auc_values[roc_result$best_model], 4), "\n")
cat("AUC values:\n")
print(round(roc_result$auc_values, 4))
cat("\n")

# ----- Method 2: Using OptimalModelSearch -----
cat("\n======= Method 2: Integrated OptimalModelSearch Approach =======\n")
cat("This approach compares more models: full GLM, backward stepwise, forward stepwise,\n")
cat("GAM, lasso, and ridge regression\n\n")

# Example 1: Plot only the best model's ROC curve
cat("Example 2.1: Plot only best model's ROC curve\n")
cat("Running: OptimalModelSearch with plot_roc=TRUE\n")
oms_result1 <- OptimalModelSearch(formula=Class~., data=dat,
                                criterion="AUC", training_percent=0.8,
                                suppress_warnings=TRUE, plot_roc=TRUE)

# Print result
cat("\nResults from OptimalModelSearch:\n")
cat("Best model:", oms_result1$best_model_name, "with AUC:", 
    round(oms_result1$performance_metric, 4), "\n\n")

# Example 2: Plot comparison of all models' ROC curves
cat("Example 2.2: Plot comparison of all models' ROC curves\n")
cat("Running: OptimalModelSearch with plot_roc=TRUE, plot_comparison=TRUE\n")
oms_result2 <- OptimalModelSearch(formula=Class~., data=dat,
                                criterion="AUC", training_percent=0.8,
                                suppress_warnings=TRUE, plot_roc=TRUE,
                                plot_comparison=TRUE)

# ----- Method 3: Using plot_model_rocs function -----
cat("\n======= Method 3: Using plot_model_rocs with OptimalModelSearch =======\n")
cat("This approach uses OptimalModelSearch first, then plots ROC curves separately\n\n")

# Run OptimalModelSearch without plotting
cat("First run OptimalModelSearch without plotting:\n")
oms_result3 <- OptimalModelSearch(formula=Class~., data=dat,
                                criterion="AUC", training_percent=0.8,
                                suppress_warnings=TRUE)

# Plot best model ROC curve
cat("\nThen plot best model's ROC curve:\n")
cat("plot_model_rocs(oms_result3)\n")
plot_model_rocs(oms_result3)

# Plot comparison ROC curves
cat("\nOr plot comparison of all models' ROC curves:\n")
cat("plot_model_rocs(oms_result3, comparison=TRUE)\n")
plot_model_rocs(oms_result3, comparison=TRUE)

# ----- Summary -----
cat("\n======= Summary of ROC Curve Plotting Methods =======\n")
cat("1. compare_model_rocs: Dedicated function comparing 3 models (stepwise, lasso, ridge)\n")
cat("   Advantage: Simple, focused comparison of these specific models\n\n")
cat("2. OptimalModelSearch + plot_roc=TRUE: Integrated approach comparing 6 models\n")
cat("   Advantage: More comprehensive model comparison with automatic plotting\n\n")
cat("3. plot_model_rocs: Post-analysis plotting of OptimalModelSearch results\n")
cat("   Advantage: Most flexible, allows customized plotting after analysis\n") 