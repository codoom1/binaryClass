# Example script demonstrating automatic ROC curve plotting
# This script shows how OptimalModelSearch can now automatically generate ROC plots
# without requiring manual plotting code

# Load required packages
library(binaryClass)
library(mlbench)

# ----- Example 1: Single ROC plot automatically -----
# Load Sonar dataset
data(Sonar)
dat <- Sonar
dat$Class <- ifelse(dat$Class=="R", 0, 1)

# Set seed for reproducible results
set.seed(123)

cat("Example 1: Automatically plot ROC curve for best model\n")
cat("---------------------------------------------------------\n")
cat("Running: OptimalModelSearch with plot_roc=TRUE\n\n")

# Run OptimalModelSearch with automatic ROC plotting
result1 <- OptimalModelSearch(formula=Class~., data=dat,
                             criterion="AUC", training_percent=0.8,
                             suppress_warnings=TRUE, plot_roc=TRUE)

# Print the result
cat("Best model:", result1$best_model_name, "with AUC:", 
    round(result1$performance_metric, 4), "\n")
cat("The ROC curve was automatically displayed\n\n")

# ----- Example 2: ROC comparison plot automatically -----
cat("Example 2: Automatically plot comparison of all model ROC curves\n")
cat("---------------------------------------------------------\n")
cat("Running: OptimalModelSearch with plot_roc=TRUE, plot_comparison=TRUE\n\n")

# Run OptimalModelSearch with automatic ROC comparison plotting
result2 <- OptimalModelSearch(formula=Class~., data=dat,
                             criterion="AUC", training_percent=0.8,
                             suppress_warnings=TRUE, plot_roc=TRUE, 
                             plot_comparison=TRUE)

# Print the result
cat("Best model:", result2$best_model_name, "with AUC:", 
    round(result2$performance_metric, 4), "\n")
cat("The comparison ROC plot was automatically displayed\n\n")

# ----- Example 3: Using the separate plot_model_rocs function -----
cat("Example 3: Using the separate plot_model_rocs function\n")
cat("---------------------------------------------------------\n")
cat("Running: OptimalModelSearch and then plot_model_rocs\n\n")

# Run OptimalModelSearch without plotting
result3 <- OptimalModelSearch(formula=Class~., data=dat,
                             criterion="AUC", training_percent=0.8,
                             suppress_warnings=TRUE)

# Use the dedicated function to plot
cat("Plotting best model ROC curve using plot_model_rocs(result)\n")
plot_model_rocs(result3)

cat("Plotting ROC curve comparison using plot_model_rocs(result, comparison=TRUE)\n")
plot_model_rocs(result3, comparison=TRUE)

cat("\nAdvantage: plot_model_rocs() provides more options like saving to PDF\n")
cat("Example: plot_model_rocs(result, comparison=TRUE, save_plot=TRUE, pdf_filename=\"my_rocs.pdf\")\n") 