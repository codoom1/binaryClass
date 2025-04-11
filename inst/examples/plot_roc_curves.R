# Example script showing how to get and plot ROC curves from OptimalModelSearch
# This script demonstrates:
# 1. Running OptimalModelSearch with AUC criterion
# 2. Plotting the ROC curve of the best model
# 3. Plotting multiple ROC curves for comparison

# Load required packages
library(binaryClass)
library(mlbench)

# ----- Example 1: Basic ROC Curve Plotting -----
# Load Sonar dataset
data(Sonar)
dat <- Sonar
dat$Class <- ifelse(dat$Class=="R", 0, 1)

# Set seed for reproducible results
set.seed(123)

# Run OptimalModelSearch with AUC criterion
result <- OptimalModelSearch(formula=Class~., data=dat,
                            criterion="AUC", training_percent=0.8, 
                            suppress_warnings=TRUE)

# Print the result
cat("Best model:", result$best_model_name, "with AUC:", 
    round(result$performance_metric, 4), "\n\n")

# ----- Example 2: Plotting the ROC Curve -----
# Plot the ROC curve of the best model
pdf("best_model_roc.pdf")
plot(result$details, main="ROC Curve for Best Model")
# Add the AUC value to the plot
text(0.7, 0.2, paste("AUC =", round(as.numeric(result$performance_metric), 3)), 
     cex=0.9)
dev.off()

# ----- Example 3: Comparing Multiple ROC Curves -----
# Access all ROC objects
all_rocs <- attr(result$details, "roc_list")

# Plot multiple ROC curves on the same graph
pdf("comparison_roc_curves.pdf")

# Initialize the plot with the first ROC curve
plot(all_rocs$full.glm, col="blue", main="Comparison of ROC Curves")

# Add other ROC curves
plot(all_rocs$lasso, col="red", add=TRUE)
plot(all_rocs$ridge, col="green", add=TRUE)
plot(all_rocs$backward.stepwise, col="purple", add=TRUE)
plot(all_rocs$forward.stepwise, col="orange", add=TRUE)

# If GAM is available, add its ROC curve
if (!is.null(all_rocs$gam)) {
  plot(all_rocs$gam, col="brown", add=TRUE)
}

# Add a diagonal reference line
abline(a=0, b=1, lty=2, col="gray")

# Add a legend
legend("bottomright", 
       legend=c("Full GLM", "Lasso", "Ridge", "Backward Stepwise", "Forward Stepwise"),
       col=c("blue", "red", "green", "purple", "orange"), 
       lwd=2)

dev.off()

# Print a message about where to find the output
cat("ROC curve plots saved as 'best_model_roc.pdf' and 'comparison_roc_curves.pdf'\n")
cat("To display ROC curves interactively, omit the pdf() and dev.off() calls\n") 