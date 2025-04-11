# Load the package
library(devtools)
load_all()

# Set seed for reproducibility
set.seed(123)

# Create a test dataset
test_df <- data.frame(
  x1 = rnorm(200),
  x2 = rnorm(200),
  x3 = rnorm(200)
)
# Create a binary outcome with some signal
test_df$y <- ifelse(0.7*test_df$x1 - 0.5*test_df$x2 + 0.3*test_df$x3 + rnorm(200, 0, 0.5) > 0, 1, 0)

cat("Running OptimalModelSearch with AUC criterion...\n")
result <- OptimalModelSearch(y ~ ., data = test_df, criterion = "AUC")

cat("Best model:", result$best_model_name, "\n")
cat("AUC:", result$performance_metric, "\n")

# Plot the ROC curve
cat("\nPlotting ROC curve...\n")
graphics.off() # Close any existing graphics devices
pdf("roc_curve_plot.pdf") # Create a PDF to save the plot

# Base plot of the ROC curve
plot(result$details, main = paste("ROC Curve for", result$best_model_name), 
     col = "blue", lwd = 2)

# Add AUC to the title
auc_value <- round(as.numeric(result$performance_metric), 3)
title(sub = paste("AUC =", auc_value))

# Add diagonal reference line
abline(a = 0, b = 1, lty = 2, col = "gray")

# Add grid for readability
grid(col = "lightgray", lty = "dotted")

# Plot the point with the optimal threshold
optimal_threshold_index <- which.max(result$details$sensitivities + result$details$specificities - 1)
points(
  1 - result$details$specificities[optimal_threshold_index],
  result$details$sensitivities[optimal_threshold_index],
  pch = 19, col = "red"
)

# Add a legend
legend("bottomright", 
       legend = c("ROC curve", "Optimal threshold"), 
       col = c("blue", "red"), 
       lwd = c(2, NA), 
       pch = c(NA, 19),
       bg = "white")

# Close the PDF device
dev.off()

cat("ROC curve saved to 'roc_curve_plot.pdf'\n")

# Compare all models' ROC curves
cat("\nComparing ROC curves for all models...\n")
pdf("roc_curves_comparison.pdf")

# Get the ROC objects for all models
stepwise_roc <- result$details
lasso_roc <- NULL
ridge_roc <- NULL

# Extract the appropriate ROC objects based on the output structure
all_details_names <- names(attributes(result$details))
if ("roc_list" %in% all_details_names) {
  roc_list <- attr(result$details, "roc_list")
  model_names <- names(roc_list)
  
  for (name in model_names) {
    if (grepl("stepwise", name)) {
      stepwise_roc <- roc_list[[name]]
    } else if (grepl("lasso", name)) {
      lasso_roc <- roc_list[[name]]
    } else if (grepl("ridge", name)) {
      ridge_roc <- roc_list[[name]]
    }
  }
} else {
  # Try to extract from the nested details structure 
  if ("stepwise.regression" %in% names(result)) {
    stepwise_roc <- result$stepwise.regression
  }
  if ("lasso.regression" %in% names(result)) {
    lasso_roc <- result$lasso.regression
  }
  if ("ridge.regression" %in% names(result)) {
    ridge_roc <- result$ridge.regression
  }
}

# If we don't have all ROC objects, rerun with the internal option to return all
if (is.null(lasso_roc) || is.null(ridge_roc)) {
  cat("Re-running with internal option to retrieve all ROC curves...\n")
  
  # Create new model using the same data
  model_labels <- c("Stepwise Regression", "Lasso Regression", "Ridge Regression")
  model_colors <- c("blue", "red", "green")
  
  # Plot new ROC curves
  par(mar = c(5, 4, 4, 8) + 0.1)  # Increase right margin for the legend
  
  # Run separate models
  result_stepwise <- glm(y ~ ., data = test_df, family = binomial)
  pred_stepwise <- predict(result_stepwise, type = "response")
  roc_stepwise <- pROC::roc(test_df$y, pred_stepwise, plot = FALSE)
  
  # Ensure we have glmnet package for lasso and ridge
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Package 'glmnet' is needed for this code to work")
  }
  
  # Prepare data for glmnet
  x <- as.matrix(test_df[, !names(test_df) %in% "y"])
  y <- test_df$y
  
  # Lasso
  cv_lasso <- glmnet::cv.glmnet(x, y, family = "binomial", alpha = 1)
  pred_lasso <- predict(cv_lasso, newx = x, s = "lambda.min", type = "response")
  roc_lasso <- pROC::roc(y, pred_lasso, plot = FALSE)
  
  # Ridge
  cv_ridge <- glmnet::cv.glmnet(x, y, family = "binomial", alpha = 0)
  pred_ridge <- predict(cv_ridge, newx = x, s = "lambda.min", type = "response")
  roc_ridge <- pROC::roc(y, pred_ridge, plot = FALSE)
  
  # Plot all three ROC curves
  plot(roc_stepwise, col = model_colors[1], lwd = 2, main = "Comparison of ROC Curves")
  lines(roc_lasso, col = model_colors[2], lwd = 2)
  lines(roc_ridge, col = model_colors[3], lwd = 2)
  
  # Add AUC values to the legend
  auc_stepwise <- round(as.numeric(pROC::auc(roc_stepwise)), 3)
  auc_lasso <- round(as.numeric(pROC::auc(roc_lasso)), 3)
  auc_ridge <- round(as.numeric(pROC::auc(roc_ridge)), 3)
  
  legend_text <- c(
    paste("Stepwise (AUC =", auc_stepwise, ")"),
    paste("Lasso (AUC =", auc_lasso, ")"),
    paste("Ridge (AUC =", auc_ridge, ")")
  )
  
  # Add a legend
  legend("bottomright", 
         legend = legend_text, 
         col = model_colors, 
         lwd = 2,
         bg = "white")
  
} else {
  # Plot the ROC curves we extracted from the OptimalModelSearch result
  plot(stepwise_roc, col = "blue", lwd = 2, main = "Comparison of ROC Curves")
  lines(lasso_roc, col = "red", lwd = 2)
  lines(ridge_roc, col = "green", lwd = 2)
  
  # Add AUC values to the legend
  auc_stepwise <- round(as.numeric(pROC::auc(stepwise_roc)), 3)
  auc_lasso <- round(as.numeric(pROC::auc(lasso_roc)), 3)
  auc_ridge <- round(as.numeric(pROC::auc(ridge_roc)), 3)
  
  legend_text <- c(
    paste("Stepwise (AUC =", auc_stepwise, ")"),
    paste("Lasso (AUC =", auc_lasso, ")"),
    paste("Ridge (AUC =", auc_ridge, ")")
  )
  
  # Add a legend
  legend("bottomright", 
         legend = legend_text, 
         col = c("blue", "red", "green"), 
         lwd = 2,
         bg = "white")
}

# Add diagonal reference line
abline(a = 0, b = 1, lty = 2, col = "gray")

# Add grid for readability
grid(col = "lightgray", lty = "dotted")

# Close the PDF device
dev.off()

cat("ROC curves comparison saved to 'roc_curves_comparison.pdf'\n\n")
cat("===== ROC PLOTTING TEST COMPLETED =====\n") 