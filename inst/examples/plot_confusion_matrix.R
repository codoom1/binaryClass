# Example script showing how to visualize confusion matrices with OptimalModelSearch
# This script demonstrates:
# 1. Running OptimalModelSearch with Accuracy criterion and automatic confusion matrix plotting
# 2. Using the plot_model_cm function to manually plot confusion matrices

# Load required packages
library(binaryClass)
library(mlbench)

# ----- Example 1: Automatic Confusion Matrix Plotting -----
# Load Pima Indians Diabetes dataset
data(PimaIndiansDiabetes)
df <- PimaIndiansDiabetes
df$diabetes <- ifelse(df$diabetes=="neg", 0, 1)

# Set seed for reproducible results
set.seed(123)

cat("\n======= Method 1: Automatic Confusion Matrix Plotting =======\n")
cat("Running OptimalModelSearch with criterion='Accuracy' and plot_cm=TRUE\n\n")

# Run OptimalModelSearch with Accuracy criterion and automatic confusion matrix plotting
result1 <- OptimalModelSearch(formula=diabetes~., data=df,
                             criterion="Accuracy", training_percent=0.8,
                             threshold=0.5, plot_cm=TRUE)

# Print the result
cat("Best model:", result1$best_model_name, "with Accuracy:", 
    round(result1$performance_metric, 4), "\n\n")

# ----- Example 2: Manual Confusion Matrix Plotting -----
cat("\n======= Method 2: Using plot_model_cm Function =======\n")
cat("Running OptimalModelSearch first, then using plot_model_cm\n\n")

# Run OptimalModelSearch without plotting
result2 <- OptimalModelSearch(formula=diabetes~., data=df,
                             criterion="Accuracy", training_percent=0.8,
                             threshold=0.5)

# Print structure of result2 for debugging
cat("\nStructure of OptimalModelSearch result:\n")
str(result2)
cat("\nClass of confusionMatrix:", class(result2$details), "\n\n")

# Use the dedicated function to plot
cat("Plotting confusion matrix using plot_model_cm(result2)\n")
plot_model_cm(result2)

# ----- Example 3: Saving Confusion Matrix to PDF -----
cat("\n======= Method 3: Saving Confusion Matrix to PDF =======\n")
cat("This example shows how to save the confusion matrix to a PDF file\n\n")

# Use plot_model_cm with save_plot option
cat("Using plot_model_cm with save_plot=TRUE\n")
plot_model_cm(result2, save_plot=TRUE, pdf_filename="confusion_matrix_example.pdf")
cat("Confusion matrix saved to 'confusion_matrix_example.pdf'\n\n")

# ----- Comparison with ROC Curves -----
cat("\n======= Comparison with ROC Curves =======\n")
cat("Different visualization methods for different criteria:\n")
cat("1. Use plot_cm=TRUE with criterion='Accuracy' to visualize confusion matrices\n")
cat("2. Use plot_roc=TRUE with criterion='AUC' to visualize ROC curves\n\n")

# Visual comparison - to show when each is appropriate
cat("Criterion determines the appropriate visualization:\n")
cat("- Accuracy criterion → Confusion Matrix (shows classification performance)\n")
cat("- AUC criterion → ROC Curve (shows sensitivity/specificity trade-offs)\n") 