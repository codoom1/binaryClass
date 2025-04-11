# Load the package
library(devtools)
load_all()

cat("===== TEST: compare_model_rocs function with iris data =====\n")

# Convert iris to a binary classification problem
data(iris)
iris_bin <- iris
# Set y=1 if Species is setosa, 0 otherwise
iris_bin$is_setosa <- as.integer(iris_bin$Species == "setosa")
iris_bin$Species <- NULL

cat("Iris data summary:\n")
cat("Number of observations:", nrow(iris_bin), "\n")
cat("Number of predictors:", ncol(iris_bin) - 1, "\n")
cat("Class distribution (is_setosa):\n")
table(iris_bin$is_setosa)

# Test the compare_model_rocs function
cat("\nRunning compare_model_rocs on iris data...\n")
result_iris <- compare_model_rocs(is_setosa ~ ., data = iris_bin, save_plot = TRUE, 
                               pdf_filename = "iris_comparison.pdf")

# Print the results
cat("\nResults:\n")
cat("AUC values:\n")
print(round(result_iris$auc_values, 4))
cat("\nBest model:", result_iris$best_model, "\n")

# Create a harder classification problem (versicolor vs others)
iris_bin2 <- iris
iris_bin2$is_versicolor <- as.integer(iris_bin2$Species == "versicolor")
iris_bin2$Species <- NULL

cat("\nIris data for versicolor classification:\n")
cat("Class distribution (is_versicolor):\n")
table(iris_bin2$is_versicolor)

# Test the compare_model_rocs function on the more difficult problem
cat("\nRunning compare_model_rocs on versicolor classification...\n")
result_versicolor <- compare_model_rocs(is_versicolor ~ ., data = iris_bin2, 
                               plot_title = "Versicolor vs Others ROC Comparison",
                               save_plot = TRUE,
                               pdf_filename = "versicolor_comparison.pdf")

# Print the results
cat("\nResults for versicolor classification:\n")
cat("AUC values:\n")
print(round(result_versicolor$auc_values, 4))
cat("\nBest model:", result_versicolor$best_model, "\n")

cat("\n===== TEST COMPLETED =====\n")
cat("ROC curves saved to 'iris_comparison.pdf' and 'versicolor_comparison.pdf'\n") 