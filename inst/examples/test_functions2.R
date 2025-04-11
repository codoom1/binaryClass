# Load the package
library(devtools)
load_all()

# Create a simple test dataset
test_df <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100),
  y = sample(c(0, 1), 100, replace = TRUE)
)

# Test plot_descrip with pairwise plots
cat("Testing plot_descrip with pairwise plots...\n")
plot_descrip(test_df, type = "pair")
cat("Successfully tested plot_descrip with pairwise plots\n")

# Test OptimalModelSearch with AUC criterion
cat("\nTesting OptimalModelSearch with AUC criterion...\n")
try({
  result <- OptimalModelSearch(y ~ x1 + x2, data = test_df, criterion = "AUC")
  cat("Best model:", result$best_model_name, "\n")
  cat("AUC:", result$performance_metric, "\n")
})
