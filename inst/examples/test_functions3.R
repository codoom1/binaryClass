# Load the package
library(devtools)
load_all()

# Create a simple test dataset
test_df <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100),
  x3 = rnorm(100),
  x4 = rnorm(100),
  y = sample(c(0, 1), 100, replace = TRUE)
)

# Test OptimalModelSearch with AIC criterion
cat("Testing OptimalModelSearch with AIC criterion...\n")
try({
  result <- OptimalModelSearch(y ~ x1 + x2 + x3 + x4, data = test_df, criterion = "AIC")
  cat("Best model:", result$best_model_name, "\n")
  cat("AIC:", result$performance_metric, "\n")
})
