# Load the package
library(devtools)
load_all()

cat("===== TEST: Sonar Data (Real-world classification) =====\n")
# Loading Sonar data from mlbench
library(mlbench)
data(Sonar)

# Check the structure of the data
cat("Sonar Data Structure:\n")
cat("Number of observations:", nrow(Sonar), "\n")
cat("Number of variables:", ncol(Sonar), "\n")
cat("Target variable distribution:\n")
table(Sonar$Class)

# Convert the target to numeric binary outcome
sonar_df <- Sonar
sonar_df$is_mine <- ifelse(sonar_df$Class == "M", 1, 0)
sonar_df$Class <- NULL

cat("\nSonar Data Subset Structure:\n")
str(sonar_df[, 1:5]) # Show just the first 5 features plus target
cat("... (plus 54 more columns)\n")

# Function to run the test with different criteria
run_test <- function(criterion) {
  cat("\n\nTesting OptimalModelSearch with", criterion, "criterion...\n")
  model_result <- NULL
  
  tryCatch({
    start_time <- Sys.time()
    model_result <- OptimalModelSearch(
      is_mine ~ ., 
      data = sonar_df, 
      criterion = criterion,
      training_percent = 0.75,
      threshold = 0.5
    )
    end_time <- Sys.time()
    execution_time <- difftime(end_time, start_time, units = "secs")
    
    cat("Runtime:", round(as.numeric(execution_time), 2), "seconds\n")
    cat("Best model:", model_result$best_model_name, "\n")
    cat(criterion, "value:", model_result$performance_metric, "\n")
    
    if (criterion == "Accuracy") {
      if ("details" %in% names(model_result) && "overall" %in% names(model_result$details)) {
        cat("Additional metrics:\n")
        cat("  Sensitivity:", round(model_result$details$byClass["Sensitivity"], 3), "\n")
        cat("  Specificity:", round(model_result$details$byClass["Specificity"], 3), "\n")
        cat("  Kappa:", round(model_result$details$overall["Kappa"], 3), "\n")
      }
    }
    
    if (criterion == "AIC" && "selected_vars" %in% names(model_result$details)) {
      cat("Number of selected variables:", length(model_result$details$selected_vars), "\n")
      if (length(model_result$details$selected_vars) <= 10) {
        cat("Selected variables:", paste(model_result$details$selected_vars, collapse=", "), "\n")
      } else {
        cat("First 10 selected variables:", paste(head(model_result$details$selected_vars, 10), collapse=", "), "...\n")
      }
    }
    
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
  })
  
  return(model_result)
}

# Test with all three criteria
accuracy_model <- run_test("Accuracy")
auc_model <- run_test("AUC")
aic_model <- run_test("AIC")

cat("\n\n===== TEST COMPLETED =====\n") 