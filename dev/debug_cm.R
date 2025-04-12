library(binaryClass)
library(mlbench)
library(caret)

# Create a simple dataset
set.seed(123)
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
prob <- 1/(1 + exp(-(1 + 0.8*x1 - 0.5*x2 + 0.3*x3)))
y <- rbinom(n, 1, prob)
test_data <- data.frame(y=y, x1=x1, x2=x2, x3=x3)

# Add some debug to see what's in the environment 
# Make dummy objects of each model type for testing
cat("Creating sample models to debug...\n")

# First create the full model ourselves
cat("Creating full GLM model...\n")
full_glm <- glm(y ~ ., data=test_data, family=binomial())
forward.stepwise <- glm(y ~ x1 + x2, data=test_data, family=binomial())
cat("Models created. Classes:\n")
cat("full_glm class:", class(full_glm), "\n")
cat("forward.stepwise class:", class(forward.stepwise), "\n")

# Run OptimalModelSearch with Accuracy criterion, forcing verbose output
cat("\nRunning OptimalModelSearch...\n")
options(warn = 1)  # Set warnings to print immediately
# Turn on debug messages
traceback_file <- "debug_trace.txt"
sink(traceback_file, append=FALSE, split=TRUE)
cat("Starting OptimalModelSearch debug trace\n\n")

# Run the function
result <- OptimalModelSearch(formula=y~., data=test_data, 
                          criterion="Accuracy", 
                          training_percent=0.8,
                          suppress_warnings=FALSE)

# Print any debugging info that might help
cat("\nDebugging OptimalModelSearch results\n")
cat("Names in result:", paste(names(result), collapse=", "), "\n")
cat("Best model name:", result$best_model_name, "\n")
cat("Original best model name (without the .Accuracy suffix):", 
    sub("\\.Accuracy.*$", "", result$best_model_name), "\n")

# Check what models were created
if (exists("full.glm", inherits=FALSE)) {
  cat("full.glm exists as a local variable\n")
} else {
  cat("full.glm does NOT exist as a local variable\n")
}

if (exists("forward.stepwise", inherits=FALSE)) {
  cat("forward.stepwise exists as a local variable\n")
} else {
  cat("forward.stepwise does NOT exist as a local variable\n")
}

sink()

# Let's examine the result structure
cat("\nResult structure:\n")
print(names(result))

# Let's check if the details field is present and has the right structure
cat("\nDetails presence and structure:\n")
cat("'details' in names(result):", "details" %in% names(result), "\n")

# Check if best_model_object is present
cat("\nbest_model_object in result:", "best_model_object" %in% names(result), "\n")
if ("best_model_object" %in% names(result)) {
  cat("Class of best_model_object:", class(result$best_model_object), "\n")
} else {
  cat("ERROR: best_model_object is missing from the results!\n")
}

cat("\nNow let's manually add the confusion matrix using our own model:\n")

# Split the data for testing
set.seed(123)
trainIndex <- caret::createDataPartition(test_data$y, p = 0.8, list = FALSE)
train <- test_data[trainIndex, ]
test <- test_data[-trainIndex, ]

# Make predictions
preds <- predict(full_glm, newdata = test, type = "response")
pred_class <- factor(ifelse(preds > 0.5, 1, 0), levels = c(0, 1))
actual <- factor(test$y, levels = c(0, 1))

# Create confusion matrix
cm <- caret::confusionMatrix(pred_class, actual)

# Add it to the result
result$details <- cm
result$best_model_object <- full_glm

cat("\nManually added confusion matrix and model object to result\n")

# Try to plot the confusion matrix
cat("\nAttempting to plot confusion matrix...\n")
tryCatch({
  pdf("debug_cm_plot.pdf")
  plot_model_cm(result)
  dev.off()
  cat("Success! Confusion matrix plotted to debug_cm_plot.pdf\n")
}, error = function(e) {
  cat("Error plotting confusion matrix:", e$message, "\n")
})

cat("\nDebug complete\n") 