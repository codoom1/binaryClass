#' Compare ROC Curves for Different Binary Classification Models
#'
#' This function fits multiple binary classification models (stepwise logistic regression,
#' lasso, and ridge) to the same dataset and plots their ROC curves for comparison.
#' It helps visualize the performance differences between models.
#'
#' @section Models Compared:
#' This function compares the following three models:
#' \itemize{
#'   \item Stepwise Logistic Regression: Full model with all predictors from formula
#'   \item Lasso Regression: L1 regularization that can shrink coefficients to zero
#'   \item Ridge Regression: L2 regularization that shrinks coefficients
#' }
#' Note that this is a simplified subset of models compared to OptimalModelSearch, which
#' additionally includes backward stepwise, forward stepwise, and GAM models.
#'
#' @section Relationship with OptimalModelSearch:
#' While this function is a standalone tool, users can also generate ROC curves using:
#' \itemize{
#'   \item \code{OptimalModelSearch(formula, data, criterion="AUC", plot_roc=TRUE)} to plot only the best model's ROC curve
#'   \item \code{OptimalModelSearch(formula, data, criterion="AUC", plot_roc=TRUE, plot_comparison=TRUE)} to plot all models' ROC curves
#'   \item \code{plot_model_rocs(result)} where result is the output from OptimalModelSearch with AUC criterion
#' }
#' The approaches differ in that \code{compare_model_rocs} only compares stepwise, lasso and ridge, 
#' while \code{OptimalModelSearch} compares full GLM, backward stepwise, forward stepwise, GAM, lasso, and ridge.
#'
#' @param formula An object of class \"formula\": a symbolic description of the
#'   model to be fitted (e.g., `response ~ predictor1 + predictor2` or `response ~ .`).
#' @param data A data frame containing the variables in the model.
#' @param training_percent A numeric value between 0 and 1 indicating the proportion
#'   of the data to use for training. Default is `0.8`.
#' @param plot_title A character string for the plot title. Default is "Comparison of ROC Curves".
#' @param save_plot Logical value indicating whether to save the plot to a PDF file.
#'   Default is FALSE.
#' @param pdf_filename A character string specifying the name of the PDF file if
#'   save_plot is TRUE. Default is "roc_curves_comparison.pdf".
#'
#' @return A list containing the ROC objects for each model and their AUC values:
#'   \itemize{
#'     \item{`stepwise_roc`}: The ROC object for the stepwise logistic regression model.
#'     \item{`lasso_roc`}: The ROC object for the lasso regression model.
#'     \item{`ridge_roc`}: The ROC object for the ridge regression model.
#'     \item{`auc_values`}: A named numeric vector of AUC values for each model.
#'     \item{`best_model`}: The name of the model with the highest AUC.
#'   }
#'
#' @export
#' @keywords models hplot
#' @concept ROC curve
#' @concept binary classification
#' @concept model comparison
#' @concept AUC comparison
#' @concept regularization
#' @concept model evaluation
#' @concept visualization
#' @concept performance comparison
#' @aliases roc_comparison roc_curves compare_rocs plot_roc_curves
#' @seealso \code{\link{OptimalModelSearch}} for more comprehensive model selection,
#'   \code{\link{plot_model_rocs}} for plotting ROC curves from OptimalModelSearch results
#' @importFrom graphics plot lines abline grid points legend par title
#' @importFrom stats predict glm binomial
#' @import pROC glmnet
#'
#' @examples
#' \dontrun{
#' # Create a sample dataset
#' set.seed(123)
#' test_df <- data.frame(
#'   x1 = rnorm(100),
#'   x2 = rnorm(100),
#'   y = sample(c(0, 1), 100, replace = TRUE)
#' )
#'
#' # Method 1: Compare ROC curves using compare_model_rocs
#' result <- compare_model_rocs(y ~ ., data = test_df)
#'
#' # Access the ROC objects
#' result$stepwise_roc
#' result$lasso_roc
#' result$ridge_roc
#'
#' # See the AUC values
#' result$auc_values
#'
#' # Check which model performed best
#' result$best_model
#'
#' # Method 2: Compare ROC curves using OptimalModelSearch (more models)
#' # Automatic ROC curve of the best model
#' oms_result <- OptimalModelSearch(formula=y ~ ., data=test_df,
#'                               criterion="AUC", plot_roc=TRUE)
#'
#' # Automatic comparison of all models (full GLM, stepwise, GAM, lasso, ridge)
#' oms_result <- OptimalModelSearch(formula=y ~ ., data=test_df,
#'                               criterion="AUC", plot_roc=TRUE, 
#'                               plot_comparison=TRUE)
#' }
compare_model_rocs <- function(formula, data, training_percent = 0.8,
                               plot_title = "Comparison of ROC Curves", 
                               save_plot = FALSE, 
                               pdf_filename = "roc_curves_comparison.pdf") {
  
  # --- NA Handling: Keep only complete cases for formula variables --- 
  formula_terms <- tryCatch(stats::terms(formula, data = data), error = function(e) NULL)
  if (is.null(formula_terms)) {
    stop("Could not process the formula provided.")
  }
  formula_vars <- all.vars(formula_terms)
  
  # Ensure formula variables exist in data
  missing_vars <- setdiff(formula_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variable(s) specified in formula not found in data: ", paste(missing_vars, collapse=", "))
  }
  data_subset <- data[, formula_vars, drop = FALSE]
  complete_rows <- stats::complete.cases(data_subset)
  data_complete <- data[complete_rows, ]
  if (nrow(data_complete) < nrow(data)) {
    warning(paste(nrow(data) - nrow(data_complete), "rows removed due to missing values in formula variables."))
  }
  if (nrow(data_complete) == 0) {
    stop("No complete cases found for the variables specified in the formula.")
  }
  # --- End NA Handling ---
  
  # Extract the response variable
  response_variable <- all.vars(formula)[1]
  
  # Divide the data into training and testing sets
  set.seed(42) # For reproducibility
  train_indices <- sample(1:nrow(data_complete), size = round(nrow(data_complete) * training_percent))
  train_data <- data_complete[train_indices, ]
  test_data <- data_complete[-train_indices, ]
  
  # Prepare model output list
  model_results <- list()
  
  # 1. Fit stepwise logistic regression
  stepwise_model <- stats::glm(formula = formula, data = train_data, family = stats::binomial)
  pred_stepwise <- stats::predict(stepwise_model, newdata = test_data, type = "response")
  
  # 2. Fit lasso regression
  x_train <- stats::model.matrix(object = formula, data = train_data)[, -1]
  y_train <- train_data[[response_variable]]
  x_test <- stats::model.matrix(object = formula, data = test_data)[, -1]
  
  cv_lasso <- glmnet::cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)
  pred_lasso <- stats::predict(cv_lasso, newx = x_test, s = "lambda.min", type = "response")
  
  # 3. Fit ridge regression
  cv_ridge <- glmnet::cv.glmnet(x_train, y_train, family = "binomial", alpha = 0)
  pred_ridge <- stats::predict(cv_ridge, newx = x_test, s = "lambda.min", type = "response")
  
  # Create ROC objects for each model
  roc_stepwise <- pROC::roc(test_data[[response_variable]], pred_stepwise, plot = FALSE)
  roc_lasso <- pROC::roc(test_data[[response_variable]], as.vector(pred_lasso), plot = FALSE)
  roc_ridge <- pROC::roc(test_data[[response_variable]], as.vector(pred_ridge), plot = FALSE)
  
  # Calculate AUC values
  auc_stepwise <- as.numeric(pROC::auc(roc_stepwise))
  auc_lasso <- as.numeric(pROC::auc(roc_lasso))
  auc_ridge <- as.numeric(pROC::auc(roc_ridge))
  
  auc_values <- c(
    "stepwise" = auc_stepwise,
    "lasso" = auc_lasso,
    "ridge" = auc_ridge
  )
  
  # Determine best model
  best_model <- names(which.max(auc_values))
  
  # Define plot colors and labels
  model_colors <- c("blue", "red", "green")
  legend_text <- c(
    paste("Stepwise (AUC =", round(auc_stepwise, 3), ")"),
    paste("Lasso (AUC =", round(auc_lasso, 3), ")"),
    paste("Ridge (AUC =", round(auc_ridge, 3), ")")
  )
  
  # Plot the ROC curves
  if (save_plot) {
    grDevices::pdf(pdf_filename)
  }
  
  # Set up plot margins
  graphics::par(mar = c(5, 4, 4, 2) + 0.1)
  
  # Create the plot
  plot(roc_stepwise, col = model_colors[1], lwd = 2, main = plot_title)
  graphics::lines(roc_lasso, col = model_colors[2], lwd = 2)
  graphics::lines(roc_ridge, col = model_colors[3], lwd = 2)
  
  # Add diagonal reference line
  graphics::abline(a = 0, b = 1, lty = 2, col = "gray")
  
  # Add grid for readability
  graphics::grid(col = "lightgray", lty = "dotted")
  
  # Add a legend
  graphics::legend("bottomright", 
         legend = legend_text, 
         col = model_colors, 
         lwd = 2,
         bg = "white")
  
  # Mark the best model's optimal threshold point
  best_roc <- if(best_model == "stepwise") roc_stepwise else if(best_model == "lasso") roc_lasso else roc_ridge
  optimal_threshold_index <- which.max(best_roc$sensitivities + best_roc$specificities - 1)
  graphics::points(
    1 - best_roc$specificities[optimal_threshold_index],
    best_roc$sensitivities[optimal_threshold_index],
    pch = 19, col = "black", cex = 1.2
  )
  
  if (save_plot) {
    grDevices::dev.off()
  }
  
  # Return the results
  return(list(
    stepwise_roc = roc_stepwise,
    lasso_roc = roc_lasso,
    ridge_roc = roc_ridge,
    auc_values = auc_values,
    best_model = best_model
  ))
} 