#' Compare ROC Curves for Different Binary Classification Models
#'
#' This function fits multiple binary classification models (full logistic regression, backward stepwise,
#' forward stepwise, GAM, lasso, and ridge) to the same dataset and plots their ROC curves 
#' in a multi-panel layout for easy comparison. Each model gets its own panel with a dedicated ROC curve.
#'
#' @section Models Compared:
#' This function compares the following models:
#' \itemize{
#'   \item Full Logistic Regression: Uses all predictors in the formula
#'   \item Backward Stepwise: Performs backward selection from the full model
#'   \item Forward Stepwise: Starts with intercept-only model and adds predictors
#'   \item GAM (Generalized Additive Model): Fits smooth terms for numeric predictors
#'   \item Lasso Regression: L1 regularization that can shrink coefficients to zero
#'   \item Ridge Regression: L2 regularization that shrinks coefficients
#' }
#'
#' @section Relationship with OptimalModelSearch:
#' This function provides all the same models as OptimalModelSearch but is focused on visualization 
#' rather than model selection. The multiple panel layout makes it easy to compare ROC curves visually.
#' \itemize{
#'   \item Use \code{compare_model_rocs()} to get a dedicated multi-panel ROC plot
#'   \item Use \code{OptimalModelSearch(criterion="AUC", plot_roc=TRUE, plot_comparison=TRUE, multi_panel=TRUE)} for integrated model selection and visualization
#' }
#'
#' @param formula An object of class \"formula\": a symbolic description of the
#'   model to be fitted (e.g., `response ~ predictor1 + predictor2` or `response ~ .`).
#' @param data A data frame containing the variables in the model.
#' @param training_percent A numeric value between 0 and 1 indicating the proportion
#'   of the data to use for training. Default is `0.8`.
#' @param save_plot Logical value indicating whether to save the plot to a PDF file.
#'   Default is FALSE.
#' @param pdf_filename A character string specifying the name of the PDF file if
#'   save_plot is TRUE. Default is "roc_curves_comparison.pdf".
#' @param suppress_warnings Logical indicating whether to suppress warnings. Default is FALSE.
#'
#' @return A list containing the ROC objects for each model and their AUC values:
#'   \itemize{
#'     \item{`roc_list`}: A list of ROC objects for each model.
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
#' @seealso \code{\link{OptimalModelSearch}} for comprehensive model selection,
#'   \code{\link{plot_model_rocs}} for plotting ROC curves from OptimalModelSearch results
#' @importFrom graphics plot abline grid points legend par mtext
#' @importFrom stats predict glm binomial model.matrix step terms complete.cases reformulate
#' @importFrom mgcv gam
#' @import pROC glmnet
#'
#' @examples
#' \dontrun{
#' # Load example data
#' library(mlbench)
#' data(Sonar)
#' dat <- Sonar
#' dat$Class <- ifelse(dat$Class=="R", 0, 1)
#'
#' # Generate multi-panel ROC plot comparing all models
#' result <- compare_model_rocs(Class ~ ., data = dat)
#'
#' # See the AUC values
#' result$auc_values
#'
#' # Check which model performed best
#' result$best_model
#' }
compare_model_rocs <- function(formula, data, training_percent = 0.8,
                               save_plot = FALSE, 
                               pdf_filename = "roc_curves_comparison.pdf",
                               suppress_warnings = FALSE) {
  
  # Set warning suppression if requested
  old_warn <- options("warn")
  if(suppress_warnings) {
    options(warn = -1)
  }
  
  # Restore warnings on exit
  on.exit(options(warn = old_warn$warn), add = TRUE)
  
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
  roc_list <- list()
  
  # 1. Fit full logistic regression
  full_glm <- stats::glm(formula = formula, data = train_data, family = stats::binomial)
  pred_full_glm <- stats::predict(full_glm, newdata = test_data, type = "response")
  
  # 2. Fit backward stepwise logistic regression
  backward_stepwise <- stats::step(full_glm, direction = "backward", trace = 0)
  pred_backward <- stats::predict(backward_stepwise, newdata = test_data, type = "response")
  
  # 3. Fit forward stepwise logistic regression
  null_model <- stats::glm(reformulate("1", response = response_variable), 
                         data = train_data, family = stats::binomial)
  forward_stepwise <- stats::step(null_model, 
                                scope = list(lower = formula(null_model), upper = formula), 
                                direction = "forward", trace = 0)
  pred_forward <- stats::predict(forward_stepwise, newdata = test_data, type = "response")
  
  # 4. Fit GAM model
  # Convert numeric predictors to smooth terms
  terms_obj <- terms(formula, data = train_data)
  predictor_names <- attr(terms_obj, "term.labels")
  predictor_classes <- sapply(train_data[predictor_names], class)
  
  # Create new formula with s() for numeric predictors
  gam_terms <- sapply(seq_along(predictor_names), function(i) {
    if (predictor_classes[i] %in% c("numeric", "integer")) {
      paste0("s(", predictor_names[i], ")")
    } else {
      predictor_names[i]
    }
  })
  
  gam_formula <- reformulate(gam_terms, response = response_variable)
  gam_model <- tryCatch({
    mgcv::gam(gam_formula, data = train_data, family = stats::binomial)
  }, error = function(e) {
    warning("GAM model failed to fit. Error: ", e$message)
    return(NULL)
  })
  
  if (!is.null(gam_model)) {
    pred_gam <- stats::predict(gam_model, newdata = test_data, type = "response")
  } else {
    pred_gam <- NULL
  }
  
  # 5. & 6. Fit lasso and ridge regression
  x_train <- stats::model.matrix(object = formula, data = train_data)[, -1]
  y_train <- train_data[[response_variable]]
  x_test <- stats::model.matrix(object = formula, data = test_data)[, -1]
  
  cv_lasso <- glmnet::cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)
  pred_lasso <- stats::predict(cv_lasso, newx = x_test, s = "lambda.min", type = "response")
  
  cv_ridge <- glmnet::cv.glmnet(x_train, y_train, family = "binomial", alpha = 0)
  pred_ridge <- stats::predict(cv_ridge, newx = x_test, s = "lambda.min", type = "response")
  
  # Create ROC objects for each model
  truth <- test_data[[response_variable]]
  
  # Full GLM
  roc_full <- pROC::roc(truth, pred_full_glm, plot = FALSE)
  roc_list[["full.glm"]] <- roc_full
  
  # Backward stepwise
  roc_backward <- pROC::roc(truth, pred_backward, plot = FALSE)
  roc_list[["backward.stepwise"]] <- roc_backward
  
  # Forward stepwise
  roc_forward <- pROC::roc(truth, pred_forward, plot = FALSE)
  roc_list[["forward.stepwise"]] <- roc_forward
  
  # GAM (if successful)
  if (!is.null(pred_gam)) {
    roc_gam <- pROC::roc(truth, pred_gam, plot = FALSE)
    roc_list[["gam"]] <- roc_gam
  }
  
  # Lasso
  roc_lasso <- pROC::roc(truth, as.vector(pred_lasso), plot = FALSE)
  roc_list[["lasso"]] <- roc_lasso
  
  # Ridge
  roc_ridge <- pROC::roc(truth, as.vector(pred_ridge), plot = FALSE)
  roc_list[["ridge"]] <- roc_ridge
  
  # Calculate AUC values for all models
  auc_values <- sapply(roc_list, function(roc) as.numeric(pROC::auc(roc)))
  
  # Determine best model
  best_model <- names(which.max(auc_values))
  
  # Plot the ROC curves in a multi-panel layout
  if (save_plot) {
    grDevices::pdf(pdf_filename, width = 10, height = 8)
  }
  
  # Define layout for multi-panel plot
  n_models <- length(roc_list)
  n_cols <- min(3, n_models)  # Maximum 3 columns
  n_rows <- ceiling(n_models / n_cols)
  
  # Define color scheme for models
  model_colors <- c(
    "full.glm" = "#E41A1C",         # Red
    "backward.stepwise" = "#377EB8", # Blue
    "forward.stepwise" = "#4DAF4A",  # Green
    "lasso" = "#984EA3",            # Purple
    "ridge" = "#FF7F00",            # Orange
    "gam" = "#FFD700"               # Gold
  )
  
  # Set up the layout
  graphics::par(mfrow = c(n_rows, n_cols))
  graphics::par(mar = c(4, 4, 3, 1) + 0.1)  # Adjust margins for panels
  
  # Plot each ROC curve in its own panel
  for (model_name in names(roc_list)) {
    # Get the ROC object
    roc_obj <- roc_list[[model_name]]
    
    # Get the color for this model
    model_color <- model_colors[model_name]
    
    # Plot individual ROC
    plot(roc_obj, 
         main = paste("ROC for", model_name),
         col = model_color,
         lwd = 2,
         xlab = "1 - Specificity (FPR)",
         ylab = "Sensitivity (TPR)",
         legacy.axes = TRUE,
         grid = TRUE)
    
    # Add AUC value in the matching color
    auc_value <- round(as.numeric(pROC::auc(roc_obj)), 3)
    graphics::text(0.7, 0.2,
                   paste("AUC =", auc_value),
                   cex = 0.9,
                   font = 2,
                   col = model_color)
    
    # Add reference line
    graphics::abline(0, 1, lty = 2, col = "gray")
    
    # Highlight the best model with a star
    if (model_name == best_model) {
      graphics::mtext("* Best Model", side = 3, line = 0.3, cex = 0.8, font = 2)
    }
  }
  
  if (save_plot) {
    grDevices::dev.off()
  }
  
  # Return the results
  return(list(
    roc_list = roc_list,
    auc_values = auc_values,
    best_model = best_model
  ))
} 