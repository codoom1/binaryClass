#' Internal helper function to draw a confusion matrix visualization
#'
#' @param cm A confusion matrix object (typically from `caret::confusionMatrix`).
#' @keywords internal
#' @export
.draw_confusion_matrix <- function(cm) {

    graphics::layout(base::matrix(c(1,1,2)))
    graphics::par(mar=c(2,2,2,2))
    graphics::plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
    graphics::title('CONFUSION MATRIX', cex.main=2)

    # create the matrix
    graphics::rect(150, 430, 240, 370, col='#3F97D0')
    graphics::text(195, 435, '0', cex=1.2)
    graphics::rect(250, 430, 340, 370, col='#F7AD50')
    graphics::text(295, 435, '1', cex=1.2)
    graphics::text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
    graphics::text(245, 450, 'Actual', cex=1.3, font=2)
    graphics::rect(150, 305, 240, 365, col='#F7AD50')
    graphics::rect(250, 305, 340, 365, col='#3F97D0')
    graphics::text(140, 400, '0', cex=1.2, srt=90)
    graphics::text(140, 335, '1', cex=1.2, srt=90)

    # add in the cm results
    res <- as.numeric(cm$table)
    graphics::text(195, 400, res[1], cex=1.6, font=2, col='white')
    graphics::text(195, 335, res[2], cex=1.6, font=2, col='white')
    graphics::text(295, 400, res[3], cex=1.6, font=2, col='white')
    graphics::text(295, 335, res[4], cex=1.6, font=2, col='white')

    # add in the specifics
    graphics::plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
    graphics::text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
    graphics::text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1)
    graphics::text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
    graphics::text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1)

    graphics::text(50, 85, names(cm$byClass[3]), cex=1.2, font=2)
    graphics::text(50, 70, round(as.numeric(cm$byClass[3]), 3), cex=1)

    graphics::text(73, 85, names(cm$byClass[4]), cex=1.2, font=2)
    graphics::text(73, 70, round(as.numeric(cm$byClass[4]), 3), cex=1)

    graphics::text(93, 85, names(cm$byClass[8]), cex=1.2, font=2)
    graphics::text(93, 70, round(as.numeric(cm$byClass[8]), 3), cex=1)

    # add in the accuracy information
    graphics::text(10, 35, names(cm$byClass[9]), cex=1.2, font=2)
    graphics::text(10, 17, round(as.numeric(cm$byClass[9]), 3), cex=1.2)

    graphics::text(30, 35, names(cm$overall[1]), cex=1.2, font=2)
    graphics::text(30, 17, round(as.numeric(cm$overall[1]), 3), cex=1.2)

    graphics::text(70, 35, names(cm$overall[2]), cex=1.2, font=2)
    graphics::text(70, 17, round(as.numeric(cm$overall[2]), 3), cex=1.2)

    graphics::text(90, 35, names(cm$byClass[11]), cex=1.2, font=2)
    graphics::text(90, 17, round(as.numeric(cm$byClass[11]), 3), cex=1.2)

    invisible(NULL) # Explicitly return NULL invisibly
  }

#' Find the Optimal Binary Classification Model
#'
#' This function trains and compares several binary classification models
#' (full logistic regression, backward stepwise, forward stepwise, GAM, Lasso, Ridge) 
#' based on a specified performance criterion (AUC, Accuracy, or AIC) using a training/test split.
#' It handles NA values by removing rows with missing data in any variable specified
#' in the formula.
#'
#' @param formula An object of class \"formula\": a symbolic description of the
#'   model to be fitted (e.g., `response ~ predictor1 + predictor2` or `response ~ .`).
#' @param data A data frame containing the variables in the model. Rows with NA
#'   values in any variable specified in the `formula` will be removed before analysis.
#' @param criterion A character string specifying the criterion for model selection.
#'   Must be one of "AUC", "Accuracy", or "AIC".
#' @param training_percent A numeric value between 0 and 1 indicating the proportion
#'   of the data to use for training. Default is `0.8`.
#' @param threshold A numeric value between 0 and 1 used as the probability
#'   threshold for classification when `criterion = "Accuracy"`. Default is `0.5`.
#' @param suppress_warnings Logical indicating whether to suppress warnings during execution. Default is FALSE.
#' @param plot_roc Logical indicating whether to automatically plot ROC curves when criterion is "AUC". Default is FALSE.
#' @param plot_comparison Logical indicating whether to plot comparison of all models' ROC curves when plot_roc is TRUE. Default is FALSE.
#' @param multi_panel Logical indicating whether to display each model in its own panel when plot_comparison is TRUE. Default is FALSE.
#' @param plot_cm Logical indicating whether to automatically plot the confusion matrix when criterion is "Accuracy". Default is FALSE.
#'
#' @section Visualization Options:
#' The function provides automatic visualization based on the chosen criterion:
#' \itemize{
#'   \item When criterion="AUC": Use plot_roc=TRUE to display the ROC curve of the best model,
#'         or with plot_comparison=TRUE to compare all models' ROC curves.
#'   \item When criterion="Accuracy": Use plot_cm=TRUE to display the confusion matrix
#'         of the best model, showing true/false positives/negatives and other metrics.
#'   \item When criterion="AIC": No visualization is provided as AIC is a numerical measure.
#' }
#'
#' @return A list containing the results:
#'   \itemize{
#'     \item{`criterion`}: The criterion used for selection.
#'     \item{`best_model_name`}: The name of the best model selected
#'            (e.g., "full.glm", "backward.stepwise", "forward.stepwise", "gam", 
#'            "lasso", "ridge", "lasso.refit.glm").
#'     \item{`performance_metric`}: The value of the specified criterion for the best model.
#'     \item{`best_model_object`}: The fitted model object for the best model.
#'     \item{`coefficients`}: Coefficients of the best model (matrix or sparse matrix).
#'     \item{`details`}: Additional details, depending on the criterion:
#'       \itemize{
#'         \item{AUC: The `roc` object from `pROC` for the best model.}
#'         \item{Accuracy: The `confusionMatrix` object from `caret` for the best model.}
#'         \item{AIC: A list containing the AIC value (`AIC_value` or `AIC_refit_value`)
#'              and potentially the names of variables selected by Lasso (`selected_vars`).}
#'       }
#'   }
#' @export
#' @importFrom stats glm predict model.matrix na.omit coef step binomial rnorm terms complete.cases as.formula AIC
#' @importFrom utils head
#' @importFrom graphics plot text abline legend
#' @import glmnet pROC caret mgcv
#'
#' @examples
#' \dontrun{
#' # Load required packages for example
#' library(mlbench)
#' data(PimaIndiansDiabetes)
#'
#' # Ensure diabetes is a factor
#' PimaIndiansDiabetes$diabetes <- as.factor(PimaIndiansDiabetes$diabetes)
#'
#' # Find best model based on AUC using all predictors
#' set.seed(42) # for reproducibility of train/test split
#' result_auc <- OptimalModelSearch(diabetes ~ ., data = PimaIndiansDiabetes, criterion = "AUC")
#' print(result_auc$best_model_name)
#' print(result_auc$performance_metric)
#' plot(result_auc$details) # Plot ROC curve
#' graphics::text(0.7, 0.2, paste("AUC =", round(pROC::auc(result_auc$details), 3)), cex = 0.9)
#'
#' # Find best model based on Accuracy with a 0.6 threshold
#' set.seed(42)
#' result_acc <- OptimalModelSearch(diabetes ~ glucose + mass + age, data = PimaIndiansDiabetes,
#'                                  criterion = "Accuracy", threshold = 0.6)
#' print(result_acc$best_model_name)
#' print(result_acc$performance_metric)
#' print(result_acc$details) # Print confusion matrix details
#'
#' # Find best model based on AIC
#' set.seed(42)
#' result_aic <- OptimalModelSearch(diabetes ~ pregnant + glucose + pressure + mass + pedigree + age,
#'                                  data = PimaIndiansDiabetes, criterion = "AIC")
#' print(result_aic$best_model_name)
#' print(result_aic$performance_metric)
#' print(result_aic$coefficients)
#'
#' # Plot all ROC curves in separate panels - clearer visualization
#' result <- OptimalModelSearch(formula=Class~., data=dat,
#'                           criterion="AUC", training_percent=0.8,
#'                           suppress_warnings=TRUE, plot_roc=TRUE, 
#'                           plot_comparison=TRUE, multi_panel=TRUE)
#' }
#'
#' @usage OptimalModelSearch(formula, data, criterion = c("AUC", "Accuracy", "AIC"),
#'                  training_percent = 0.8, threshold = 0.5, suppress_warnings = FALSE,
#'                  plot_roc = FALSE, plot_comparison = FALSE, multi_panel = FALSE, plot_cm = FALSE)
OptimalModelSearch = function(formula, data, criterion = c("AUC", "Accuracy", "AIC"), 
                           training_percent = 0.8, threshold = 0.5, suppress_warnings = FALSE,
                           plot_roc = FALSE, plot_comparison = FALSE, multi_panel = FALSE, plot_cm = FALSE){
#options(warn = -1, message = FALSE) # Avoid global option changes

# Match argument for criterion
criterion <- match.arg(criterion)

# Set warning suppression if requested
old_warn <- options("warn")
if(suppress_warnings) {
  options(warn = -1)
}

# --- NA Handling: Keep only complete cases for formula variables --- 
# Get all variable names involved in the formula, expanding '.' if present
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

# Initialize results list
results <- list()

# Extract the response variable (from the complete data)
response_variable <- all.vars(formula)[1]

### Divide the data into training and testing data
train.percent <- training_percent

# Set the seed for reproducibility # Removed set.seed(123)
# Calculate the number of training rows based on the percentage
num_training_rows <- round(nrow(data_complete) * train.percent)

# Create a vector of row indices for the training set
training.rows <- sample(1:nrow(data_complete), size = num_training_rows)
# Create the training and validation batches
train.data <- data_complete[training.rows, ]
test.data <- data_complete[-training.rows, ]

### Fitting the different models with the training data

## 1. Fitting full GLM model with all predictors
full.glm <- stats::glm(formula = formula, data = train.data, family = stats::binomial)

## 2. Backward stepwise selection
backward.stepwise <- stats::step(full.glm, direction = "backward", trace = 0) # Suppress step output

## 3. Forward stepwise selection
# Start with an intercept-only model
null.model <- stats::glm(as.formula(paste(response_variable, "~ 1")), 
                          data = train.data, family = stats::binomial)
# Perform forward selection
forward.stepwise <- stats::step(null.model, 
                                scope = list(lower = as.formula(paste(response_variable, "~ 1")), 
                                            upper = formula),
                                direction = "forward", trace = 0)

## 4. Fitting GAM model using smooth terms
# We'll need to check if mgcv package is available
if (!requireNamespace("mgcv", quietly = TRUE)) {
  warning("Package 'mgcv' is needed for GAM model but is not available. Skipping GAM.")
  gam.model <- NULL
} else {
  # Create a GAM formula by adding s() around each numeric predictor
  # First, identify numeric predictors
  pred_classes <- sapply(train.data[, setdiff(names(train.data), response_variable), drop = FALSE], 
                         function(x) is.numeric(x))
  numeric_preds <- names(pred_classes)[pred_classes]
  
  if (length(numeric_preds) > 0) {
    # Create the GAM formula
    gam_terms <- paste(sapply(numeric_preds, function(x) paste0("s(", x, ")")), collapse = " + ")
    
    # Add non-numeric predictors as is
    non_numeric_preds <- names(pred_classes)[!pred_classes]
    if (length(non_numeric_preds) > 0) {
      gam_terms <- paste(gam_terms, paste(non_numeric_preds, collapse = " + "), sep = " + ")
    }
    
    gam_formula <- as.formula(paste(response_variable, "~", gam_terms))
    
    # Fit the GAM model
    gam.model <- tryCatch({
      mgcv::gam(gam_formula, family = stats::binomial, data = train.data)
    }, error = function(e) {
      warning(paste("Error fitting GAM model:", e$message))
      NULL
    })
  } else {
    warning("No numeric predictors found for GAM smooth terms. Skipping GAM.")
    gam.model <- NULL
  }
}

## 5. Fitting with Lasso regression
# Fit the lasso regression model
x <- stats::model.matrix(object = formula, data = train.data)[,-1]
y <- train.data[[response_variable]]
lasso.model <- glmnet::glmnet(x, y, family = "binomial", alpha = 1)

# Use cross-validation to choose the lambda parameter
cv.lasso <- glmnet::cv.glmnet(x, y, family = "binomial", alpha = 1)
best.lambda_lasso <- cv.lasso$lambda.min

# Get lasso model coefficients at the best lambda
lasso.coef <- stats::coef(lasso.model, s = best.lambda_lasso)

## 6. Fitting with ridge regression models
ridge.model <- glmnet::glmnet(x, y, family = "binomial", alpha = 0)
# Use cross-validation to choose the lambda parameter
cv.ridge <- glmnet::cv.glmnet(x, y, family = "binomial", alpha = 0)
best.lambda_ridge <- cv.ridge$lambda.min

# Get ridge model coefficients at the best lambda
ridge.coef <- stats::coef(ridge.model, s = best.lambda_ridge)

### Testing the model using the test data
## 1. Full GLM model predictions
pred.full.glm <- stats::predict(full.glm, newdata = test.data, type = "response")

## 2. Backward stepwise model predictions
pred.backward.stepwise <- stats::predict(backward.stepwise, newdata = test.data, type = "response")

## 3. Forward stepwise model predictions
pred.forward.stepwise <- stats::predict(forward.stepwise, newdata = test.data, type = "response")

## 4. GAM model predictions (if available)
if (!is.null(gam.model)) {
  pred.gam <- stats::predict(gam.model, newdata = test.data, type = "response")
} else {
  pred.gam <- NULL
}

## 5-6. Prepare data for Lasso/Ridge predictions
# Create a new dataframe that excludes the response variable
# Get the variables from the formula
vars <- all.vars(formula)

if(vars[2] != ".") {
  # Remove the response variable
  predictor_vars <- vars[!(vars %in% response_variable)]
  
  # Select only these variables from test.data
  test.data.noresponse <- test.data[, names(test.data) %in% vars]
  testdata <- stats::model.matrix(object = formula, data = test.data.noresponse)[,-1]
} else {
  testdata <- stats::model.matrix(object = formula, data = test.data)[,-1]
}

## 5. Lasso predictions using the best lambda
pred.lasso <- stats::predict(cv.lasso, newx = testdata, s = best.lambda_lasso, type = "response")

## 6. Ridge predictions using the best lambda
pred.ridge <- stats::predict(ridge.model, newx = testdata, s = best.lambda_ridge, type = "response")

## Comparing models based on the specified criterion
if(criterion == "AUC") {
  # Ensure criterion is set in results list
  results$criterion <- "AUC"
  
  ## Creating ROC curves for each model
  
  # 1. ROC for full GLM
  roc_full.glm <- suppressWarnings(
    pROC::roc(test.data[[response_variable]] ~ pred.full.glm, plot = FALSE, print.auc = TRUE)
  )
  
  # 2. ROC for backward stepwise
  roc_backward.stepwise <- suppressWarnings(
    pROC::roc(test.data[[response_variable]] ~ pred.backward.stepwise, plot = FALSE, print.auc = TRUE)
  )
  
  # 3. ROC for forward stepwise
  roc_forward.stepwise <- suppressWarnings(
    pROC::roc(test.data[[response_variable]] ~ pred.forward.stepwise, plot = FALSE, print.auc = TRUE)
  )
  
  # 4. ROC for GAM (if available)
  if (!is.null(pred.gam)) {
    roc_gam <- suppressWarnings(
      pROC::roc(test.data[[response_variable]], pred.gam, plot = FALSE, print.auc = TRUE)
    )
    gam_auc <- as.numeric(pROC::auc(roc_gam))
  } else {
    roc_gam <- NULL
    gam_auc <- NA
  }
  
  # 5. ROC for Lasso
  roc_lasso <- suppressWarnings(
    pROC::roc(test.data[[response_variable]], as.vector(pred.lasso), plot = FALSE, print.auc = TRUE)
  )
  
  # 6. ROC for Ridge
  roc_ridge <- suppressWarnings(
    pROC::roc(test.data[[response_variable]], as.vector(pred.ridge), plot = FALSE, print.auc = TRUE)
  )
  
  ## Extracting the AUCs
  full.glm_auc <- as.numeric(pROC::auc(roc_full.glm))
  backward.stepwise_auc <- as.numeric(pROC::auc(roc_backward.stepwise))
  forward.stepwise_auc <- as.numeric(pROC::auc(roc_forward.stepwise))
  lasso_auc <- as.numeric(pROC::auc(roc_lasso))
  ridge_auc <- as.numeric(pROC::auc(roc_ridge))
  
  # Put the AUCs in a named vector, excluding NA values
  aucs <- c(
    full.glm = full.glm_auc,
    backward.stepwise = backward.stepwise_auc,
    forward.stepwise = forward.stepwise_auc,
    lasso = lasso_auc,
    ridge = ridge_auc
  )
  
  # Add GAM if available
  if (!is.na(gam_auc)) {
    aucs <- c(aucs, gam = gam_auc)
  }
  
  # Find the name of the model with the highest AUC
  best_model <- names(which.max(aucs))
  
  # Store results
  results$best_model_name <- best_model
  results$performance_metric <- aucs[[best_model]]
  
  # Store the best model object and coefficients
  if (best_model == "full.glm") {
    results$best_model_object <- full.glm
    results$coefficients <- stats::coef(full.glm)
    results$details <- roc_full.glm
  } else if (best_model == "backward.stepwise") {
    results$best_model_object <- backward.stepwise
    results$coefficients <- stats::coef(backward.stepwise)
    results$details <- roc_backward.stepwise
  } else if (best_model == "forward.stepwise") {
    results$best_model_object <- forward.stepwise
    results$coefficients <- stats::coef(forward.stepwise)
    results$details <- roc_forward.stepwise
  } else if (best_model == "gam") {
    results$best_model_object <- gam.model
    results$coefficients <- stats::coef(gam.model)
    results$details <- roc_gam
  } else if (best_model == "lasso") {
    results$best_model_object <- cv.lasso
    results$coefficients <- lasso.coef
    results$details <- roc_lasso
  } else if (best_model == "ridge") {
    results$best_model_object <- cv.ridge
    results$coefficients <- ridge.coef
    results$details <- roc_ridge
  }
  
  # Store all ROC objects in attributes
  roc_list <- list(
    full.glm = roc_full.glm,
    backward.stepwise = roc_backward.stepwise,
    forward.stepwise = roc_forward.stepwise,
    lasso = roc_lasso,
    ridge = roc_ridge
  )
  
  # Add GAM ROC if available
  if (!is.null(roc_gam)) {
    roc_list$gam <- roc_gam
  }
  
  # Attach the ROC list to the details for potential later use
  attr(results$details, "roc_list") <- roc_list
  
} else if(criterion == "Accuracy") {
  # Ensure criterion is set in results list
  results$criterion <- "Accuracy"
  
  # Define the threshold for classification
  threshold <- threshold
  
  # Get the true labels
  truth.test <- as.factor(test.data[[response_variable]])
  
  # Prepare confusion matrices for each model
  
  # 1. Full GLM
  predict.full.glm <- as.factor(ifelse(pred.full.glm > threshold, "1", "0"))
  CM_full.glm <- caret::confusionMatrix(predict.full.glm, truth.test)
  
  # 2. Backward stepwise
  predict.backward.stepwise <- as.factor(ifelse(pred.backward.stepwise > threshold, "1", "0"))
  CM_backward.stepwise <- caret::confusionMatrix(predict.backward.stepwise, truth.test)
  
  # 3. Forward stepwise
  predict.forward.stepwise <- as.factor(ifelse(pred.forward.stepwise > threshold, "1", "0"))
  CM_forward.stepwise <- caret::confusionMatrix(predict.forward.stepwise, truth.test)
  
  # 4. GAM (if available)
  if (!is.null(pred.gam)) {
    predict.gam <- as.factor(ifelse(pred.gam > threshold, "1", "0"))
    CM_gam <- caret::confusionMatrix(predict.gam, truth.test)
    gam_accuracy <- CM_gam$overall['Accuracy']
  } else {
    CM_gam <- NULL
    gam_accuracy <- NA
  }
  
  # 5. Lasso
  predict.lasso <- as.factor(ifelse(pred.lasso > threshold, "1", "0"))
  CM_lasso <- caret::confusionMatrix(predict.lasso, truth.test)
  
  # 6. Ridge
  predict.ridge <- as.factor(ifelse(pred.ridge > threshold, "1", "0"))
  CM_ridge <- caret::confusionMatrix(predict.ridge, truth.test)
  
  # Extract accuracy values
  full.glm_accuracy <- CM_full.glm$overall['Accuracy']
  backward.stepwise_accuracy <- CM_backward.stepwise$overall['Accuracy']
  forward.stepwise_accuracy <- CM_forward.stepwise$overall['Accuracy']
  lasso_accuracy <- CM_lasso$overall['Accuracy']
  ridge_accuracy <- CM_ridge$overall['Accuracy']
  
  # Put the accuracies in a named vector, excluding NA values
  accuracies <- c(
    full.glm = full.glm_accuracy,
    backward.stepwise = backward.stepwise_accuracy,
    forward.stepwise = forward.stepwise_accuracy,
    lasso = lasso_accuracy,
    ridge = ridge_accuracy
  )
  
  # Add GAM if available
  if (!is.na(gam_accuracy)) {
    accuracies <- c(accuracies, gam = gam_accuracy)
  }
  
  # Find the best model based on the highest accuracy
  best_model <- names(which.max(accuracies))
  best_metric <- max(accuracies)
  
  # Store results
  results$best_model_name <- best_model
  results$performance_metric <- best_metric
  
  # Store the best model object, coefficients, and confusion matrix
  if (best_model == "full.glm") {
    results$best_model_object <- full.glm
    results$coefficients <- stats::coef(full.glm)
    results$details <- CM_full.glm
  } else if (best_model == "backward.stepwise") {
    results$best_model_object <- backward.stepwise
    results$coefficients <- stats::coef(backward.stepwise)
    results$details <- CM_backward.stepwise
  } else if (best_model == "forward.stepwise") {
    results$best_model_object <- forward.stepwise
    results$coefficients <- stats::coef(forward.stepwise)
    results$details <- CM_forward.stepwise
  } else if (best_model == "gam") {
    results$best_model_object <- gam.model
    results$coefficients <- stats::coef(gam.model)
    results$details <- CM_gam
  } else if (best_model == "lasso") {
    results$best_model_object <- cv.lasso
    results$coefficients <- lasso.coef
    results$details <- CM_lasso
  } else if (best_model == "ridge") {
    results$best_model_object <- cv.ridge
    results$coefficients <- ridge.coef
    results$details <- CM_ridge
  }
  
} else if(criterion == "AIC") {
  # Ensure criterion is set in results list
  results$criterion <- "AIC"
  
  # --- AIC Comparison Logic ---
  # Calculate AIC for each model (except Ridge which can't be used for AIC)
  
  # 1. AIC for full GLM
  aic_full_glm <- stats::AIC(full.glm)
  if (!is.finite(aic_full_glm)) aic_full_glm <- Inf
  
  # 2. AIC for backward stepwise GLM
  aic_backward_stepwise <- stats::AIC(backward.stepwise)
  if (!is.finite(aic_backward_stepwise)) aic_backward_stepwise <- Inf
  
  # 3. AIC for forward stepwise GLM
  aic_forward_stepwise <- stats::AIC(forward.stepwise)
  if (!is.finite(aic_forward_stepwise)) aic_forward_stepwise <- Inf
  
  # 4. AIC for GAM (if available)
  if (!is.null(gam.model)) {
    aic_gam <- stats::AIC(gam.model)
    if (!is.finite(aic_gam)) aic_gam <- Inf
  } else {
    aic_gam <- Inf
  }
  
  # 5. Refit GLM for Lasso selected variables
  # Use the cv.lasso object to get coefficients at lambda.min
  lasso_coef_at_min <- stats::coef(cv.lasso, s = "lambda.min")
  lasso_selected_vars <- rownames(lasso_coef_at_min)[-1][lasso_coef_at_min[-1, 1] != 0] # Exclude intercept
  aic_lasso_refit <- Inf # Default to infinite AIC
  
  if (length(lasso_selected_vars) > 0) {
    # Create formula with selected variables
    refit_formula_str <- paste(response_variable, "~", paste(lasso_selected_vars, collapse = " + "))
    refit_formula <- tryCatch(stats::as.formula(refit_formula_str), error=function(e) NULL)
    
    if(!is.null(refit_formula)) {
      # Refit GLM
      lasso_refit_glm <- tryCatch(
        stats::glm(refit_formula, data = train.data, family = stats::binomial),
        error = function(e) {warning("GLM refit for Lasso variables failed: ", e$message); NULL}
      )
      if (!is.null(lasso_refit_glm)) {
        aic_lasso_refit <- stats::AIC(lasso_refit_glm)
        if (!is.finite(aic_lasso_refit)) aic_lasso_refit <- Inf
      }
    } else {
      warning("Failed to create refit formula for Lasso variables.")
    }
  } else {
    # If Lasso selected no variables, use intercept-only model AIC
    intercept_only_formula <- stats::as.formula(paste(response_variable, "~ 1"))
    intercept_only_glm <- tryCatch(
      stats::glm(intercept_only_formula, data = train.data, family = stats::binomial),
      error = function(e) {warning("Intercept-only GLM fit failed: ", e$message); NULL}
    )
    if (!is.null(intercept_only_glm)) {
      aic_lasso_refit <- stats::AIC(intercept_only_glm) # AIC of intercept-only model
      if (!is.finite(aic_lasso_refit)) aic_lasso_refit <- Inf
    }
  }
  
  # 6. Ridge cannot be used for AIC (excluded)
  
  # Compare AICs (exclude Ridge)
  aics <- c(
    full.glm = aic_full_glm,
    backward.stepwise = aic_backward_stepwise,
    forward.stepwise = aic_forward_stepwise,
    lasso.refit.glm = aic_lasso_refit
  )
  
  # Add GAM if available
  if (aic_gam < Inf) {
    aics <- c(aics, gam = aic_gam)
  }
  
  # Update warning message
  warning("AIC criterion compares GLM models (full, backward stepwise, forward stepwise), GAM (if available), and a GLM refitted using variables selected by Lasso. Ridge regression is excluded from AIC comparison.")
  
  # Find the model with minimum AIC
  best_model_name <- names(which.min(aics))
  
  # Store results
  results$best_model_name <- best_model_name
  results$performance_metric <- aics[[best_model_name]]
  
  # Store appropriate model object and coefficients based on best model
  if (best_model_name == "full.glm") {
    results$best_model_object <- full.glm
    results$coefficients <- stats::coef(full.glm)
    results$details <- list(AIC_value = aic_full_glm)
  } else if (best_model_name == "backward.stepwise") {
    results$best_model_object <- backward.stepwise
    results$coefficients <- stats::coef(backward.stepwise)
    results$details <- list(AIC_value = aic_backward_stepwise)
  } else if (best_model_name == "forward.stepwise") {
    results$best_model_object <- forward.stepwise
    results$coefficients <- stats::coef(forward.stepwise)
    results$details <- list(AIC_value = aic_forward_stepwise)
  } else if (best_model_name == "gam") {
    results$best_model_object <- gam.model
    results$coefficients <- stats::coef(gam.model)
    results$details <- list(AIC_value = aic_gam)
  } else if (best_model_name == "lasso.refit.glm") {
    # Store original Lasso results as the selection method was Lasso
    results$best_model_object <- lasso_refit_glm
    results$coefficients <- stats::coef(lasso_refit_glm)
    results$details <- list(
      AIC_refit_value = aic_lasso_refit, 
      selected_vars = lasso_selected_vars
    )
  }
  
} else {
  stop("Invalid criterion specified. Must be 'AUC', 'Accuracy', or 'AIC'.")
}

# If plot_roc is TRUE and criterion is "AUC", plot the ROC curves
if (plot_roc && criterion == "AUC") {
  if (requireNamespace("binaryClass", quietly = TRUE)) {
    # Use the plot_model_rocs function if available
    binaryClass::plot_model_rocs(results, comparison = plot_comparison, multi_panel = multi_panel)
  } else {
    # Fallback to basic plotting if the function is not available
    # (this could happen during package development)
    if (plot_comparison) {
      # Plot comparison of all ROC curves
      all_rocs <- attr(results$details, "roc_list")
      if (!is.null(all_rocs)) {
        model_names <- names(all_rocs)
        model_colors <- c("blue", "red", "green", "purple", "orange", "brown")
        colors_to_use <- model_colors[1:min(length(model_names), length(model_colors))]
        
        # Plot the first curve
        plot(all_rocs[[1]], col=colors_to_use[1], main="Comparison of ROC Curves")
        
        # Add other curves
        if (length(model_names) > 1) {
          for (i in 2:length(model_names)) {
            plot(all_rocs[[i]], col=colors_to_use[i], add=TRUE)
          }
        }
        
        # Add legend
        graphics::legend("bottomright", legend=model_names, col=colors_to_use, lwd=2)
      } else {
        warning("No multiple ROC objects found for comparison.")
        plot(results$details, main=paste("ROC Curve for", results$best_model_name))
        graphics::text(0.7, 0.2, 
             paste("AUC =", round(as.numeric(results$performance_metric), 3)),
             cex=0.9)
      }
    } else {
      # Plot only the best model
      plot(results$details, main=paste("ROC Curve for", results$best_model_name))
      graphics::text(0.7, 0.2, 
           paste("AUC =", round(as.numeric(results$performance_metric), 3)),
           cex=0.9)
    }
  }
}

# If plot_cm is TRUE and criterion is "Accuracy", plot the confusion matrix
if (plot_cm && criterion == "Accuracy") {
  if (requireNamespace("binaryClass", quietly = TRUE)) {
    # Use the plot_model_cm function if available
    tryCatch({
      binaryClass::plot_model_cm(results)
    }, error = function(e) {
      # Fallback to basic caret confusion matrix printing
      print(results$details)
      warning("Could not plot confusion matrix with plot_model_cm: ", e$message)
    })
  } else {
    # Fallback to basic caret confusion matrix plotting
    print(results$details)
  }
}

# Reset warning settings to original state at the end
on.exit(options(warn = old_warn$warn), add = TRUE)
  
# Store the formula as an attribute for extract_best_model function
attr(results, "formula") <- deparse(formula)
  
# Return results
return(results)
}

#' Extract the best model from the results of OptimalModelSearch
#'
#' This function extracts the best model from the results of OptimalModelSearch
#' and returns the actual fitted model object.
#'
#' @param results A list containing the results of OptimalModelSearch.
#' @param data A data frame containing the original data used for model fitting.
#' @return The best model object from the results.
#' @export
extract_best_model <- function(results, data) {
  if (is.null(results$best_model_object)) {
    stop("No best model object found in results.")
  }
  return(results$best_model_object)
}














