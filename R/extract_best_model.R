#' Extract Best Model from OptimalModelSearch Results
#'
#' This function takes the results from OptimalModelSearch and returns the actual
#' best model object that can be used for further analysis, prediction, or examination.
#'
#' @param results A list returned by OptimalModelSearch function
#' @param data The original data frame used for model fitting
#' @param refit Logical indicating whether to refit the model on the full dataset (TRUE)
#'              or return the model fitted on the training set (FALSE). Default is TRUE.
#'
#' @return A model object of the appropriate class:
#'   \itemize{
#'     \item For full.glm: A glm object
#'     \item For backward.stepwise or forward.stepwise: A glm object
#'     \item For gam: A gam object from the mgcv package
#'     \item For lasso or ridge: A cv.glmnet object from the glmnet package
#'   }
#'
#' @details
#' The returned model can be used for prediction with \code{predict()}, examination
#' of coefficients, or any other analysis typically performed on the respective model class.
#' 
#' When \code{refit=TRUE} (the default), the function refits the model on the complete dataset
#' for improved prediction performance.
#'
#' @export
#' @importFrom stats glm binomial step predict reformulate
#' @importFrom mgcv gam
#' @importFrom glmnet cv.glmnet glmnet
#'
#' @examples
#' \dontrun{
#' # Find the best model
#' library(mlbench)
#' data(Sonar)
#' dat <- Sonar
#' dat$Class <- ifelse(dat$Class=="R", 0, 1)
#' 
#' # Run OptimalModelSearch
#' result <- OptimalModelSearch(formula=Class~., data=dat,
#'                            criterion="AUC", suppress_warnings=TRUE)
#'                            
#' # Extract the best model
#' best_model <- extract_best_model(result, dat)
#' 
#' # Use the model for prediction
#' predictions <- predict(best_model, newdata=dat[1:5,], type="response")
#' print(predictions)
#' 
#' # Examine model coefficients (if applicable to the model type)
#' if(inherits(best_model, "glm") || inherits(best_model, "gam")) {
#'   print(summary(best_model))
#' }
#' }
extract_best_model <- function(results, data, refit = TRUE) {
  # Check input
  if (!is.list(results) || 
      !all(c("criterion", "best_model_name", "performance_metric") %in% names(results))) {
    stop("Input must be results from OptimalModelSearch")
  }
  
  # Extract the formula from the results object
  formula_text <- attr(results, "formula")
  if (is.null(formula_text)) {
    stop("Could not find formula information in results")
  }
  formula <- stats::as.formula(formula_text)
  
  # Identify the best model type
  best_model_name <- results$best_model_name
  
  # Extract the base model type (removing any criterion suffixes)
  # This handles cases like "full.glm.Accuracy"
  model_parts <- strsplit(best_model_name, "\\.")[[1]]
  if (length(model_parts) > 1) {
    # If the last part is the same as the criterion, remove it
    if (tolower(model_parts[length(model_parts)]) == tolower(results$criterion)) {
      model_parts <- model_parts[-length(model_parts)]
    }
    
    # Check if the model type is two parts (e.g., "full.glm")
    if (length(model_parts) >= 2 && model_parts[2] == "glm") {
      model_type <- paste(model_parts[1:2], collapse=".")
    } else {
      model_type <- model_parts[1]
    }
  } else {
    model_type <- model_parts[1]
  }
  
  # If not refitting, check if we have stored models
  if (!refit) {
    if ("models" %in% names(attributes(results))) {
      stored_models <- attr(results, "models")
      if (best_model_name %in% names(stored_models)) {
        return(stored_models[[best_model_name]])
      }
    }
    warning("Stored model not found. Refitting model even though refit=FALSE")
  }
  
  # Refit the model based on its type
  model <- switch(
    model_type,
    "full" = stats::glm(formula, data = data, family = stats::binomial),
    
    "full.glm" = stats::glm(formula, data = data, family = stats::binomial),
    
    "backward" = {
      full_model <- stats::glm(formula, data = data, family = stats::binomial)
      stats::step(full_model, direction = "backward", trace = 0)
    },
    
    "forward" = {
      # Start with intercept-only model
      null_model <- stats::glm(reformulate("1", response = all.vars(formula)[1]), 
                               data = data, family = stats::binomial)
      # Do forward selection
      stats::step(null_model, scope = list(lower = formula(null_model), upper = formula), 
                  direction = "forward", trace = 0)
    },
    
    "gam" = {
      # Convert numeric predictors to smooth terms
      terms_obj <- terms(formula, data = data)
      predictor_names <- attr(terms_obj, "term.labels")
      predictor_classes <- sapply(data[predictor_names], class)
      
      # Create new formula with s() for numeric predictors
      gam_terms <- sapply(seq_along(predictor_names), function(i) {
        if (predictor_classes[i] %in% c("numeric", "integer")) {
          paste0("s(", predictor_names[i], ")")
        } else {
          predictor_names[i]
        }
      })
      
      gam_formula <- reformulate(gam_terms, response = all.vars(formula)[1])
      mgcv::gam(gam_formula, data = data, family = stats::binomial)
    },
    
    "lasso" = {
      # Prepare data for glmnet
      response_var <- all.vars(formula)[1]
      y <- data[[response_var]]
      x <- stats::model.matrix(formula, data = data)[, -1] # Remove intercept
      
      # Fit lasso model
      glmnet::cv.glmnet(x, y, family = "binomial", alpha = 1)
    },
    
    "ridge" = {
      # Prepare data for glmnet
      response_var <- all.vars(formula)[1]
      y <- data[[response_var]]
      x <- stats::model.matrix(formula, data = data)[, -1] # Remove intercept
      
      # Fit ridge model
      glmnet::cv.glmnet(x, y, family = "binomial", alpha = 0)
    },
    
    # Default if model type not recognized
    stop(paste("Unrecognized model type:", model_type))
  )
  
  return(model)
} 