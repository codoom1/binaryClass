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
#' Note: For glmnet models (lasso and ridge), use the \code{\link{predict_model}} function 
#' instead of \code{predict()} directly to handle the data format conversions automatically.
#'
#' @export
#' @keywords models regression
#' @concept binary classification
#' @concept model selection
#' @concept optimal model
#' @concept prediction
#' @concept model extraction
#' @concept model refitting
#' @aliases get_best_model model_extraction
#' @seealso 
#'   \code{\link{OptimalModelSearch}} for generating the results object
#'   \code{\link{predict_model}} for making predictions with extracted models
#' @importFrom stats glm binomial step predict reformulate model.matrix
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
#' # Use the predict_model function for prediction (works for all model types)
#' predictions <- predict_model(best_model, newdata=dat[1:5,], 
#'                             formula=Class~., type="response")
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
        model <- stored_models[[best_model_name]]
        # Store model_type as an attribute for later reference
        attr(model, "model_type") <- model_type
        attr(model, "original_formula") <- formula
        return(model)
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
  
  # Store model_type as an attribute for later reference
  attr(model, "model_type") <- model_type
  attr(model, "original_formula") <- formula
  
  return(model)
}

#' Make predictions from a model extracted by extract_best_model
#'
#' This function provides a unified interface for making predictions with any
#' model type returned by the extract_best_model function, handling the 
#' appropriate data formatting for each model type.
#'
#' @param model A model object returned by extract_best_model
#' @param newdata A data frame containing the new data for prediction
#' @param formula The formula used to fit the model (required for glmnet models)
#' @param type The type of prediction to return, usually "response" for probabilities
#' @param ... Additional arguments passed to the underlying predict method
#'
#' @return A vector of predictions
#'
#' @details
#' This function automatically detects the model type and applies the appropriate
#' prediction method. For glmnet models (lasso and ridge), it handles the conversion
#' of the data frame to a model matrix as required by the predict.cv.glmnet method.
#'
#' @export
#' @examples
#' \dontrun{
#' # Extract the best model from OptimalModelSearch results
#' best_model <- extract_best_model(result, data)
#' 
#' # Make predictions on new data
#' predictions <- predict_model(best_model, newdata = new_data, 
#'                             formula = y ~ x1 + x2)
#' }
predict_model <- function(model, newdata, formula = NULL, type = "response", ...) {
  if (missing(newdata)) {
    stop("newdata must be provided for prediction")
  }
  
  # Try to get model type from model attributes
  model_type <- attr(model, "model_type")
  original_formula <- attr(model, "original_formula")
  
  # If formula is explicitly provided, use it
  if (!is.null(formula)) {
    use_formula <- formula
  } else if (!is.null(original_formula)) {
    use_formula <- original_formula
  } else {
    # For non-glmnet models, formula might not be needed
    use_formula <- NULL
  }
  
  # Handle prediction based on model type
  if (!is.null(model_type) && model_type %in% c("lasso", "ridge")) {
    # For glmnet models, we need to convert to model matrix
    if (is.null(use_formula)) {
      stop("Formula is required for prediction with lasso/ridge models")
    }
    
    # Convert newdata to model matrix
    x_new <- stats::model.matrix(use_formula, newdata)[, -1]  # Remove intercept
    
    # Make prediction
    pred <- predict(model, newx = x_new, s = "lambda.min", type = type, ...)
    
    # Return as vector (not matrix)
    if (is.matrix(pred)) {
      return(as.vector(pred))
    } else {
      return(pred)
    }
  } else if (inherits(model, "cv.glmnet")) {
    # Handle case when model_type is not set but model is glmnet
    if (is.null(use_formula)) {
      stop("Formula is required for prediction with glmnet models")
    }
    
    # Convert newdata to model matrix
    x_new <- stats::model.matrix(use_formula, newdata)[, -1]  # Remove intercept
    
    # Make prediction
    pred <- predict(model, newx = x_new, s = "lambda.min", type = type, ...)
    
    # Return as vector (not matrix)
    if (is.matrix(pred)) {
      return(as.vector(pred))
    } else {
      return(pred)
    }
  } else {
    # For standard models, use standard predict
    return(predict(model, newdata = newdata, type = type, ...))
  }
} 