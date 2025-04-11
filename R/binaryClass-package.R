#' Binary Classification Model Selection and Evaluation
#'
#' The binaryClass package provides a comprehensive toolkit for binary 
#' classification tasks in R. It implements powerful functions for model 
#' training, evaluation, and prediction, with a focus on comparing different 
#' classification approaches.
#'
#' @section Key Functions:
#' \itemize{
#'   \item \code{\link{OptimalModelSearch}}: Automatically compares multiple binary 
#'         classification models and selects the best model based on AUC, 
#'         Accuracy, or AIC
#'   \item \code{\link{extract_best_model}}: Extracts the best model from 
#'         OptimalModelSearch results for further use
#'   \item \code{\link{compare_model_rocs}}: Compare ROC curves for different models
#'   \item \code{\link{plot_model_rocs}}: Plot ROC curves from model results
#'   \item \code{\link{plot_model_cm}}: Plot confusion matrix from model results
#'   \item \code{\link{plot_descrip}}: Create descriptive visualizations for binary
#'         classification datasets
#' }
#'
#' @docType package
#' @name binaryClass
#' @aliases binaryClass-package
#' @keywords package classification modeling
#' @concept binary classification
#' @concept model selection
#' @concept ROC curve
#' @concept AUC
#' @concept predictive modeling
#' @concept statistical learning
#' @importFrom stats terms predict na.omit model.matrix step
NULL 