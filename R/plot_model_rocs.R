#' Plot ROC Curves from OptimalModelSearch Results
#'
#' This function takes the results from OptimalModelSearch with AUC criterion
#' and plots the ROC curves. It can plot either the best model's ROC curve or
#' a comparison of multiple models.
#'
#' @section Models Compared:
#' This function can display ROC curves for any or all of the following models compared by OptimalModelSearch:
#' \itemize{
#'   \item Full Logistic Regression: Uses all predictors in the formula
#'   \item Backward Stepwise: Performs backward selection from the full model
#'   \item Forward Stepwise: Starts with intercept-only model and adds predictors
#'   \item GAM (Generalized Additive Model): Fits smooth terms for numeric predictors
#'   \item Lasso Regression: L1 regularization that can shrink coefficients to zero
#'   \item Ridge Regression: L2 regularization that shrinks coefficients
#' }
#'
#' @param results A list returned by OptimalModelSearch with criterion="AUC"
#' @param comparison Logical indicating whether to plot all models for comparison. Default is FALSE.
#' @param save_plot Logical indicating whether to save the plot to a PDF file. Default is FALSE.
#' @param pdf_filename A character string specifying the name of the PDF file if
#'   save_plot is TRUE. Default is "roc_curves.pdf".
#' @param plot_title A character string for the plot title. Default is auto-generated.
#'
#' @return The ROC plot is displayed and the ROC object(s) used are returned invisibly.
#' @export
#' @importFrom graphics plot text abline legend par
#' @importFrom grDevices pdf dev.off
#'
#' @examples
#' \dontrun{
#' # Run OptimalModelSearch with AUC criterion
#' library(mlbench)
#' data(Sonar)
#' dat <- Sonar
#' dat$Class <- ifelse(dat$Class=="R", 0, 1)
#' result <- OptimalModelSearch(formula=Class~., data=dat, criterion="AUC")
#'
#' # Plot the best model's ROC curve
#' plot_model_rocs(result)
#'
#' # Plot comparison of all models' ROC curves
#' plot_model_rocs(result, comparison=TRUE)
#' }
plot_model_rocs <- function(results, comparison = FALSE, save_plot = FALSE, 
                            pdf_filename = "roc_curves.pdf", 
                            plot_title = NULL) {
  
  # Check if results are from OptimalModelSearch with AUC criterion
  if (!is.list(results) || 
      !all(c("criterion", "best_model_name", "performance_metric", "details") %in% names(results)) ||
      results$criterion != "AUC") {
    stop("Input must be results from OptimalModelSearch with criterion='AUC'")
  }
  
  # Get the ROC object of the best model
  best_roc <- results$details
  
  # Start PDF device if save_plot is TRUE
  if (save_plot) {
    grDevices::pdf(pdf_filename)
    on.exit(grDevices::dev.off())
  }
  
  if (comparison) {
    # Get all ROC objects
    all_rocs <- attr(best_roc, "roc_list")
    if (is.null(all_rocs)) {
      warning("No multiple ROC objects found. Plotting only best model.")
      comparison <- FALSE
    } else {
      # Set the title for comparison plot
      if (is.null(plot_title)) {
        plot_title <- "Comparison of ROC Curves"
      }
      
      # Define colors and legend text for each model
      model_names <- names(all_rocs)
      model_colors <- c("blue", "red", "green", "purple", "orange", "brown", "pink", "gray")
      colors_to_use <- model_colors[1:min(length(model_names), length(model_colors))]
      
      # Create legend text with AUC values
      legend_text <- sapply(1:length(model_names), function(i) {
        model_name <- model_names[i]
        model_roc <- all_rocs[[model_name]]
        model_auc <- round(as.numeric(pROC::auc(model_roc)), 3)
        return(paste(model_name, "(AUC =", model_auc, ")"))
      })
      
      # Initialize the plot with the first ROC curve
      plot(all_rocs[[1]], col=colors_to_use[1], main=plot_title)
      
      # Add other ROC curves
      if (length(model_names) > 1) {
        for (i in 2:length(model_names)) {
          plot(all_rocs[[i]], col=colors_to_use[i], add=TRUE)
        }
      }
      
      # Add diagonal reference line
      graphics::abline(a=0, b=1, lty=2, col="gray")
      
      # Add a legend
      graphics::legend("bottomright", legend=legend_text, 
             col=colors_to_use, lwd=2)
      
      # Return all ROCs invisibly
      return(invisible(all_rocs))
    }
  }
  
  # If not comparison or comparison failed, plot only best model's ROC
  if (!comparison) {
    # Set the title for single model plot
    if (is.null(plot_title)) {
      plot_title <- paste("ROC Curve for", results$best_model_name)
    }
    
    # Plot the ROC curve
    plot(best_roc, main=plot_title)
    
    # Add the AUC value to the plot
    graphics::text(0.7, 0.2, 
         paste("AUC =", round(as.numeric(results$performance_metric), 3)),
         cex=0.9)
    
    # Return the best ROC invisibly
    return(invisible(best_roc))
  }
} 