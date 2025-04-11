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
#' @param multi_panel Logical indicating whether to display each model in its own panel when comparison=TRUE. Default is FALSE.
#' @param save_plot Logical indicating whether to save the plot to a PDF file. Default is FALSE.
#' @param pdf_filename A character string specifying the name of the PDF file if
#'   save_plot is TRUE. Default is "roc_curves.pdf".
#' @param plot_title A character string for the plot title. Default is auto-generated.
#'
#' @return The ROC plot is displayed and the ROC object(s) used are returned invisibly.
#' @export
#' @importFrom graphics plot text abline legend par
#' @importFrom grDevices pdf dev.off colorRampPalette
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
#' # Plot comparison of all models' ROC curves on one panel
#' plot_model_rocs(result, comparison=TRUE)
#'
#' # Plot each model's ROC curve in its own panel
#' plot_model_rocs(result, comparison=TRUE, multi_panel=TRUE)
#' }
plot_model_rocs <- function(results, comparison = FALSE, multi_panel = FALSE,
                            save_plot = FALSE, pdf_filename = "roc_curves.pdf", 
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
  
  # Set graphical parameters for better plots
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  
  if (comparison) {
    # Get all ROC objects
    all_rocs <- attr(best_roc, "roc_list")
    if (is.null(all_rocs)) {
      warning("No multiple ROC objects found. Plotting only best model.")
      comparison <- FALSE
    } else {
      # Define colors and legend text for each model
      model_names <- names(all_rocs)
      
      # Use a better color palette with distinct colors
      if (requireNamespace("RColorBrewer", quietly = TRUE)) {
        # Use RColorBrewer if available
        n_models <- length(model_names)
        if (n_models <= 8) {
          model_colors <- RColorBrewer::brewer.pal(max(3, n_models), "Set1")
        } else {
          model_colors <- RColorBrewer::brewer.pal(8, "Set1")
          model_colors <- c(model_colors, RColorBrewer::brewer.pal(n_models - 8, "Set2"))
        }
      } else {
        # Fallback to standard R colors
        model_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF")
        if (length(model_names) > length(model_colors)) {
          color_func <- grDevices::colorRampPalette(model_colors)
          model_colors <- color_func(length(model_names))
        }
      }
      
      # Create legend text with AUC values
      legend_text <- sapply(1:length(model_names), function(i) {
        model_name <- model_names[i]
        model_roc <- all_rocs[[model_name]]
        model_auc <- round(as.numeric(pROC::auc(model_roc)), 3)
        return(paste(model_name, "(AUC =", model_auc, ")"))
      })
      
      if (multi_panel) {
        # Set up multi-panel layout
        n_models <- length(model_names)
        if (n_models <= 3) {
          graphics::par(mfrow = c(1, n_models))
        } else if (n_models <= 6) {
          graphics::par(mfrow = c(2, ceiling(n_models/2)))
        } else {
          graphics::par(mfrow = c(3, ceiling(n_models/3)))
        }
        
        # Set margins for multi-panel plots
        graphics::par(mar = c(4, 4, 3, 1) + 0.1)
        
        # Plot each model in its own panel
        for (i in 1:n_models) {
          model_name <- model_names[i]
          model_roc <- all_rocs[[model_name]]
          model_auc <- round(as.numeric(pROC::auc(model_roc)), 3)
          
          panel_title <- paste("ROC for", model_name)
          
          # Plot the ROC curve
          plot(model_roc, 
               col = model_colors[i], 
               lwd = 2.5,
               main = panel_title,
               xlab = "1 - Specificity (FPR)",
               ylab = "Sensitivity (TPR)",
               legacy.axes = TRUE,
               asp = 1)
          
          # Add grid for readability
          graphics::grid(lty = "dotted", col = "lightgray")
          
          # Add AUC text
          graphics::text(0.7, 0.2,
                        paste("AUC =", model_auc),
                        cex = 1.1,
                        font = 2,
                        col = model_colors[i])
        }
        
        # Add a title to the entire plot
        if (!is.null(plot_title)) {
          graphics::mtext(plot_title, outer = TRUE, line = -1.5, cex = 1.2, font = 2)
        }
      } else {
        # Set the title for comparison plot
        if (is.null(plot_title)) {
          plot_title <- "Comparison of ROC Curves"
        }
        
        # Set margins for single panel plot
        graphics::par(mar = c(5, 4, 4, 2) + 0.1)
        
        # Plot all ROC curves on a single panel
        plot(all_rocs[[1]], 
             col = model_colors[1], 
             lwd = 2,
             main = plot_title,
             xlab = "1 - Specificity (False Positive Rate)",
             ylab = "Sensitivity (True Positive Rate)",
             legacy.axes = TRUE,  # Use standard x-axis direction
             asp = 1)  # Force aspect ratio to be 1
        
        # Add other ROC curves
        if (length(model_names) > 1) {
          for (i in 2:length(model_names)) {
            plot(all_rocs[[i]], 
                 col = model_colors[i], 
                 lwd = 2, 
                 add = TRUE,
                 legacy.axes = TRUE)
          }
        }
        
        # Add grid for readability
        graphics::grid(lty = "dotted", col = "lightgray")
        
        # Add a legend with a semi-transparent background and border
        graphics::legend("bottomright", 
                        legend = legend_text, 
                        col = model_colors[1:length(model_names)], 
                        lwd = 2,
                        bg = "#FFFFFFCC",  # White with alpha transparency
                        box.col = "darkgray",
                        box.lwd = 1,
                        cex = 0.8)
      }
      
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
    
    # Set margins for single model plot
    graphics::par(mar = c(5, 4, 4, 2) + 0.1)
    
    # Plot the ROC curve with improved appearance
    plot(best_roc, 
         main = plot_title,
         col = "#377EB8",  # Use a nice blue color
         lwd = 2.5,        # Thicker line for visibility
         xlab = "1 - Specificity (False Positive Rate)",
         ylab = "Sensitivity (True Positive Rate)",
         legacy.axes = TRUE,  # Use standard x-axis direction
         grid = TRUE,       # Add grid lines
         asp = 1)           # Force aspect ratio to be 1
    
    # Add the AUC value to the plot
    auc_value <- round(as.numeric(results$performance_metric), 3)
    graphics::text(0.7, 0.2,
                  paste("AUC =", auc_value),
                  cex = 1.1,
                  font = 2,
                  col = "#377EB8")
    
    # Return the best ROC invisibly
    return(invisible(best_roc))
  }
} 