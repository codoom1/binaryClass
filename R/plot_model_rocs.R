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
  
  # Extract the base model name (removing any criterion suffixes)
  # This handles cases like "full.glm.Accuracy"
  best_model_name <- results$best_model_name
  model_parts <- strsplit(best_model_name, "\\.")[[1]]
  if (length(model_parts) > 1) {
    # If the last part is the same as the criterion, remove it
    if (tolower(model_parts[length(model_parts)]) == tolower(results$criterion)) {
      best_model_name <- paste(model_parts[-length(model_parts)], collapse=".")
    }
  }
  
  # Extract list of ROC objects if available
  all_rocs <- attr(best_roc, "roc_list")

  # For multi-panel comparison
  if (comparison && multi_panel && !is.null(all_rocs)) {
    # Set up multi-panel layout for better visualization
    n_models <- length(all_rocs)
    n_cols <- min(3, n_models)  # Maximum 3 columns
    n_rows <- ceiling(n_models / n_cols)
    
    # Get model names from the ROC list
    model_names <- names(all_rocs)
    
    # Define color palette (replace yellow with a darker gold color)
    model_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFD700")
    colors_to_use <- model_colors[1:min(length(model_names), length(model_colors))]
    
    # Set up the layout
    graphics::par(mfrow = c(n_rows, n_cols))
    graphics::par(mar = c(4, 4, 3, 1) + 0.1)  # Adjust margins for panels
    
    # Plot each ROC curve in its own panel
    for (i in 1:length(model_names)) {
      # Clean up model name for display
      display_name <- model_names[i]
      display_parts <- strsplit(display_name, "\\.")[[1]]
      if (length(display_parts) > 1) {
        # If the last part is the same as the criterion, remove it
        if (tolower(display_parts[length(display_parts)]) == tolower(results$criterion)) {
          display_name <- paste(display_parts[-length(display_parts)], collapse=".")
        }
      }
      
      # Determine color for this model
      model_color <- switch(display_name,
                           "full.glm" = "#E41A1C",       # Red
                           "backward.stepwise" = "#377EB8", # Blue
                           "forward.stepwise" = "#4DAF4A",  # Green
                           "lasso" = "#984EA3",           # Purple
                           "ridge" = "#FF7F00",           # Orange
                           "gam" = "#FFD700",             # Gold (instead of yellow)
                           colors_to_use[i %% length(colors_to_use) + 1])
      
      # Plot individual ROC
      plot(all_rocs[[i]], 
           main = paste("ROC for", display_name),
           col = model_color,
           lwd = 2,
           xlab = "1 - Specificity (FPR)",
           ylab = "Sensitivity (TPR)",
           legacy.axes = TRUE,
           grid = TRUE)
      
      # Add AUC value in the matching color
      auc_value <- round(as.numeric(pROC::auc(all_rocs[[i]])), 3)
      graphics::text(0.7, 0.2,
                     paste("AUC =", auc_value),
                     cex = 0.9,
                     font = 2,
                     col = model_color)
      
      # Add reference line
      graphics::abline(0, 1, lty = 2, col = "gray")
    }
    
    # Return the ROC objects invisibly
    return(invisible(all_rocs))
  }
  
  # For single-panel comparison
  if (comparison && !multi_panel && !is.null(all_rocs)) {
    # Prepare model names and colors
    model_names <- names(all_rocs)
    # Use a color-blind friendly palette
    model_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFD700")
    colors_to_use <- model_colors[1:min(length(model_names), length(model_colors))]
    
    # Set up the plot
    graphics::par(mar = c(5, 4, 4, 2) + 0.1)
    
    # Clean up model names for display
    display_names <- sapply(model_names, function(name) {
      name_parts <- strsplit(name, "\\.")[[1]]
      if (length(name_parts) > 1) {
        # If the last part is the same as the criterion, remove it
        if (tolower(name_parts[length(name_parts)]) == tolower(results$criterion)) {
          return(paste(name_parts[-length(name_parts)], collapse="."))
        }
      }
      return(name)
    })
    
    # Map models to consistent colors
    model_colors_map <- list()
    for (i in 1:length(display_names)) {
      display_name <- display_names[i]
      model_colors_map[[i]] <- switch(display_name,
                           "full.glm" = "#E41A1C",       # Red
                           "backward.stepwise" = "#377EB8", # Blue
                           "forward.stepwise" = "#4DAF4A",  # Green
                           "lasso" = "#984EA3",           # Purple
                           "ridge" = "#FF7F00",           # Orange
                           "gam" = "#FFD700",             # Gold (instead of yellow)
                           colors_to_use[i])
    }
    
    # Plot the first ROC curve
    plot(all_rocs[[1]], 
         main = "Comparison of ROC Curves",
         col = model_colors_map[[1]],
         lwd = 2.5,
         xlab = "1 - Specificity (False Positive Rate)",
         ylab = "Sensitivity (True Positive Rate)",
         legacy.axes = TRUE,
         grid = TRUE)
    
    # Add other ROC curves
    if (length(model_names) > 1) {
      for (i in 2:length(model_names)) {
        plot(all_rocs[[i]], 
             col = model_colors_map[[i]], 
             lwd = 2.5,
             add = TRUE)
      }
    }
    
    # Add the reference diagonal line
    graphics::abline(0, 1, lty = 2, col = "gray")
    
    # Add legend with cleaned model names
    graphics::legend("bottomright", 
                    legend = display_names,
                    col = unlist(model_colors_map),
                    lwd = 2.5,
                    cex = 0.8,
                    bg = "white")
    
    # Return the ROC objects invisibly
    return(invisible(all_rocs))
  }
  
  # For single model plot
  if (!comparison) {
    # Set the title for single model plot
    if (is.null(plot_title)) {
      plot_title <- paste("ROC Curve for", best_model_name)
    }
    
    # Set margins for single model plot
    graphics::par(mar = c(5, 4, 4, 2) + 0.1)
    
    # Determine color based on model name
    model_color <- switch(best_model_name,
                         "full.glm" = "#E41A1C",       # Red
                         "backward.stepwise" = "#377EB8", # Blue
                         "forward.stepwise" = "#4DAF4A",  # Green
                         "lasso" = "#984EA3",           # Purple
                         "ridge" = "#FF7F00",           # Orange
                         "gam" = "#FFD700",             # Gold (instead of yellow)
                         "#377EB8")  # Default blue if none match
    
    # Plot the ROC curve with improved appearance
    plot(best_roc, 
         main = plot_title,
         col = model_color,  # Use matched color
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
                  col = model_color)
    
    # Return the best ROC invisibly
    return(invisible(best_roc))
  }
} 