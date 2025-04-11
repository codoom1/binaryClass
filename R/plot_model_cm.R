#' Plot Confusion Matrix from OptimalModelSearch Results
#'
#' This function takes the results from OptimalModelSearch with Accuracy criterion
#' and plots the confusion matrix. It provides a visual representation of the model's
#' classification performance.
#'
#' @section When to Use:
#' Use this function when you want to visualize the confusion matrix from an OptimalModelSearch
#' result that used the "Accuracy" criterion. The confusion matrix shows true positives, 
#' false positives, true negatives, and false negatives, along with performance metrics like 
#' sensitivity, specificity, and PPV.
#'
#' @param results A list returned by OptimalModelSearch with criterion="Accuracy"
#' @param save_plot Logical indicating whether to save the plot to a PDF file. Default is FALSE.
#' @param pdf_filename A character string specifying the name of the PDF file if
#'   save_plot is TRUE. Default is "confusion_matrix.pdf".
#'
#' @return The confusion matrix object from the best model is returned invisibly
#' @export
#' @importFrom graphics layout plot rect text title par
#' @importFrom grDevices pdf dev.off
#'
#' @examples
#' \dontrun{
#' # Run OptimalModelSearch with Accuracy criterion
#' library(mlbench)
#' data(PimaIndiansDiabetes)
#' df <- PimaIndiansDiabetes
#' df$diabetes <- ifelse(df$diabetes=="neg", 0, 1)
#' result <- OptimalModelSearch(formula=diabetes~., data=df, criterion="Accuracy")
#'
#' # Plot the confusion matrix
#' plot_model_cm(result)
#' }
plot_model_cm <- function(results, save_plot = FALSE, pdf_filename = "confusion_matrix.pdf") {
  
  # Print debug information
  cat("Debugging plot_model_cm:\n")
  cat("results$criterion:", results$criterion, "\n")
  cat("results$best_model_name:", results$best_model_name, "\n")
  
  # Check if results are from OptimalModelSearch with Accuracy criterion
  if (!is.list(results) || 
      !all(c("criterion", "best_model_name", "performance_metric", "details") %in% names(results))) {
    stop("Input must be results from OptimalModelSearch")
  }
  
  # Check if details is a confusion matrix
  cm <- results$details
  if (!is.list(cm) || !all(c("table", "byClass", "overall") %in% names(cm))) {
    stop("The details object does not appear to be a valid confusion matrix")
  }
  
  # Start PDF device if save_plot is TRUE
  if (save_plot) {
    grDevices::pdf(pdf_filename)
    on.exit(grDevices::dev.off())
  }
  
  # Draw the confusion matrix visualization
  graphics::layout(matrix(c(1,1,2)))
  graphics::par(mar=c(2,2,2,2))
  graphics::plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  graphics::title(paste('CONFUSION MATRIX -', results$best_model_name), cex.main=1.5)

  # Create the matrix
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

  # Add in the cm results
  res <- as.numeric(cm$table)
  graphics::text(195, 400, res[1], cex=1.6, font=2, col='white')
  graphics::text(195, 335, res[2], cex=1.6, font=2, col='white')
  graphics::text(295, 400, res[3], cex=1.6, font=2, col='white')
  graphics::text(295, 335, res[4], cex=1.6, font=2, col='white')

  # Add in the specifics
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

  # Add in the accuracy information
  graphics::text(10, 35, names(cm$byClass[9]), cex=1.2, font=2)
  graphics::text(10, 17, round(as.numeric(cm$byClass[9]), 3), cex=1.2)

  graphics::text(30, 35, names(cm$overall[1]), cex=1.2, font=2)
  graphics::text(30, 17, round(as.numeric(cm$overall[1]), 3), cex=1.2)

  graphics::text(70, 35, names(cm$overall[2]), cex=1.2, font=2)
  graphics::text(70, 17, round(as.numeric(cm$overall[2]), 3), cex=1.2)

  graphics::text(90, 35, names(cm$byClass[11]), cex=1.2, font=2)
  graphics::text(90, 17, round(as.numeric(cm$byClass[11]), 3), cex=1.2)
  
  # Return the confusion matrix invisibly
  return(invisible(cm))
} 