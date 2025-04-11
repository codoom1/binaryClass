#' Generate Descriptive Plots for Variables in a Data Frame
#'
#' This function creates various plots to visualize individual variables or pairwise
#' relationships between a response variable (assumed to be the first column)
#' and other explanatory variables in a data frame.
#'
#' @param data A data frame containing the variables to be plotted. The response
#'   variable should be in the first column if `type = "pair"`.
#' @param type A character string specifying the type of plot. Must be one of
#'   "ind" or "pair".
#' @param ppv An integer (1 or 2) specifying the number of plots per variable
#'   when `type = "ind"`. This argument is ignored if `type = "pair"`.
#'
#' @return Invisible NULL. Plots are generated on the current graphics device.
#' @export
#' @importFrom graphics barplot boxplot hist mosaicplot text layout par plot title rect
#' @importFrom stats scatter.smooth na.omit
#'
#' @examples
#' \dontrun{
#' # --- Examples for plot_descrip ---
#'
#' # Basic usage with iris dataset
#' data(iris)
#'
#' # Individual plots (one per variable)
#' plot_descrip(iris, type = "ind", ppv = 1)
#'
#' # Individual plots (two per numeric variable, one for categorical)
#' plot_descrip(iris, type = "ind", ppv = 2)
#'
#' # Pairwise plots (assuming Sepal.Length is the response)
#' plot_descrip(iris, type = "pair")
#'
#' # --- Pairwise plots with a factor response ---
#' data(mtcars)
#' # Make copies to modify
#' mtcars_mod <- mtcars
#' # Treat 'cyl' as a factor response
#' mtcars_mod$cyl <- as.factor(mtcars_mod$cyl)
#' # Plot relationships between 'cyl' and other variables
#' plot_descrip(mtcars_mod[, c("cyl", "mpg", "wt", "gear")], type = "pair")
#'
#' # --- Handling character variables ---
#' # Create some character data
#' char_data <- data.frame(
#'   response = rnorm(50),
#'   category = sample(c("A", "B", "C"), 50, replace = TRUE),
#'   group = sample(c("X", "Y"), 50, replace = TRUE)
#' )
#' # Individual plots (should create barplots for character columns)
#' plot_descrip(char_data, type = "ind", ppv = 1)
#'
#' # Pairwise plots with character predictor
#' plot_descrip(char_data, type = "pair")
#'
#' # --- Edge case: Single column ---
#' plot_descrip(iris[, "Sepal.Length", drop = FALSE], type = "ind", ppv = 1)
#'
#' # --- Handling too many categories in 'pair' type ---
#' # Create data with a categorical variable having many levels
#' iris_many_levels <- iris
#' # Convert Sepal.Width to character and create many unique values artificially
#' iris_many_levels$ManyCats <- as.character(round(iris_many_levels$Sepal.Width * 100))
#' # Check number of levels (should be > 15)
#' print(paste("Number of unique values for ManyCats:", length(unique(iris_many_levels$ManyCats))))
#' # Plot pairwise with Sepal.Length as response
#' # Should print a message for 'ManyCats' and skip its plot
#' plot_descrip(iris_many_levels[, c("Sepal.Length", "Petal.Length", "ManyCats")], type = "pair")
#' }
plot_descrip <- function(data, type, ppv){

  # --- Input Validation ---
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  if (ncol(data) < 1) {
    stop("'data' must have at least one column.")
  }
  if (!type %in% c("ind", "pair")) {
    stop("'type' must be either 'ind' or 'pair'.")
  }
  if (type == "ind") {
    if (missing(ppv)){
       stop("Argument 'ppv' must be specified when type = \"ind\"")
    }
    if (!ppv %in% c(1, 2)) {
       stop("When type is 'ind', 'ppv' must be 1 or 2.")
    }
  }
  if (type == "pair" && ncol(data) < 2) {
    stop("When type is 'pair', 'data' must have at least two columns (response + explanatory).")
  }
  # --- End Input Validation ---


  if(type=="ind"){
    for (i in 1:ncol(data)){
      col_data <- data[, i]
      col_name <- colnames(data)[i]

      if (ppv==1){
        ### Create a plot for each variable to be analysed.
        if (is.factor(col_data) || is.character(col_data)){
          # Use helper for barplot
          .plot_barplot_ind(col_data, col_name, i)
        } else if (is.numeric(col_data)) {
          ## For numeric variables, create a boxplot for them.
          graphics::boxplot(col_data, col = i + 2, las = 2,
                  ylab = col_name,
                  main = paste0("Boxplot of \n", col_name))
        } else {
          warning(paste("Column", col_name, "has an unsupported class:", class(col_data)[1]))
        }

      } else if(ppv==2){
        if (is.factor(col_data) || is.character(col_data)){
           # Use helper for barplot
          .plot_barplot_ind(col_data, col_name, i)
          # The original code called midpoints but didn't seem to use it here
          # If a second plot was intended for ppv=2 categorical, it wasn't implemented.

        } else if (is.numeric(col_data)) {
          ## For numeric variables, create a boxplot and a histogram.
          graphics::boxplot(col_data, col = i + 2,
                  ylab = col_name,
                  main = paste0("Boxplot of \n", col_name))

          graphics::hist(col_data, col = i + 2,
               xlab = col_name,
               ylab = "Frequency",
               main = paste0("Histogram of \n", col_name))
        } else {
           warning(paste("Column", col_name, "has an unsupported class:", class(col_data)[1]))
        }
      }
    }
  }else if(type=="pair"){

    # Handle NAs for pairwise plots - na.omit for mosaic, others handle internally
    response_var <- data[,1]
    response_name <- colnames(data)[1]

    for (i in 2:ncol(data)){
      expl_var <- data[, i]
      expl_name <- colnames(data)[i]

      # Numeric/Integer Response vs Numeric/Integer Explanatory -> Scatterplot
      if(is.numeric(response_var) && is.numeric(expl_var)){
        stats::scatter.smooth(response_var ~ expl_var, col=i+3,
                       xlab=expl_name,
                       ylab=response_name,
                       main=paste0(" Scatterplot of  \n ", response_name," vs ", expl_name))

      # Numeric/Integer Response vs Factor/Character Explanatory -> Boxplot
      } else if(is.numeric(response_var) && (is.factor(expl_var) || is.character(expl_var))){
        if(length(unique(stats::na.omit(expl_var)))>=15){
          cat("\nToo many categories for", expl_name, "to plot vs", response_name)
        } else {
          graphics::boxplot(response_var ~ as.factor(expl_var), col=i+1, # Ensure factor for boxplot
                  ylab=response_name,
                  xlab=expl_name,
                  main=paste0(" Boxplot of  \n ", response_name," vs ", expl_name))
        }

      # Factor/Character Response vs Numeric/Integer Explanatory -> Boxplot
      } else if((is.factor(response_var) || is.character(response_var)) && is.numeric(expl_var)){
        graphics::boxplot(expl_var ~ as.factor(response_var), col = i+2, # Ensure factor for boxplot
                ylab = expl_name,
                xlab = response_name,
                main = paste0("Boxplot of\n", response_name, " vs ", expl_name))

      # Factor/Character Response vs Factor/Character Explanatory -> Mosaic Plot
      } else if((is.factor(response_var) || is.character(response_var)) && (is.factor(expl_var) || is.character(expl_var))){
        temp_expl_var <- stats::na.omit(expl_var)
        if(length(unique(temp_expl_var))>=15){
          cat("\nToo many categories for", expl_name, "to plot vs", response_name)
        } else {
          # Create temporary data frame with complete cases for this pair
          pair_data <- stats::na.omit(data.frame(resp = response_var, expl = expl_var))
          if (nrow(pair_data) > 0){
            # Ensure both are factors for mosaicplot
            pair_data$resp <- as.factor(pair_data$resp)
            pair_data$expl <- as.factor(pair_data$expl)

            col1=sample(1:5,1)
            col2=sample(6:11,1)
            graphics::mosaicplot(pair_data$expl ~ pair_data$resp, color=c(col1,col2), las=2, cex.axis=0.7,
                       xlab=expl_name,
                       ylab=response_name,
                       main=paste0(" Mosaicplot of  \n ", response_name," vs ", expl_name),
                       shade=FALSE)
          } else {
            cat("\nNo complete cases for plotting", response_name, "vs", expl_name)
          }
        }
      } else {
        warning(paste("Unsupported combination of types for pairwise plot:", class(response_var)[1], "vs", class(expl_var)[1]))
      }
    }
  }
  invisible(NULL) # Return NULL invisibly
} 

#' Internal helper function for plotting barplots
#'
#' @param x The vector of data for the variable.
#' @param var_name The name of the variable (for plot title/labels).
#' @param col_idx The column index (used for default color selection).
#' @param max_levels The maximum number of unique levels before percentages are omitted (default 13).
#' @keywords internal
#' @export
.plot_barplot_ind <- function(x, var_name, col_idx, max_levels = 13) {
  ## Helper function for plotting barplots in 'ind' mode

  ## Create a table for the categorical variables.
  data_table <- table(x)

  # Calculate the percentages
  percentages <- round((data_table / sum(data_table)) * 100, 1)

  # Create the barplot and capture the midpoints of the bars
  plot_title <- paste0("Barplot of \n", var_name)
  plot_ylim <- c(0 ,max(data_table)+.25*max(data_table))

  if (length(unique(x)) > max_levels) {
    # Plot without percentages if too many levels
    midpoints <- graphics::barplot(data_table, col = col_idx + 1, las = 2,
                         main = plot_title,
                         ylim = plot_ylim)
  } else {
    # Plot with percentages
    midpoints <- graphics::barplot(data_table, col = col_idx + 1, las = 2,
                         xlab = var_name,
                         main = plot_title,
                         ylim = plot_ylim)
    graphics::text(x = midpoints, y = data_table, labels = paste0(percentages, "%"), pos = 3)
  }
  invisible(midpoints) # Return midpoints invisibly, consistent with original
} 