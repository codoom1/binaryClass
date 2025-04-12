pkgname <- "binaryClass"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "binaryClass-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('binaryClass')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("OptimalModelSearch")
### * OptimalModelSearch

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: OptimalModelSearch
### Title: A Binary Classification Model Selection
### Aliases: OptimalModelSearch

### ** Examples


##==Example 1: Accuracy criterion====##
## No test: 
data(PimaIndiansDiabetes, package="mlbench")
data.t <- PimaIndiansDiabetes
data.t$diabetes <- ifelse(data.t$diabetes=="neg", 0, 1)
result <- OptimalModelSearch(formula=diabetes~., data=data.t,
                            criterion="Accuracy", training_percent=0.8, 
                            threshold=0.54, suppress_warnings=TRUE)
# Display simplified results
result$criterion
result$best_model_name
result$performance_metric
## End(No test)

##==Example 2: AUC criterion====##
## No test: 
data(Sonar, package="mlbench")
dat <- Sonar
dat$Class <- ifelse(dat$Class=="R", 0, 1)
result <- OptimalModelSearch(formula=Class~., data=dat,
                           criterion="AUC", training_percent=0.8,
                           suppress_warnings=TRUE)
# Display simplified results
result$criterion
result$best_model_name
result$performance_metric
## End(No test)

##==Example 3: AIC criterion====##
## No test: 
data(PimaIndiansDiabetes, package="mlbench")
data.t <- PimaIndiansDiabetes
data.t$diabetes <- ifelse(data.t$diabetes=="neg", 0, 1)
result <- OptimalModelSearch(formula=diabetes~., data=data.t,
                            criterion="AIC", training_percent=0.8,
                            suppress_warnings=TRUE)
# Display simplified results
result$criterion
result$best_model_name
result$performance_metric
## End(No test)

##==Example 4: ROC Visualization with Multi-Panel Plot====##
## No test: 
data(Sonar, package="mlbench")
dat <- Sonar
dat$Class <- ifelse(dat$Class=="R", 0, 1)
# Plot all ROC curves in separate panels - clearer visualization
result <- OptimalModelSearch(formula=Class~., data=dat,
                          criterion="AUC", training_percent=0.8,
                          suppress_warnings=TRUE, plot_roc=TRUE, 
                          plot_comparison=TRUE, multi_panel=TRUE)
# No need to print anything as this example focuses on visualization
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("OptimalModelSearch", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("compare_model_rocs")
### * compare_model_rocs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compare_model_rocs
### Title: Compare ROC Curves for Different Binary Classification Models
### Aliases: compare_model_rocs roc_comparison roc_curves compare_rocs
###   plot_roc_curves
### Keywords: hplot models

### ** Examples

## Not run: 
##D # Load example data
##D library(mlbench)
##D data(Sonar)
##D dat <- Sonar
##D dat$Class <- ifelse(dat$Class=="R", 0, 1)
##D 
##D # Generate multi-panel ROC plot comparing all models
##D result <- compare_model_rocs(Class ~ ., data = dat)
##D 
##D # See the AUC values
##D result$auc_values
##D 
##D # Check which model performed best
##D result$best_model
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compare_model_rocs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("extract_best_model")
### * extract_best_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: extract_best_model
### Title: Extract the best model from the results of OptimalModelSearch
### Aliases: extract_best_model get_best_model model_extraction
### Keywords: models regression

### ** Examples

## Not run: 
##D # Find the best model
##D library(mlbench)
##D data(Sonar)
##D dat <- Sonar
##D dat$Class <- ifelse(dat$Class=="R", 0, 1)
##D 
##D # Run OptimalModelSearch
##D result <- OptimalModelSearch(formula=Class~., data=dat,
##D                            criterion="AUC", suppress_warnings=TRUE)
##D                            
##D # Extract the best model
##D best_model <- extract_best_model(result, dat)
##D 
##D # Use the predict_model function for prediction (works for all model types)
##D predictions <- predict_model(best_model, newdata=dat[1:5,], 
##D                             formula=Class~., type="response")
##D print(predictions)
##D 
##D # Examine model coefficients (if applicable to the model type)
##D if(inherits(best_model, "glm") || inherits(best_model, "gam")) {
##D   print(summary(best_model))
##D }
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("extract_best_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_descrip")
### * plot_descrip

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_descrip
### Title: Generate Descriptive Plots for Variables in a Data Frame
### Aliases: plot_descrip

### ** Examples

## Not run: 
##D # --- Examples for plot_descrip ---
##D 
##D # Basic usage with iris dataset
##D data(iris)
##D 
##D # Individual plots (one per variable)
##D plot_descrip(iris, type = "ind", ppv = 1)
##D 
##D # Individual plots (two per numeric variable, one for categorical)
##D plot_descrip(iris, type = "ind", ppv = 2)
##D 
##D # Pairwise plots (assuming Sepal.Length is the response)
##D plot_descrip(iris, type = "pair")
##D 
##D # --- Pairwise plots with a factor response ---
##D data(mtcars)
##D # Make copies to modify
##D mtcars_mod <- mtcars
##D # Treat 'cyl' as a factor response
##D mtcars_mod$cyl <- as.factor(mtcars_mod$cyl)
##D # Plot relationships between 'cyl' and other variables
##D plot_descrip(mtcars_mod[, c("cyl", "mpg", "wt", "gear")], type = "pair")
##D 
##D # --- Handling character variables ---
##D # Create some character data
##D char_data <- data.frame(
##D   response = rnorm(50),
##D   category = sample(c("A", "B", "C"), 50, replace = TRUE),
##D   group = sample(c("X", "Y"), 50, replace = TRUE)
##D )
##D # Individual plots (should create barplots for character columns)
##D plot_descrip(char_data, type = "ind", ppv = 1)
##D 
##D # Pairwise plots with character predictor
##D plot_descrip(char_data, type = "pair")
##D 
##D # --- Edge case: Single column ---
##D plot_descrip(iris[, "Sepal.Length", drop = FALSE], type = "ind", ppv = 1)
##D 
##D # --- Handling too many categories in 'pair' type ---
##D # Create data with a categorical variable having many levels
##D iris_many_levels <- iris
##D # Convert Sepal.Width to character and create many unique values artificially
##D iris_many_levels$ManyCats <- as.character(round(iris_many_levels$Sepal.Width * 100))
##D # Check number of levels (should be > 15)
##D print(paste("Number of unique values for ManyCats:", length(unique(iris_many_levels$ManyCats))))
##D # Plot pairwise with Sepal.Length as response
##D # Should print a message for 'ManyCats' and skip its plot
##D plot_descrip(iris_many_levels[, c("Sepal.Length", "Petal.Length", "ManyCats")], type = "pair")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_descrip", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_model_cm")
### * plot_model_cm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_model_cm
### Title: Plot Confusion Matrix from OptimalModelSearch Results
### Aliases: plot_model_cm

### ** Examples

## Not run: 
##D # Run OptimalModelSearch with Accuracy criterion
##D library(mlbench)
##D data(PimaIndiansDiabetes)
##D df <- PimaIndiansDiabetes
##D df$diabetes <- ifelse(df$diabetes=="neg", 0, 1)
##D result <- OptimalModelSearch(formula=diabetes~., data=df, criterion="Accuracy")
##D 
##D # Plot the confusion matrix
##D plot_model_cm(result)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_model_cm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_model_rocs")
### * plot_model_rocs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_model_rocs
### Title: Plot ROC Curves from OptimalModelSearch Results
### Aliases: plot_model_rocs

### ** Examples

## Not run: 
##D # Run OptimalModelSearch with AUC criterion
##D library(mlbench)
##D data(Sonar)
##D dat <- Sonar
##D dat$Class <- ifelse(dat$Class=="R", 0, 1)
##D result <- OptimalModelSearch(formula=Class~., data=dat, criterion="AUC")
##D 
##D # Plot the best model's ROC curve
##D plot_model_rocs(result)
##D 
##D # Plot comparison of all models' ROC curves on one panel
##D plot_model_rocs(result, comparison=TRUE)
##D 
##D # Plot each model's ROC curve in its own panel
##D plot_model_rocs(result, comparison=TRUE, multi_panel=TRUE)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_model_rocs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("predict_model")
### * predict_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: predict_model
### Title: Make predictions from a model extracted by extract_best_model
### Aliases: predict_model

### ** Examples

## Not run: 
##D # Extract the best model from OptimalModelSearch results
##D best_model <- extract_best_model(result, data)
##D 
##D # Make predictions on new data
##D predictions <- predict_model(best_model, newdata = new_data, 
##D                             formula = y ~ x1 + x2)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("predict_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
