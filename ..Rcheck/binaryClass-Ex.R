pkgname <- "binaryClass"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('binaryClass')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("OptimalModelSearch")
### * OptimalModelSearch

flush(stderr()); flush(stdout())

### Name: OptimalModelSearch
### Title: A Binary Classification Model Selection
### Aliases: OptimalModelSearch

### ** Examples


##==Example 1====##
## Using the Accuracy criterion
## Loading the PIMA Diabetes data
require(mlbench)
data.t <- PimaIndiansDiabetes2
data.t$diabetes <-ifelse(data.t$diabetes=="neg",0,1)
OptimalModelSearch(formula=diabetes~., data=data.t,
criterion="Accuracy", training_percent=0.8, threshold=0.54)


##==Example 2====##
## Using the AUC criterion
library(mlbench)
dat<- Sonar
dat$Class <- ifelse(dat$Class=="R",0,1)
OptimalModelSearch(formula=Class~., data=dat,
criterion="AUC", training_percent=0.8)


##==Example 3====##
## Using AIC criterion
## Loading the PIMA Diabetes data
require(mlbench)
data.t <- PimaIndiansDiabetes2
data.t$diabetes <-ifelse(data.t$diabetes=="neg",0,1)
OptimalModelSearch(formula=diabetes~., data=data.t,
criterion="AIC", training_percent=0.8)




cleanEx()
nameEx("dot-draw_confusion_matrix")
### * dot-draw_confusion_matrix

flush(stderr()); flush(stdout())

### Name: .draw_confusion_matrix
### Title: Find the Optimal Binary Classification Model
### Aliases: .draw_confusion_matrix
### Keywords: internal

### ** Examples

## Not run: 
##D # Load required packages for example
##D library(mlbench)
##D data(PimaIndiansDiabetes)
##D 
##D # Ensure diabetes is a factor
##D PimaIndiansDiabetes$diabetes <- as.factor(PimaIndiansDiabetes$diabetes)
##D 
##D # Find best model based on AUC using all predictors
##D set.seed(42) # for reproducibility of train/test split
##D result_auc <- OptimalModelSearch(diabetes ~ ., data = PimaIndiansDiabetes, criterion = "AUC")
##D print(result_auc$best_model_name)
##D print(result_auc$performance_metric)
##D plot(result_auc$details) # Plot ROC curve
##D graphics::text(0.7, 0.2, paste("AUC =", round(pROC::auc(result_auc$details), 3)), cex = 0.9)
##D 
##D # Find best model based on Accuracy with a 0.6 threshold
##D set.seed(42)
##D result_acc <- OptimalModelSearch(diabetes ~ glucose + mass + age, data = PimaIndiansDiabetes,
##D                                  criterion = "Accuracy", threshold = 0.6)
##D print(result_acc$best_model_name)
##D print(result_acc$performance_metric)
##D print(result_acc$details) # Print confusion matrix details
##D 
##D # Find best model based on AIC
##D set.seed(42)
##D result_aic <- OptimalModelSearch(diabetes ~ pregnant + glucose + pressure + mass + pedigree + age,
##D                                  data = PimaIndiansDiabetes, criterion = "AIC")
##D print(result_aic$best_model_name)
##D print(result_aic$performance_metric)
##D print(result_aic$coefficients)
## End(Not run)



cleanEx()
nameEx("dot-plot_barplot_ind")
### * dot-plot_barplot_ind

flush(stderr()); flush(stdout())

### Name: .plot_barplot_ind
### Title: Generate Descriptive Plots for Variables in a Data Frame
### Aliases: .plot_barplot_ind
### Keywords: internal

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



cleanEx()
nameEx("plot.descrip")
### * plot.descrip

flush(stderr()); flush(stdout())

### Name: plot.descrip
### Title: Descriptive Plots
### Aliases: plot.descrip

### ** Examples


## Example 1
head(iris)
data1=iris
## Generate one plot for each column of data1

plot.descrip(data1,type="ind", ppv=1 )



## Example 2
#Generate one plot for each factor/character variables
head(airquality)
data3=airquality

plot.descrip(data3,type="ind", ppv=2 )

## Example 3
## plot the pairwise relationship between mpg

head(mtcars)
data4=mtcars
plot.descrip(data4,type="pair")




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
