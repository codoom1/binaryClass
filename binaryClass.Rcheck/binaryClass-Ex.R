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


##==Example 1====##
## Using the Accuracy criterion
## Loading the PIMA Diabetes data
require(mlbench)
data(PimaIndiansDiabetes)
data.t <- PimaIndiansDiabetes
data.t$diabetes <-ifelse(data.t$diabetes=="neg",0,1)
OptimalModelSearch(formula=diabetes~., data=data.t,
criterion="Accuracy", training_percent=0.8, threshold=0.54)


##==Example 2====##
## Using the AUC criterion
library(mlbench)
data(Sonar)
dat<- Sonar
dat$Class <- ifelse(dat$Class=="R",0,1)
OptimalModelSearch(formula=Class~., data=dat,
criterion="AUC", training_percent=0.8)


##==Example 3====##
## Using AIC criterion
## Loading the PIMA Diabetes data
require(mlbench)
data(PimaIndiansDiabetes)
data.t <- PimaIndiansDiabetes
data.t$diabetes <-ifelse(data.t$diabetes=="neg",0,1)
OptimalModelSearch(formula=diabetes~., data=data.t,
criterion="AIC", training_percent=0.8)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("OptimalModelSearch", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
