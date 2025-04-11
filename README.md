# binaryClass: R Package for Binary Classification

## Overview
The `binaryClass` package provides a comprehensive toolkit for binary classification tasks in R. It implements powerful functions for model training, evaluation, and prediction, with a focus on comparing different classification approaches.

## Key Features
- **OptimalModelSearch**: Automatically compares multiple binary classification models and selects the best model based on AUC, Accuracy, or AIC. Models compared include:
  - Full Logistic Regression
  - Backward Stepwise Selection
  - Forward Stepwise Selection 
  - GAM (Generalized Additive Model)
  - Lasso Regression
  - Ridge Regression

- **Visualization Options**:
  - **ROC Curves**: For AUC criterion - visualize model discrimination ability
    - Automatic plotting with `OptimalModelSearch(..., criterion="AUC", plot_roc=TRUE)`
    - Comparison plotting with `plot_model_rocs()`
  - **Confusion Matrices**: For Accuracy criterion - visualize classification performance
    - Automatic plotting with `OptimalModelSearch(..., criterion="Accuracy", plot_cm=TRUE)`
    - Manual plotting with `plot_model_cm()`
  
- **Additional Tools**:
  - `compare_model_rocs`: Standalone function to compare ROC curves for stepwise, lasso, and ridge models
  - `plot_descrip`: Create descriptive visualizations for binary classification datasets

## Installation
```r
# Install from GitHub (when available)
# devtools::install_github("username/binaryClass")

# Or install locally
# install.packages("path/to/binaryClass_1.0.0.tar.gz", repos = NULL)
```

## Usage Examples

### Model Selection
```r
# Load example data
library(binaryClass)
library(mlbench)
data(PimaIndiansDiabetes)
df <- PimaIndiansDiabetes
df$diabetes <- ifelse(df$diabetes=="neg", 0, 1)

# Find best model using AUC criterion with ROC curve
result_auc <- OptimalModelSearch(formula=diabetes~., data=df, 
                                criterion="AUC", plot_roc=TRUE)

# Find best model using Accuracy criterion with confusion matrix
result_acc <- OptimalModelSearch(formula=diabetes~., data=df, 
                                criterion="Accuracy", plot_cm=TRUE)

# Find best model using AIC criterion
result_aic <- OptimalModelSearch(formula=diabetes~., data=df, 
                                criterion="AIC")
```

### Visualizations
```r
# ROC curves for AUC criterion
plot_model_rocs(result_auc, comparison=TRUE)

# Confusion matrix for Accuracy criterion
plot_model_cm(result_acc)

# Standalone ROC curve comparison
compare_model_rocs(formula=diabetes~., data=df)
```

## License
This package is licensed under the MIT License. 