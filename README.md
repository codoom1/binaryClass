# binaryClass: R Package for Binary Classification

[![R-CMD-check](https://github.com/codoom1/binaryClass/actions/workflows/r-package.yml/badge.svg)](https://github.com/codoom1/binaryClass/actions/workflows/r-package.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

## Overview
The `binaryClass` package provides a comprehensive toolkit for binary classification tasks in R. It implements powerful functions for model training, evaluation, and prediction, with a focus on comparing different classification approaches.

## Installation

### From GitHub Release
```r
# Install the released version from GitHub
install.packages("https://codoom1.github.io/binaryClass/binaryClass_1.0.0.tar.gz", repos = NULL)
```

### From GitHub with devtools
```r
# Install development version
remotes::install_github("codoom1/binaryClass")
```

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
                                criterion="AUC", plot_roc=TRUE,
                                suppress_warnings=TRUE)

# Find best model using Accuracy criterion with confusion matrix
result_acc <- OptimalModelSearch(formula=diabetes~., data=df, 
                                criterion="Accuracy", plot_cm=TRUE)

# Find best model using AIC criterion
result_aic <- OptimalModelSearch(formula=diabetes~., data=df, 
                                criterion="AIC")
```

### Extract and Use the Best Model

```r
# Get the best model from results
best_model <- extract_best_model(result_auc)

# Use it for predictions on new data
new_pred <- predict(best_model, newdata=new_data)
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

## Dependencies
- `stats`
- `MASS`
- `gam`
- `glmnet`
- `pROC`
- `caret`
- `e1071`
- `graphics`
- `grDevices`

## License
This package is licensed under the MIT License.

## Contributing
Contributions to improve `binaryClass` are welcome. Please feel free to submit a pull request or open an issue on GitHub. 