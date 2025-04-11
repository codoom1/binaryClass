test_that("plot_descrip functions correctly", {
  # Load test data
  data(iris)
  
  # Create a binary classification dataset
  iris_binary <- iris[iris$Species != "virginica", ]
  iris_binary$Species <- factor(iris_binary$Species)
  
  # Test basic functionality (no error)
  expect_error(plot_descrip(iris_binary, type = "ind", ppv = 1), NA)
  
  # Test with type="pair"
  expect_error(plot_descrip(iris_binary, type = "pair"), NA)
  
  # Test with ppv=2
  expect_error(plot_descrip(iris_binary, type = "ind", ppv = 2), NA)
  
  # Test with invalid type
  expect_error(plot_descrip(iris_binary, type = "invalid", ppv = 1))
  
  # Test with invalid ppv
  expect_error(plot_descrip(iris_binary, type = "ind", ppv = 3))
  
  # Test with missing ppv when type is "ind"
  expect_error(plot_descrip(iris_binary, type = "ind"))
  
  # Test with non-data frame input
  expect_error(plot_descrip(as.matrix(iris_binary), type = "ind", ppv = 1))
}) 