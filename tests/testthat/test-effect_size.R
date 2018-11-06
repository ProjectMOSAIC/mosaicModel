context("mod_effect is working")

test_that("problems with formulas identifying the variable are dealt with correctly", {
  model <- lm(mpg ~ hp + cyl, data = mtcars)
  expect_error(mod_effect(model))
  expect_error(mod_effect(model, ~ gear))
  expect_error(mod_effect(model, ~ bogus))
})

test_that("for classifiers the class level for which the effect size is being reported is contained in the output", {
  library(MASS)
  my_mod <- lda(Species ~ Petal.Length + Petal.Width, data = iris)
  res <- mod_effect(my_mod, ~ Petal.Length, bootstrap = 10, class_level = "virginica")
  expect_true(".class_level" %in% names(res))
  expect_true("virginica" %in% res$.class_level)
  # and for the default class level
  res <- mod_effect(my_mod, ~ Petal.Length, bootstrap = 10)
  expect_true(".class_level" %in% names(res))
  expect_true("setosa" %in% res$.class_level)

})
