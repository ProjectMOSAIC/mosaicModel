context("mod_effect is working")

test_that("problems with formulas identifying the variable are dealt with correctly", {
  model <- lm(mpg ~ hp + cyl, data = mtcars)
  expect_error(mod_effect(model))
  expect_error(mod_effect(model, ~ gear))
  expect_error(mod_effect(model, ~ bogus))
})
