context("Training models")

test_that("dispatch on architecture works", {
  mod1 <- mod_train("lm", mpg ~ hp, data = mtcars)
  expect_true(inherits(mod1, "lm"))
})

test_that("unquoted architecture works", {
  mod1 <- mod_train(lm, mpg ~ hp, data = mtcars )
  expect_true(inherits(mod1, "lm"))
})

test_that("finds missing required params", {
  stop("Don't have any examples yet.")
})
