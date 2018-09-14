context("Model-type specific functions for evaluating models at given inputs")

library(rpart)
library(randomForest)
library(datasets)
conf_names <<- c("model_output", "lower", "upper")

test_that("lm methods work", {
  mod <- lm(mpg ~ hp + cyl, data = mtcars)
  dat <- data.frame(hp = 100, cyl=6)

  expect_equivalent(dim(mod_output(mod))[2], 1L)
  res1 <- mod_output(mod)
  expect_equal(nrow(res1), nrow(mtcars))
  res <- mod_output(mod, data = dat)
  expect_equal(nrow(res), nrow(dat))
  res <- mod_output(mod, data = dat, interval = "confidence")
  expect_equal(names(res), conf_names)
  res <- mod_output(mod, data = dat, interval = "prediction")
  expect_equal(names(res), conf_names)
  expect_error(mod_output(mod, data = dat, interval = "bogus"))
  expect_true(sum(abs(res1$model_output[1:5] - c(21.217, 21.217, 26.071, 21.217, 15.444))) < 0.01,
              "lm output wrong")
  expect_lt(
    mod_output(mod, data = dat, interval = "confidence", level = 0.95)[1,"lower"],
    mod_output(mod, data = dat, interval = "confidence", level = 0.90)[1,"lower"]
  )
})

test_that("glm methods work", {
  expect_warning(
    mod <- glm(I(mpg > 20) ~ hp + cyl, data = mtcars, family = binomial)
  )
  expect_equivalent(dim(mod_output(mod))[2], 1L)
  res <- mod_output(mod, data = mtcars, interval = "confidence")
  expect_equal(nrow(res), nrow(mtcars))
  expect_equal(names(res), conf_names)
  expect_true(sum(abs(res[1,] - c(0.63, 0.184, 0.928))) < 0.01,
              "logistic output wrong")
  counts <- c(18,17,15,20,10,20,25,13,12)
  outcome <- gl(3,1,9)
  treatment <- gl(3,3)
  d.AD <- data.frame(treatment, outcome, counts)
  mod2 <- glm(counts ~ outcome + treatment, data = d.AD, family = poisson)
  res2 <- mod_output(mod2, data = d.AD, interval = "none")
  expect_equal(nrow(res2), nrow(d.AD))
  res2 <- mod_output(mod2, data = d.AD, interval = "confidence")
  expect_equal(nrow(res2), nrow(d.AD))
  expect_equal(names(res2), conf_names)
  expect_error(mod_output(mod2, data = d.AD, interval = "prediction"))
  expect_true(sum(abs(res2$model_output[1:6] - c(21, 13.33333, 15.6667, 21, 13.3333, 15.666667))) < .01,
              "poisson output wrong")
  expect_lt(
    mod_output(mod2, data = d.AD, interval = "confidence", level = 0.95)[1, "lower"],
    mod_output(mod2, data = d.AD, interval = "confidence", level = 0.90)[1, "lower"]
  )
})

test_that("rlm methods work", {
  mod <- MASS::rlm(mpg ~ hp + cyl, data = mtcars)
  expect_equivalent(mod_eval(mod, data = mtcars, append=FALSE), mod_output(mod, data = mtcars))
  dat <- data.frame(hp = 100, cyl=6)
  res <- mod_output(mod)
  expect_equal(nrow(res), nrow(mtcars))
  res <- mod_output(mod, data = dat)
  expect_equal(nrow(res), nrow(dat))
  res <- mod_output(mod, data = dat, interval = "confidence")
  expect_equal(names(res), conf_names)
  expect_warning(res <- mod_output(mod, data = dat, interval = "prediction"))
  expect_equal(names(res), conf_names)
  expect_error(mod_output(mod, data = dat, interval = "bogus"))
  expect_lt(
    mod_output(mod, data = dat, interval = "confidence", level = 0.95)[1, "lower"],
    mod_output(mod, data = dat, interval = "confidence", level = 0.90)[1, "lower"]
  )
})

test_that("rpart methods work", {
  mtcars2 <<- mtcars %>% dplyr::mutate(mileage = cut(mpg, c(5, 15, 25, 40)))
  mod1 <- rpart::rpart(mpg ~ ., data = mtcars2)
  mod2 <- rpart::rpart(mileage ~ . - mpg, data = mtcars2)
  expect_equal(dim(mod_output(mod1))[2], 1L)
  expect_equal(dim(mod_output(mod2))[2], 3L)
  res1 <- mod_output(mod1, data = mtcars2)
  res2 <- mod_output(mod2, data = mtcars2)
  expect_equal(nrow(res1), nrow(mtcars2))
  expect_equal(nrow(res2), nrow(mtcars2))
  expect_error(res <- mod_output(mod1, data = mtcars2, interval = "confidence"))
  expect_true(sum(abs(res1$model_output[1:6] - c(18.264, 18.264, 26.664, 18.264, 18.264, 18.264))) < 0.01,
              "rpart regression output wrong")
  expect_true(sum(abs(res2[31,] - c(0.857, 0.143, 0))) < 0.01,
              "rpart classification output wrong")
  expect_true(all(names(res2) == c("(5,15]", "(15,25]", "(25,40]")),
              "rpart classification output has wrong column names")
})

test_that("Random forest methods work", {
  library(randomForest)
  # regression
  mod <- randomForest(mpg ~ hp + cyl, data = mtcars)
  expect_equivalent(mod_eval(mod, append=FALSE, data = mtcars), mod_output(mod, data = mtcars))
  res <- mod_output(mod, data = mtcars)
  expect_true("model_output" %in% names(res))
  expect_equal(nrow(res), nrow(mtcars))
  expect_error(res <- mod_output(mod, interval = "confidence"))
  expect_true(sum(abs(res$model_output[1:6] -
                        c(20.397, 20.397, 24.957, 20.397, 16.794, 19.927))) < 1,
              "regression output wrong") # output somewhat random

  # classifier
  mtcars$mileage <- cut(mtcars$mpg, c(5, 15, 25, 40))
  mod <- rpart::rpart(mileage ~ . - mpg, data = mtcars)
  res <- mod_output(mod, data = mtcars)
  expect_equal(nrow(res), nrow(mtcars))
  expect_error(res <- mod_output(mod, data = mtcars, interval = "confidence"))
  expect_true(sum(abs( c(0, 0.143, 0.857) - res[3,])) < 0.1, "classifer output wrong") # output somewhat random
})


test_that("Nonlinear least squares methods work",{
  mod <- nls(temp ~ A+B*exp(-k*time), data=mosaicData::CoolingWater, start=list(A=50,B=50,k=1/20))
  res <- mod_output(mod)
  expect_equal(nrow(res), nrow(mosaicData::CoolingWater))
})




