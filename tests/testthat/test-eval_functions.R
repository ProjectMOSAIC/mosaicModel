context("Model-type specific functions for evaluating models at given inputs")

test_that("randomForest works", {
  library(randomForest)
  r_mod <- randomForest(mpg ~ hp + cyl, data = mtcars)
  mtcars$mpg_class <- cut(mtcars$mpg, c(5,15,25,40))
  c_mod <- randomForest(mpg_class ~ hp + cyl, data = mtcars)
  r_out <- eval_randomForest(r_mod)
  c_out <- eval_randomForest(c_mod)
  expect_true(sum(abs(r_out$model_output[1:6] - 
                        c(20.397, 20.397, 24.957, 20.397, 16.794, 19.927))) < 1,
              "regression output wrong") # output somewhat random
  expect_true(sum(abs( c(0.064, 0.936, 0) - c_out[5,])) < 0.1, "classifer output wrong") # output somewhat random
})

test_that("linear prediction works (e.g. lm, glm, rlm)", {
          mod1 <- lm(mpg ~ hp + cyl, data = mtcars)
          mod2 <- glm(I(mpg > 20) ~ hp + cyl, data = mtcars) # don't worry about the family
          mod3 <- MASS::rlm(mpg ~ hp + cyl, data = mtcars)
          
          r1 <- eval_linear(mod1, interval = "confidence")
          r2 <- eval_linear(mod2, interval = "confidence")
          r3 <- eval_linear(mod3, interval = "confidence")
          
          expect_true(sum(abs(r1$model_output[1:5] - c(21.217, 21.217, 26.071, 21.217, 15.444))) < 0.01,
                      "lm output wrong")
          expect_true(sum(abs(r2$model_output[1:5] - c(.50317, .50317, .9723, .50317, 0.00439))) < 0.01,
                      "logistic output wrong")
          expect_true(sum(abs(r3$model_output[1:5] - c(20.858, 20.858, 25.218, 20.858, 15.643)))< 0.01,
                      "robust linear output wrong")
})

expect_that("logistic prediction works", {
          mod4 <- glm(I(mpg > 20) ~ hp + cyl, data = mtcars, family = binomial())
          r4 <- eval_logistic(mod4, interval = "confidence")
          expect_true(ncol(r4) == 2 && all(names(r4) == c("TRUE", "FALSE")), 
                      "logistic output names should be TRUE and FALSE")
          ones <- r4$`TRUE` + r4$`FALSE`
          expect_true( all(abs(ones - 1 < 0.000000001)), 
                       "logistic columns should add to 1.")
})

test_that("poisson regression works", {
  # example from glm() help page
  counts <- c(18,17,15,20,10,20,25,13,12)
  outcome <- gl(3,1,9)
  treatment <- gl(3,3)
  d.AD <- data.frame(treatment, outcome, counts)
  m5 <- glm(counts ~ outcome + treatment, data = d.AD, family = poisson())
  r1 <- eval_poisson(m5)
  r2 <- eval_poisson(m5, interval = "confidence")
  expect_true(sum(abs(r1$model_output[1:6] - c(21, 13.33333, 15.6667, 21, 13.3333, 15.666667))) < .01)
})

test_that("rpart regression works", {
  mod1 <- rpart(mpg ~ ., data = mtcars)
  mtcars$mileage <- cut(mtcars$mpg, c(5, 15, 25, 40))
  mod2 <- rpart(mileage ~ . - mpg, data = mtcars)
  
})