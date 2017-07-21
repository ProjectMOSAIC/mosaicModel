context("Model-type specific functions for evaluating models at given inputs")

test_that("randomForest works", {
  library(randomForest)
  r_mod <- randomForest(mpg ~ hp + cyl, data = mtcars)
  mtcars$mpg_class <- cut(mtcars$mpg, c(5,15,25,40))
  c_mod <- randomForest(mpg_class ~ hp + cyl, data = mtcars)
  r_out <- mosaicModel:::eval_randomForest(r_mod)
  c_out <- mosaicModel:::eval_randomForest(c_mod)
  expect_true(sum(abs(r_out$model_output[1:6] - 
                        c(20.397, 20.397, 24.957, 20.397, 16.794, 19.927))) < 1,
              "regression output wrong") # output somewhat random
  expect_true(sum(abs( c(0.064, 0.936, 0) - c_out[5,])) < 0.1, "classifer output wrong") # output somewhat random
})

test_that("linear prediction works (e.g. lm, glm, rlm)", {
          mod1 <- lm(mpg ~ hp + cyl, data = mtcars)
          mod2 <- glm(I(mpg > 20) ~ hp + cyl, data = mtcars) # gaussian family
          mod3 <- MASS::rlm(mpg ~ hp + cyl, data = mtcars)
          
          r1 <- mosaicModel:::eval_lm(mod1, interval = "confidence")
          r2 <- mosaicModel:::eval_lm(mod2, interval = "confidence")
          r3 <- mosaicModel:::eval_lm(mod3, interval = "confidence")
          
          expect_true(sum(abs(r1$model_output[1:5] - c(21.217, 21.217, 26.071, 21.217, 15.444))) < 0.01,
                      "lm output wrong")
          expect_true(sum(abs(r2$model_output[1:5] - c(.50317, .50317, .9723, .50317, 0.00439))) < 0.01,
                      "logistic output wrong")
          expect_true(sum(abs(r3$model_output[1:5] - c(20.858, 20.858, 25.218, 20.858, 15.643)))< 0.01,
                      "robust linear output wrong")
})

test_that("logistic prediction works", {
          mod4 <- glm(I(mpg > 20) ~ hp + cyl, data = mtcars, family = binomial())
          r4 <- mosaicModel:::eval_logistic(mod4, interval = "confidence")
          expect_true(sum(abs(r4[1,] - c(0.63, 0.184, 0.928))) < 0.01, 
                      "model values wrong")
})

test_that("eval_glm correctly dispatches models", {
  # example from glm() help page
  counts <- c(18,17,15,20,10,20,25,13,12)
  outcome <- gl(3,1,9)
  treatment <- gl(3,3)
  d.AD <- data.frame(treatment, outcome, counts)
  m1 <- glm(counts ~ outcome + treatment, data = d.AD, family = poisson()) 
  m2 <- glm(I(mpg > 20) ~ hp + cyl, data = mtcars, family = binomial())
  r1 <- mosaicModel:::eval_glm(m1)
  r2 <- mosaicModel:::eval_glm(m2, interval = "confidence")
  expect_true(sum(abs(m1$model_output[1:6] - c(21, 13.33333, 15.6667, 21, 13.3333, 15.666667))) < .01,
              "poisson output wrong")
  expect_true(sum(abs(r2[1,] - c(0.63, 0.184, 0.928))) < 0.01, 
              "logistic output wrong")
})

test_that("poisson regression works", {
  # example from glm() help page
  counts <- c(18,17,15,20,10,20,25,13,12)
  outcome <- gl(3,1,9)
  treatment <- gl(3,3)
  d.AD <- data.frame(treatment, outcome, counts)
  m5 <- glm(counts ~ outcome + treatment, data = d.AD, family = poisson())
  r1 <- mosaicModel:::eval_poisson(m5)
  r2 <- mosaicModel:::eval_poisson(m5, interval = "confidence")
  r3 <- mosaicModel:::eval_glm(m5)
  expect_true(sum(abs(r1$model_output[1:6] - c(21, 13.33333, 15.6667, 21, 13.3333, 15.666667))) < .01,
              "poisson output wrong")

})

test_that("rpart regression works", {
  mod1 <- rpart::rpart(mpg ~ ., data = mtcars)
  mtcars$mileage <- cut(mtcars$mpg, c(5, 15, 25, 40))
  mod2 <- rpart::rpart(mileage ~ . - mpg, data = mtcars)
  r1 <- mosaicModel:::eval_rpart(mod1)
  r2 <- mosaicModel:::eval_rpart(mod2)
  expect_true(sum(abs(r1$model_output[1:6] - c(18.264, 18.264, 26.664, 18.264, 18.264, 18.264))) < 0.01,
              "rpart regression output wrong")
  expect_true(sum(abs(r2[31,] - c(0.857, 0.143, 0))) < 0.01,
              "rpart classification output wrong")
  expect_true(all(names(r2) == c("(5,15]", "(15,25]", "(25,40]")),
              "rpart classification output has wrong column names")
})

test_that("Getting the model evaluation function works", {
  mod1 <- lm(mpg ~ hp + cyl, data = mtcars)
  mod2 <- glm(I(mpg > 20) ~ hp + cyl, data = mtcars) #gaussian family
  mod3 <- MASS::rlm(mpg ~ hp + cyl, data = mtcars)
  mod4 <- glm(I(mpg > 20) ~ hp + cyl, data = mtcars, family = binomial())
  counts <- c(18,17,15,20,10,20,25,13,12)
  outcome <- gl(3,1,9)
  treatment <- gl(3,3)
  d.AD <- data.frame(treatment, outcome, counts)
  mod5 <- glm(counts ~ outcome + treatment, data = d.AD, family = poisson())
  mtcars$mileage <- cut(mtcars$mpg, c(5, 15, 25, 40))
  mod6 <- rpart::rpart(mileage ~ . - mpg, data = mtcars)
  library(randomForest)
  mod7 <- randomForest(mpg ~ hp + cyl, data = mtcars)
  f1 <- mosaicModel:::get_eval_function(mod1) #lm
  f2 <- mosaicModel:::get_eval_function(mod2) #glm gaussian
  f3 <- mosaicModel:::get_eval_function(mod3) #rlm
  f4 <- mosaicModel:::get_eval_function(mod4) #logistic
  f5 <- mosaicModel:::get_eval_function(mod5) #poisson
  f6 <- mosaicModel:::get_eval_function(mod6) #rpart
  f7 <- mosaicModel:::get_eval_function(mod7) #randomForest
  expect_true(all.equal(f1$eval_fun, mosaicModel:::eval_lm), "wrong eval function")
  expect_true(all.equal(f3$eval_fun, mosaicModel:::eval_lm), "wrong eval function")
  expect_true(all.equal(f4$eval_fun, mosaicModel:::eval_glm), "wrong eval function")
  expect_true(all.equal(f5$eval_fun, mosaicModel:::eval_glm), "wrong eval function")
  expect_true(all.equal(f6$eval_fun, mosaicModel:::eval_rpart), "wrong eval function")
  expect_true(all.equal(f7$eval_fun, mosaicModel:::eval_randomForest), "wrong eval function")
  
})







