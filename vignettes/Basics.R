## ----setup, include = FALSE----------------------------------------------
library(ggformula)
library(tidyverse)
library(mosaicModel)
library(randomForest)
library(caret)
knitr::opts_chunk$set(fig.align = "center", fig.show = "hold", out.width = "45%")

## ------------------------------------------------------------------------
mtcars <- mtcars %>% mutate(transmission = ifelse(am, "manual", "automatic"))
gf_point(mpg ~ hp, color = ~ transmission, data = mtcars)

## ------------------------------------------------------------------------
fuel_mod_1 <- lm(mpg ~ hp * transmission, data = mtcars)
fuel_mod_2 <- lm(mpg ~ ns(hp, 2) * transmission, data = mtcars)

## ----out.width = "30%"---------------------------------------------------
mod_plot(fuel_mod_1) %>% gf_theme(legend.position = "top")
mod_plot(fuel_mod_2) %>% gf_theme(legend.position = "top")

## ------------------------------------------------------------------------
gf_point(Sepal.Length ~ Petal.Length, color = ~ Species, data = iris) %>%
  gf_theme(legend.position = "top")

## ------------------------------------------------------------------------
library(randomForest)
iris_mod_1 <- randomForest(Species ~ Sepal.Length + Petal.Length, data = iris)
library(caret)
iris_mod_2 <- train(Species ~., data = iris, method = "knn",
 preProcess = c("center", "scale"),
 tuneLength = 10)

## ------------------------------------------------------------------------
mod_plot(iris_mod_1) %>% gf_theme(legend.position = "top")

## ----out.width = "80%", fig.width = 8, fig.height = 8--------------------
mod_plot(iris_mod_2, class_level = "setosa") %>% gf_theme(legend.position = "top")

## ----fig.out="40%", fig.keep = "hold"------------------------------------
mod_plot(iris_mod_2, ~ Petal.Length + Petal.Width) %>% gf_theme(legend.position = "top")
mod_plot(iris_mod_2, ~ Petal.Length + Petal.Width + Sepal.Width) %>% gf_theme(legend.position = "top")

## ------------------------------------------------------------------------
mod_eval(fuel_mod_1, transmission = "manual", hp = 200)

## ------------------------------------------------------------------------
mod_eval(fuel_mod_1)

## ------------------------------------------------------------------------
f1 <- mod_fun(fuel_mod_1)
f1(hp = 200:203, transmission = "manual")
f2 <- mod_fun(fuel_mod_2)
f2(hp = 200:203, transmission = "manual")

## ------------------------------------------------------------------------
mod_eval(iris_mod_1, nlevels = 2)

## ------------------------------------------------------------------------
mod_effect(fuel_mod_2, ~ hp)

## ------------------------------------------------------------------------
mod_effect(fuel_mod_2, ~ hp, hp = c(100, 200), transmission = "manual")
mod_effect(fuel_mod_2, ~ hp, nlevels = 3)

## ------------------------------------------------------------------------
mod_effect(fuel_mod_2, ~ hp, step = 0.1, nlevels = 1)

## ------------------------------------------------------------------------
mod_error(fuel_mod_2)

## ------------------------------------------------------------------------
mod_error(fuel_mod_2, testdata = mtcars[1:10,])

## ------------------------------------------------------------------------
methods(mod_eval_fun)

## ---- fig.show='hold', fig.cap = "The caption of this figure."-----------
plot(1:10)
plot(10:1)

