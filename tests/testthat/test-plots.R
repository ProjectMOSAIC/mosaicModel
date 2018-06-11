context("Plots do what we expect")


data(SAT, package = "mosaicData")
mod1 <- lm(sat ~ poly(expend, 2), data = SAT)
mod_plot(mod1, ~ expend, bootstrap = 10) 

mod_plot(mod1, interval = "confidence")
