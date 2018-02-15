context("interval-calculations")
library(mosaicModel)

test_that("CI on quantitative variables give correct values and have right names", {
  t1 <- mosaicCore::df_stats(~ hp, data = mtcars, mn = ci.mean, md = ci.median, sd = ci.sd)
  expect_equal(
    c(t1$mn_lower, t1$mn_upper),
    c(121.9679, 171.4071),
    tolerance = 0.001
  )
  expect_equal(
    c(t1$md_lower, t1$md_upper), 
    c(109, 175),
    tolerance = 0.001
  )
  expect_equal(
    c(t1$sd_lower, t1$sd_upper), 
    c(54.96708, 91.15293),
    tolerance = 0.001
  )
})


