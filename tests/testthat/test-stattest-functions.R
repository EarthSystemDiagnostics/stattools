test_that("zero autocorrelation reproduces default t.test results", {

  x <- rnorm(1000)
  y <- rnorm(1000)

  expect_equal(stats::t.test(x), t_test(x))
  expect_equal(stats::t.test(x, y), t_test(x, y))
  
})

test_that("zero autocorrelation reproduces default ks.test results", {

  x <- rnorm(1000)
  y <- rnorm(1000)

  expect_equal(stats::ks.test(x, y), ks_test(x, y))
  expect_equal(stats::ks.test(x, y, alternative = "less"),
               ks_test(x, y, alternative = "less"))
  expect_equal(stats::ks.test(x, y, alternative = "greater"),
               ks_test(x, y, alternative = "greater"))

  x <- rnorm(99)
  y <- rnorm(99)

  expect_equal(stats::ks.test(x, y), ks_test(x, y))
  expect_equal(stats::ks.test(x, y, alternative = "less"),
               ks_test(x, y, alternative = "less"))
  expect_equal(stats::ks.test(x, y, alternative = "greater"),
               ks_test(x, y, alternative = "greater"))

})
