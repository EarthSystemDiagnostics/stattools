test_that("DOF calculation works", {
  expect_equal(getEffectiveDOF(10, 0.), 10)
  expect_equal(round(getEffectiveDOF(10, 0.6), 6), 3.072636)
})
