test_that("shift correlation works.", {

  expected1 <- data.frame(i = -2, cor = 1, row.names = as.integer(3))
  expected2 <- data.frame(i = -2, shift = -10, cor = 1,
                          row.names = as.integer(3))

  y1 <- 1 : 4
  y2 <- c(1, 2, 1, 5)

  shifts <- -4 : 1

  expect_error(ShiftCorrelation(y1[1], y2, shifts))
  expect_equal(ShiftCorrelation(y1, y2, shifts), expected1)
  expect_equal(ShiftCorrelation(y1, y2, shifts, unit = 5), expected2)

})
