library(testthat)
library(rpm)

test_that("optimal.alternative.indices", {

  a <- matrix(c(
                1, 1, 2,
                1, 1, 1.5,
                2, 1, 1), ncol=3, byrow=TRUE)

  opts <- optimal.alternative.indices(a)

  expect_equal(opts, c(1, 3))
})
