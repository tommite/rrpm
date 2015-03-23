library(testthat)
library(hitandrun)
library(rpm)

test_that("potopt.indices.constr", {
  constr <- simplexConstraints(3)
  constr <- mergeConstraints(ordinalConstraint(3, 1, 2), constr)

  a <- matrix(c(
                1, 2, 1,
                2, 1, 1,
                1, 1, 2), ncol=3, byrow=TRUE)

  opts <- potopt.indices(a, constr)

  expect_equal(opts, c(2, 3))
})

