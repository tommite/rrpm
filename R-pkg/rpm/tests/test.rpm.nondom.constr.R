library(testthat)
library(rpm)

sort.matrix <- function(mat) {
  dd <- as.data.frame(mat)
  as.matrix(dd[ do.call(order, dd), ])
}

test_that("rpm.nondom.constr", {
  ## Load in the data and results
  data <- read.csv('data/data.csv')[,-1]
  results <- read.csv('data/results-constr.csv', sep=';')[,-1]
  colnames(results) <- paste0('x', 1:15)

  budget <- 300

  ext.pts <- matrix(c(1, 0, 0,
                      0.5, 0.5, 0,
                      0.333, 0.333, 0.333), ncol=3, byrow=TRUE)

  non.dom <- rpm.nondom(data, budget, Wext=ext.pts, nr.eff=100)

  expect_equivalent(sort.matrix(non.dom), sort.matrix(results))
})
