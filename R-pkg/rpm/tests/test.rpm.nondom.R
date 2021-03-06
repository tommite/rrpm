library(testthat)
library(rpm)

sort.matrix <- function(mat) {
  dd <- as.data.frame(mat)
  as.matrix(dd[ do.call(order, dd), ])
}

test_that("rpm.nondom", {
  ## Load in the data and results
  data <- read.csv('data/data.csv')[,-1]
  results <- read.csv('data/results.csv')[,-1]

  proj.scores <- data[,1:3]
  proj.costs <- data[,4]
  budget <- matrix(300)

  non.dom <- rpm.nondom.costs(proj.scores, proj.costs, budget, nr.eff=100)
  non.dom2 <- rpm.nondom.costs(proj.scores, proj.costs,
                               budget, Wext=matrix(c(0, 0, 1, 0, 1, 0, 1, 0, 0), ncol=3), nr.eff=100)
  non.dom3 <- rpm.nondom.costs(proj.scores, proj.costs, budget, Wext=diag(ncol(data)-1), nr.eff=100)

  ## Check that constraint generation works
  expect_equivalent(sort.matrix(non.dom), sort.matrix(non.dom2))
  ## Check that order of points doesnt matter
  expect_equivalent(sort.matrix(non.dom2), sort.matrix(non.dom3))
  expect_equivalent(sort.matrix(non.dom), sort.matrix(results))
})
