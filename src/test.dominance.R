source('dominance.R')


a <- matrix(c(
    1, 1, 1,
    1, 2, 2,
    1, 2, 3), ncol=3, byrow=TRUE)

b <- matrix(c(
    1, 1, 1,
    2, 2, 2,
    1, 2, 3), ncol=3, byrow=TRUE)

dom <- row.dominance(a, b)
stopifnot(all(dom == c(1, 1, 0)))
