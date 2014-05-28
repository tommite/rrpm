sort.matrix <- function(mat) {
    dd <- as.data.frame(mat)
    as.matrix(dd[ do.call(order, dd), ])
}
