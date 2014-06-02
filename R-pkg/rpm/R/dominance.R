row.dominance <- function(a, b) {
    stopifnot(ncol(a) == ncol(b))
    stopifnot(is.matrix(a))
    stopifnot(is.matrix(b))

    result <- array(0, dim=nrow(a));

    result <- .C("row_dominance", a, b, as.integer(ncol(a)), as.integer(nrow(a)),
       as.integer(nrow(b)), result=result, DUP=FALSE, PACKAGE='rpm')$result
    result
}
