
rowwise.sums <- function(mat, vec) {
  stopifnot(ncol(mat) == length(vec))

  result <- matrix(0.0, nrow=nrow(mat), ncol=ncol(mat))
  result <- .C("rowwise_sums",
               as.numeric(mat),
               as.numeric(vec),
               as.integer(nrow(mat)), as.integer(ncol(mat)),
               result=result, PACKAGE='rpm')$result
  result
}
