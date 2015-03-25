optimize.pf <- function(values, constr.A, constr.B, var.type='I') {
    values <- as.vector(values)
    stopifnot(nrow(constr.A) == length(constr.B))

    m <- length(values)

    obj.func <- L_objective(values)
    constr <- L_constraint(constr.A, rep('<=', length(constr.B)), constr.B)
    ip <- OP(obj.func, constr, types=rep(var.type, m),
             bounds=V_bound(1:m, 1:m, rep(0, m), rep(1, m)),
             maximum=TRUE)
    res <- ROI_solve(ip, .solver)

    stopifnot(res$status$code == 0)
    res
}

compute.right.add.C <- function(projects, constr.A, constr.Bmat) {
  projects <- as.matrix(projects)
  result <- matrix(0.0, nrow=nrow(constr.Bmat), ncol=ncol(projects)) # each col = 1 attribute
  stopifnot(nrow(projects) == ncol(constr.A))
  result <- .C("compute_right_add",
               as.numeric(projects),
               as.numeric(constr.A),
               as.vector(constr.Bmat),
               as.integer(nrow(projects)), as.integer(ncol(projects)),
               as.integer(ncol(constr.Bmat)), as.integer(nrow(constr.Bmat)),
               result=result, PACKAGE='rpm')$result
  return(result)
}
