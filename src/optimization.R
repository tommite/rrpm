library(ROI)

## onLoad
.solver <- NULL
solvers <- ROI_installed_solvers()
if (!is.na(solvers['symphony'])) {
    .solver <<- 'symphony'
} else if (!is.na(solvers['glpk'])) {
    .solver <<- 'glpk'
} else {
    stop("No ROI Symphony or GLPK plugin installed")
}

optimize.pf <- function(values, costs, budget, var.type='I') {
    values <- as.vector(values)
    stopifnot(length(values) == length(costs))

    m <- length(values)

    obj.func <- L_objective(values)
    constr <- L_constraint(costs, c('<='), budget)
    ip <- OP(obj.func, constr, types=rep(var.type, m),
             bounds=V_bound(1:m, 1:m, rep(0, m), rep(1, m)),
             maximum=TRUE)
    res <- ROI_solve(ip, .solver)
    stopifnot(res$status$code == 0)
    res
}

