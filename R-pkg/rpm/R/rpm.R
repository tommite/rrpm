EPS <- 1E-5

## Copy-pasted from hitandrun (could export the methods instead?)
filterConstraints <- function(constr, sel) {
  list(constr = constr[['constr']][sel, , drop=FALSE],
       rhs = constr[['rhs']][sel],
       dir = constr[['dir']][sel])
}
eq.constr <- function(constr) {
  filterConstraints(constr, constr[['dir']] == '=')
}
iq.constr <- function(constr) {
  x <- filterConstraints(constr, constr[['dir']] != '=')
  stopifnot(all(x[['dir']] == '<='))
  x
}
####

make.vertices <- function(n, constraints=NULL) {
  stopifnot(n > 1)
  stopifnot(is.null(constraints) || ncol(constraints$constr) == n)

  constr <- eliminateRedundant(mergeConstraints(simplexConstraints(n), constraints))
  eq <- eq.constr(constr)
  iq <- iq.constr(constr)

  h <- 
    if (length(eq[['dir']]) > 0) {
      makeH(iq$constr, iq$rhs, eq$constr, eq$rhs)
    } else {
      makeH(iq$constr, iq$rhs)
    }
  as.matrix(unclass(scdd(h)$output))[,-c(1:2), drop=FALSE]
}

###
## Computes RPM non-dominated portfolios
## projects: a data frame of project data,
## one row per project, last column cost information
## budget: total available budget
## nr.eff: how many random efficient portfolios to create in the beginning
## PRECOND: all project costs must be <= budget
###
rpm.nondom <- function(projects, budget, Wext=make.vertices(ncol(projects)-1), nr.eff=1000) {
    stopifnot(all(projects[,ncol(projects)] <= budget)) # PRECOND
    
    m <- nrow(projects)
    
    Pk <- matrix(0, ncol=m, nrow=2) # initial portfolios
    Pk[2,1] <- 1
    colnames(Pk) <- paste('x', 1:m, sep='')

    Pd <- gen.Pd(projects, budget, nr.eff)

    ## round k = 2, ..., m
    for (k in 2:m) {
        Pk.with.xk <- Pk
        Pk.with.xk[,k] = 1
        Pk <- rbind(Pk, filter.feasible(Pk.with.xk, projects, budget))
        if (k < m) {
            pk.size <- nrow(Pk)
            Pk <- filter.Uk.dom(Pk, Pd, projects, budget, k, Wext)
            message('Round ', k, '/', m, ' : ', nrow(Pk), '/', pk.size, ' PF left after filtering')
        }
    }
    filter.dominated(Pk, projects)
}

har.constr.to.roi <- function(constr) {
  map.dir <- function(dir) {
    replace(dir, dir=='=', '==')
  }
  L_constraint(L = constr$constr,
       dir = map.dir(constr$dir),
       rhs = constr$rhs)
}

optimality.constraints <- function(alt, all.perfs) {
  mat <- aaply(all.perfs, 1, '-', alt)
  mat <- mat[rowSums(abs(mat)) > 0,]
  list(constr=mat,
       dir=rep('<=', nrow(mat)),
       rhs=rep(-EPS, nrow(mat)))
}

## Computes row indices of optimal alternatives given restrictions on weights
##
## perfs: a matrix of alternative performances
## w.constr: weight constraints (of type used in 'hitandrun' package)
optimal.alternative.indices <- function(perfs, w.constr=simplexConstraints(ncol(perfs))) {
  opt <- aaply(perfs, 1, function(perf) {
    m <- length(perf)
    har.constr <- mergeConstraints(w.constr, optimality.constraints(perf, perfs))
    obj <- L_objective(rep(1, m))
    roi.constr <- har.constr.to.roi(har.constr)
    ip <- OP(obj, roi.constr, bounds=V_bound(1:m, 1:m, rep(0, m), rep(1, m)), maximum=TRUE)
    res <- ROI_solve(ip, .solver)
    res$status$code == 0
  })
  as.vector(which(opt))
}

filter.dominated <- function(proj.inds, projects) {
    all <- proj.inds %*% as.matrix(projects[,-ncol(projects)])
    proj.inds[sort(maximalvectors.indices(all)),,drop=FALSE]
}

## Filters portfolios in Pk with respect to
## being Uk-dominated by reference portfolios
## in Pd
filter.Uk.dom <- function(Pk, Pd, projects, budget, k, Wext) {
    stopifnot(k < ncol(Pk))
    proj.data <- projects[,-ncol(projects)]
    proj.costs <- projects[,ncol(projects)]

    m <- ncol(Pk)

    value.left <- as.vector(budget - (Pk %*% proj.costs))
    
    left.side <- Pd %*% as.matrix(proj.data) %*% t(Wext)
    right.side.base <- Pk %*% as.matrix(proj.data) %*% t(Wext)
    right.side.add <- compute.right.add.C(projects[(k+1):m,,drop=FALSE], value.left) %*% t(Wext)
    
    right.side <- right.side.base + right.side.add
    
    dom.rel <- row.dominance(right.side + EPS, left.side)
    
    Pk[!dom.rel,, drop=FALSE]
}

## Tries to generate size portfolios, some might be duplicates
## so the return value might be with less rows
gen.Pd <- function(projects, budget, size) {
  stopifnot(size > 1)
    proj.inds <- matrix(0, ncol=nrow(projects), nrow=size)
    proj.data <- projects[,-ncol(projects)]
    proj.costs <- projects[,ncol(projects)]
    weights <- simplex.sample(ncol(projects)-1, size)$samples

    unique(aaply(weights, 1, function(w) { optimize.pf(w %*% t(proj.data), proj.costs, budget)$solution }))
}

filter.feasible <- function(proj.inds, projects, budget) {
    total.use <- proj.inds %*% as.matrix(projects)
    total.cost <- total.use[,ncol(total.use)]
    proj.inds[which(total.cost <= budget),]
}
