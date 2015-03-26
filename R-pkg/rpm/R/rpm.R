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

rpm.nondom.costs <- function(projects, costs, budget, Wext=make.vertices(ncol(projects)), nr.eff=100) {
  costs <- as.vector(costs)
  constr.A <- matrix(costs, nrow=1)
  constr.B <- matrix(budget)
  rpm.nondom(projects, constr.A, constr.B, Wext, nr.eff)
}

###
## Computes RPM non-dominated portfolios
## projects: a data frame of project data (m projects), one row per project
## constr.A: the constraint coefficient matrix (q constraints, size q x m)
## constr.B: the constraint limit vector of length q
## nr.eff: how many random efficient portfolios to create in the beginning
## PRECOND: all project costs must be <= budget
###
rpm.nondom <- function(projects, constr.A, constr.B, Wext=make.vertices(ncol(projects)), nr.eff=1000) {
    m <- nrow(projects)
    
    Pk <- matrix(0, ncol=m, nrow=2) # initial portfolios
    Pk[2,1] <- 1
    colnames(Pk) <- paste('x', 1:m, sep='')

    Pd <- gen.Pd(projects, constr.A, constr.B, nr.eff)
    message('Found ', nrow(Pd), ' distinct candidate portfolios for dominance checking')

    ## round k = 2, ..., m
    for (k in 2:m) {
        Pk.with.xk <- Pk
        Pk.with.xk[,k] = 1
        Pk <- rbind(Pk, filter.feasible(Pk.with.xk, constr.A, constr.B))
        if (k < m && k >= m/2) { # only filter from the second half onwards
          pk.size <- nrow(Pk)
          Pk <- filter.Uk.dom(Pk, Pd, projects, constr.A, constr.B, k, Wext)
          message('Round ', k, '/', m, ' : ', nrow(Pk), '/', pk.size, ' PF left after filtering')
        } else {
          message('Round ', k, '/', m, ' : ', nrow(Pk), ' feasible PF')
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

## Computes row indices of potentially optimal alternatives given restrictions on weights
##
## perfs: a matrix of alternative performances
## w.constr: weight constraints (of type used in 'hitandrun' package)
potopt.indices <- function(perfs, w.constr=simplexConstraints(ncol(perfs))) {
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
    all <- proj.inds %*% as.matrix(projects)
    proj.inds[sort(maximalvectors.indices(all)),,drop=FALSE]
}

## Filters portfolios in Pk with respect to
## being Uk-dominated by reference portfolios
## in Pd
filter.Uk.dom <- function(Pk, Pd, projects, constr.A, constr.B, k, Wext) {
    stopifnot(k < ncol(Pk))

    projects <- as.matrix(projects)

    m <- ncol(Pk)

    ##  constr.B.left <- as.matrix(aaply(Pk %*% t(constr.A), 1, function(x) { constr.B - x }))
    constr.B.left <- rowwise.sums(-(Pk %*% t(constr.A)), constr.B)
    
    left.side <- Pd %*% projects %*% t(Wext)
    right.side.base <- Pk %*% projects %*% t(Wext)
    right.side.add <- compute.right.add.C(projects[(k+1):m,,drop=FALSE],
                                          constr.A[,(k+1):m,drop=FALSE], constr.B.left) %*% t(Wext)
    right.side <- right.side.base + right.side.add
    
    dom.rel <- row.dominance(right.side + EPS, left.side)
    
    Pk[!dom.rel,, drop=FALSE]
}

## Tries to generate size portfolios, some might be duplicates
## so the return value might be with less rows
gen.Pd <- function(projects, constr.A, constr.B, size) {
  stopifnot(size > 1)
  proj.inds <- matrix(0, ncol=nrow(projects), nrow=size)
  weights <- simplex.sample(ncol(projects), size)$samples

  unique(aaply(weights, 1, function(w) { optimize.pf(w %*% t(projects), constr.A, constr.B)$solution }))
}

filter.feasible <- function(proj.inds, constr.A, constr.B) {
  satisfy.all <- function(x) { all(x <= constr.B) }
  ##  inds <- aaply(constr.A %*% t(proj.inds), 2, satisfy.all)
  inds <- colSums(((constr.A %*% t(proj.inds)) - as.vector(constr.B)) <= 0) == length(as.vector(constr.B))
  proj.inds[inds,]
}
