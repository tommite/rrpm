library(plyr)
library(hitandrun)
library(ror)
source('optimization.R')
source('dominance.R')

###
## Computes RPM non-dominated portfolios
## projects: a data frame of project data,
## one row per project, last column cost information
## budget: total available budget
## PRECOND: all project costs must be <= budget
###
rpm.nondom <- function(projects, budget) {
    stopifnot(all(projects[,ncol(projects)] <= budget)) # PRECOND
    
    m <- nrow(projects)
    
    Pk <- matrix(0, ncol=m, nrow=2) # initial portfolios
    Pk[2,1] <- 1
    colnames(Pk) <- paste('x', 1:m, sep='')

    Pd <- gen.Pd(projects, budget)

    ## round k = 1, ..., m
    for (k in 2:m) {
        Pk.with.xk <- Pk
        Pk.with.xk[,k] = 1
        Pk <- rbind(Pk, filter.feasible(Pk.with.xk, projects, budget))
        if (k < m) {
            pk.size <- nrow(Pk)
            Pk <- filter.Uk.dom(Pk, Pd, projects, budget, k)
            message('Round ', k, '/', m, ' : ', nrow(Pk), '/', pk.size, ' PF left after filtering')
        }
    }
    filter.dominated(Pk, projects)    
}

filter.dominated <- function(proj.inds, projects) {
    all <- proj.inds %*% as.matrix(projects[,1:ncol(projects)-1])
    proj.inds[sort(maximalvectors.indices(all)),]
}

## Filters portfolios in Pk with respect to
## being Uk-dominated by reference portfolios
## in Pd
filter.Uk.dom <- function(Pk, Pd, projects, budget, k, Wext=diag(ncol(projects)-1)) {
    stopifnot(k < ncol(Pk))
    proj.data <- projects[,-ncol(projects)]
    proj.costs <- projects[,ncol(projects)]

    m <- ncol(Pk)

    value.left <- as.vector(budget - (Pk %*% proj.costs))
    
    left.side <- Pd %*% as.matrix(proj.data) %*% Wext
    right.side.base <- Pk %*% as.matrix(proj.data) %*% Wext
    right.side.add <- compute.right.add.C(projects[(k+1):m,], value.left)
    ## <- t(aaply(Wext, 1, function(w) {
    ##     laply(1:nrow(Pk), function(pf.ind) {
    ##         optimize.pf(w %*% t(proj.data[(k+1):m,]),
    ##                     proj.costs[(k+1):m],
    ##                     value.left[pf.ind], var.type='C')$objval
    ##     })
    ## }))
    right.side <- right.side.base + right.side.add

    dom.rel <- row.dominance(right.side, left.side)
    
    Pk[!dom.rel,]
}

## Checks whether array a dominates array b
dom <- function(a, b) {
    a <- as.array(a)
    b <- as.array(b)
    stopifnot(length(a) == length(b))
    all(a >= b) && any(a > b)
}

## Tries to generate size portfolios, some might be duplicates
## so the return value might be with less rows
gen.Pd <- function(projects, budget, size=1000) {
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
