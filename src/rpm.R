library(plyr)
library(hitandrun)
source('optimization.R')

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
        print(k)
        Pk.with.xk <- Pk
        Pk.with.xk[,k] = 1
        Pk.tilde <- rbind(Pk, filter.feasible(Pk.with.xk, projects, budget))
        Pk <- Pk.tilde
    }
}

## Tries to generate size portfolios, some might be duplicates
## so the return value might be with less rows
gen.Pd <- function(projects, budget, size=100) {
    proj.inds <- matrix(0, ncol=nrow(projects), nrow=size)
    proj.data <- projects[,-ncol(projects)]
    proj.costs <- projects[,ncol(projects)]
    weights <- simplex.sample(ncol(projects)-1, 100)$samples

    unique(aaply(weights, 1, function(w) { optimize.pf(w %*% t(proj.data), proj.costs, budget) }))
}

filter.feasible <- function(proj.inds, projects, budget) {
    total.use <- proj.inds %*% as.matrix(projects)
    total.cost <- total.use[,ncol(total.use)]
    proj.inds[which(total.cost <= budget),]
}
