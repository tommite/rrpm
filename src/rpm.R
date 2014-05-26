library(plyr)

###
## Computes RPM non-dominated portfolios
## projects: a data frame of project data,
## one row per project, last column cost information
## budget: total available budget
## PRECOND: all project costs must be <= budget
###
rpm.nondom <- function(projects, budget) {
    stopifnot(all(projects[,ncol(projects)] <= budget)) # PRECOND
    
    Pk <- matrix(0, ncol=m, nrow=2) # initial portfolios
    Pk[2,1] <- 1
    colnames(Pk) <- paste('x', 1:m, sep='')

    ## round k = 1, ..., m
    for (k in 2:m) {
        Pk.with.xk <- Pk
        Pk.with.xk[,k] = 1
        Pk.tilde <- rbind(Pk, filter.feasible(Pk.with.xk, projects, budget))
    }
}


filter.feasible <- function(proj.inds, projects, budget) {
    total.use <- proj.inds %*% as.matrix(projects)
    total.cost <- total.use[,ncol(total.use)]
    proj.inds[which(total.cost <= budget),]
}
