library(rpm)

## Load in the data and results
data <- read.xls('../data/problem2.xlsx', sheet=1, skip=1)[,c(1:4,6)]
rownames(data) <- data[,1]
data <- data[,-1]
results <- read.xls('../data/problem2.xlsx', sheet=2)
rownames(results) <- results[,1]
results <- results[,-1]

budget <- 300

non.dom <- rpm.nondom(data, budget)

stopifnot(all(sort.matrix(non.dom) - sort.matrix(results) == 0))
