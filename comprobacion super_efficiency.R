# article comporbation: https://www.hindawi.com/journals/mpe/2020/2458343/
library(Benchmarking)

dataframe <- matrix (
  data = NA,
  nrow = 5,
  ncol = 3
)

dataframe[1, ] <- c(2,	4,	2)
dataframe[2, ] <- c(8,	3,	4)
dataframe[3, ] <- c(12,	0,	6)
dataframe[4, ] <- c(8,	4,	5)
dataframe[5, ] <- c(5,	5,	5)



dataframe <- as.data.frame(dataframe)
names(dataframe) <- c("x1", "x2", "y")

x <- 1:2
y <- 3

result_super_efficiency <- sdea (
  X = as.matrix(dataframe[, x]),
  Y = as.matrix(dataframe[, y]),
  RTS = "vrs",
  ORIENTATION = "in"
)$eff; result_super_efficiency
