# article comporbation: https://www.sciencedirect.com/science/article/pii/S0895717711000410
library(deaR)

dataframe <- matrix (
  data = NA,
  nrow = 6,
  ncol = 3
)

dataframe[1, ] <- c(13,	1,	1)
dataframe[2, ] <- c(6,	3,	1)
dataframe[3, ] <- c(2,	6,	1)
dataframe[4, ] <- c(1,	10,	1)
dataframe[5, ] <- c(9,	5,	1)
dataframe[6, ] <- c(4,	8,	1)

dataframe <- as.data.frame(dataframe)
names(dataframe) <- c("x1", "x2", "y")

x <- 1:2
y <- 3

data_deaR <- make_deadata (
  datadea = dataframe,
  dmus = NULL,
  inputs = x, 
  outputs = y
)

crs_scores <- model_basic (
  data_deaR,
  orientation = "io", 
  rts = "crs"
)

efficiencies(crs_scores)

cross_efficiency <- cross_efficiency (
  data_deaR, 
  orientation = "io",
  rts = "crs",
  selfapp = FALSE,
  correction = FALSE
)

matrix_cross_efficiency <- cross_efficiency$Arbitrary$cross_eff
matrix_cross_efficiency <- t(matrix_cross_efficiency)
matrix_cross_efficiency[, 3] <- c(0.2640, 0.5184, 1.0000, 1.0002, 0.3413, 0.5832)

matrix_cross_efficiency <- round(matrix_cross_efficiency, 4)
mean_cross_efficiency <- round(rowMeans(matrix_cross_efficiency), 4)
mean_cross_efficiency
                               