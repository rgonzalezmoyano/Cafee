devtools::document()
devtools::load_all()
library("ggplot2")
library("FuzzyR")

set.seed(314)

# Simulated data
data <- reffcy (
  DGP = "cobb_douglas_XnY1",
  parms = list (
    N = 50,
    nX = 1
  )
)

x <- 1
y <- 2

tech_xmat <- as.matrix(data[, x])
tech_ymat <- as.matrix(data[, y])
eval_xmat <- as.matrix(data[, x])
eval_ymat <- as.matrix(data[, y])

data$rad_inp <- rad_inp (
  tech_xmat = tech_xmat,
  tech_ymat = tech_ymat,
  eval_xmat = eval_xmat,
  eval_ymat = eval_ymat,
  convexity = TRUE,
  returns = "variable"
)[, 1]

fis <- tipperGUI2()
