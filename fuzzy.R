devtools::document()
devtools::load_all()

library("ggplot2")
library("Benchmarking")

set.seed(314)

# Simulated data
data <- reffcy (
  DGP = "cobb_douglas_XnY1",
  parms = list (
    N = 100,
    nX = 1
  )
)

x <- 1
y <- 2


# compute random error
random_error <- rnorm(n = 100, mean = 0, sd = 0.02)

# compute new vector of outputs with random error
data[, y] <- data[, y] * exp(random_error)

# ================================== #
# Additive measure with standard DEA #
# ================================== #

tech_xmat <- as.matrix(data[, x])
tech_ymat <- as.matrix(data[, y])
eval_xmat <- as.matrix(data[, x])
eval_ymat <- as.matrix(data[, y])

data$rad_out <- rad_out (
  tech_xmat = tech_xmat,
  tech_ymat = tech_ymat,
  eval_xmat = eval_xmat,
  eval_ymat = eval_ymat,
  convexity = TRUE,
  returns = "variable"
)[, 1]

# ================================== #
#         Bootstrapping DEA          #
# ================================== #

bootstrapping_dea <- dea.boot (
  tech_xmat, 
  tech_ymat, 
  NREP = 200,
  ORIENTATION = "out",
  alpha = 0.01
  )

data$btd_li <- bootstrapping_dea[["conf.int"]][, 1]
data$btd_pm <- bootstrapping_dea[["eff.bc"]]
data$btd_ls <- bootstrapping_dea[["conf.int"]][, 2]

# ================================== #
#    Stochastic Frontier Analysis    #
# ================================== #

# stochastic_frontier_analysis <- sfa (
#   tech_xmat, 
#   tech_ymat
# )

# data$sfa <- te.add.sfa(stochastic_frontier_analysis)

# ================================== #
#    Supper Efficiency with DEA      #
# ================================== #

super_efficiency <- sdea (
  tech_xmat, 
  tech_ymat,
  ORIENTATION = "out"
)

data$sup_eff <- super_efficiency[["eff"]]

# ================================== #
#               Gráfico              #
# ================================== #

# dea_frontier <- data[, x] * data[, "rad_inp"]

data$y_min <- data$y * data$btd_li
data$y_eff <- data$y * data$btd_pm
data$y_max <- data$y * data$btd_ls

ggplot(data) +
  geom_errorbar(aes(x = x1, y = y_eff, ymin = y_min, ymax = y_max), width = 0.2) +
  geom_point(aes(x = x1, y = y_eff), color = "blue") +
  geom_line(aes(x = x1, y = yD), color = "red") +
  geom_text(aes(x = x1, y = y, label = rownames(data))) +
  theme_bw()

fis <- tipperGUI2()

