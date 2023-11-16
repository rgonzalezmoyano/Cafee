devtools::document() # actualizar manuales de ayuda
devtools::load_all() # actualizar el codigo
library("ggplot2")
library("deaR")

set.seed(314)

# Simulated data
data <- reffcy (
  DGP = "cobb_douglas_XnY1",
  parms = list (
    N = 50,
    nX = 1
  )
)

ggplot(data = data) +
  geom_point(aes(x = x1, y = y)) +
  geom_line(aes(x = x1, y = yD, color = "red")) +
  theme_bw()

scores_comparation <- data.frame(
  BCC = rep(NA, 50),
  agresivo = rep(NA, 50)
)

tech_xmat <- as.matrix(data[, x])
tech_ymat <- as.matrix(data[, y])
eval_xmat <- as.matrix(data[, x])
eval_ymat <- as.matrix(data[, y])

bcc_scores <- rad_out (
  tech_xmat = tech_xmat,
  tech_ymat = tech_ymat,
  eval_xmat = eval_xmat,
  eval_ymat = eval_ymat,
  convexity = TRUE,
  returns = "variable"
)

bcc_scores_inp <- rad_inp (
  tech_xmat = as.matrix(data[, x]),
  tech_ymat = as.matrix(data[, y]),
  eval_xmat = as.matrix(data[, x]),
  eval_ymat = as.matrix(data[, y]),
  convexity = TRUE,
  returns = "variable"
) 

fdh_scores_inp <- rad_inp (
  tech_xmat = as.matrix(data[, x]),
  tech_ymat = as.matrix(data[, y]),
  eval_xmat = as.matrix(data[, x]),
  eval_ymat = as.matrix(data[, y]),
  convexity = FALSE,
  returns = "variable"
) 

ccr_scores_inp <- rad_inp (
  tech_xmat = as.matrix(data[, x]),
  tech_ymat = as.matrix(data[, y]),
  eval_xmat = as.matrix(data[, x]),
  eval_ymat = as.matrix(data[, y]),
  convexity = FALSE,
  returns = "constant"
) 


CrossEfficiency <- CrossEfficiency(x = data.frame(data$x1),
                                   y = data.frame(data$y),
                                   rts = "vrs",
                                   orientation = "input")