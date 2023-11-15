library("deaR")
library("ggplot2")
devtools::load_all()

set.seed(314)

data <- reffcy (
  DGP = "cobb_douglas_XnY1",
  parms = list (
    N = 150,
    nX = 1
  )
)

x <- 1
y <- 2

datadea <- make_deadata (
  datadea = data, 
  dmus = NULL, 
  inputs = x, 
  outputs = y
  )

cross <- cross_efficiency (
  datadea,
  orientation = "io",
  rts = "vrs",
  correction = TRUE,
  M2 = TRUE,
  M3 = TRUE
  )

bcc_scores <- rad_inp (
  tech_xmat = as.matrix(data[, x]),
  tech_ymat = as.matrix(data[, y]),
  eval_xmat = as.matrix(data[, x]),
  eval_ymat = as.matrix(data[, y]),
  convexity = TRUE,
  returns = "variable"
)

resultados <- data

resultados$bcc <- bcc_scores[, 1]
resultados$arb <- cross[["Arbitrary"]][["e"]]
resultados$ben <- cross[["M2_ben"]][["e"]]
resultados$agr <- cross[["M3_agg"]][["e"]]

ggplot(data = resultados) +
  geom_point(aes(x = bcc, y = bcc - arb)) +
  theme_bw()
