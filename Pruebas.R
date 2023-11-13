devtools::document() # actualizar manuales de ayuda
devtools::load_all() # actualizar el codigo
library("ggplot2")

#install.packages("MultiplierDEA")


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

# data <- reffcy (
#   DGP = "translog_X2Y2",
#   parms = list (
#     N = 25,
#     border = 0,
#     noise = TRUE
#   )
# )
# 
# x <- 1:2
# y <- 3:4

# efficiency orientation
orientation <- "output"

# metrics for model evaluation
MySummary <- function (data, lev = NULL, model = NULL) {
  
  # accuracy and kappa
  acc_kpp <- defaultSummary(data, lev, model)
  
  # AUC, sensitivity and specificity
  auc_sen_spe <- twoClassSummary(data, lev, model)
  
  # precision and recall
  pre_rec <- prSummary(data, lev, model)
  
  c(acc_kpp, auc_sen_spe, pre_rec)
} 

# Parameters for controlling the training process
trControl <- trainControl (
  method = "cv",
  number = 5,
  summaryFunction = MySummary,
  classProbs = TRUE,
  savePredictions = "all"
)

hold_out <- 0.15

methods <- list (
  # "knn" = list (
  #   k = 5:15
  # ),
  # "gbm" = list (
  #   n.trees = c(50, 100, 150),
  #   interaction.depth = c(1, 2, 3),
  #   shrinkage = c(0.01, 0.1, 0.2),
  #   n.minobsinnode = c(1, 3, 5)
  # ),
  # "svmRadial" = list (
  # C = c(0.01, 0.1, 1, 10),
  #   sigma = c(0.001, 0.01, 0.1, 1)
  #   ),
  "svmPoly" = list(
    "degree" = c(1, 2, 3, 4, 5),
    "scale" = c(0.1, 1, 10),
    "C" = c(0.1, 1, 10, 100)
  )
  # "rf" = list (
  #   mtry = c(1, 2)
  #   ),
  # "earth" = list (
  #   nprune = c(5, 10, 15, 20, 25),
  #   degree = c(1)
  # )
)

# https://topepo.github.io/caret/train-models-by-tag.html

metric = "F1"

# Result
prueba <- efficiency_estimation (
  data = data,
  x = x,
  y = y,
  orientation = orientation,
  trControl = trControl,
  method = methods,
  metric = "F1",
  hold_out = hold_out
  )

prueba

final_model <- prueba

scores <- compute_scores (
  data = data,
  x = x,
  y = y,
  final_model = final_model,
  orientation = orientation
  )

# CrossEfficiency: Cross Efficiency Model
library(MultiplierDEA)

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



prueba <- t(CrossEfficiency$ce_ave)

# DeaR
library(deaR)
datadea <- make_deadata(datadea = data, dmus = NULL, inputs = x, outputs = y)

DeaR <- cross_efficiency(datadea,
                         orientation = "io",
                         rts = "vrs",
                         #selfapp = TRUE,
                         correction = TRUE,
                         M2 = TRUE,
                         M3 = TRUE)

DeaR_contstante <- cross_efficiency(datadea,
                         orientation = "io",
                         rts = "crs",
                         #selfapp = TRUE,
                         correction = TRUE,
                         M2 = TRUE,
                         M3 = TRUE)

m2 <- DeaR[["M2_agg"]][["cross_eff"]]
m2_ben <- DeaR[["M2_ben"]][["cross_eff"]]
m3 <- DeaR[["M3_agg"]][["cross_eff"]]
m3_ben <- DeaR[["M3_ben"]][["cross_eff"]]

m3_crs <- DeaR_contstante[["M3_agg"]][["cross_eff"]]
m3_ben_crs <- DeaR_contstante[["M3_ben"]][["cross_eff"]]

colmeans_m2 <- colMeans(m2) # agresivo
colmeans_m2_ben <- colMeans(m2_ben)
colmeans_m3 <- colMeans(m3)
colmeans_m3_ben <- colMeans(m3_ben)

colmeans_m3_crs <- colMeans(m3_crs) # agresivo
colmeans_m3_ben_crs <- colMeans(m3_ben_crs)


#scores_comparation$MultiplierDEA <- c(CrossEfficiency$ce_ave)
scores_comparation$BCC <- bcc_scores_inp
#scores_comparation$agresivo_m2 <- colmeans_m2
scores_comparation$agresivo <- colmeans_m3
scores_comparation$benevolente <- colmeans_m3_ben
scores_comparation$FDH <- as.vector(fdh_scores_inp)
scores_comparation$CCR <- as.vector(ccr_scores_inp)
# scores_comparation$agresivo_CCR <- as.vector(colmeans_m3_crs)
# scores_comparation$benevolente_CCR <- as.vector(colmeans_m3_ben_crs)

mean(scores_comparation$BCC)
mean(scores_comparation$agresivo)
cor(scores_comparation$agresivo, scores_comparation$BCC)

projections_BCC <- data.frame(
  x1 = data$x1 * scores_comparation$BCC,
  y = data$y
)

projections_agresivo <- data.frame(
  x1 = data$x1 * scores_comparation$agresivo,
  y = data$y
)

# ============= #
# Generate plot # Grafico de dispersión BCC y diferencias agre y ben
# ============= #

data_frame_13_11 <- data.frame(
  diferencias = colmeans_m3_ben - colmeans_m3,
  BCC = bcc_scores_inp
)


ggplot(data = data_frame_13_11) +
  geom_point(aes(x = BCC, y = diferencias))

cor(data_frame_13_11$diferencias, data_frame_13_11$BCC)


# ============= #
# Generate plot #
# ============= #

ggplot(data = scores_comparation) +
  geom_point(aes(x = BCC, y = agresivo)) + 
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))

ggplot() +
  geom_point(data = data, aes(x = x1, y = y)) +
  geom_point(data = projections_BCC, aes(x = x1, y = y), color = "blue") +
  geom_point(data = projections_agresivo, aes(x = x1, y = y), color = "red")

# ============= #
# Generate plot #
# ============= #

# make a grid of the predictors
rng.x <- range(data[1])

if (max(data[3] > max(data[2]))) {
  top <- max(data[3])
} else {
  top <- max(data[2])
}

if (min(data[3] < min(data[2]))) {
  bottom <- min(data[3])
} else {
  bottom <- min(data[2])
}

rng.y <- range(bottom, top)

grid <- expand.grid (
  x1 = seq(rng.x[1], rng.x[2], length = 150),
  y = seq(rng.y[1], rng.y[2], length = 150)
)

# grid <- expand.grid (
#   x1 = seq(1, 6, length = 150),
#   y = seq(1, 8, length = 150)
# )

grid$decision <- predict(final_model, grid, type = "raw")

 i <- 2
# i <- c(12, 13, 15, 16, 17, 18, 23, 24)

scores_r <- scores[scores$score_cafee == 1.095, ]
r <- rownames(scores_r)
filter_ggplot_data <- data[r,]

ggplot(data = data) +
  geom_point(aes(x = x1, y = y)) +
  geom_point(data = grid, aes(x = x1, y = y, color = decision), size = 0.75, alpha = 0.8) +
  scale_color_manual(values = c("not_efficient" = "pink", "efficient" = "lightgreen")) +
  geom_point(data = filter_ggplot_data, aes(x = x1, y = y), colour = "orange", size = 1.5) +
  theme_bw() +
  theme(legend.position = "none")




ggplot(data = data) +
  geom_point(data = grid, aes(x = x1, y = y, color = decision), size = 0.75, alpha = 0.8) +
  geom_line(aes(x = x1, y = yD), linewidth = 1, linetype = "dotted") +
  geom_point(aes(x = x1, y = y), size = 0.75) +
  geom_point(aes(x = x1[i], y = y[i]), colour = "black", size = 3) +
  scale_color_manual(values = c("not_efficient" = "pink", "efficient" = "lightgreen")) +
  #ggtitle(paste("Frontera mejor modelo ", prueba$method, sep = "")) +
  theme_bw() +
  theme(
    axis.title.x = element_text (
      size = 12, face = "bold", color = "#921F30",
      margin = margin(t = 10)),
    axis.title.y = element_text (
      size = 12, face = "bold", color = "#921F30",
      margin = margin(r = 10)),
    axis.text = element_text (
      size = 12, color = "black"),
    plot.margin = unit(c(1.25, 1.25, 1.25, 1.25), "lines"),
    plot.title = element_text (
      size = 12, face = "bold", color = "#921F30",
      margin = margin(b = 10)
    ),
    legend.position = "none"
  )

scores[i]

data <- cbind(data, scores)


ggplot(data = data) +
  geom_text(aes(x = x1, y = y, label = rownames(data))) +
  # geom_line(aes(x = x1, y = y * score_yD), color = "red") +
  # geom_line(aes(x = x1, y = y * score_DEA), color = "green") +
  # geom_line(aes(x = x1, y = y * score_cafee), color = "blue") +
  # geom_line(aes(x = x1, y = front_stoned), color = "orange") +
  theme_bw()

new_data <- new_data[, 1:3]

ggplot(data = new_data) +
  geom_text(aes(x = x1, y = y, color = efficient, label = rownames(data))) +
  # geom_line(aes(x = x1, y = y * score_yD), color = "red") +
  # geom_line(aes(x = x1, y = y * score_DEA), color = "green") +
  # geom_line(aes(x = x1, y = y * score_cafee), color = "blue") +
  # geom_line(aes(x = x1, y = front_stoned), color = "orange") +
  theme_bw()
