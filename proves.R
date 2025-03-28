devtools::document() # actualizar manuales de ayuda
devtools::load_all() # actualizar el codigo
library("ggplot2")

set.seed(314)

# Simulated data
data <- reffcy (
  DGP = "cobb_douglas_XnY1",
  parms = list (
    N = 150,
    nX = 1
  )
)

copy_data <- data

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

hold_out <- 0.10

methods <- list (
  "knn" = list (
    k = 5:15
  ),
  "gbm" = list (
    n.trees = c(50, 100, 150),
    interaction.depth = c(1, 2, 3),
    shrinkage = c(0.01, 0.1, 0.2),
    n.minobsinnode = c(1, 3, 5)
  ),
  "svmRadial" = list (
    C = c(0.01, 0.1, 1, 10),
    sigma = c(0.001, 0.01, 0.1, 1)
    ),
  "svmPoly" = list (
    "degree" = c(1, 2, 3, 4, 5),
    "scale" = c(0.1, 1, 10),
    "C" = c(0.1, 1, 10, 100, 1000)
  ),
  "rf" = list (
    mtry = c(1, 2)
    ),
  "earth" = list (
    nprune = c(5, 10, 15, 20, 25),
    degree = c(1)
  )
)

# https://topepo.github.io/caret/train-models-by-tag.html

metric = "F1"
target_method <- c("additive") #, "bootstrapping_dea") # bootstrapping_dea additive

# Result
final_model <- efficiency_estimation (
  data = data,
  x = x,
  y = y,
  z <- 0,
  orientation = orientation,
  trControl = trControl,
  method = methods,
  target_method = target_method,
  metric = metric,
  hold_out = hold_out,
  convexity = TRUE
)

data$prob <- round(predict(final_model, data[, 1:2], type = "prob")$efficient, 2)

scores <- compute_scores (
  data = data,
  x = x,
  y = y,
  final_model = final_model,
  orientation = orientation
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

ggplot(data = copy_data) +
  geom_point(aes(x = x1, y = y)) +
  #geom_text(aes(x = x1, y = y, label = rownames(data))) +
  theme_bw()

ggplot(data = data) +
  geom_point(aes(x = x1, y = y, color = class_efficiency)) +
  geom_line(data = copy_data, aes(x = x1, y = yD), linewidth = 1, linetype = "dotted") +
  #geom_text(aes(x = x1, y = y, label = rownames(data))) +
  theme_bw()
  


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
copy_data <- data
rng.x <- range(copy_data[1])

if (max(copy_data[3] > max(copy_data[2]))) {
  top <- max(copy_data[3])
} else {
  top <- max(copy_data[2])
}

if (min(copy_data[3] < min(copy_data[2]))) {
  bottom <- min(copy_data[3])
} else {
  bottom <- min(copy_data[2])
}

rng.y <- range(bottom, top)

grid <- expand.grid (
  x1 = seq(rng.x[1], rng.x[2], length = 300),
  y = seq(rng.y[1], rng.y[2], length = 300)
)

# grid <- expand.grid (
#   x1 = seq(1, 6, length = 150),
#   y = seq(1, 8, length = 150)
# )

grid$decision <- predict(final_model, grid, type = "raw")

# i <- 2
# i <- c(12, 13, 15, 16, 17, 18, 23, 24)

# scores_r <- scores[scores$score_cafee == 1.095, ]
# r <- rownames(scores_r)
# filter_ggplot_data <- data[r,]

ggplot(data = data) +
  geom_point(data = grid, aes(x = x1, y = y, color = decision), size = 0.6, alpha = 0.8) +
  geom_point(aes(x = x1, y = y)) +
  scale_color_manual(values = c("not_efficient" = "pink", "efficient" = "lightgreen")) +
  geom_line(data = copy_data, aes(x = x1, y = yD), linewidth = 1, linetype = "dotted") +
  theme_bw() +
  theme(legend.position = "none")

index_SV <- final_model[["finalModel"]]@SVindex
SV <- data[index_SV, ]

# grafico de SV
ggplot(data = SV) +
  geom_point(data = grid, aes(x = x1, y = y, color = decision), size = 0.75, alpha = 0.8) +
  geom_point(aes(x = x1, y = y), size = 1.6) +
  geom_point(data = data, aes(x = x1, y = y), alpha = 0.2) +
  scale_color_manual(values = c("not_efficient" = "pink", "efficient" = "lightgreen")) +
  geom_line(data = copy_data, aes(x = x1, y = yD), linewidth = 1, linetype = "dotted") +
  theme_bw() +
  theme(legend.position = "none")




ggplot(data = data) +
  geom_point(data = grid, aes(x = x1, y = y, color = decision), size = 0.75, alpha = 0.7) +
  geom_line(aes(x = x1, y = yD), linewidth = 1, linetype = "dotted") +
  geom_point(aes(x = x1, y = y), size = 2) +
  #geom_point(aes(x = x1[i], y = y[i]), colour = "black", size = 3) +
  scale_color_manual(values = c("not_efficient" = "red", "efficient" = "lightgreen")) +
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
