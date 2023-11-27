devtools::document() # actualizar manuales de ayuda
devtools::load_all() # actualizar el codigo
library("ggplot2")

set.seed(314)

# Simulated data
data <- reffcy (
  DGP = "add_scenario_XnY1",
  parms = list (
    N = 50,
    scenario = "A"
  )
)

x <- 1
y <- 2

# compute random error
random_error <- rnorm(n = 50, mean = 0, sd = 0.02)

# compute new vector of outputs with random error
data[, y] <- data[, y] * exp(random_error)

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
  "svmPoly" = list(
    "degree" = c(1, 2, 3, 4, 5),
    "scale" = c(0.1, 1, 10),
    "C" = c(0.1, 1, 10, 100)
  ),
  "rf" = list (
    mtry = c(1, 2)
    ),
  "earth" = list (
    nprune = c(5, 10, 15, 20, 25),
    degree = c(1)
  )
)


metric = "F1"
target_method <- "bootstrapping_dea"

devtools::load_all()

# Result
final_model <- efficiency_estimation (
  data = data,
  x = x,
  y = y,
  orientation = orientation,
  target_method = target_method,
  trControl = trControl,
  method = methods,
  metric = metric,
  hold_out = hold_out
)

grid <- expand.grid (
  x1 = seq(min(data$x1), max(data$x1), length = 100),
   y = seq(min(data$y), max(data$y), length = 100)
)

prob <- round(predict(final_model, grid, type = "prob")[1], 2)
grid$prob <- prob$efficient

ggplot() +
  geom_tile(data = grid, aes(x = x1, y = y, fill = prob)) +
  geom_text(data = data, aes(x = x1, y = y, label = rownames(data))) +
  geom_line(data = data, aes(x = x1, y = yD), color = "red") +
  scale_fill_gradientn (
    colours = c("pink", "white", "lightgreen"),
    values = scales::rescale(c(0, final_model[["cut_off"]], 1))
    ) +
  theme_bw() +
  theme(panel.background = element_blank())


ggplot() +
  geom_point(data = data, aes(x = x1, y = y, color = class_efficiency)) +
  # geom_point(data = eval_data, aes(x = x1, y = y)) +
  theme_bw()
