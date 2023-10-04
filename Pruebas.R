devtools::document() # actualizar manuales de ayuda
devtools::load_all() # actualizar el codigo

set.seed(314)

# Simulated data
data <- reffcy (
  DGP = "cobb_douglas_XnY1",
  parms = list (
    N = 30,
    nX = 1
  )
)

x <- 1
y <- 2

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
    "degree" = c(2, 3, 4),
    "scale" = c(0.0001, 0.001, 0.01, 0.1, 1),
    "C" = c(seq(0, 100, length.out = 10), seq(200, 1000, length.out = 3))
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
