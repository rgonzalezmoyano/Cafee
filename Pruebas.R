devtools::document() # actualizar manuales de ayuda
devtools::load_all() # actualizar el codigo

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

# efficiency orientation
orientation <- "output"

# Parameters for controlling the training process
trControl <- trainControl (
  method = "cv",
  number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = "all"
)

hold_out <- 0.1

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
  "rf" = list (
    mtry = c(1, 2)
    ),
  "earth" = list (
    nprune = c(5, 10, 15, 20, 25),
    degree = c(1)
  )
  # ),
  # "avNNet" = list (
  #   size = c(5, 10, 20),
  #   decay = c(0, 0.001, 0.01, 0.1),
  #   bag = c(TRUE, FALSE)
  # )
)
# https://topepo.github.io/caret/train-models-by-tag.html

metric = "Balanced Accuracy"

# Result
prueba <- efficiency_estimation (
  data = data,
  x = x,
  y = y,
  orientation = orientation,
  trControl = trControl,
  method = methods,
  metric = "Balanced Accuracy",
  hold_out = hold_out
  )

prueba