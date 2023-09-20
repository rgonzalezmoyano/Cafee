devtools::document() # actualizar manuales de ayuda
devtools::load_all() # actualizar el codigo

set.seed(314)

# Simulated data
data <- reffcy (
  DGP = "add_scenario_XnY1",
  parms = list (
    N = 100,
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
  savePredictions = "all"
)

methods <- list (
  "svmRadial" = list (
    "C" = seq(1, 100, length.out = 10),
    "sigma" = seq(0, 10, length.out = 20)
    ),
  "rf" = list (
    "mtry" = c(1)
  ),
  "earth" = list (
    nprune = c(5, 10, 15, 20, 25),
    degree = c(1)
  )
)

# https://topepo.github.io/caret/train-models-by-tag.html

# Result
prueba <- efficiency_estimation (
  data = data,
  x = x,
  y = y,
  orientation = orientation,
  trControl = trControl,
  method = methods,
  metric = "Kappa"
  )

prueba

