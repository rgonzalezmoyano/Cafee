devtools::document() # actualizar manuales de ayuda
devtools::load_all() # actualizar el codigo

set.seed(314)

# Simulated data
N <- 100
scenario <- "A"
data <- AddScenario(N, scenario) # x1, y, yD

x <- 1
y <- 2

# Optimization orientation
orientation <- "output"

# Data train
trControl <- list (
  method = "cv",
  number = 5 # Either the number of folds or number of resampling iterations
  )

methods <- list (
  "svmLinear" = list (
    "C" = c (
      seq(0, 100, length.out = 10), 
      seq(200, 1000, length.out = 10)
      )
    ),
  "svmRadial" = list (
    "C" = c (
      seq(0, 100, length.out = 10), 
      seq(200, 1000, length.out = 10)
      ),
    "sigma" = seq(0, 10, length.out = 20)
    ),
  "svmPoly" = list (
    "degree" = c(2, 3, 4),
    "scale" = c(0.0001, 0.001, 0.01, 0.1, 1),
    "C" = c (
      seq(0, 100, length.out = 10), 
      seq(200, 1000, length.out = 10)
      )
    )
  )
# https://topepo.github.io/caret/train-models-by-tag.html

# Result
prueba <- DEA.Classifier(data, DMUs, x, y, trControl, methods, orientation);prueba