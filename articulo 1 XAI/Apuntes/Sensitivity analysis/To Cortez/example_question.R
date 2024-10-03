# libraries 
library("rminer")

# load data
load("train_data.RData")

# m model: SVM
m <- fit(
  ClassEfficiency~.,
  data = train_data,
  model = "ksvm",
  kernel = "polydot",
  scale = "none",
  kpar = list(
    degree = 3,
    scale = 0.1
  ),
  C = 10
)

# importance variables
importance <- Importance(
  M = m,
  RealL = 7, # Levels
  data = train_data, # data
  method = "GSA",
  measure = "AAD",
  interactions = 2:5,
  responses = TRUE
)

importance$value
importance$imp
