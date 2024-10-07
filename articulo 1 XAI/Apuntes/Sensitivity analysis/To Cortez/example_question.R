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

# m is a fitted model, svm.
# train_data is the data, class, 4 inputs.
tr = train_data[,c(2:5,1)]
m2 = fit(
  ClassEfficiency~.,
  tr,model="ksvm",
  kernel="polydot",
  scale="none",
  kpar=list(
    degree = 3,
    scale = 0.1),
  C = 10
  )

I2 = Importance(m2, tr, method = "GSA", interactions = 1:4)
cm2 = agg_matrix_imp(I2)

agg_matrix_imp(
  I2,
  INP = I2$inputs,
  measure=I2$measure,
  Aggregation = I2$agg,
  method = I2$method,
  outdata = NULL,
  L = I2$Llevels,
  ameth = "xy",
  Tolerance = 0.1
  )

aggregate_imp()
print("show Table 8 DSA results (from the reference):")
print(round(cm2$m1, digits=2))
print(round(cm2$m2, digits=2))

fcm2=cmatrixplot(cm2,threshold=0.1) 

# the graph shows the interactions between 2 inputs, the darker, the stronger they are.

























# importance variables
importance <- Importance(
  M = m,
  RealL = 7, # Levels
  data = tr, # data
  method = "GSA",
  measure = "AAD",
  interactions = 1:4,
  responses = TRUE
)

importance$value
importance$imp
agg_matrix_imp(importance)

