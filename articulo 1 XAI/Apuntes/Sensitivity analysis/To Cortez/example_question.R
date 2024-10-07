# libraries 
library("rminer")

# load data
load("train_data.RData")

tr=train_data[,c(2:5,1)]

# m model: SVM
m <- fit(
  ClassEfficiency~.,
  data = tr,
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

I2 = Importance(
  m2,
  tr,
  method = "GSA",
  interactions=1:4
)

cm2=agg_matrix_imp(I2)

print(round(cm2$m1,digits=2))
print(round(cm2$m2,digits=2))
fcm2=cmatrixplot(cm2,threshold=0.1) 