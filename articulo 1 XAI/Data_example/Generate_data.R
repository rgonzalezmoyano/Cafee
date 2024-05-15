### Example simulation

# libraries
library("ggplot2")

# generate data
set.seed(314)

# Simulated data
data <- reffcy (
  DGP = "cobb_douglas_XnY1",
  parms = list (
    N = 30,
    nX = 1
  )
)

# plot
ggplot() +
  geom_point(data = data, aes(x = x1, y = y))

# library(openxlsx)
# write.xlsx(data, "data_example.xlsx")

# x and y indexes
x <- 1
y <- 2

# import

# different types to label
target_method <- "additive"

set.seed(314)
methods <- list (
  "svmPoly" = list(
    hyparams = list(
      "degree" = c(1, 2, 3),
      "scale" = c( 0.1, 1, 10),
      "C" = c(0.1, 1, 10)
    )
   )
  
  # # svm
  # "svmPoly" = list(
  #     hyparams = list(
  #       "degree" = c(1, 2, 3, 4, 5),
  #       "scale" = c(0.001, 0.1, 1, 10, 100),
  #       "C" = c(0.001, 0.1, 1, 10, 100)
  #     )
  # ),
  # "svmRadial" = list(
  #   hyparams = list(
  #     "sigma" = c(0.01, 0.1, 1, 10, 100),
  #     "C" = c(0.001, 0.1, 1, 10, 100)
  #   )
  # ),
  
  # # random forest
  # "rf" = list (
  #   options = list (
  #     ntree = c(500) # c(100, 500, 1000)
  #   ),
  #   hyparams = list(
  #     mtry = c(4)
  #   )
  # ),
  
  # # neuronal network
  # "nnet" = list(
  #   hyparams = list(
  #     "size" = c(1, 5, 10, 20),
  #     "decay" = c(0, 0.1, 0.01, 0.001, 0,0001)
  #   ),
  #   options = list (
  #     maxit = 1000
  #   )
  #   
  # )
  
)

# =========== #
# score cafee #
# =========== #    

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

# https://topepo.github.io/caret/train-models-by-tag.html

metric = "Accuracy"

convexity <- TRUE

# preProcess
data <- data_2018
# data <- data[1:30, ]
idx_NA <- which(is.na(data$SCHLTYPE))
data <- data[-idx_NA,]

# save scores region
list_region <- list()

# new  dataset of scores result
scores <- matrix (
  ncol = length(methods),
  nrow = nrow(data)
) 

# change to data.frame
scores <- as.data.frame(scores)

# change names
score_names <- names(methods)
names(scores) <- score_names

# save model information
list_method <- list()  
