# source("/home/PI/ricardo.gonzalezm/cafee/R/balanced_data.R")
# source("/home/PI/ricardo.gonzalezm/cafee/R/compute_scores.R")
# source("/home/PI/ricardo.gonzalezm/cafee/R/efficiency_estimation.R")
# source("/home/PI/ricardo.gonzalezm/cafee/R/efficiency_scores.R")
# source("/home/PI/ricardo.gonzalezm/cafee/R/preprocessing.R")
# source("/home/PI/ricardo.gonzalezm/cafee/R/projection.R")
# source("/home/PI/ricardo.gonzalezm/cafee/R/simulations.R")
# source("/home/PI/ricardo.gonzalezm/cafee/R/training.R")

# ============================= #
# valencian comunity  companies #
# ============================= #

# ===
# libraries
# ===
devtools::load_all()
library(caret)
library(Benchmarking)
library(magrittr)
library(dplyr)
library(deaR)
library(haven)
library(e1071)
library(rminer)

# ===
# load data
# ===

# ======================= #
# Valencian Comunity 2018 #
# ======================= #

load("C:/Users/Ricardo/OneDrive - UMH/Documentos/Cafee/articulo 1 XAI/data_valencia_comunity/firms.RData")
#load("C:/Users/Ricardo/Documents/Doctorado EOMA/Cafee/articulo 1 XAI/data_valencia_comunity/firms.RData")
data <- firms

# save a copy
data_original <- data

# make changes realted to class
data <- change_class(data = data, to_factor = c(5,6))

# filter to valencian comunity
data <- data[data$autonomous_community == "Comunidad Valenciana",]

# ===
# Information to cafee
# ===

# x and y indexes
x <- c(9:12)
y <- c(8)
#z <- c(2, 8) # environment variables

# different types to label
target_method <- "additive"

seed <- 0

print(seed)
set.seed(seed)

methods <- list (
  # neuronal network
  "nnet" = list(
    hyparams = list(
      "size" = c(1, 5, 10, 15, 20, 30),
      "decay" = c(0, 0.1, 0.01, 0.001, 0,0001)
    ),
    options = list (
      maxit = 1000,
      softmax = TRUE
    )
  )
  # # svm
  # "svmPoly" = list(
  #   hyparams = list(
  #     "degree" = c(1, 2, 3, 4, 5),
  #     "scale" = c(0.001, 0.1, 1, 10, 100),
  #     "C" = c(0.001, 0.1, 1, 10, 100)
  #   )
  # )
  
  # # svm
  # "svmPoly" = list(
  #     hyparams = list(
  #       "degree" = c(5), # 5
  #       "scale" = c(0.1),# 0.1
  #       "C" = c(10) # 10
  #     )
  # ),
  # # neuronal network
  # "nnet" = list(
  #   hyparams = list(
  #     "size" = c(20),
  #     "decay" = c(1)
  #     ),
  #   options = list (
  #     maxit = 1000,
  #     softmax = TRUE
  #   )
  # )
  
)

# =========== #
# score cafee #
# =========== #    

# efficiency orientation
convexity <- TRUE
returns <- "variable"

# SMOTE proportions
balance_data <- c(0.4, 4)

# ML metric
metric = "F"

# scenarios to peer
scenarios <- seq(0.75, 0.95, 0.1)

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

# parameters for controlling the training process
trControl <- trainControl (
  method = "cv",
  number = 5,
  summaryFunction = MySummary,
  classProbs = TRUE,
  savePredictions = "all"
)

hold_out <- 0.00 # https://topepo.github.io/caret/train-models-by-tag.html

# save model information
list_method <- list()  

# loop method
for (i in 1:length(methods)) {
  
  # for (a in 1:nrow(balance_data)) {
  #   
  # }
  # console information
  print(paste("METODO:", i,  names(methods)[i]))
  print("")
  
  # model result
  final_model <- efficiency_estimation (
    data = data,
    x = x,
    y = y,
    #z = z,
    balance_data = balance_data,
    trControl = trControl,
    method = methods[i],
    target_method = target_method,
    metric = metric,
    hold_out = hold_out,
    scenarios = scenarios
  )
  
  list_method[[i]] <- final_model
  
} # end bucle for (methods)  

names(list_method) <- names(methods)

# save(list_method, file = "resultados_art_XAI_NN_CV.RData")
#
library(openxlsx)
#write.xlsx(list_method$nnet$metrics, file = "metrics_NN.xlsx")
# write.xlsx(list_method$svmPoly$metrics, file = "metrics_SVM.xlsx")
# 
# 
# 
# data_complete_NN <- cbind(data[, c(x,y)], list_method[["nnet"]][["data_contrafactual"]])
# data_complete_SVM <- cbind(data[, c(x,y)], list_method[["svmPoly"]][["data_contrafactual"]])
# 
# write.xlsx(data_complete_NN, file = "data_complete_NN.xlsx")
# write.xlsx(data_complete_SVM, file = "data_complete_SVM.xlsx")
# 
# write.xlsx(list_method[["svmPoly"]][["resume_metrics"]], file = "statistics_metrics_SVM.xlsx")
# write.xlsx(list_method[["nnet"]][["resume_metrics"]], file = "statistics_metrics_NN.xlsx")

# 
# # get
# 
# # Columnas en las que quieres contar los negativos
# columnas_interes <- c("total_assets", "employees", "fixed_assets", "personal_expenses", "operating_income")
# 
# # Función para contar negativos en columnas específicas de un data.frame
# contar_negativos <- function(df, columnas) {
#   # Asegurarse de que las columnas existen en el data.frame
#   columnas <- columnas[columnas %in% names(df)]
#   # Contar los valores negativos en cada columna especificada
#   sapply(df[columnas], function(col) sum(col < 0, na.rm = TRUE))
# }
# 
# resultado_SVM <- lapply(list_method$svmPoly$data_contrafactual, contar_negativos, columnas = columnas_interes); resultado_SVM
# resultado_NN <- lapply(list_method$nnet$data_contrafactual, contar_negativos, columnas = columnas_interes); resultado_NN




# ## train NN KERAS
# # load keras library and others
# library(keras)
# library(tidyverse)
# 
# k_data <- list_method[["svmPoly"]][["finalModel"]][["trainingData"]]
# nrow(k_data)
# 
# names(k_data)[1] <- "ClassEfficiency"
# 
# k_data <- k_data[,c(2:length(k_data), 1)]
# 
# k_data$ClassEfficiency <- as.numeric(k_data$ClassEfficiency)
# # 1 efficient; 2 #ineficient
# 
# k_data$ClassEfficiency <- k_data$ClassEfficiency - 1
# 
# k_folds <- createFolds(k_data$ClassEfficiency, k = trControl$number)
# 
# k_x <- 1:length(x)
# k_y <- (length(x) + 1):(length(x) + length(y))
# 
# 
# 
# fold <- 1
# for (fold in 1:length(k_folds)) {
#   
#   # dataset of CV
#   index_fold <- k_folds[[fold]]
# 
#   # # separating into train and test
#   # index <- sample(2, nrow(k_data_cv), replace = TRUE, prob = c(train_threshold, 1 - train_threshold))
#   
#   x_train <- as.matrix(k_data[-index_fold, c(k_x, k_y)])
#   y_train <- k_data[-index_fold, max(k_y) + 1]
# 
#   x_test <- as.matrix(k_data[index_fold, c(k_x, k_y)])
#   y_test <- k_data[index_fold, max(k_y) + 1]
#   
#   # save predictions to create confusion matrix
#   y_test01 <- y_test 
#   
#   y_train <- to_categorical(y_train, 2) #4 categorias
#   y_test <- to_categorical(y_test, 2)
# 
#   
# }







# # ====== #
# # server #
# # ====== #
# 
# file <- paste(DGP, "_", scenario_char, "_", N_char, "_", noise_char, ".RData", sep = "")
# save(simulaciones, file = file)
# 
# file_information <- paste("information_", DGP, "_", scenario_char, "_", N_char, "_", noise_char, ".RData", sep = "")
# save(list_information, file = file_information)