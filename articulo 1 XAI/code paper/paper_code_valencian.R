source("/home/PI/ricardo.gonzalezm/cafee/R/balanced_data.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/compute_scores.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/efficiency_estimation.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/efficiency_scores.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/preprocessing.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/projection.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/simulations.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/training.R")

# ========== #
# Spain 2018 #
# ========== #

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

#############
# PISA 2018 #
#############
load("C:/Users/Ricardo/OneDrive - UMH/Documentos/Cafee/articulo 1 XAI/data_valencia_comunity/firms.RData")
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
target_method <- "BCC"

set.seed(314)
methods <- list (
  # svm
  "svmPoly" = list(
      hyparams = list(
        "degree" = c(1, 2, 3, 4, 5),
        "scale" = c(0.001, 0.1, 1, 10, 100),
        "C" = c(0.001, 0.1, 1, 10, 100)
      )
  ),
  # neuronal network
  "nnet" = list(
    hyparams = list(
      "size" = c(1, 5, 10, 20),
      "decay" = c(0, 0.1, 0.01, 0.001, 0,0001)
      ),
    options = list (
      maxit = 1000
    )
  )
  
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

hold_out <- 0.00
# https://topepo.github.io/caret/train-models-by-tag.html

metric = "Accuracy"

convexity <- TRUE
returns <- "variable"

# save model information
list_method <- list()  

# bucle region
for (i in 1:length(methods)) {
  
  # console information
  print(paste("METODO:", i,  names(methods)[i]))
  print("")
  
  # model result
  final_model <- efficiency_estimation (
    data = data,
    x = x,
    y = y,
    #z = z,
    orientation = orientation,
    trControl = trControl,
    method = methods[i],
    target_method = target_method,
    metric = metric,
    hold_out = hold_out,
    convexity = convexity,
    returns = returns
  )

  # detecting importance variables
  # necesary data to calculate importance in rminer
  train_data <- final_model$final_model[["trainingData"]]
  names(train_data)[1] <- "ClassEfficiency"
  
  dataset_dummy <- model.matrix(ClassEfficiency~ . - 1, data = train_data)
  train_data <- cbind(train_data[1], dataset_dummy)
  
  train_data <- train_data[,c(2:length(train_data),1)]
  class(final_model$final_model$finalModel) <- c("model")
  
  # importance with our model of Caret
  mypred <- function(M, data) {
    return (predict(M, data[-length(data)], type = "prob"))
  }
  
  # Define methods and measures
  methods_SA <- c("1D-SA") # c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
  measures_SA <- c("AAD") #  c("AAD", "gradient", "variance", "range")
  
  levels <- 7
  
  if (names(methods)[i] == "nnet") {
    # with rminer
    m <- fit(
      ClassEfficiency ~.,
      data = train_data,
      model = "mlp",
      scale = "none",
      size = final_model$final_model$bestTune$size,
      decay = final_model$final_model$bestTune$decay
    )
    
    # Calculate the importance for the current method and measure
    importance <- Importance(
      M = m,
      RealL = levels, # Levels
      data = train_data,
      method = methods_SA,
      measure = measures_SA,
      baseline = "mean", # mean, median, with the baseline example (should have the same attribute names as data).
      responses = TRUE,
    )
    
  } else {
    # Calculate the importance for the current method and measure
    importance <- Importance(
      M = final_model$final_model$finalModel,
      RealL = levels, # Levels
      data = train_data, # data
      method = methods_SA,
      measure = measures_SA,
      baseline = "mean", # mean, median, with the baseline example (should have the same attribute names as data).
      responses = TRUE,
      PRED = mypred,
      outindex = length(train_data) # length(train_data)
    )
  }
  
  result_SA <- as.data.frame(t((round(importance$imp, 3))))[, -length(importance$imp)]
  rownames(result_SA) <- NULL
  names(result_SA) <- names(train_data)[-length(train_data)]
  
  # information model
  list <- list()
  
  list[[1]] <- final_model$final_model
  list[[2]] <- final_model$selected_model_metrics
  list[[3]] <- importance
  list[[4]] <- result_SA

  names(list) <- c("finalModel", "metrics", "SA", "imporance")
  
  list_method[[i]] <- list
  
} # end bucle for (methods)  
  
names(list_method) <- names(methods)

# information_region <- list()
# information_region[[1]] <- scores
# information_region[[2]] <- list_method
# 
# 
# names(information_region) <- c("scores","ML_models")
# 
# # names final object
# names(information_region) <- c("scores","ML_models")
#   
# names(information_region[["ML_models"]]) <- names(methods)
#   
# names(information_region[["ML_models"]][1]) <- "model_fit"
# names(information_region[["ML_models"]][2]) <- "metrics_model"
# 
# for(i in 1:length(methods)) {
#   names(information_region[["ML_models"]][[names(methods[i])]]) <- c("model", "importance")
# }
# 
# save(information_region, file = "resultados_art_XAI_def.RData")
# 
# 
# library(Benchmarking)
# 
# scores_sdea <- sdea (
#   X = as.matrix(data[, y]),
#   Y = as.matrix(data[, x]),
#   RTS = "vrs",
#   ORIENTATION = "out"
# )$eff; scores_sdea
# min(scores_sdea)
# scores_sdea <- round(scores_sdea, 3)
# 
# scores <- as.data.frame(cbind(information_region[[1]][["svmPoly"]], information_region[[1]][["nnet"]]))
# names(scores) <- c("svmPoly", "nnet")
#   
# # XAI paper
# result_final <- as.data.frame(
#   matrix(
#     data = NA,
#     ncol = 5,
#     nrow = nrow(data)
#   )
# )
# 
# scores_sdea <- ifelse(scores_sdea == "-Inf", NA, scores_sdea)
# scores_final <- cbind(scores_sdea, scores)
# 
# omit <- which(is.na(scores_final$svmPoly))
# 
# round(cor(x = information_region[[1]][["svmPoly"]][-omit], y = information_region[[1]][["nnet"]][-omit], method = "spearman"), 3)
# round(cor(x = DEA_score, y = information_region[[1]][["nnet"]], method = "spearman"), 3)
# round(cor(x = DEA_score[-omit], y = information_region[[1]][["svmPoly"]][-omit], method = "spearman"), 3)
# 
# DEA_score <- rad_out (
#   tech_xmat = as.matrix(data[, x]),
#   tech_ymat = as.matrix(data[, y]),
#   eval_xmat = as.matrix(data[, x]),
#   eval_ymat = as.matrix(data[, y]),
#   convexity = TRUE,
#   returns = "variable"
# ) 
# 
# entrenamiento_data <- final_model[["final_model"]][["trainingData"]]
# proporcion <- prop.table(table(entrenamiento_data$.outcome)) 
# 
# save.image("Information_R_PISA.RData")

# # ====== #
# # server #
# # ====== #
# 
# file <- paste(DGP, "_", scenario_char, "_", N_char, "_", noise_char, ".RData", sep = "")
# save(simulaciones, file = file)
# 
# file_information <- paste("information_", DGP, "_", scenario_char, "_", N_char, "_", noise_char, ".RData", sep = "")
# save(list_information, file = file_information)