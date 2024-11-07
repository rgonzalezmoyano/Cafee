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

#############
# PISA 2018 #
#############
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
seed <- 12
repeat {
  
  print(seed)
  set.seed(seed) # 7 en nnet, 3 muy pocos; 
  
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
  
  need_rep <- FALSE
  
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
      m <- rminer::fit(
        ClassEfficiency ~.,
        data = train_data,
        model = "mlp",
        scale = "none",
        size = final_model$final_model$bestTune$size,
        decay = final_model$final_model$bestTune$decay
        #entropy = FALSE
        #softmax = TRUE
      )
      
      # Calculate the importance for the current method and measure
      importance <- Importance(
        M = m,
        RealL = levels, # Levels
        data = train_data,
        method = methods_SA,
        measure = measures_SA,
        baseline = "mean", # mean, median, with the baseline example (should have the same attribute names as data).
        responses = TRUE
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
    
    if (names(methods)[i] == "nnet") {
      final_model_p <- final_model$final_model
    } else {
      final_model_p <- final_model$final_model
    }
    
    print(paste("Inputs importance: ",sum(result_SA[1:length(x)])))
    print(paste("Outputs importance: ",sum(result_SA[(length(x)+1):(length(x)+length(y))])))
    print(seed)
    
    # Calcular probabilidad de eficiencia para cada fila
    eff_vector <- apply(data[, c(x,y)], 1, function(row) {
      
      row_df <- as.data.frame(t(row))
      colnames(row_df) <- names(data[, c(x,y)])
      
      pred <- unlist(predict(final_model_p, row_df, type = "prob")[1])
      
      return(pred)
    })
    
    idx_max_eff <- which.max(eff_vector)
    max_eff <- unname(eff_vector[idx_max_eff])
    
    # to get probabilities senarios
    scenarios <- seq(0.75, 0.95, 0.1)
    scenarios <- c(scenarios, max_eff) 
    
    data_list <- list() # all results have scenarios[e] probability
    data_real_list <- list()
    data_beta <- list()
    metrics_list <- list()
    peer_list <- list()
    
    for (e in 1:length(scenarios)) {
      
      # new x and y in data_scenario
      x_target <- 1:length(x)
      y_target <- (length(x)+1):(length(x)+length(y))
      
      data_scenario <- compute_target (
        data = data[, c(x,y)],
        x = x_target,
        y = y_target,
        #z = z,
        final_model = final_model_p,
        cut_off = scenarios[e],
        imp_vector = result_SA
      )
      
      if(any(data_scenario$data_scenario[, c(x_target,y_target)] < 0 | any(is.na(data_scenario$betas)))) {
        seed <- seed + 1
        need_rep <- TRUE
        break
      }
      
      # determinate peer
      # first, determinate efficient units
      idx_eff <- which(data_scenario$betas <= 0)
      
      # save distances structure
      save_dist <- matrix(
        data = NA,
        ncol = length(idx_eff),
        nrow = nrow(data)
      )
      
      # calculate distances
      for (unit_eff in idx_eff) {
        # set reference
        reference <- data[unit_eff, c(x,y)]
        
        distance <- unname(apply(data[, c(x,y)], 1, function(x) sqrt(sum((x - reference)^2))))
        
        # get position in save results
        idx_dis <- which(idx_eff == unit_eff)
        save_dist[,idx_dis] <- as.matrix(distance)
      }
      
      # # change to dataframe
      # save_dist <- as.data.frame(save_dist)
      # names(save_dist) <- idx_eff
      
      near_idx_eff <- apply(save_dist, 1, function(row) {
        
        which.min(abs(row))
      
        })
      
      peer_restult <- matrix(
        data = NA,
        ncol = 1,
        nrow = nrow(data)
      )
      
      peer_restult[, 1] <- idx_eff[near_idx_eff]
      
      # change data to not worst the efficeint units
      data_real <- data_scenario$data_scenario
      
      data_real[idx_eff,] <- data[idx_eff, c(x,y)]
      
      # save data_scenario
      data_list[[e]] <- data_scenario$data_scenario
      
      # save data real
      data_real_list[[e]] <- data_real
      
      # save beta
      data_beta[[e]] <- data_scenario$betas
      
      #save_peer
      peer_list[[e]] <- peer_restult
      
      # metrics: mean, median, sd
      main_metrics <- as.data.frame(matrix(
        data = NA,
        ncol = ncol(data[,c(x,y)]),
        nrow = 3
      ))
      # CHANGE?
      main_metrics[1,] <- apply(data_real, 2, mean, na.rm = TRUE)
      main_metrics[2,] <- apply(data_real, 2, median, na.rm = TRUE)
      main_metrics[3,] <- apply(data_real, 2, sd, na.rm = TRUE)
      
      names(main_metrics) <- names(data_scenario$data_scenario)
      row.names(main_metrics) <- c("mean", "median", "sd")
      
      metrics_list[[e]] <- main_metrics
      
    }
    
    if (need_rep == TRUE) {
      break
    }
    
    names(data_list) <- scenarios
    names(data_real_list) <- scenarios
    names(data_beta) <- scenarios
    names(metrics_list) <- scenarios
    names(peer_list) <- scenarios
    
    # information model
    list <- list()
    
    list[[1]] <- final_model$final_model
    list[[2]] <- final_model$selected_model_metrics
    list[[3]] <- importance
    list[[4]] <- result_SA
    list[[5]] <- data_list
    list[[6]] <- data_real_list
    list[[7]] <- peer_list
    list[[8]] <- data_beta
    list[[9]] <- metrics_list
    list[[10]] <- seed
    list[[11]] <- c(unname(idx_max_eff), max_eff)
    
    names(list) <- c("finalModel", "metrics", "SA", "imporance", "data_contrafactual", "data_real", "peer", "beta", "resume_metrics", "seed", "most_eff")
    
    list_method[[i]] <- list
    
  } # end bucle for (methods)  
  
  if (need_rep == FALSE) {
    break
  }
  
}

names(list_method) <- names(methods)

save(list_method, file = "resultados_art_XAI_NN_CV.RData")
#
# library(openxlsx)
# write.xlsx(list_method$nnet$metrics, file = "metrics_NN.xlsx")
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


# get

# Columnas en las que quieres contar los negativos
columnas_interes <- c("total_assets", "employees", "fixed_assets", "personal_expenses", "operating_income")

# Función para contar negativos en columnas específicas de un data.frame
contar_negativos <- function(df, columnas) {
  # Asegurarse de que las columnas existen en el data.frame
  columnas <- columnas[columnas %in% names(df)]
  # Contar los valores negativos en cada columna especificada
  sapply(df[columnas], function(col) sum(col < 0, na.rm = TRUE))
}

resultado_SVM <- lapply(list_method$svmPoly$data_contrafactual, contar_negativos, columnas = columnas_interes); resultado_SVM
resultado_NN <- lapply(list_method$nnet$data_contrafactual, contar_negativos, columnas = columnas_interes); resultado_NN




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