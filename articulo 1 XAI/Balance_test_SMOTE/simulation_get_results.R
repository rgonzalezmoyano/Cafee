# get metric ML and importance

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
library(openxlsx)


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
)

# =========== #
# score cafee #
# =========== #    

# efficiency orientation
prop_loop <- c(0, 0.2, 0.3, 0.4, 0.5)
prop_inef_loop <- c(0, 4, 3, 2)
grid <- expand.grid(prop_inef_loop = prop_inef_loop, prop_loop = prop_loop)
grid <- grid[-c(2:4),]



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


results <- as.data.frame(matrix(
  data = NA,
  ncol = 19,
  nrow = nrow(grid)
))

results[,1] <- grid$prop_loop
results[,2] <- grid$prop_inef_loop

for (row in 1:nrow(results)) {
  
  balance_data <- c(grid$prop_loop[row], grid$prop_inef_loop[row])
  
  # bucle methods
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
      balance_data = balance_data,
      eff_level = eff_level,
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
    
    # save results
    results[row, 3] <- nrow(final_model$final_model$trainingData)
    results[row, 4:14] <- final_model$selected_model_metrics
    results[row, 15:ncol(results)] <- result_SA
  } # end bucle for (methods)  
  
} # end loop grid

names(results) <- c("eff_%", "make ineff unit each", "number dataset", names(final_model$selected_model_metrics), names(result_SA))
write.xlsx(results, file = "results_NN.xlsx")
#write.xlsx(result_SA, file = "result_NN.xlsx")
