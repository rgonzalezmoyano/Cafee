#New code paper1 valencian example
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
library(fastDummies)
library(keras)
#library(MLmetrics)


# ===
# load data
# ===
# ======================= #
# Valencian Comunity 2018 #
# ======================= #

#load("C:/Users/Ricardo/OneDrive - UMH/Documentos/Cafee/articulo 1 XAI/data_valencia_comunity/firms.RData")
load("C:/Users/Ricardo/Documents/Doctorado EOMA/Cafee/articulo 1 XAI/data_valencia_comunity/firms.RData")
data <- firms

# save a copy
data_original <- data

# # make changes realted to class
# data <- change_class(data = data, to_factor = c(5,6))

# filter to valencian comunity
data <- data[data$autonomous_community == "Comunidad Valenciana",]

# ===
# Information to cafee
# ===

# x and y indexes
x <- c(9:12)
y <- c(8)
z <- NULL

# different types to label
# target_method <- "additive"
# convexity = TRUE
# returns = "variable"

# Step 1: label data
label_efficiency <- label_efficiency(
  data = data,
  x = x,
  y = y
)

sum(label_efficiency$data_proportions$n_efficient)

# training phase
# create a validation dataset
# Create train and validation data
hold_out <- 0.15 # https://topepo.github.io/caret/train-models-by-tag.html

# set seed 
seed <- 0

# Crear índice de validación
valid_index <- createDataPartition(
  label_efficiency[["data_labeled"]]$class_efficiency,
  p = hold_out,
  list = FALSE
)

set.seed(0)

# Dividir dataset en entrenamiento y validación
valid_data <- label_efficiency[["data_labeled"]][valid_index, ]
train_data <- label_efficiency[["data_labeled"]][-valid_index, ]
new_x <- label_efficiency[["index"]][["x"]]
new_y <- label_efficiency[["index"]][["y"]]
new_z <- label_efficiency[["index"]][["z"]]


prop.table(table(train_data$class_efficiency))

# addresing imbalance
balance <- c(seq(0.20, 0.5, 0.05 )) # c(NA, 0.2, 0.3, 0.4, 0.5)

train_data_SMOTE <- SMOTE_data(
  data = train_data,
  x = new_x,
  y = new_y,
  z = new_z,
  balance_data = balance
)


copy_train_data <- train_data
copy_train_data_SMOTE <- train_data_SMOTE
copy_valid_data <- valid_data
save_data <- list(copy_train_data, copy_train_data_SMOTE, copy_valid_data, label_efficiency, valid_index)
#save(save_data, file = "save_data_valencian_2.Rdata")


# save results
# Definir los nombres de las columnas
column_names <- c("imbalance", "size", "decay")

overall_names <- c("Accuracy", "Kappa", "AccuracyLower", "AccuracyUpper",
                   "AccuracyNull", "AccuracyPValue", "McnemarPValue")

byClass_names <- c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value",
                   "Precision", "Recall", "F1", "Prevalence", "Detection Rate",
                   "Detection Prevalence", "Balanced Accuracy")

result_names <- c(column_names, overall_names, byClass_names) 

# Crear un dataframe vacío con esas columnas
df_hyperparams <- data.frame(matrix(ncol = length(result_names), nrow = 0))
colnames(df_hyperparams) <- result_names

# Options for fit the model
# metrics for model evaluation
MySummary <- function (data, lev = NULL, model = NULL) {
  
  # accuracy and kappa
  acc_kpp <- defaultSummary(data, lev, model)
  
  # AUC, sensitivity and specificity
  auc_sen_spe <- twoClassSummary(data, lev, model)
  
  # precision and recall
  pre_rec <- prSummary(data, lev, model)
  
  # Balanced Accuracy: (Sensitivity + Specificity) / 2
  bal_acc <- (auc_sen_spe["Sens"] + auc_sen_spe["Spec"]) / 2

  
  # Combinar todas las métricas en un solo vector
  c(acc_kpp, auc_sen_spe, pre_rec, Balanced_Accuracy = bal_acc)
  
} 

# parameters for controlling the training process
trControl <- trainControl (
  method = "cv",
  number = 5,
  summaryFunction = MySummary,
  classProbs = TRUE,
  savePredictions = "all"
)

# grid
grid <- expand.grid(
  size = c(1, 5, 10, 15, 20, 30),  
  decay = c(0.1, 0.01, 0.001, 0.0001) 
)

save_models <- list()

for(balance_i in balance) {
  print(balance_i)
  # train model
  model_nnet <- train(
    class_efficiency ~ .,               
    data = train_data_SMOTE[[as.character(balance_i)]],               
    method = "nnet",           
    trControl = trControl,     
    tuneGrid = grid,            
    metric = "Balance_Accuracy",            
    maxit = 200,                
    trace = FALSE              
  )
  
  # metrics of tunig
  new_df_hy <- model_nnet[["results"]][, -c(12:24)]
  
  # add balance accuracy
  new_df_hy$Balance_accuracy <- (new_df_hy$Sens + new_df_hy$Precision)/2
  
  # sort
  new_df_hy <- new_df_hy %>%
    arrange(desc(Balance_accuracy), desc("F"), desc(Precision), desc(Sens), desc(AUC))
  
  # the best
  best_model_train <- new_df_hy[1,]
  
  # best grid
  best_tuning <- new_df_hy[1,1:2]
  
  best_model_nnet <- train(
    class_efficiency ~ .,               
    data = train_data_SMOTE[[as.character(balance_i)]],               
    method = "nnet",           
    trControl = trainControl(method = "none", classProbs = TRUE),     
    tuneGrid = best_tuning,            
    metric = "Balance_Accuracy",            
    maxit = 200,                
    trace = FALSE              
  )
  
  save_models[[as.character(balance_i)]] <- best_model_nnet
  
  y_hat <- predict(best_model_nnet, valid_data)
  y_obs <- valid_data$class_efficiency
  
  confusion_matrix <-  confusionMatrix (
    data = y_hat,
    reference = y_obs,
    mode = "everything",
    positive = "efficient"
  )
  
  df_hyperparams <- rbind(df_hyperparams, data.frame(
    imbalance = balance_i,
    size  = best_tuning[1],
    decay = best_tuning[2],
    Accuracy = confusion_matrix$overall["Accuracy"],
    Kappa = confusion_matrix$overall["Kappa"],
    AccuracyLower = confusion_matrix$overall["AccuracyLower"],
    AccuracyUpper = confusion_matrix$overall["AccuracyUpper"],
    AccuracyNull = confusion_matrix$overall["AccuracyNull"],
    AccuracyPValue = confusion_matrix$overall["AccuracyPValue"],
    McnemarPValue = confusion_matrix$overall["McnemarPValue"],
    Sensitivity = confusion_matrix$byClass["Sensitivity"],
    Specificity = confusion_matrix$byClass["Specificity"],
    Pos_Pred_Value = confusion_matrix$byClass["Pos Pred Value"], 
    Neg_Pred_Value = confusion_matrix$byClass["Neg Pred Value"],
    Precision = confusion_matrix$byClass["Precision"],
    Recall = confusion_matrix$byClass["Recall"],
    F1 = confusion_matrix$byClass["F1"],
    Prevalence = confusion_matrix$byClass["Prevalence"],
    Detection_Rate = confusion_matrix$byClass["Detection Rate"],
    Detection_Prevalence = confusion_matrix$byClass["Detection Prevalence"],
    Balanced_Accuracy = confusion_matrix$byClass["Balanced Accuracy"]
  ))
  
}


df_hyperparams

# sort
df_hyperparams <- df_hyperparams %>%
  arrange(desc(Balanced_Accuracy), desc(F1), desc(Precision), desc(Sensitivity))

View(df_hyperparams)

final_grid <- df_hyperparams[1,2:3]

final_model <- train(
  class_efficiency ~ .,               
  data = label_efficiency[["data_labeled"]],               
  method = "nnet",           
  trControl = trainControl(method = "none", classProbs = TRUE),     
  tuneGrid = final_grid,            
  metric = "Balance_Accuracy",            
  maxit = 200,                
  trace = FALSE              
)

# ============================== #
# detecting importance variables #
# ============================== #

# necesary data to calculate importance in rminer
train_data <- final_model[["trainingData"]]
names(train_data)[1] <- "ClassEfficiency"

if(!(is.null(z))) {
  
  dataset_dummy <- dummy_cols(train_data,  select_columns = c(names(train_data))[z+1]) %>%
    select(-c(names(train_data))[z+1])
  
  train_data <- dataset_dummy
  
  
  to_factor <- c((x+y+1):ncol(train_data))
  train_data <- change_class(train_data, to_factor = to_factor)
  
} else {
  
  train_data <- train_data[,c(2:length(train_data),1)]
  
}

# Define methods and measures
methods_SA <- c("1D-SA") # c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
measures_SA <- c("AAD") #  c("AAD", "gradient", "variance", "range")

levels <- 7
set.seed(0)
# with rminer
m <- rminer::fit(
  ClassEfficiency ~.,
  data = train_data,
  model = "mlp",
  scale = "none",
  size = final_grid$size,
  decay = final_model$decay
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

round(importance$imp, 4)

data <- label_efficiency[["data_labeled"]]

# =========== #
# get ranking #
# =========== #

data_rank <- data[, c(new_x,new_y)]

eff_vector <- apply(data_rank, 1, function(row) {
  
  row_df <- as.data.frame(t(row))
  
  colnames(row_df) <- names(data_rank)
  
  pred <- unlist(predict(final_model, row_df, type = "prob")[1])
  
  return(pred)
})

eff_vector <- as.data.frame(round(eff_vector, 4))

id <- as.data.frame(c(1:nrow(data)))
names(id) <- "id"
eff_vector <- cbind(id, eff_vector)
names(eff_vector)[2] <- "eff_vector"

ranking_order <- eff_vector[order(eff_vector$eff_vector, decreasing = TRUE), ]


