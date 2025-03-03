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


# ===
# load data
# ===
#############
# PISA 2018 #
#############
data_2018 <- read_dta("C:/Users/Ricardo/Downloads/Data Spain PISA 2018.dta")
data_2018$Region <- as.factor(data_2018$Region)
data_2018$SCHLTYPE <- as.factor(data_2018$SCHLTYPE)

# preProces
data_NA <- data_2018[which(is.na(data_2018$SCHLTYPE)), ]
borrar <- table(data_NA$Region)

inf_NA <- matrix(
  data = NA,
  ncol = 3,
  nrow = length(unique(data_2018$Region))
)

inf_NA <- as.data.frame(inf_NA)

names(inf_NA) <- c("region", "num_NA", "percent_NA")

idx_reg <- sort(unique(data_2018$Region))

inf_NA$region <- idx_reg

for (i in 1:nrow(inf_NA)) {

  i_data <- data_2018 %>%
    filter(Region == i)

  value <- sum(apply(i_data, 1, anyNA))

  inf_NA$num_NA[i] <- value
  inf_NA$percent_NA[i] <- round(value / nrow(i_data) * 100, 2)

}

# save errors and NA in models
inf_NA

# preProcess
data <- data_2018
#data <- data[1:200, ]
idx_NA <- which(is.na(data$SCHLTYPE))
data <- data[-idx_NA,]

# ===
# Information to cafee
# ===

# x and y indexes
x <- c(10, 7, 6)
y <- c(3:5)
z <- c(2) # environment variables    , 8

# different types to label
# target_method <- "additive"
# convexity = TRUE
# returns = "variable"

# Step 1: label data
label_efficiency <- label_efficiency(
  data = data,
  x = x,
  y = y,
  z = z
)

# label_efficiency_NoZ <- label_efficiency(
#   data = data,
#   x = x,
#   y = y
# )

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

prop.table(table(train_data$class_efficiency))

# addresing imbalance
balance <- c(0.3, 0.5) # c(NA, 0.2, 0.3, 0.4, 0.5)

train_data_SMOTE <- SMOTE_data(
  data = train_data,
  x = label_efficiency[["index"]][["x"]],
  y = label_efficiency[["index"]][["y"]],
  z = label_efficiency[["index"]][["z"]],
  balance_data = balance
)

copy_train_data <- train_data
copy_valid_data <- valid_data

# Crear folds para validación cruzada
k_folds <- 5
validation_split <- 1 / k_folds

# Procesar folds y validación
if (!is.null(z)) {
  
  # change train_data
  # Convertir los índices de z a nombres de columnas antes de pasarlos a dummy_cols()
  z_column_names <- colnames(label_efficiency[["data_labeled"]])[label_efficiency$index$z]
  
  # Aplicar dummy encoding a las columnas correctas
  dataset_dummy <- dummy_cols(
    .data = train_data,
    select_columns = z_column_names,  # Ahora pasa nombres en vez de índices
    remove_selected_columns = TRUE
  )
  
  to_factor <- c((length(c(x, y)) + 1):ncol(dataset_dummy))
  dataset_dummy <- change_class(dataset_dummy, to_factor = to_factor)
  
  train_data <- cbind(dataset_dummy, train_data[,"class_efficiency"])
  names(train_data)[ncol(train_data)] <- names(copy_train_data["class_efficiency"])
  
  # change valid_data
  # Aplicar dummy encoding a las columnas correctas
  dataset_dummy <- dummy_cols(
    .data = valid_data,
    select_columns = z_column_names,  # Ahora pasa nombres en vez de índices
    remove_selected_columns = TRUE
  )
  
  to_factor <- c((length(c(x, y)) + 1):ncol(dataset_dummy))
  dataset_dummy <- change_class(dataset_dummy, to_factor = to_factor)
  
  valid_data <- cbind(dataset_dummy, valid_data[,"class_efficiency"])
  names(valid_data)[ncol(valid_data)] <- names(copy_valid_data["class_efficiency"])
}

# Definir hiperparámetros a explorar
learning_rates <- c(0.01, 0.001, 0.0001)
neurons_list <- c(16, 32, 64, 128)
dropout_rates <- c(0, 0.1, 0.2, 0.3, 0.5)
batch_sizes <- c(1, 8, 16, 32)
epochs_list <- c(100)
activations <- c("relu", "tanh", "leaky_relu")  # Agregamos la función de activación
activations <- c("relu")  # Agregamos la función de activación

# Definir hiperparámetros a explorar
learning_rates <- c(0.001)
hidden_layers <- c(10)
neurons_list <- c(32)
dropout_rates <- c(0)
batch_sizes <- c(1)
epochs_list <- c(50)
activations <- c("relu")  # Agregamos la función de activación

# save results
# Definir los nombres de las columnas
column_names <- c("hidden_layers", "neurons", "learning_rate", "activation",
                  "epochs", "dropout_rate", "batch_size")

overall_names <- c("Accuracy", "Kappa", "AccuracyLower", "AccuracyUpper",
                   "AccuracyNull", "AccuracyPValue", "McnemarPValue")

byClass_names <- c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value",
                   "Precision", "Recall", "F1", "Prevalence", "Detection Rate",
                   "Detection Prevalence", "Balanced Accuracy")

result_names <- c(column_names, overall_names, byClass_names) 

# Crear un dataframe vacío con esas columnas
df_hyperparams <- data.frame(matrix(ncol = length(result_names), nrow = 0))
colnames(df_hyperparams) <- result_names

# Almacenar resultados
results <- data.frame(
  learning_rate = numeric(),
  neurons = integer(),
  dropout_rate = numeric(),
  batch_size = integer(),
  epochs = integer(),
  activation = character(),
  mean_accuracy = numeric(),
  sd_accuracy = numeric()
)

# Loop sobre todas las combinaciones de hiperparámetros
for (lr in learning_rates) {
  for (hidden_layer in hidden_layers) {
    for (neurons in neurons_list) {
      for (dropout in dropout_rates) {
        for (batch in batch_sizes) {
          for (epoch in epochs_list) {
            for (activation in activations) {
              
              cat("\n🔹 Probando configuración: LR =", lr, 
                  ", Hidden Layers =", hidden_layer,
                  ", Neurons =", neurons, 
                  ", Dropout =", dropout, 
                  ", Batch =", batch, 
                  ", Epochs =", epoch, "\n")
              
              # Almacenar los scores de los folds
              cv_scores <- c()
              
              # Separar variables predictoras y etiquetas; normalize
              normalize_zscore <- function(x) {
                return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
              }
              
              x_train <- as.matrix(train_data[, !colnames(train_data) %in% "class_efficiency"])
              x_train <- apply(x_train, 2, as.numeric)
              x_train <- apply(x_train, 2, normalize_zscore)
              y_train <- ifelse(train_data$class_efficiency == "efficient", 1, 0)
              
              x_val <- as.matrix(valid_data[, !colnames(valid_data) %in% "class_efficiency"])
              x_val <- apply(x_val, 2, as.numeric)
              x_val <- apply(x_val, 2, normalize_zscore)
              y_val <- ifelse(valid_data$class_efficiency == "efficient", 1, 0)
              
              
              create_model <- function(X){
                
                # ncol(X) es el numero de neuronas de entrada 
                # Utilizamos la funcion de activacion ReLu en la capa oculta
                # Tenemos un problema de clasificacion binaria 0/1 -> neurona de salida sigmoid
                
                # model <- keras_model_sequential() %>% 
                #   layer_dense(units = neurons, activation = 'relu', input_shape = ncol(X)) %>% 
                #   layer_dense(units = 1, activation = 'sigmoid')
                
                # Inicializar el modelo
                model <- keras_model_sequential()
                
                # Agregar la primera capa oculta con la entrada
                model %>% layer_dense(units = neurons, activation = 'relu', input_shape = ncol(X))
                
                # Agregar capas ocultas adicionales según el número de elementos en hidden_layers
                if (hidden_layer > 1) {
                  
                  for (i in 2:hidden_layer) {
                    model %>% layer_dense(units = neurons, activation = activation)
                  }
                  
                }
                
                # Agregar la capa de salida (sigmoide para clasificación binaria)
                model %>% layer_dense(units = 1, activation = 'sigmoid')
                
                # Devolvemos el modelo creado 
                return(model)
              }
              
              model1 <- create_model(x_train) 
              summary(model1)
             
              model1 %>% compile(
                loss = "binary_crossentropy",
                optimizer = "adam",    # sgd
                metric = c("accuracy") # accuracy; F1Score
              ) 
              
              # training
              history1 <- model1 %>% fit(x_train, y_train,
                                         epochs = epoch,
                                         batch_size = batch,
                                         validation_split = validation_split)
              
              # Evaluacion 
              history1
              
              metrics1 <- model1 %>% evaluate(x_val, y_val)
              metrics1
              
              # Prediccion
              pred1 <- model1 %>% predict(x_val) 
              pred1 
              # si es mayor que 0.5, pasa a 1 y si no a 0
              pred1  <- round(pred1) # 0.5 but can it change? 0.75, 0.85, 0.95
              pred1 
              
              y_obs <- factor(y_val, levels = c(0, 1))
              y_hat <- factor(pred1, levels = c(0, 1)) 
              
              #create confusion matrix and calculate metrics related to confusion matrix
              confusion_matrix <- confusionMatrix(
                data = y_hat,
                reference = y_obs,
                mode = "everything",
                positive = "1"
              )#[["byClass"]]
              
              # Guardar resultados
              mean_acc <- confusion_matrix$overall[1]
              F1 <- confusion_matrix$byClass[7]
              
              results <- rbind(results, data.frame(
                learning_rate = lr,
                hidden_layer = hidden_layer,
                neurons = neurons,
                dropout_rate = dropout,
                batch_size = batch,
                epochs = epoch,
                accuracy = mean_acc,
                F1 = F1
              ))
              
              cat("Accuracy promedio:", mean_acc, "F1 Score:", F1, "\n\n")

            }
          }  
        } 
      }
    }
  }
}

# Mostrar los mejores hiperparámetros
best_model <- results[which.max(results$mean_accuracy), ]
print("\n🏆 Mejor configuración encontrada:")
print(results)






print(seed)
set.seed(seed)

methods <- list (
  # neuronal network
  "nnet" = list(
    hyparams = list(
      "size" = c(1, 5, 10, 20), # c(1, 5, 10, 15, 20)
      "decay" = c(0.1, 0.01, 0.001, 0.0001)
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

# SMOTE proportions
balance_data <- c(seq(0.2, 0.5, 0.05)) # c(0, seq(0.20, 0.5, 0.05))

# ML metric
metric = "F"

# scenarios to peer
scenarios <- seq(0.75, 0.95, 0.1) # seq(0.75, 0.95, 0.1)

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

# save model information
list_method <- list()  

set.seed(314)
data <- data[1:100,]
# # loop method
# for (i in 1:length(methods)) {
# 
#   # console information
#   print(paste("METODO:", i,  names(methods)[i]))
#   print("")
# 
#   # model result
#   final_model <- efficiency_estimation (
#     data = data,
#     x = x,
#     y = y,
#     z = z,
#     balance_data = balance_data,
#     trControl = trControl,
#     method = methods[i],
#     target_method = target_method,
#     metric = metric,
#     hold_out = hold_out,
#     scenarios = scenarios
#   )
#   
#   list_method[[i]] <- final_model
#   
# } # end bucle for (methods)  

names(list_method) <- names(methods)

#save(list_method, file = "results_XAI2.RData")

library(openxlsx)

# write.xlsx(list_method$nnet$metrics, file = "metrics_NN.xlsx")
# write.xlsx(list_method$svmPoly$metrics, file = "metrics_SVM.xlsx")
#write.xlsx(summary(data[, c(9:12, 8)]), file = "summary.xlsx")

list_method[["nnet"]][["final_model"]][["trainingData"]][1:97,]
# 
plot(density(list_method[["nnet"]][["ranking_order"]][["eff_vector"]]), 
     main = "Gráfico de Densidad Probability", 
     xlab = "Valores", 
     ylab = "Densidad", 
     col = "blue", 
     lwd = 2)
hist(list_method[["nnet"]][["ranking_order"]][["eff_vector"]], 
     probability = TRUE, 
     col = rgb(0, 0, 1, 0.3), 
     border = "white", 
     add = TRUE)

plot(density(list_method[["nnet"]][["data_scenario_list"]][["0.75"]][["betas"]][["beta"]]), 
     main = "Gráfico de Densidad Betas", 
     xlab = "Valores", 
     ylab = "Densidad", 
     col = "blue", 
     lwd = 2)
hist(list_method[["nnet"]][["data_scenario_list"]][["0.75"]][["betas"]][["beta"]], 
     probability = TRUE, 
     col = rgb(0, 0, 1, 0.3), 
     border = "white", 
     add = TRUE)

data_complete_NN <- cbind(data[, c(x,y)], list_method[["nnet"]][["data_contrafactual"]])
# data_complete_SVM <- cbind(data[, c(x,y)], list_method[["svmPoly"]][["data_contrafactual"]])
# 
# write.xlsx(data_complete_NN, file = "data_complete_NN.xlsx")
# write.xlsx(data_complete_SVM, file = "data_complete_SVM.xlsx")
# 
# write.xlsx(list_method[["svmPoly"]][["resume_metrics"]], file = "statistics_metrics_SVM.xlsx")
# write.xlsx(list_method[["nnet"]][["resume_metrics"]], file = "statistics_metrics_NN.xlsx")

write.xlsx(list_method[["nnet"]][["real_decision_balance"]], file = "real_decision_balance.xlsx")
write.xlsx(list_method[["nnet"]][["train_decision_balance"]], file = "train_decision_balance.xlsx") 
write.xlsx(list_method[["nnet"]][["result_SA"]], file = "SA.xlsx")
write.xlsx(list_method[["nnet"]][["eff_vector"]], file = "eff.xlsx") 
write.xlsx(list_method[["nnet"]][["ranking_order"]], file = "rank.xlsx")
write.xlsx(list_method[["nnet"]][["data_scenario_list"]], file = "data_sce.xlsx")
write.xlsx(list_method[["nnet"]][["metrics_list"]], file = "metrics.xlsx")

write.xlsx(as.data.frame(list_method[["nnet"]][["peer_list"]][["0.75"]]), file = "peer75.xlsx")
write.xlsx(as.data.frame(list_method[["nnet"]][["peer_list"]][["0.85"]]), file = "peer85.xlsx")
write.xlsx(as.data.frame(list_method[["nnet"]][["peer_list"]][["0.95"]]), file = "peer95.xlsx")

write.xlsx(as.data.frame(list_method[["nnet"]][["peer_weight_list"]][["0.75"]]), file = "peer_w75.xlsx")
write.xlsx(as.data.frame(list_method[["nnet"]][["peer_weight_list"]][["0.85"]]), file = "peer_w85.xlsx")
write.xlsx(as.data.frame(list_method[["nnet"]][["peer_weight_list"]][["0.95"]]), file = "peer_w95.xlsx")

# 
list_method[["nnet"]][["peer_list"]][["0.75"]] == list_method[["nnet"]][["peer_weight_list"]][["0.75"]]
list_method[["nnet"]][["peer_list"]][["0.85"]] == list_method[["nnet"]][["peer_weight_list"]][["0.85"]]
list_method[["nnet"]][["peer_list"]][["0.95"]] == list_method[["nnet"]][["peer_weight_list"]][["0.95"]]

model <- list_method[["nnet"]][["final_model"]]
data_train <- list_method[["nnet"]][["final_model"]][["trainingData"]][,-1]

eff_vector <- apply(data_train, 1, function(row) {
  
  row_df <- as.data.frame(t(row))
  colnames(row_df) <- names(data_train)
  
  pred <- unlist(predict(model, row_df, type = "prob")[1])
  
  return(pred)
})

eff_vector <- as.data.frame(eff_vector)

id <- as.data.frame(c(1:nrow(data_train)))
names(id) <- "id"
eff_vector <- cbind(id, eff_vector)

eff_vector$unit <- "real"
eff_vector$unit[98:233] <- "synthetic"
eff_vector$unit <- as.factor(eff_vector$unit)
eff_vector$histogram <- cut(
  eff_vector$eff_vector,
  breaks = seq(0, 1, by = 0.1),  # Intervalos de 0.1
  include.lowest = TRUE          # Incluir el límite inferior
)

library(ggplot2)
ggplot(data = eff_vector, aes(x = histogram, fill = unit)) +
  geom_bar(color = "black", alpha = 0.8, position = "stack") + # Cambiar a posición "stack"
  scale_fill_manual(
    values = c("real" = "orange", "synthetic" = "darkgreen")
  ) +
  labs(
    title = "Train Data efficiencies: Real (97) VS Synthetic (136)",
    x = "x",
    y = "Frecuency",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotar etiquetas para mejor legibilidad
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Centrar título
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

ggplot(data = eff_vector[eff_vector$unit == "synthetic",], aes(x = eff_vector, fill = unit)) +
  geom_density(alpha = 0.8, color = "black") + # Cambiar a densidad
  scale_fill_manual(
    values = c("real" = "orange", "synthetic" = "darkgreen")
  ) +
  labs(
    title = "Train Data Efficiencies: Real (97) VS Synthetic (136)",
    x = "x",
    y = "Density",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotar etiquetas para mejor legibilidad
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Centrar título
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

length(which(eff_vector$eff_vector[eff_vector$unit == "real"] < 0.25))



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