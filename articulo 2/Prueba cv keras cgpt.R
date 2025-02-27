library(keras)
library(tensorflow)
library(caret)
library(fastDummies)
library(pROC)

# **Simulación de datos similares a los tuyos**
set.seed(123)
n <- 100

train_data <- data.frame(
  ESCS_m2 = rnorm(n, mean = 2, sd = 1),
  TSRATIO = rnorm(n, mean = 7, sd = 2),
  EDUSHORT2 = rnorm(n, mean = 4, sd = 1.5),
  PVSCIE_m = rnorm(n, mean = 450, sd = 50),
  PVMATH_m = rnorm(n, mean = 460, sd = 50),
  PVREAD_m = rnorm(n, mean = 455, sd = 50),
  Region = factor(sample(1:17, n, replace = TRUE)),
  class_efficiency = factor(sample(c("efficient", "not_efficient"), n, replace = TRUE))
)

# **Convertir `Region` en dummies antes de CV**
train_data <- dummy_cols(.data = train_data, select_columns = "Region", remove_selected_columns = TRUE)

# Crear folds con createFolds()
k_folds <- 5
folds <- createFolds(train_data$class_efficiency, k = k_folds, list = TRUE, returnTrain = FALSE)

# **Asegurar que todos los folds tengan la misma estructura**
full_colnames <- colnames(train_data)

fix_fold_structure <- function(data_fold, full_colnames) {
  missing_cols <- setdiff(full_colnames, colnames(data_fold))
  for (col in missing_cols) {
    data_fold[[col]] <- 0
  }
  return(data_fold[, full_colnames])
}

# **Preprocesar folds**
folds_processed <- list()
for (i in 1:k_folds) {
  val_idx <- folds[[i]]
  train_idx <- setdiff(seq_len(nrow(train_data)), val_idx)
  
  train_fold <- train_data[train_idx, ]
  val_fold <- train_data[val_idx, ]
  
  train_fold <- fix_fold_structure(train_fold, full_colnames)
  val_fold <- fix_fold_structure(val_fold, full_colnames)
  
  folds_processed[[i]] <- list(train = train_fold, val = val_fold)
}

# **Definir hiperparámetros**
learning_rates <- c(0.01, 0.001, 0.0001)
neurons_list <- c(16, 32, 64)
dropout_rates <- c(0.1, 0.2, 0.3)
batch_sizes <- c(16, 32)
epochs_list <- c(20, 50)
activations <- c("relu", "tanh", "leaky_relu")  # Agregamos la función de activación
activations <- c("relu")  # Agregamos la función de activación


# # **Definir hiperparámetros**
# learning_rates <- c(0.01, 0.001, 0.0001)
# neurons_list <- c(16)
# dropout_rates <- c(0.1)
# batch_sizes <- c(16)
# epochs_list <- c(20)
# activations <- c("relu")  # Agregamos la función de activación

# **Calcular el número total de configuraciones**
total_configs <- length(learning_rates) * length(neurons_list) * length(dropout_rates) * 
  length(batch_sizes) * length(epochs_list) * length(activations)
config_counter <- 0  

# **Crear un dataframe vacío con nombres correctos**
results <- data.frame(
  learning_rate = numeric(), neurons = integer(),
  dropout_rate = numeric(), batch_size = integer(), epochs = integer(),
  activation = character(),
  accuracy = numeric(), precision = numeric(), recall = numeric(),
  specificity = numeric(), f1_score = numeric(), balanced_accuracy = numeric(),
  kappa = numeric(), auc = numeric(), stringsAsFactors = FALSE
)

# **Ejecutar Grid Search de Hiperparámetros**
for (lr in learning_rates) {
  for (neurons in neurons_list) {
    for (dropout in dropout_rates) {
      for (batch in batch_sizes) {
        for (epoch in epochs_list) {
          for (activation in activations) {
            
            cat(sprintf("\n🔹 Configuración: LR = %f, Neurons = %d, Dropout = %.1f, Batch = %d, Epochs = %d, Activation = %s\n", 
                        lr, neurons, dropout, batch, epoch, activation))
            
            # Almacenar métricas de los folds
            metrics_results <- matrix(nrow = k_folds, ncol = 8)  # 8 métricas
            stop()
            for (i in 1:k_folds) {
              train_fold <- folds_processed[[i]]$train
              val_fold <- folds_processed[[i]]$val
              
              X_train <- as.matrix(train_fold[, !colnames(train_fold) %in% "class_efficiency"])
              y_train <- ifelse(train_fold$class_efficiency == "efficient", 1, 0)
              
              X_val <- as.matrix(val_fold[, !colnames(val_fold) %in% "class_efficiency"])
              y_val <- ifelse(val_fold$class_efficiency == "efficient", 1, 0)
              
              # **Modelo**
              model <- keras_model_sequential() %>%
                layer_dense(units = neurons, activation = activation, input_shape = ncol(X_train)) %>%
                layer_dropout(rate = dropout) %>%
                layer_dense(units = neurons / 2, activation = activation) %>%
                layer_dense(units = 1, activation = "sigmoid")  # Salida para clasificación binaria
              
              model %>% compile(
                loss = "binary_crossentropy",
                optimizer = optimizer_adam(learning_rate = lr),
                metrics = c("accuracy")
              )
              
              # **Entrenar modelo**
              model %>% fit(
                X_train, y_train, epochs = epoch, batch_size = batch,
                validation_data = list(X_val, y_val), verbose = 0
              )
              
              # **Predicciones**
              y_pred_prob <- model %>% predict(X_val)
              y_pred <- ifelse(y_pred_prob > 0.5, 1, 0)
              
              # **Matriz de confusión**
              cm <- table(y_val, y_pred)
              
              TP <- sum(y_val == 1 & y_pred == 1)
              TN <- sum(y_val == 0 & y_pred == 0)
              FP <- sum(y_val == 0 & y_pred == 1)
              FN <- sum(y_val == 1 & y_pred == 0)
              
              # **Calcular métricas**
              accuracy <- (TP + TN) / sum(cm)
              precision <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)
              recall <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)
              specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), NA)
              f1_score <- ifelse((precision + recall) > 0, 2 * (precision * recall) / (precision + recall), NA)
              balanced_accuracy <- (recall + specificity) / 2
              kappa <- (accuracy - ((TP + FP) * (TP + FN) + (TN + FP) * (TN + FN)) / (sum(cm)^2)) / (1 - ((TP + FP) * (TP + FN) + (TN + FP) * (TN + FN)) / (sum(cm)^2))
              auc <- as.numeric(auc(roc(y_val, y_pred_prob)))
              
              metrics_results[i, ] <- c(accuracy, precision, recall, specificity, f1_score, balanced_accuracy, kappa, auc)
              
              config_counter <- config_counter + 1
              progress <- round((config_counter / total_configs) * 100, 2)
              cat(sprintf("✅ Progreso: %.2f%% completado (%d de %d configuraciones)\n", progress, config_counter, total_configs))
            }
            
            avg_metrics <- colMeans(metrics_results, na.rm = TRUE)
            results <- rbind(results, c(lr, neurons, dropout, batch, epoch, activation, avg_metrics))
          }
        }
      }
    }
  }
}

colnames(results) <- c("learning_rate", "neurons", "dropout_rate", "batch_size", "epochs", "activation",
                       "accuracy", "precision", "recall", "specificity", "f1_score", "balanced_accuracy", "kappa", "auc")

print("📊 Tabla final de resultados:")
print(results)

write.csv(results, "resultados_grid_search.csv", row.names = FALSE)
