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
#data <- data[1:500,]
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
hold_out <- 0.2 # https://topepo.github.io/caret/train-models-by-tag.html

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
balance <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6) # c(NA, 0.2, 0.3, 0.4, 0.5)
#balance <- c(0.1) # c(NA, 0.2, 0.3, 0.4, 0.5)

# train_data_SMOTE <- SMOTE_data(
#   data = train_data,
#   x = new_x,
#   y = new_y,
#   z = new_z,
#   balance_data = balance
# )
# #save(train_data_SMOTE, file = "train_data_SMOTE_010.Rdata")
# 
# copy_train_data <- train_data
# copy_train_data_SMOTE <- train_data_SMOTE
# copy_valid_data <- valid_data
# save_data <- list(
#   "copy_train_data" = copy_train_data,
#   "copy_train_data_SMOTE" = copy_train_data_SMOTE,
#   "copy_valid_data" = copy_valid_data,
#   "label_efficiency" = label_efficiency,
#   "valid_index" = label_efficiency)
# save(save_data, file = "save_data_train.Rdata")
load("articulo 2/save_data_train.Rdata")
train_data_SMOTE <- save_data[["copy_train_data_SMOTE"]]
copy_valid_data <- save_data[["copy_valid_data"]]
copy_train_data <- save_data[["copy_train_data"]]


# save results
# Definir los nombres de las columnas
column_names <- c("imbalance", "hidden_layers", "neurons", "learning_rate", "activation",
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


# Crear folds para validación cruzada
k_folds <- 5
validation_split <- 1 / k_folds

# Definir hiperparámetros a explorar
learning_rates <- c(0.01, 0.001, 0.0001)
neurons_list <- c(16, 32, 64, 128)
hidden_layers <- c(5, 10, 20, 30)
dropout_rates <- c(0, 0.1, 0.2, 0.3, 0.5)
batch_sizes <- c(1, 8, 16, 32)
epochs_list <- c(5, 15, 30)
activations <- c("relu", "tanh", "leaky_relu")  # Agregamos la función de activación

optimizer <-  "adam"  # sgd, adam
metric <- "F1Score" # accuracy; F1Score

# # Definir hiperparámetros a explorar
# learning_rates <- c(0.01)
# hidden_layers <- c(5)
# neurons_list <- c(16)
# dropout_rates <- c(0.5)
# batch_sizes <- c(16)
# epochs_list <- c(30)
# activations <- c("leaky_relu")  # Agregamos la función de activación

if (!is.null(z)) {
# Convertir los índices de z a nombres de columnas antes de pasarlos a dummy_cols()
z_column_names <- colnames(label_efficiency[["data_labeled"]])[label_efficiency$index$z]
}
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

for (balance_i in 1:length(balance)) {
 
  train_data <- train_data_SMOTE[[balance_i]]

  # Procesar folds y validación
  if (!is.null(z)) {
    
    # change train_data
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
    
  }
  
  # Loop sobre todas las combinaciones de hiperparámetros
  for (lr in learning_rates) {
    for (hidden_layer in hidden_layers) {
      for (neurons in neurons_list) {
        for (dropout in dropout_rates) {
          for (batch in batch_sizes) {
            for (epoch in epochs_list) {
              for (activation in activations) {
                
                cat("\n🔹 Probando configuración: imbalance =", balance[balance_i],
                    ", LR =", lr, 
                    ", Hidden Layers =", hidden_layer,
                    ", Neurons =", neurons, 
                    ", Dropout =", dropout, 
                    ", Batch =", batch, 
                    ", Epochs =", epoch,
                    ", activation function =", activation, "\n")
                
                # Almacenar los scores de los folds
                cv_scores <- c()
                
                # Separar variables predictoras y etiquetas; normalize
                normalize_zscore <- function(x) {
                  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
                }
                
                # TRAIN SET 
                # no class_efficiency 
                x_train <- as.matrix(train_data[, !colnames(train_data) %in% "class_efficiency"])
                
                # change to numeric
                x_train <- apply(x_train, 2, as.numeric)
                
                # normalize only x and y
                # first, separete
                x_train_norm <- x_train[, c(new_x, new_y)]
                
                # second, normalize
                x_train_norm <- apply(x_train_norm, 2, normalize_zscore)
                
                # join inputs and outputs with z
                x_train <- cbind(x_train_norm, x_train[, -c(new_x, new_y)])
                  
                # change class to numeric
                y_train <- ifelse(train_data$class_efficiency == "efficient", 1, 0)
                
                # VALIDATION SET
                # the same process
                x_val <- as.matrix(valid_data[, !colnames(valid_data) %in% "class_efficiency"])
                x_val <- apply(x_val, 2, as.numeric)
                x_val_norm <- x_val[, c(new_x, new_y)]
                x_val_norm <- apply(x_val_norm, 2, normalize_zscore)
                x_val <- cbind(x_val_norm, x_val[, -c(new_x, new_y)])
                
                y_val <- ifelse(valid_data$class_efficiency == "efficient", 1, 0)
                
                
                create_model <- function(X){
                  
                  # Inicializar el modelo
                  model <- keras_model_sequential()
                  
                  # Agregar la primera capa oculta con la entrada
                  model %>% layer_dense(units = neurons, activation = activation, input_shape = ncol(X))
                  
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
                
                model1 %>% keras::compile(
                  loss = "binary_crossentropy",
                  optimizer = optimizer,    # sgd, adam
                  metric = metric # accuracy; F1Score
                ) 
                
                # training
                history1 <- model1 %>% keras::fit(x_train, y_train,
                                           epochs = epoch,
                                           batch_size = batch,
                                           validation_split = validation_split)
                
                # Evaluacion 
                history1
                
                metrics1 <- model1 %>% keras::evaluate(x_val, y_val)
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
                confusion_matrix <- caret::confusionMatrix(
                  data = y_hat,
                  reference = y_obs,
                  mode = "everything",
                  positive = "1"
                )#[["byClass"]]
                
                # Guardar resultados
                mean_acc <- confusion_matrix$overall[1]
                F1 <- confusion_matrix$byClass[7]
                
                df_hyperparams <- rbind(df_hyperparams, data.frame(
                  imbalance = balance[balance_i],
                  hidden_layer = hidden_layer,
                  neurons = neurons,
                  learning_rate = lr,
                  activation = activation,
                  epochs = epoch,
                  dropout_rate = dropout,
                  batch_size = batch,
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
              
              
                
                cat("Accuracy promedio:", mean_acc, "F1 Score:", F1, "\n\n")
                
                
              }
            }  
          } 
        }
      }
    }
  }
}

top_model <- df_hyperparams[order(-df_hyperparams$F1), ]
top_model <- top_model[1,]

# Entrenar mejor red neuronal
lr <- top_model[,"learning_rate"]
hidden_layer <- top_model[,"hidden_layer"]
neurons <- top_model[,"neurons"]
dropout <- top_model[,"dropout_rate"]
batch <- top_model[,"batch_size"]
epoch <- top_model[,"epochs"]
activation <- top_model[,"activation"]
train_data <- train_data_SMOTE[[as.character(top_model[,"imbalance"])]]
train_data <- rbind(train_data, copy_valid_data)

# Procesar folds y validación
if (!is.null(z)) {
  
  # change train_data
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
  
}

cat("\n🔹 Probando configuración: imbalance =", as.character(top_model[,"imbalance"]),
    ", LR =", lr, 
    ", Hidden Layers =", hidden_layer,
    ", Neurons =", neurons, 
    ", Dropout =", dropout, 
    ", Batch =", batch, 
    ", Epochs =", epoch,
    ", activation function =", activation, "\n")

# Separar variables predictoras y etiquetas; normalize
normalize_zscore <- function(x) {
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

# TRAIN SET 
# no class_efficiency 
x_train <- as.matrix(train_data[, !colnames(train_data) %in% "class_efficiency"])

# change to numeric
x_train <- apply(x_train, 2, as.numeric)

# normalize only x and y
# first, separete
x_train_norm <- x_train[, c(new_x, new_y)]

# second, normalize
x_train_norm <- apply(x_train_norm, 2, normalize_zscore)

# join inputs and outputs with z
x_train <- cbind(x_train_norm, x_train[, -c(new_x, new_y)])

# change class to numeric
y_train <- ifelse(train_data$class_efficiency == "efficient", 1, 0)


create_model <- function(X){
  
  # Inicializar el modelo
  model <- keras_model_sequential()
  
  # Agregar la primera capa oculta con la entrada
  model %>% layer_dense(units = neurons, activation = activation, input_shape = ncol(X))
  
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

model1 %>% keras::compile(
  loss = "binary_crossentropy",
  optimizer = optimizer,    # sgd, adam
  metric = metric # accuracy; F1Score
) 

# training
history1 <- model1 %>% keras::fit(x_train, y_train,
                                  epochs = epoch,
                                  batch_size = batch)

# Evaluacion 
history1

# Prediccion
pred1 <- model1 %>% predict(x_train) 
pred1 
# si es mayor que 0.5, pasa a 1 y si no a 0
pred1  <- round(pred1) # 0.5 but can it change? 0.75, 0.85, 0.95
pred1 


# 
# library(caret)
# 
# # Paso 1: Métrica base
# base_pred <- round(predict(model1, x_val))
# base_f1 <- F_meas(data = factor(base_pred), reference = factor(y_val), relevant = "1")
# 
# # Paso 2: Permutación de variables
# importance <- numeric(ncol(x_val))
# names(importance) <- colnames(x_val)
# 
# for (i in 1:ncol(x_val)) {
#   x_val_perm <- x_val
#   x_val_perm[, i] <- sample(x_val_perm[, i])  # romper la relación
#   perm_pred <- round(predict(model1, x_val_perm))
#   perm_f1 <- F_meas(data = factor(perm_pred), reference = factor(y_val), relevant = "1")
#   importance[i] <- base_f1 - perm_f1  # caída en F1
# }
# 
# # Paso 3: Mostrar resultados
# importance <- sort(importance, decreasing = TRUE)
# barplot(importance, las = 2, main = "Variable importance (drop in F1)")
# 

scan_variable <- function(x, l = 7) {
  # Divide el rango de x en l niveles equiespaciados
  seq(from = min(x), to = max(x), length.out = l)
}

generate_sensitivity_data <- function(x_data, var_index, l = 7, baseline = NULL) {
  n_vars <- ncol(x_data)
  
  if (is.null(baseline)) {
    baseline <- colMeans(x_data)  # vector con medias por defecto
  }
  
  scan_values <- scan_variable(x_data[, var_index], l)
  
  sens_data <- matrix(rep(baseline, each = l), nrow = l)
  sens_data[, var_index] <- scan_values  # reemplaza solo la variable i
  
  return(sens_data)
}

get_model_output <- function(model, x_input) {
  as.vector(predict(model, x_input))
}

compute_sensitivity_measures <- function(y_hat) {
  r <- max(y_hat) - min(y_hat)
  g <- mean(abs(diff(y_hat)))
  v <- var(y_hat)
  return(c(range = r, gradient = g, variance = v))
}

run_1d_sensitivity_analysis <- function(model, x_data, l = 6) {
  n_vars <- ncol(x_data)
  sens_results <- matrix(NA, nrow = n_vars, ncol = 3)
  colnames(sens_results) <- c("range", "gradient", "variance")
  rownames(sens_results) <- colnames(x_data)
  
  for (i in 1:n_vars) {
    x_sens <- generate_sensitivity_data(x_data, i, l)
    y_hat <- get_model_output(model, x_sens)
    sens_results[i, ] <- compute_sensitivity_measures(y_hat)
  }
  
  return(as.data.frame(sens_results))
}

numeric_vars <- colnames(x_train)[c(new_x, new_y)]

# Extraer nombres de dummies (categorías codificadas)
all_vars <- colnames(x_train)
dummy_vars <- setdiff(all_vars, numeric_vars)

# Agrupar dummies por categoría (aquí ejemplo con Region_)
dummy_groups <- list(
  Region = grep("^Region_", dummy_vars, value = TRUE)
)

compute_permutation_importance <- function(model, x_data, y_true, numeric_vars) {
  base_preds <- as.vector(predict(model, x_data))
  
  importance <- sapply(numeric_vars, function(var) {
    x_permuted <- x_data
    x_permuted[, var] <- sample(x_permuted[, var])  # permuta la columna
    perm_preds <- as.vector(predict(model, x_permuted))
    mean(abs(base_preds - perm_preds))  # cambio promedio en la predicción
  })
  
  return(data.frame(
    variable = numeric_vars,
    mean_change = importance,
    type = "numeric"
  ))
}

analyze_categorical_importance <- function(model, x_data, factor_dummies, group_label = "Factor") {
  if (is.matrix(x_data)) {
    x_data <- as.data.frame(x_data)
  }
  
  available <- factor_dummies[factor_dummies %in% colnames(x_data)]
  if (length(available) < 2) stop("No hay suficientes columnas válidas en factor_dummies.")
  
  mean_preds <- sapply(available, function(cat) {
    idx <- which(x_data[, cat] == 1)
    if (length(idx) == 0) return(NA)
    x_mat <- as.matrix(x_data[idx, , drop = FALSE])
    preds <- as.vector(predict(model, x_mat))
    mean(preds)
  })
  
  mean_preds <- mean_preds[!is.na(mean_preds)]
  
  if (length(mean_preds) < 2) stop("No hay suficientes categorías con datos.")
  
  range_val <- max(mean_preds) - min(mean_preds)
  variance_val <- var(mean_preds)
  gradient_val <- mean(abs(diff(sort(mean_preds))))
  
  return(data.frame(
    variable = group_label,
    range = range_val,
    variance = variance_val,
    gradient = gradient_val,
    type = "categorical"
  ))
}

# 1. Variables numéricas
num_importance <- compute_permutation_importance(model1, x_train, y_train, numeric_vars)

# 2. Categóricas agrupadas (Region)
cat_importance <- analyze_categorical_importance(model1, x_train, dummy_groups$Region, group_label = "Region")

# Unir resultados
df_importancia <- rbind(
  num_importance[, c("variable", "mean_change", "type")],
  data.frame(variable = cat_importance$variable,
             mean_change = cat_importance$range,  # usamos range como proxy de importancia
             type = cat_importance$type)
)

df_importancia$absolute_importance <- df_importancia$mean_change
df_importancia$relative_importance <- df_importancia$absolute_importance / sum(df_importancia$absolute_importance)

df_importancia$relative_importance <- round(df_importancia$relative_importance, 4)
df_importancia <- df_importancia[order(-df_importancia$relative_importance), ]

# Ordenar de mayor a menor importancia
df_importancia <- df_importancia[order(-df_importancia$mean_change), ]

# Ver resultado
print(df_importancia)

result_SA <- df_importancia[names(train_data)[c(new_x, new_y)], c("variable", "absolute_importance")]
result_SA$relative_importance <- result_SA$absolute_importance / sum(result_SA$absolute_importance)

# get ranking
# REAL SET 
# no class_efficiency 
train_data <- rbind(copy_train_data, copy_valid_data)

# Procesar folds y validación
if (!is.null(z)) {
  
  # change train_data
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
  
}

x_train <- as.matrix(train_data[, !colnames(train_data) %in% "class_efficiency"])

# change to numeric
x_train <- apply(x_train, 2, as.numeric)

# normalize only x and y
# first, separete
x_train_norm <- x_train[, c(new_x, new_y)]

# second, normalize
x_train_norm <- apply(x_train_norm, 2, normalize_zscore)

# join inputs and outputs with z
x_train <- cbind(x_train_norm, x_train[, -c(new_x, new_y)])

# change class to numeric
y_train <- ifelse(train_data$class_efficiency == "efficient", 1, 0)

eff_vector <- model1 %>% predict(x_train) 
eff_vector 

eff_vector <- as.data.frame(eff_vector)
names(eff_vector) <- "eff_vector"

id <- as.data.frame(c(1:nrow(train_data)))
names(id) <- "id"
eff_vector <- cbind(id, eff_vector)

ranking_order <- eff_vector[order(eff_vector$eff_vector, decreasing = TRUE), ]

hist(ranking_order$eff_vector)                  

# ============================= #
# to get probabilities senarios #
# ============================= #
scenarios <- c(0.75, 0.85, 0.95)
data_scenario_list <- list()
metrics_list <- list()
peer_list <- list()
peer_weight_list <- list()
na_count_list <- list()
n_not_prob_list <- list()

for (e in 1:length(scenarios)) {
  print(paste("scenario: ", scenarios[e]))
  print(final_model)
  
  data_scenario <- compute_target(
    data = x_train,
    x = new_x,
    y = new_y,
    z = new_z,
    final_model = model1,
    cut_off = scenarios[e],
    imp_vector = result_SA$relative_importance
  )
  
  if(all(is.na(data_scenario$data_scenario))) {
    print("all na")
    browser()
    
    # peer
    peer_restult <- NA
    
    # save_peer
    peer_list[[e]] <- peer_restult
    
    # main_metrics
    main_metrics <- NA
    
    # save main_metrics
    metrics_list[[e]] <- main_metrics
    
    print("pause")
    
  } else {
    
    if(any(data_scenario$data_scenario[, c(x,y)] < 0)) {
      
      data_scenario$data_scenario[apply(data_scenario$data_scenario, 1, function(row) any(row < 0) || any(is.na(row))), ] <- NA
      
      na_idx <- which(apply(data_scenario$data_scenario, 1, function(row) any(is.na(row))))
      data_scenario$betas[na_idx,] <- NA
    }
    
    data_scenario_list[[e]] <- data_scenario
  }
}
