devtools::load_all()
library(fastDummies)
library(dplyr)
library(rminer)

# load model
load("C:/Users/Ricardo/OneDrive - UMH/Documentos/Cafee/articulo 2/results_XAI2.RData")

final_model

# set dataset
data <- final_model[["trainingData"]]


# index
x <- c(1,2,3)
y <- c(4,5,6)
z <- 7

# order data
data <- data[, c(2:ncol(data),1)]

names(data) <- c(names(data)[c(x,y,z)], "class_efficiency")


# SA


if(!(is.null(z))) {
  
  class <- data$class_efficiency

  data <- data[,-ncol(data)]

  dataset_dummy <- dummy_cols(data,  select_columns = c(names(data))[z]) %>%
    select(-c(names(data))[z])

  z_idx <- max(y) + 1
  to_factor <- c(z_idx:ncol(dataset_dummy))
  data <- change_class(dataset_dummy, to_factor = to_factor)
  
} else {
  
  class <- data$class_efficiency

  data <- data[,-ncol(data)]

}

data_class <- cbind(data,class)

# train ML
# Cargar librerías necesarias
library(caret)

# Configurar el control del entrenamiento
train_control <- trainControl(
  method = "cv",  # Validación cruzada
  number = 5      # Número de folds
)

# Definir la cuadrícula de hiperparámetros para `size` y `decay`
grid <- expand.grid(
  size = c(2, 5, 10),   # Número de neuronas en la capa oculta
  decay = c(0, 0.01, 0.1)  # Regularización L2
)

# Entrenar el modelo con `nnet`
final_model <- train(
  class ~ .,  # Variable objetivo ~ predictores
  data = data_class,           # Tus datos
  method = "nnet",       # Modelo de red neuronal
  trControl = train_control, 
  tuneGrid = grid,       # Especificar la cuadrícula de hiperparámetros
  linout = FALSE,         # Regresión (cambia a FALSE para clasificación)
  trace = FALSE,         # Evita impresión de iteraciones
  MaxNWts = 1000,        # Límite de pesos
  maxit = 200            # Iteraciones
)

# Ver los mejores hiperparámetros elegidos
print(final_model$bestTune)


# Define methods and measures
methods_SA <- c("1D-SA") # c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
measures_SA <- c("AAD") #  c("AAD", "gradient", "variance", "range")

levels <- 7

# importance with our model of Caret
mypred <- function(M, data) {
  return (predict(M, data, type = "prob"))
}

# Check predictions
test_preds <- mypred(final_model, data)
print(test_preds)

task = rminer::defaultask(task, model = "default", data[1, outindex])

# Calculate the importance for the current method and measure
importance <- Importance(
  M = final_model,
  RealL = levels, # Levels
  data = data_class, # data
  method = methods_SA,
  measure = measures_SA,
  baseline = "mean", # mean, median, with the baseline example (should have the same attribute names as data).
  responses = TRUE,
  PRED = mypred,
  outindex = ncol(data_class), # length(train_data)
  task = "prob"
)
