library(readxl)
recopilacion_datos <- read_excel("simulations/cobb_douglas_XnY1/results/hold_out_0.10 v3/recopilacion_datos.xlsx")

data <- recopilacion_datos
data

new_data <- data[, c(2:8, 13:length(data))]

# cafee_DEA
values <- as.numeric(unlist(strsplit(new_data$hyperparameters_cafee_DEA, split = "[, ]+")))
        
pos_degree <- seq(1, 60, by = 3) 
pos_scale <- seq(2, 60, by = 3)
pos_C <- seq(3, 60, by = 3)

matrix <- matrix (
  data = NA,
  ncol = 3,
  nrow = nrow(data)
)

matrix[, 1] <- values[pos_degree]
matrix[, 2] <- values[pos_scale]
matrix[, 3] <- values[pos_C]

new_data$cafee_DEA_degree <- matrix[, 1]
new_data$cafee_DEA_scale <-  matrix[, 2]
new_data$cafee_DEA_C <-  matrix[, 3]

# cafee_BDEA
values <- as.numeric(unlist(strsplit(new_data$hyperparameters_cafee_BDEA, split = "[, ]+")))

pos_degree <- seq(1, 60, by = 3) 
pos_scale <- seq(2, 60, by = 3)
pos_C <- seq(3, 60, by = 3)

matrix <- matrix (
  data = NA,
  ncol = 3,
  nrow = nrow(data)
)

matrix[, 1] <- values[pos_degree]
matrix[, 2] <- values[pos_scale]
matrix[, 3] <- values[pos_C]

new_data$cafee_BDEA_degree <- matrix[, 1]
new_data$cafee_BDEA_scale <-  matrix[, 2]
new_data$cafee_BDEA_C <-  matrix[, 3]

# correlation
library(ggplot2)

data_0 <- new_data[new_data$N == "150" , ]

recuento <- table(data_0$cafee_BDEA_scale)
recuento <- unname(recuento)

dataframe <- data.frame (
  x = sort(unique(data_0$cafee_BDEA_scale)),
  y = as.vector(recuento)
)




ggplot(data = dataframe) +
  geom_bar(aes(x = factor(x), y = y, fill = factor(x)), stat = "identity") +
  xlab("Scale") +
  ylab("Frecuencia") +
  ggtitle("Recuento de Hiperparámetros")





