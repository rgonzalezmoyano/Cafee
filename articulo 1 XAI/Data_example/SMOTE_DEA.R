# SMOTE DEA
n_row <- 6


# data_generator
set.seed(123)  # Fijamos la semilla para reproducibilidad

# Parámetros de la función Cobb-Douglas
A <- 1      # Factor de escala
alpha <- 0.7  # Exponente del input

# Generamos valores positivos para X (el insumo)
X <- runif(n_row, min = 1, max = 6)  # 6 valores para X entre 1 y 10

# Calculamos Y usando la forma Cobb-Douglas con un solo insumo
Y <- A * (X^alpha)

# Creamos un data.frame con los datos
data <- data.frame(x = X, y = Y)


# manual

data <- matrix(
  data = NA,
  ncol = 2,
  nrow = n_row
)

data[, 1] <- c(1,2,3,4,6,8)
data[, 2] <- c(1,4,7,8,9, 10)

data <- as.data.frame(data)
names(data) <- c("x", "y")

ggplot() +
  geom_point(data = data, aes(x = x, y = y)) +
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  xlim(0, 10) +
  ylim(0, 10) 
             