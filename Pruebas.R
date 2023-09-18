devtools::document() # actualizar manuales de ayuda
devtools::load_all() # actualizar el codigo

set.seed(314)

# Simulated data
N <- 100
scenario <- "A"
#scenario <- "C"
data <- AddScenario(N, scenario) # x1, y, yD

DMUs <- NULL
x <- 1
y <- 2
#x <- c(1,2) # C
#y <- 3 # C

# Aplication
library(readxl)
#PISA_Madrid_2014 <- read_excel("PISA-Madrid-2014.xlsx")

#data <- as.data.frame(PISA_Madrid_2014)
#DMUs <- 1
#x <- c(2:4)
#y <- c(5:6)

# Optimization orientation
orientation <- "output"

# Data train
trControl <- list (
  method = "cv",
  number = 5 # Either the number of folds or number of resampling iterations
  )

methods <- list (
  "svmLinear" = list (
    "C" = c (
      seq(0, 100, length.out = 10), 
      seq(200, 1000, length.out = 10)
      )
    ),
  "svmRadial" = list (
    "C" = c (
      seq(0, 100, length.out = 10), 
      seq(200, 1000, length.out = 10)
      ),
    "sigma" = seq(0, 10, length.out = 20)
    ),
  "svmPoly" = list (
    "degree" = c(2, 3, 4),
    "scale" = c(0.0001, 0.001, 0.01, 0.1, 1),
    "C" = c (
      seq(0, 100, length.out = 10), 
      seq(200, 1000, length.out = 10)
      )
    )
  )
# https://topepo.github.io/caret/train-models-by-tag.html

# Result
prueba <- DEA.Classifier(data, DMUs, x, y, trControl, methods, orientation);prueba


i <- 5

library(kernlab)
# make a grid of the predictors
rng.x <- range(prueba$data[1])
rng.y <- range(prueba$data[2])

n_points <- 25
grid <- expand.grid(x = seq(rng.x[1], rng.x[2], length = n_points),
                    y = seq(rng.y[1], rng.y[2], length = n_points))

grid$decision <- predict(prueba$best_model_fit$finalModel, grid, type = "decision")[, 1]

# separating.border <- grid[between(grid$decision, - 0.5, 0.5), ]

# Malla puntos
# Rango de los predictores
#rango_X1 <- range(0, 14)
rango_X1 <- range(min(prueba$data[x]), max(prueba$data[x]))

#rango_X2 <- range(0, 5.7)
rango_X2 <- range(min(prueba$data[y]), max(prueba$data[y]))

# Interpolación de puntos
new_x1 <- seq(from = rango_X1[1], to = rango_X1[2], length = 200)
new_x2 <- seq(from = rango_X2[1], to = rango_X2[2], length = 200)
nuevos_puntos <- expand.grid(x1 = new_x1, y = new_x2)

# Predicción según el modelo
predicciones <- predict(object = prueba$best_model_fit$finalModel, newdata = nuevos_puntos)

# Se almacenan los puntos predichos para dar color a las regiones
color_regiones <- data.frame(nuevos_puntos, y = predicciones)

prueba$score[i]

print(paste("Estamos evaluando a la DMU", i))
if (orientation == "input") {
  print(paste0("Con orientación input, la DMU debe disminuir un ", (round(1 - prueba$score[i], digits = 4)) * 100, "% sus inputs para ser considerada como eficiente por el modelo"))

  punto_optimo <- data.frame( # input point
    x = c(prueba$data[i, x], prueba$solution_point[i,1]),
    y = c(prueba$data[i, y], prueba$data[i, y])
  )

  ggplot(prueba$data)+
    geom_point(data = color_regiones, aes(x = x1, y = y, color = as.factor(y.1)),
               size = 0.4, alpha = 0.7) + # malla
    geom_point(aes(x = x1, y = y, color = ClassEfficiency),
               size = 2.5) +  # DMUs
    geom_point(data = punto_optimo, aes(x = x, y = y), size = 2) +
    geom_line(data = punto_optimo, aes(x = x, y = y), linetype = 6) +
    # geom_point(data = separating.border, aes(x = x, y = y), size = 0.1) +
    geom_contour(data = grid, aes(x = x, y = y, z = decision),
                 breaks = 0, col = "black") +
    #geom_point(data = punto_optimo2, aes(x = x_opt, y = y)) + # ori_input
    #geom_line(data = data, aes(x = x1, y = yD), linetype = 6) + # yD theorical
    theme_bw()

} else {
  print(paste0("Con orientación output, la DMU debe aumentar un ", (round(prueba$score[i] - 1, digits = 4)) * 100, "% sus outputs para ser considerada como eficiente por el modelo"))
  punto_optimo <- data.frame( # output point
    x = c(prueba$data[i, x], prueba$data[i, x]),
    y = c(prueba$data[i, y], prueba$solution_point[i,1])
  )

  ggplot(prueba$data)+
    geom_point(data = color_regiones, aes(x = x1, y = y, color = as.factor(y.1)),
               size = 0.4, alpha = 0.7) + # malla
    geom_point(aes(x = x1, y = y, color = ClassEfficiency),
               size = 2.5) +  # DMUs
    geom_point(data = punto_optimo, aes(x = x, y = y), size = 2) +
    geom_line(data = punto_optimo, aes(x = x, y = y), linetype = 6) +
    # geom_point(data = separating.border, aes(x = x, y = y), size = 0.1) +
    geom_contour(data = grid, aes(x = x, y = y, z = decision),
                 breaks = 0, col = "black") +
    #geom_point(data = punto_optimo2, aes(x = x_opt, y = y)) + # ori_input
    #geom_line(data = data, aes(x = x1, y = yD), linetype = 6) + # yD theorical
    theme_bw()
}

library("WriteXLS")


# Luego escribimos el siguiente comando:
WriteXLS(prueba$resume, "SAD_DEA_SVM.xlsx")


library(readxl)

SAD <- as.data.frame(rownames(data))
colnames(SAD) <- c("DMUs")
SAD <- cbind(SAD, prueba$data)
SAD <- cbind(SAD, prueba$resume[, c(3,4)])


# Luego escribimos el siguiente comando:

write.table(SAD,"DATOS_SAD.txt")

DATOS_SAD <- read.csv("C:/Users/Ricardo/Desktop/DEA.Classifier/DATOS_SAD.txt",
                      sep = "")

