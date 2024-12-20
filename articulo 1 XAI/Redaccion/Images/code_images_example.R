# Toy example

# libraries

devtools::load_all()
library(caret)
library(Benchmarking)
library(magrittr)
library(dplyr)
library(deaR)
library(haven)
library(e1071)
library(rminer)

library(ggplot2)
library(rminer)


# Configuración
seed <- 0
set.seed(seed)
n <- 30

# Generación de inputs x1 e x2
x1 <- runif(n, 1, 10)
x2 <- runif(n, 1, 10)

# Generar una función de producción con curvatura
A <- 1.5
Q_star <- A * (x1^0.5 + x2^0.5)  # Función de frontera más curvada (convexa)

# Introducir ineficiencia
inefficiency <- runif(n, 0.7, 1)  # Factor de ineficiencia
Q <- Q_star * inefficiency        # Q observada

# Crear un DataFrame
data <- data.frame(x = x1, x2 = x2, y = Q, inefficiency = inefficiency)
data_gra <- data

# Gráfico de dispersión de x1 y Q (proyección en 2D)
ggplot(data) +
  geom_point(aes(x = x1, y = Q), color = "blue") +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, max(Q_star) * 1.1)) +
  theme_bw() +
  labs(title = "Dataset toy-example 30 DMUs",
       x = "y",
       y = "y")

data <- data[, c(1,3)]

# x and y indexes
x <- 1
y <- 2
#z <- c(2, 8) # environment variables

# different types to label
target_method <- "additive"

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

# SMOTE proportions
balance_data <- list(
  balance_proportions = c(seq(0.20, 0.4, 0.05)) #c(0, seq(0.20, 0.5, 0.05)) # seq(0.20, 0.4, 0.05) c(0, 0.2, 0.3, 0.4, 0.5)  0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5),
  #sub_frontier = "1/3"
) # 0.5 y 1/5

# balance_data <- list(
#   balance_proportions = c(0.5)  #0.2, c(0.2, 0.4),
#   #sub_frontier = "1/4"
# )

# ML metric
metric = "F"

# scenarios to peer
scenarios <- 0.75 # seq(0.75, 0.95, 0.1)

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

hold_out <- 0.00 # https://topepo.github.io/caret/train-models-by-tag.html

# save model information
list_method <- list()  

set.seed(314)

# loop method
for (i in 1:length(methods)) {
  
  # console information
  print(paste("METODO:", i,  names(methods)[i]))
  print("")
  
  # model result
  final_model <- efficiency_estimation (
    data = data,
    x = x,
    y = y,
    # z = z,
    balance_data = balance_data,
    trControl = trControl,
    method = methods[i],
    target_method = target_method,
    metric = metric,
    hold_out = hold_out,
    scenarios = scenarios
  )
  
  list_method[[i]] <- final_model
  
} # end bucle for (methods)  

names(list_method) <- names(methods)

### graph
### determinate efficient class BALANCED INPUT

data <- list_method[["nnet"]][["final_model"]][["trainingData"]]
data <- cbind(data[-1], data[1])
names(data) <- c("x", "y", "class_efficiency")

eval_data <- data[1:n,]
efficient_data <- data[data$class_efficiency == "efficient", ]

plot1 <- ggplot() +
  
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 10)) +
  
  # name DMUs
  geom_text(data = eval_data[eval_data$class_efficiency == "efficient", ],
            aes(x = x, y = y, label = row.names(eval_data[eval_data$class_efficiency == "efficient", ])),
            vjust = -1, hjust = 1) +
  
  geom_point(data = eval_data, aes(x = x, y = y, color = class_efficiency)) +
  
  scale_color_manual(values = c("green4", "red"), name = "Class", labels = c("efficient", "inefficient")) +
  labs(x = "input", y = "output") +
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  xlim(0, 10) +
  ylim(0, 10) +
  
  theme_bw() +
  theme(legend.position = "bottom")

plot1

#ggsave(plot = plot1, dpi = 600, filename = "DEA_label_efficient.png")

# ============= #
# Generate plot #
# ============= #

# make a grid of the predictors
grid <- expand.grid (
  x = seq(0, 10, length = 300),
  y = seq(0, 10, length = 300)
)

model <- list_method[["nnet"]][["final_model"]][["finalModel"]]
grid$label <- predict(model, grid, type = "class") #"raw"

grid$label <- factor(grid$label)


plot5 <- ggplot(data = eval_data) +
  
  # exes
  xlim(0, 10) +
  ylim(0, 10) +
  
  geom_point(data = grid, aes(x = x, y = y, color = label), size = 0.6, alpha = 0.3) +
  geom_point(aes(x = x, y = y)) +
  # scale_color_manual(values = c("not_efficient" = "pink", "efficient" = "lightgreen")) +
  scale_color_manual(values = c("olivedrab2", "pink"), name = "Class", labels = c("efficient", "inefficient")) +
  
  labs(x = "input", y = "output") +
  
  # # name DMUs
  # geom_text(data = data[n,],
  #           aes(x = x1, y = y, label = row.names(data[n,])),
  #           vjust = -1, hjust = 1.5) +
  # 
  # # name projection
  # geom_text(data = data[31,],
  #           aes(x = x1, y = y, label = "3'"),
  #           vjust = -1, hjust = 1.5) +
  # 
  # # projection segment
  # geom_segment(x = data[n,1], y = data[n,2], xend = data[31,1], yend = data[31,2], linetype = "dashed") +
  # 
  # # color of projection
  # geom_point(data = data[31,], aes(x = x1, y = y), color = "yellow4", size = 2) +
  # 
 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(legend.position = "bottom")

plot5


#ggsave(plot = plot5, dpi = 600, filename = "SVM_balance_pro.png")

result_scores <- as.data.frame(
  matrix(
    data = NA,
    ncol = 3,
    nrow = nrow(data)
  )
)

names(result_scores) <- c("DMU", "DEA score", "SVM score")

result_scores$DMU <- 1:nrow(data)

result_scores$`DEA score` <- round(bcc_scores_out, 3)
result_scores$`SVM score` <- scores

cor(x= result_scores$`DEA score`, y = result_scores$`SVM score`, method = "pearson")

library(openxlsx)
write.xlsx(result_scores, "result_example.xlsx")
devtools::load_all(
  
)