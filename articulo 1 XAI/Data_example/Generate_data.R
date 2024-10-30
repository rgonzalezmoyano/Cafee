### Example simulation

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

library("ggplot2")
library("rminer")

# generate data
set.seed(1997)

# Simulated data
data <- reffcy (
  DGP = "cobb_douglas_XnY1",
  parms = list (
    N = 30,
    nX = 1
  )
)

# plot
ggplot() +
  geom_point(data = data, aes(x = x1, y = y)) +
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(legend.position = "bottom")

# 
# library(openxlsx)
# write.xlsx(data, "data_example.xlsx")

# x and y indexes
x <- 1
y <- 2
z <- NULL

# import

# different types to label
target_method <- "BCC"

set.seed(314)
methods <- list (
  # svm
  "svmPoly" = list(
      hyparams = list(
        "degree" = c(1, 2, 3, 4, 5),
        "scale" = c(0.001, 0.1, 1, 10, 100),
        "C" = c(0.001, 0.1, 1, 10, 100)
      )
  )
)

# =========== #
# score cafee #
# =========== #    


# efficiency orientation
orientation <- "output"

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

  

names(list_method) <- names(methods)


################################################################################
### probabilities
# to get probabilities senarios
scenarios <- seq(0.65, 0.95, 0.05)

data_copy <- data
data <- data[, c(x,y)]

final_model <- final_model$final_model
cut_off <- 0.65
imp_vector = result_SA
# e <- 7
# 
# matrix with optimal values based on probability of being efficient
data_contr <- as.data.frame(
  matrix(
    data = NA,
    ncol = length(scenarios),
    nrow = nrow(data)
  )
)

names(data_contr) <- c(scenarios) 

colum <- 1
for (e in 1:length(scenarios)) {
  data_scenarios <- compute_target (
    data = data[, c(x,y)],
    x = 1:length(x),
    y = (length(x)+1):(length(x)+length(y)),
    #z = z,
    final_model = final_model_p,
    # orientation = orientation,
    cut_off = scenarios[e],
    imp_vector = result_SA
  )
  
  data_contr[, colum] <- data_scenarios[, (length(x)+1):(length(x)+length(y))]
  
  colum <- colum + 1
}

################################################################################

### determinate efficient class
efficient_data_0 <- data[, class_efficiency == "efficient"]

plot <- ggplot() +
  geom_point(data = data, aes(x = x1, y = y, color = class_efficiency)) +
  scale_color_manual(values = c("green4", "red"), name = "Class", labels = c("efficient", "inefficient")) +
  labs(x = "input", y = "output") +
  
  # name DMUs
  geom_text(data = data[data$class_efficiency == "efficient", ],
            aes(x = x1, y = y, label = row.names(data[data$class_efficiency == "efficient", ])),
            vjust = -1, hjust = 1) + 
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  xlim(0, 10) +
  ylim(0, 10) +

  theme_bw() +
  theme(legend.position = "bottom")

plot

ggsave(plot = plot, dpi = 600, filename = "DEA_label.png")

### determinate efficient class BALANCED INPUT
data_gra <- data
data <- eval_data
efficient_data_0 <- data_gra[data_gra$class_efficiency == "efficient", ]

plot1 <- ggplot() +
  
  # original points
  geom_point(data = data, aes(x = x1, y = y), size = 1) +
  
  # name DMUs
  geom_text(data = data[data$class_efficiency == "efficient", ],
            aes(x = x1, y = y, label = row.names(data[data$class_efficiency == "efficient", ])),
            vjust = -1, hjust = 1) +
  
  geom_point(data = efficient_data_0, aes(x = x1, y = y, color = class_efficiency)) +
  
  scale_color_manual(values = "green4", name = "Class") +
  labs(x = "input", y = "output") +
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  xlim(0, 10) +
  ylim(0, 10) +
  
  theme_bw() +
  theme(legend.position = "bottom")

plot1

ggsave(plot = plot1, dpi = 600, filename = "DEA_label_efficient.png")

### plot2 no efficient
inefficient_data_0 <- data_gra[data_gra$class_efficiency == "not_efficient", ]

plot2 <- ggplot() +
  
  # name DMUs
  geom_text(data = data[data$class_efficiency == "efficient", ],
            aes(x = x1, y = y, label = row.names(data[data$class_efficiency == "efficient", ])),
            vjust = -1, hjust = 1) +
  
  geom_point(data = inefficient_data_0, aes(x = x1, y = y, color = class_efficiency)) +
  
  scale_color_manual(values = "red", name = "Class", labels = "inefficient") +
  labs(x = "input", y = "output") +
  
  # original points
  geom_point(data = data, aes(x = x1, y = y), size = 1.3) +
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  xlim(0, 10) +
  ylim(0, 10) +
  
  theme_bw() +
  theme(legend.position = "bottom")

plot2

ggsave(plot = plot2, dpi = 600, filename = "DEA_label_inefficient.png")


### plot3
efficient_data <- rbind(efficient_data_0, new_dmu_values)


plot3 <- ggplot() +
  geom_point(data = data, aes(x = x1, y = y, color = class_efficiency)) +
  scale_color_manual(values = "green4", name = "Class") +
  labs(x = "input", y = "output") +
  theme_bw() +
  theme(legend.position = "bottom")

plot3


ggsave(plot = plot3, dpi = 600, filename = "DEA_label3.png")

### plot4
plot4 <- ggplot() +
  geom_point(data = data_gra, aes(x = x1, y = y, color = class_efficiency)) +
  scale_color_manual(values = c("green4", "red"), name = "Class", labels = c("efficient", "inefficient")) +
  labs(x = "input", y = "output") +
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  xlim(0, 10) +
  ylim(0, 10) +
  
  theme_bw() +
  theme(legend.position = "bottom")


plot4


ggsave(plot = plot4, dpi = 600, filename = "DEA_label_all.png")

# ============= #
# Generate plot #
# ============= #

# make a grid of the predictors
grid <- expand.grid (
  x1 = seq(0, 10, length = 300),
  y = seq(0, 10, length = 300)
)

# grid <- expand.grid (
#   x1 = seq(1, 6, length = 150),
#   y = seq(1, 8, length = 150)
# )

grid$label <- predict(final_model$final_model, grid, type = "raw")

# i <- 2
# i <- c(12, 13, 15, 16, 17, 18, 23, 24)

# scores_r <- scores[scores$score_cafee == 1.095, ]
# r <- rownames(scores_r)
# filter_ggplot_data <- data[r,]
all_dmu <- data
data <- eval_data

data <- all_dmu
n <- 3
# draw projection
new <- data.frame(
  x1 = data[n,1],
  y = data[n,2] * scores_cafee[n],
  yD = data[n,3]
)

data <- rbind(data, new)

plot5 <- ggplot(data = data) +
  geom_point(data = grid, aes(x = x1, y = y, color = label), size = 0.6, alpha = 0.3) +
  geom_point(aes(x = x1, y = y)) +
  # scale_color_manual(values = c("not_efficient" = "pink", "efficient" = "lightgreen")) +
  scale_color_manual(values = c("olivedrab2", "pink"), name = "Class", labels = c("efficient", "inefficient")) +

  labs(x = "input", y = "output") +
  
  # name DMUs
  geom_text(data = data[n,],
            aes(x = x1, y = y, label = row.names(data[n,])),
            vjust = -1, hjust = 1.5) +
  
  # name projection
  geom_text(data = data[31,],
            aes(x = x1, y = y, label = "3'"),
            vjust = -1, hjust = 1.5) +
  
  # projection segment
  geom_segment(x = data[n,1], y = data[n,2], xend = data[31,1], yend = data[31,2], linetype = "dashed") +
  
  # color of projection
  geom_point(data = data[31,], aes(x = x1, y = y), color = "yellow4", size = 2) +
  
  # exes
  xlim(0, 10) +
  ylim(0, 10) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(legend.position = "bottom")

plot5


ggsave(plot = plot5, dpi = 600, filename = "SVM_balance_pro.png")

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