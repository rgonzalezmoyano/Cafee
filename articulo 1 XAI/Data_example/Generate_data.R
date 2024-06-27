### Example simulation

# libraries
library("ggplot2")

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
target_method <- "additive"

set.seed(314)
methods <- list (
  "svmPoly" = list(
    hyparams = list(
      "degree" = c(3),
      "scale" = c(1),
      "C" = c(1)
    )
   )
  
  # # svm
  # "svmPoly" = list(
  #     hyparams = list(
  #       "degree" = c(1, 2, 3, 4, 5),
  #       "scale" = c(0.001, 0.1, 1, 10, 100),
  #       "C" = c(0.001, 0.1, 1, 10, 100)
  #     )
  # ),
  # "svmRadial" = list(
  #   hyparams = list(
  #     "sigma" = c(0.01, 0.1, 1, 10, 100),
  #     "C" = c(0.001, 0.1, 1, 10, 100)
  #   )
  # ),
  
  # # random forest
  # "rf" = list (
  #   options = list (
  #     ntree = c(500) # c(100, 500, 1000)
  #   ),
  #   hyparams = list(
  #     mtry = c(4)
  #   )
  # ),
  
  # # neuronal network
  # "nnet" = list(
  #   hyparams = list(
  #     "size" = c(1, 5, 10, 20),
  #     "decay" = c(0, 0.1, 0.01, 0.001, 0,0001)
  #   ),
  #   options = list (
  #     maxit = 1000
  #   )
  #   
  # )
  
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

hold_out <- 0.10

# https://topepo.github.io/caret/train-models-by-tag.html

metric = "Accuracy"

convexity <- TRUE

# preProcess
# data <- data_2018
# # data <- data[1:30, ]
# idx_NA <- which(is.na(data$SCHLTYPE))
# data <- data[-idx_NA,]

# save scores region
list_region <- list()

# new  dataset of scores result
scores <- matrix (
  ncol = length(methods),
  nrow = nrow(data)
) 

# change to data.frame
scores <- as.data.frame(scores)

# change names
score_names <- names(methods)
names(scores) <- score_names

# save model information
list_method <- list()  



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

grid$label <- predict(final_model, grid, type = "raw")

# i <- 2
# i <- c(12, 13, 15, 16, 17, 18, 23, 24)

# scores_r <- scores[scores$score_cafee == 1.095, ]
# r <- rownames(scores_r)
# filter_ggplot_data <- data[r,]
all_dmu <- data
data <- eval_data

plot5 <- ggplot(data = data) +
  geom_point(data = grid, aes(x = x1, y = y, color = label), size = 0.6, alpha = 0.3) +
  geom_point(aes(x = x1, y = y)) +
  # scale_color_manual(values = c("not_efficient" = "pink", "efficient" = "lightgreen")) +
  scale_color_manual(values = c("olivedrab2", "pink"), name = "Class", labels = c("efficient", "inefficient")) +

  labs(x = "input", y = "output") +
  # # name DMUs
  # geom_text(data = data,
  #           aes(x = x1, y = y, label = row.names(data)),
  #           vjust = -1, hjust = 1) +
  # exes
  xlim(0, 10) +
  ylim(0, 10) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(legend.position = "bottom")

plot5


ggsave(plot = plot5, dpi = 600, filename = "SVM_balance.png")

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