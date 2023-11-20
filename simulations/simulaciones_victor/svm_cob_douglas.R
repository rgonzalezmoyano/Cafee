devtools::load_all()
# ================= #
# cobb_douglas_XnY1 #
# ================= #

# libraries
library(caret)

set.seed(314)

# ===
# parameters
# ===
DGP <- "cobb_douglas_XnY1"
N <- 25
nX <- 1

# ===
# Table
# ===

reps <- c(1:5)

# x and y index
x <- 1
y <- 2

for (rep in reps) {
  assign(paste0("df_cobb_douglas_XnY1_", N, "_", rep), simulaciones)
  
  name <- paste0("df_cobb_douglas_XnY1_", N, "_", rep)
  
  # ===
  # Generate data
  # ===
  data <- reffcy (
    DGP = "cobb_douglas_XnY1",
    parms = list (
      N = N,
      nX = nX
    )
  )
  
  scores <- data.frame(
    score_yD = rep(NA, N),
    score_DEA = rep(NA, N),
    score_cafee = rep(NA, N)
  )
  
  # ======== #
  # score yD #
  # ======== #
  
  scores$score_yD <- data[, "yD"] / data[, y]
  
  # ========= #
  # score DEA #
  # ========= #
  
  tech_xmat <- as.matrix(data[, x])
  tech_ymat <- as.matrix(data[, y])
  eval_xmat <- as.matrix(data[, x])
  eval_ymat <- as.matrix(data[, y])
  
  bcc_scores <- rad_inp (
    tech_xmat = tech_xmat,
    tech_ymat = tech_ymat,
    eval_xmat = eval_xmat,
    eval_ymat = eval_ymat,
    convexity = TRUE,
    returns = "variable"
  )
  
  scores$score_DEA <- as.vector(bcc_scores)
  
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
  
  hold_out <- 0.15
  
  methods <- list (
    "svmPoly" = list(
      "degree" = c(1, 2, 3, 4, 5),
      "scale" = c(0.1, 1, 10),
      "C" = c(0.1, 1, 10, 100, 1000)
    )
  )
  
  # https://topepo.github.io/caret/train-models-by-tag.html
  
  metric = "F1"
  
  # Result
  final_model <- efficiency_estimation (
    data = data,
    x = x,
    y = y,
    orientation = orientation,
    trControl = trControl,
    method = methods,
    metric = "F1",
    hold_out = hold_out
  )
  
  scores_cafee <- compute_scores (
    data = data,
    x = x,
    y = y,
    final_model = final_model,
    orientation = orientation
  )
  
  scores$score_cafee <- as.vector(scores_cafee)
  
  data <- cbind(data, scores)
  
  # ============ #
  # correlations #
  # ============ #
  
  data$corr_yD_DEA <- as.numeric (
    cor(scores$score_yD, scores$score_DEA, use = "everything", method = "pearson")
  )
  
  # index of NA if there are
  if (any(is.na(scores$score_cafee)) == FALSE) {
    
    # no NA case
    filter_data <- scores
    
  } else {
    
    # there are NA
    idx_NA <- which(is.na(scores$score_cafee))
    
    filter_data <- scores[-idx_NA, ]
    
  }
  
  data$corr_yD_cafee <- as.numeric (
    cor(filter_data$score_yD, filter_data$score_cafee, use = "everything", method = "pearson")
  )
  
  # ============ #
  # MSE and bias #
  # ============ #
  
  # DEA measures
  diff_error <- scores[, "score_yD"] - scores[, "score_DEA"]
  
  data$mse_DEA <- round(mean(diff_error ^ 2), 3)
  data$bias_DEA <- round(mean(diff_error), 3)
  
  # Cafee measures
  if (any(is.na(scores$score_cafee)) == FALSE) {
    
    # no NA case
    diff_error <- scores[, "score_yD"] - scores[, "score_cafee"]
    
  } else {
    
    # there are NA
    diff_error <- scores[-idx_NA, "score_yD"] - scores[-idx_NA, "score_cafee"]
    
  }
  
  data$mse_cafee <- round(mean(diff_error ^ 2), 3)
  data$bias_cafee <- round(mean(diff_error), 3)
  
  # ===
  # Crear data.frame
  # ===
  
  assign(paste0("df_cobb_douglas_XnY1_", N, "_", rep), data)
  
}