# ===
# libraries
# ===

library(caret)

# ===
# sources
# ===

# source("/home/PI/vespana/aces/R/ACES.R")

# ===
# parameters
# ===

# data generation process
DGP <- "add_scenario_XnY1"
# sample size
N <- 300
# levels of random noise
noise <- c(0, 0.005, 0.01, 0.03)
# scenario
scenario <- "B"

# ===
# Table
# ===

repl <- 50

simulaciones <- data.frame (
  # general
  id = rep(NA, repl),
  DGP = rep(NA, repl),
  scenario = rep(NA, repl),
  N = rep(NA, repl),
  noise = rep(NA, repl),
  technique = rep(NA, repl),
  
  # parameters
  degree = rep(NA, repl),
  scale = rep(NA, repl),
  C = rep(NA, repl),
  
  # correlations
  corr_yD_DEA = rep(NA, repl),
  corr_yD_cafee = rep(NA, repl),
  
  # mse
  mse_DEA = rep(NA, repl),
  mse_cafee = rep(NA, repl),
  
  # bias
  bias_DEA = rep(NA, repl),
  bias_cafee = rep(NA, repl)
)

# ===
# x and y indexes
# ===

if (scenario %in% c("A", "B")) {
  x <- 1
  y <- 2
} else if (scenario %in% c("C", "E")) {
  x <- 1:2
  y <- 3
} else {
  x <- 1:3
  y <- 4
}

# ===
# general information
# ===

# number of experiment
simulaciones$id <- 1:repl
# data generating process
simulaciones$DGP <- DGP
# type of scenario
simulaciones$scenario <- scenario
# sample size
simulaciones$N <- N
# technique
simulaciones$technique <- "svmPoly"

# ===
# set seed
# ===

set.seed(314)

for (std_dev in noise) {
  
  simulaciones$noise <- std_dev
  
  for (i in 1:repl) {
    
    # ===
    # generate data
    # ===
    
    data <- reffcy (
      DGP = DGP,
      parms = list (
        N = N,
        scenario = scenario
      )
    )
    
    # compute random error
    random_error <- rnorm(n = N, mean = 0, sd = std_dev)
    
    # compute new vector of outputs with random error
    data[, y] <- data[, y] * exp(random_error)
    
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
    
    bcc_scores <- rad_out (
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
    
    simulaciones$degree[i] <- final_model[["bestTune"]][["degree"]]
    simulaciones$scale[i] <- final_model[["bestTune"]][["scale"]]
    simulaciones$C[i] <- final_model[["bestTune"]][["C"]]
    
    scores_cafee <- compute_scores (
      data = data,
      x = x,
      y = y,
      final_model = final_model,
      orientation = orientation
    )
    
    scores$score_cafee <- as.vector(scores_cafee)
    
    # ============ #
    # correlations #
    # ============ #
    
    simulaciones$corr_yD_DEA[i] <- as.numeric(cor(scores$score_yD, scores$score_DEA,
                                                  use = "everything", method = "pearson")
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
    
    simulaciones$corr_yD_cafee[i] <- as.numeric(cor(filter_data$score_yD, filter_data$score_cafee,
                                                    use = "everything", method = "pearson")
    )
    
    # ============ #
    # MSE and bias #
    # ============ #
    
    # DEA measures
    diff_error <- scores[, "score_yD"] - scores[, "score_DEA"]
    
    simulaciones$mse_DEA[i] <- round(mean(diff_error ^ 2), 3)
    simulaciones$bias_DEA[i] <- round(mean(diff_error), 3)
    
    # Cafee measures
    if (any(is.na(scores$score_cafee)) == FALSE) {
      
      # no NA case
      diff_error <- scores[, "score_yD"] - scores[, "score_cafee"]
      
    } else {
      
      # there are NA
      diff_error <- scores[-idx_NA, "score_yD"] - scores[-idx_NA, "score_cafee"]
      
    }
    
    simulaciones$mse_cafee[i] <- round(mean(diff_error ^ 2), 3)
    simulaciones$bias_cafee[i] <- round(mean(diff_error), 3)
    
  }
  
  if (N == 25) {
    N_char <- "025"
  } else if (N == 50) {
    N_char <- "050"
  } else {
    N_char <- N
  }
  
  if (std_dev == 0) {
    noise_char <- "0.000"
  } else if (std_dev == 0.005) {
    noise_char <- "0.005"
  } else if (std_dev == 0.01) {
    noise_char <- "0.010"
  } else if (std_dev == 0.03) {
    noise_char <- "0.030"
  } else {
    noise_char <- as.character(noise)
  }
  
  directory <- getwd()
  
  # Nombre de la carpeta en la que deseas guardar el objeto
  folder <- paste("/simulations/", DGP, sep = "")
  
  new_directory <- paste(directory, folder, sep ="")
  
  setwd(new_directory)
  
  file <- paste(DGP,  "_", scenario, "_", N_char, "_", noise_char, ".RData", sep = "")
  save(simulaciones, file = file)
  
  setwd(directory)
  
}
