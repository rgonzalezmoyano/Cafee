# library(dplyr)
# library(Rglpk)
# library(tictoc)
# library(lpSolveAPI)
# library(quadprog)

# source("/home/PI/vespana/aces/R/ACES.R")
# source("/home/PI/vespana/aces/R/backward_algorithm.R")
# source("/home/PI/vespana/aces/R/C2NLS.R")
# source("/home/PI/vespana/aces/R/efficiency_scores.R")
# source("/home/PI/vespana/aces/R/estimate_coefficients.R")
# source("/home/PI/vespana/aces/R/forward_algorithm.R")
# source("/home/PI/vespana/aces/R/predictions.R")
# source("/home/PI/vespana/aces/R/simulations.R")
# source("/home/PI/vespana/aces/R/smoothing_algorithm.R")

# ================= #
# cobb_douglas_XnY1 #
# ================= #

# libraries
library(caret)

# ===
# parameters
# ===
DGP <- "add_scenario_XnY1"
N <- 300
noise <- c(0, 0.005, 0.01, 0.03)
# scenario 
s <- "B"

# ===
# Table
# ===

simulaciones <- data.frame (
  # general
  id = rep(NA, N),
  scenario = rep(NA, N),
  N = rep(NA, N),
  noise = rep(NA, N),
  technique = rep(NA, N),

  # scores
  score_yD = rep(NA, N),
  score_DEA = rep(NA, N),
  score_cafee = rep(NA, N),
  
  # correlations
  corr_yD_DEA = rep(NA, N),
  corr_yD_cafee = rep(NA, N),
  
  # mse
  mse_DEA = rep(NA, N),
  mse_cafee = rep(NA, N),
  
  # bias
  bias_DEA = rep(NA, N),
  bias_cafee = rep(NA, N)
  )


# ===
# Generate data
# ===

set.seed(314)

data <- reffcy (
  DGP = "add_scenario_XnY1",
  parms = list (
    N = N,
    scenario = s
  )
)

data_original <- data

# x and y index
x <- 1
y <- 2

for (std_dev in noise) {
  
  data <- data_original
  
  # compute random error
  random_error <- rnorm(n = N, mean = 0, sd = std_dev)
  
  # compute new vector of outputs
  data[, y] <- data[, y] * exp(random_error)
  
  # general informations
  simulaciones$id <- 1:N 
  simulaciones$scenario <- 1
  simulaciones$N <- N
  simulaciones$noise <- std_dev
  simulaciones$technique <- "svmPoly"
  
  # ======== #
  # score yD #
  # ======== #
  
  for (i in 1:N) {
    
    simulaciones$score_yD[i] <- data_original[i, "yD"] / data[i, y]
    
  }
  
  # ========= #
  # score DEA #
  # ========= #
  
  rad_out <- function (
    tech_xmat, tech_ymat, eval_xmat, eval_ymat, convexity, returns
  ) {
    
    # number of DMUs in the technology
    tech_dmu <- nrow(tech_xmat)
    
    # number of DMUs to be evaluated
    eval_dmu <- nrow(eval_xmat)
    
    # initialize vector of scores
    scores <- matrix(nrow = eval_dmu, ncol = 1)
    
    # number of inputs and outputs
    nX <- ncol(tech_xmat)
    nY <- ncol(tech_ymat)
    
    for (d in 1:eval_dmu) {
      
      objVal <- matrix(ncol = 1 + tech_dmu, nrow = 1)
      objVal[1] <- 1
      
      lps <- make.lp(nrow = 0, ncol = 1 + tech_dmu)
      lp.control(lps, sense = 'max')
      set.objfn(lps, objVal)
      
      # inputs
      for (xi in 1:nX) {
        add.constraint(lps, xt = c(0, tech_xmat[, xi]), "<=",  rhs = eval_xmat[d, xi])
      }
      
      # outputs
      for (yi in 1:nY) {
        add.constraint(lps, xt = c(- eval_ymat[d, yi], tech_ymat[, yi]), ">=", rhs = 0)
      }
      
      # technology
      if (returns == "variable") {
        if (convexity) {
          add.constraint(lprec = lps, xt = c(0, rep(1, tech_dmu)), type = "=", rhs = 1)
        } else {
          add.constraint(lprec = lps, xt = c(0, rep(1, tech_dmu)), type = "=", rhs = 1)
          set.type(lps, columns = 1:tech_dmu + 1, type = c("binary"))
        }
      }
      
      solve(lps)
      scores[d, ] <- get.objective(lps)
    }
    
    return(scores)
  }
  
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
  
  simulaciones$score_DEA <- as.vector(bcc_scores)
  
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
      "degree" = c(2, 3, 4),
      "scale" = c(0.0001, 0.001, 0.01, 0.1, 1),
      "C" = c(seq(0, 100, length.out = 10), seq(200, 1000, length.out = 3))
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
  
  scores <- compute_scores (
    data = data,
    x = x,
    y = y,
    final_model = final_model,
    orientation = orientation
  )
  
  simulaciones$score_cafee <- as.vector(scores)
  
  # ============ #
  # correlations #
  # ============ #
  
  simulaciones$corr_yD_DEA <- as.numeric(cor(simulaciones$score_yD, simulaciones$score_DEA,
                                             use = "everything", method = "pearson")
  )
  
  idx_NA <- which(is.na(simulaciones$score_cafee))
  
  filter_data <- simulaciones[-idx_NA, ]
  
  simulaciones$corr_yD_cafee <- as.numeric(cor(filter_data$score_yD, filter_data$score_cafee,
                                               use = "everything", method = "pearson")
  )
  
  # ============ #
  # MSE and bias #
  # ============ #
  
  diff_error <- data[, "yD"] - data[, y] * simulaciones$score_DEA
  simulaciones$mse_DEA <- round(mean(diff_error ^ 2), 3)
  simulaciones$bias_DEA <- round(mean(diff_error), 3)
  
  diff_error <- data[-idx_NA, "yD"] - data[-idx_NA, y] * simulaciones$score_cafee[-idx_NA]
  simulaciones$mse_cafee <- round(mean(diff_error ^ 2), 3)
  simulaciones$bias_cafee <- round(mean(diff_error), 3)
  
  
  if (N == 25) {
    N <- "025"
  } else if (N == 50) {
    N <- "050"
  } else {
    N <- N
  }
  
  if (std_dev == 0) {
    noise <- "0.000"
  } else if (std_dev == 0.005) {
    noise <- "0.005"
  } else if (std_dev == 0.01) {
    noise <- "0.010"
  } else if (std_dev == 0.03) {
    noise <- "0.030"
  } else {
    noise <- as.character(noise)
  }
  
  directory <- getwd()
  
  # Nombre de la carpeta en la que deseas guardar el objeto
  folder <- paste("/simulations/", DGP, sep = "")
  
  new_directory <- paste(directory, folder, sep ="")
  
  setwd(new_directory)
  
  file <- paste("cob_douglas_XnY1_", s, "_", N, "_", noise, ".RData", sep = "")
  save(simulaciones, file = file)
  
  setwd(directory)
  
}
