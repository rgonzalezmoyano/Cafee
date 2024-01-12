source("/home/PI/ricardo.gonzalezm/cafee/R/balanced_data.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/compute_scores.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/efficiency_estimation.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/efficiency_scores.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/preprocessing.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/projection.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/simulations.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/training.R")

# ================= #
# cobb_douglas_XnY1 #
# ================= #

# ===
# libraries
# ===

library(caret)
library(Benchmarking)
library(magrittr)
library(dplyr)

# ===
# parameters
# ===

DGP <- "cobb_douglas_XnY1"
N <- 50
noise <- c(0, 0.02, 0.05)
nX <- 6

# ===
# Table
# ===

repl <- 100

simulaciones <- data.frame (
  # general
  id = rep(NA, repl),
  DGP = rep(NA, repl),
  scenario = rep(NA, repl),
  N = rep(NA, repl),
  noise = rep(NA, repl),
  technique = rep(NA, repl),
  
  # correlations
  corr_yD_DEA = rep(NA, repl),
  corr_yD_BDEA = rep(NA, repl),
  corr_yD_cafee_DEA = rep(NA, repl),
  corr_yD_cafee_BDEA = rep(NA, repl),
  
  # mse
  mse_DEA = rep(NA, repl),
  mse_BDEA = rep(NA, repl),
  mse_cafee_DEA = rep(NA, repl),
  mse_cafee_BDEA = rep(NA, repl),
  
  # bias
  bias_DEA = rep(NA, repl),
  bias_BDEA = rep(NA, repl),
  bias_cafee_DEA = rep(NA, repl),
  bias_cafee_BDEA = rep(NA, repl)
  
)

# x and y indexes

if (nX == 1) {
  x <- 1
  y <- 2
} else if(nX == 3) {
  x <- 1:3
  y <- 4
} else if (nX == 6) {
  x <- 1:6
  y <- 7
} else if(nX == 9) {
  x <- 1:9
  y <- 10
} else if (nX == 12) {
  x <- 1:12
  y <- 13
} else if (nX == 15) {
  x <- 1:15
  y <- 16
}

# ===
# general information
# ===

# number of experiment
simulaciones$id <- 1:repl

# data generating process
simulaciones$DGP <- DGP

# type of scenario
simulaciones$scenario <- nX

# sample size
simulaciones$N <- N

# technique
simulaciones$technique <- "svmPoly"

# different types to label
label_type <- c("additive", "bootstrapping_dea")

set.seed(314)

for (std_dev in noise) {
  
  simulaciones$noise <- std_dev
  
  for (i in 1:repl) {

    repeat {
      
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
      
      # compute random error
      random_error <- rnorm(n = N, mean = 0, sd = std_dev)
      
      # compute new vector of outputs
      data[, y] <- data[, y] * exp(random_error)
      
      scores <- data.frame (
        score_yD = rep(NA, N),
        score_DEA = rep(NA, N),
        score_BDEA = rep(NA, N),
        score_cafee_DEA = rep(NA, N),
        score_cafee_BDEA = rep(NA, N)
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
      
      # =========================== #
      # score BD (Bootstraping DEA) #
      # =========================== #
      
      try_bdea <- tryCatch (
        {
          dea.boot (
            tech_xmat,
            tech_ymat,
            NREP = 200,
            ORIENTATION = "out",
            alpha = 0.01
          )[["eff.bc"]]
        },
        error = function(e) NULL
      )

      if (!is.null(try_bdea)) {

        bootstrapping_dea <- try_bdea
        scores$score_BDEA <- as.vector(bootstrapping_dea)

        break
      }
    }
    
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
    
    methods <- list (
      "svmPoly" = list(
        "degree" = c(1, 2, 3, 4, 5),
        "scale" = c(0.1, 1, 10),
        "C" = c(0.1, 1, 10, 100, 1000)
      )
    )
    
    # https://topepo.github.io/caret/train-models-by-tag.html
    
    metric = "F1"
    
    for (target_method in label_type) {
      
      # Result
      try_final_model <- tryCatch (
        {
          final_model <- efficiency_estimation (
            data = data,
            x = x,
            y = y,
            orientation = orientation,
            trControl = trControl,
            method = methods,
            target_method = target_method,
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
          
          if (target_method == "additive") {
            scores["score_cafee_DEA"] <- as.vector(scores_cafee)
            
          } else if (target_method == "bootstrapping_dea") {
            scores["score_cafee_BDEA"] <- as.vector(scores_cafee)
          }
          
        },
        error = function(e) NULL
      )

    }
    
    # ============ #
    # correlations #
    # ============ #
    
    # corr yD vs score_DEA
    
    simulaciones$corr_yD_DEA[i] <- as.numeric (
      cor (
        scores$score_yD, 
        scores$score_DEA, 
        use = "everything", 
        method = "pearson"
      )
    )
    
    # corr yD vs score_BDEA

    simulaciones$corr_yD_BDEA[i] <- as.numeric (
      cor (
        scores$score_yD,
        scores$score_BDEA,
        use = "everything",
        method = "pearson"
      )
    )
    
    # corr yD vs score_cafee_DEA
    
    if (any(is.na(scores$score_cafee_DEA)) == FALSE) {
      
      # there are not NA cases
      filtered_data <- scores
      
    } else {
      
      # there are NA cases
      # all are missing
      if (length(which(is.na(scores$score_cafee_DEA))) == N) {
        
        filtered_data <- scores
        
      } else {
        
        idx_NA_cafee_DEA <- which(is.na(scores$score_cafee_DEA))
        filtered_data <- scores[- idx_NA_cafee_DEA, ]
        
      }
      
    }
    
    simulaciones$corr_yD_cafee_DEA[i] <- as.numeric (
      cor (
        filtered_data$score_yD, 
        filtered_data$score_cafee_DEA, 
        use = "everything", 
        method = "pearson"
        )
    )
    
    # corr yD vs score_cafee_BDEA
    
    if (any(is.na(scores$score_cafee_BDEA)) == FALSE) {

      # there are not NA cases
      filtered_data <- scores

    } else {

      # there are NA cases
      # all are missing
      if (length(which(is.na(scores$score_cafee_DEA))) == N) {

        filtered_data <- scores

      } else {

        idx_NA_cafee_BDEA <- which(is.na(scores$score_cafee_BDEA))
        filtered_data <- scores[- idx_NA_cafee_BDEA, ]

      }

    }

    simulaciones$corr_yD_cafee_BDEA[i] <- as.numeric (
      cor (
        filtered_data$score_yD,
        filtered_data$score_cafee_BDEA,
        use = "everything",
        method = "pearson"
        )
    )
    
    # ============ #
    # MSE and bias #
    # ============ #
    
    # DEA measures
    diff_error <- scores[, "score_yD"] - scores[, "score_DEA"]
    
    simulaciones$mse_DEA[i] <- round(mean(diff_error ^ 2), 3)
    simulaciones$bias_DEA[i] <- round(mean(diff_error), 3)
    
    # BDEA measures
    diff_error <- scores[, "score_yD"] - scores[, "score_BDEA"]

    simulaciones$mse_BDEA[i] <- round(mean(diff_error ^ 2), 3)
    simulaciones$bias_BDEA[i] <- round(mean(diff_error), 3)
    
    # cafee_DEA measures
    if (any(is.na(scores$score_cafee_DEA)) == FALSE) {
      
      # there are not NA cases
      diff_error <- scores[, "score_yD"] - scores[, "score_cafee_DEA"]
      
    } else {
      
      # there are NA cases
      # all are missing
      if (length(which(is.na(scores$score_cafee_DEA))) == N) {
        
        diff_error <- as.vector(matrix(NA, nrow = N, ncol = 1))
        
      } else {
       
        diff_error <- scores[- idx_NA_cafee_DEA, "score_yD"] - scores[- idx_NA_cafee_DEA, "score_cafee_DEA"]
        
      }
      
    }
    
    simulaciones$mse_cafee_DEA[i] <- round(mean(diff_error ^ 2), 3)
    simulaciones$bias_cafee_DEA[i] <- round(mean(diff_error), 3)
    
    # cafee_BDEA measures

    if (any(is.na(scores$score_cafee_BDEA)) == FALSE) {

      # there are not NA cases
      diff_error <- scores[, "score_yD"] - scores[, "score_cafee_BDEA"]

    } else {

      # there are NA cases
      # all are missing
      if (length(which(is.na(scores$score_cafee_DEA))) == N) {

        diff_error <- as.vector(matrix(NA, nrow = N, ncol = 1))

      } else {

        diff_error <- scores[- idx_NA_cafee_BDEA, "score_yD"] - scores[- idx_NA_cafee_BDEA, "score_cafee_BDEA"]

      }

    }

    simulaciones$mse_cafee_BDEA[i] <- round(mean(diff_error ^ 2), 3)
    simulaciones$bias_cafee_BDEA[i] <- round(mean(diff_error), 3)
    
    # round results
    simulaciones[, 7:ncol(simulaciones)] <- round(simulaciones[, 7:ncol(simulaciones)], 3)
  }
  
  # to character to save name
  if (N == 25) {
    N_char <- "025"
  } else if (N == 50) {
    N_char <- "050"
  } else {
    N_char <- N
  }
  
  if (nX == 1) {
    scenario_char <- "01"
  } else if (nX == 3) {
    scenario_char <- "03"
  } else if (nX == 6) {
    scenario_char <- "06"
  } else if (nX == 9) {
    scenario_char <- "09" 
  } else if (nX == 12) {
    scenario_char <- "12"
  } else {
    scenario_char <- as.character(nX)
  }
  
  if (std_dev == 0) {
    noise_char <- "0.000"
  }  else if (std_dev == 0.02) {
    noise_char <- "0.020"
  } else if (std_dev == 0.05) {
    noise_char <- "0.050"
  } else {
    noise_char <- as.character(std_dev)
  }
  
  # ====== #
  # server #
  # ====== #
  
  file <- paste(DGP, "_", scenario_char, "_", N_char, "_", noise_char, ".RData", sep = "")
  save(simulaciones, file = file)
  
  # ========== #
  # local save #
  # ========== #
  
  # directory <- getwd()
  # 
  # # Nombre de la carpeta en la que deseas guardar el objeto
  # folder <- paste("/simulations/", DGP, sep = "")
  # 
  # new_directory <- paste(directory, folder, sep ="")
  # 
  # setwd(new_directory)
  # 
  # file <- paste(DGP, "_", scenario_char, "_", N_char, "_", noise_char, ".RData", sep = "")
  # save(simulaciones, file = file)
  # 
  # setwd(directory)
}
