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
library(deaR)

# ===
# load data
# ===



# ===
# Table
# ===

# x and y indexes
x <- 1
y <- 2

# different types to label
label_type <- "additive"

set.seed(314)
  
list_information <- list()

methods <- list (
  "svmPoly" = list(
    "degree" = c(1, 2, 3, 4, 5, 6, 7),
    "scale" = c(0.001, 0.1, 1, 10),
    "C" = c(0.001, 0.1, 1, 10, 100, 1000)
  ),
  
)
  
  for (i in 1:repl) {
    
    
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
        "degree" = c(1, 2, 3, 4, 5, 6, 7),
        "scale" = c(0.001, 0.1, 1, 10),
        "C" = c(0.001, 0.1, 1, 10, 100, 1000)
      )
    )
    
    # https://topepo.github.io/caret/train-models-by-tag.html
    
    metric = "F1"
    
    for (target_method in label_type) {
      
      # Result
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
      
      if (target_method == "additive") {
        
        # probability to be not efficient
        p_cafee$p_cafee_DEA <- predict(final_model, data, type = "prob")[2]
        
        # ======================= #
        # 0.5 cut off is selected #
        # ======================= #
        scores_cafee <- compute_scores (
          data = data,
          x = x,
          y = y,
          final_model = final_model,
          orientation = orientation,
          cut_off = 0.5
        )
        
        scores["score_cafee_DEA_50"] <- as.vector(scores_cafee)
        
        # Pvalues
        pvalues_spearman$pvalues_cafee_DEA_50 <- cor.test (
          x = data[, "yD"], y = data[, y] * scores$score_cafee_DEA_50,
          alternative = "two.sided",
          method = "spearman",
          conf.level = 0.95)[["p.value"]]
        
        pvalues_kendall$pvalues_cafee_DEA_50 <- cor.test (
          x = data[, "yD"], y = data[, y] * scores$score_cafee_DEA_50,
          alternative = "two.sided",
          method = "kendall",
          conf.level = 0.95)[["p.value"]]
        
        # ======================== #
        # bset cut off is selected #
        # ======================== #
        scores_cafee <- compute_scores (
          data = data,
          x = x,
          y = y,
          final_model = final_model,
          orientation = orientation,
          cut_off = final_model[["cut_off"]]
        )
        
        scores["score_cafee_DEA_cut_off"] <- as.vector(scores_cafee)
        
        # Pvalues
        pvalues_spearman$pvalues_cafee_DEA_cut_off <- cor.test (
          x = data[, "yD"], y = data[, y] * scores$score_cafee_DEA_cut_off,
          alternative = "two.sided",
          method = "spearman",
          conf.level = 0.95)[["p.value"]]
        
        pvalues_kendall$pvalues_cafee_DEA_cut_off <- cor.test (
          x = data[, "yD"], y = data[, y] * scores$score_cafee_DEA_cut_off,
          alternative = "two.sided",
          method = "kendall",
          conf.level = 0.95)[["p.value"]]
        
        # cut_off value
        simulaciones$cut_off_cafee_DEA[i] <- final_model[["cut_off"]]
        
        # ==================================== #
        # hyperparameters information ML model #
        # ==================================== #
        simulaciones$technique_cafee_DEA[i] <- final_model[["method"]]
        simulaciones$hyperparameters_cafee_DEA[i] <- toString(final_model[["bestTune"]])
        
      } else if (target_method == "bootstrapping_dea") {
        
        # probability to be not efficient
        p_cafee$p_cafee_BDEA <-  predict(final_model, data, type = "prob")[2]
        
        # ======================= #
        # 0.5 cut off is selected #
        # ======================= #
        scores_cafee <- compute_scores (
          data = data,
          x = x,
          y = y,
          final_model = final_model,
          orientation = orientation,
          cut_off = 0.5
        )
        
        scores["score_cafee_BDEA_50"] <- as.vector(scores_cafee)
        
        # Pvalues
        pvalues_spearman$pvalues_cafee_BDEA_50 <- cor.test (
          x = data[, "yD"], y = data[, y] * scores$score_cafee_BDEA_50,
          alternative = "two.sided",
          method = "spearman",
          conf.level = 0.95)[["p.value"]]
        
        pvalues_kendall$pvalues_cafee_BDEA_50 <- cor.test (
          x = data[, "yD"], y = data[, y] * scores$score_cafee_BDEA_50,
          alternative = "two.sided",
          method = "kendall",
          conf.level = 0.95)[["p.value"]]
        
        # ======================== #
        # bset cut off is selected #
        # ======================== #
        scores_cafee <- compute_scores (
          data = data,
          x = x,
          y = y,
          final_model = final_model,
          orientation = orientation,
          cut_off = final_model[["cut_off"]]
        )
        
        scores["score_cafee_BDEA_cut_off"] <- as.vector(scores_cafee)
        
        # Pvalues
        pvalues_spearman$pvalues_cafee_BDEA_cut_off <- cor.test (
          x = data[, "yD"], y = data[, y] * scores$score_cafee_BDEA_cut_off,
          alternative = "two.sided",
          method = "spearman",
          conf.level = 0.95)[["p.value"]]
        
        pvalues_kendall$pvalues_cafee_BDEA_cut_off <- cor.test (
          x = data[, "yD"], y = data[, y] * scores$score_cafee_BDEA_cut_off,
          alternative = "two.sided",
          method = "kendall",
          conf.level = 0.95)[["p.value"]]
        
        # cut_off value
        simulaciones$cut_off_cafee_BDEA[i] <- final_model[["cut_off"]]
        
        # ==================================== #
        # hyperparameters information ML model #
        # ==================================== #
        simulaciones$technique_cafee_BDEA[i] <- final_model[["method"]]
        simulaciones$hyperparameters_cafee_BDEA[i] <- toString(final_model[["bestTune"]])
        
      }
      
    }
    
    # ============ #
    # correlations #
    # ============ #
    
    ###
    # corr yD vs score_DEA
    ###
    simulaciones$corr_pearson_yD_DEA[i] <- as.numeric (
      cor (
        scores$score_yD, 
        scores$score_DEA, 
        use = "everything", 
        method = "pearson"
      )
    )
    
    simulaciones$corr_spearman_yD_DEA[i] <- as.numeric (
      cor (
        scores$score_yD, 
        scores$score_DEA, 
        use = "everything", 
        method = "spearman"
      )
    )
    
    simulaciones$corr_kendall_yD_DEA[i] <- as.numeric (
      cor (
        scores$score_yD, 
        scores$score_DEA, 
        use = "everything", 
        method = "kendall"
      )
    )
    
    ###
    # corr yD vs score_BDEA
    ###
    simulaciones$corr_pearson_yD_BDEA[i] <- as.numeric (
      cor (
        scores$score_yD,
        scores$score_BDEA,
        use = "everything",
        method = "pearson"
      )
    )
    
    simulaciones$corr_spearman_yD_BDEA[i] <- as.numeric (
      cor (
        scores$score_yD,
        scores$score_BDEA,
        use = "everything",
        method = "spearman"
      )
    )
    
    simulaciones$corr_kendall_yD_BDEA[i] <- as.numeric (
      cor (
        scores$score_yD,
        scores$score_BDEA,
        use = "everything",
        method = "kendall"
      )
    )
    
    ###
    # corr yD vs score_cross_efficiency
    ###
    simulaciones$corr_pearson_yD_cross_eff[i] <- as.numeric (
      cor (
        scores$score_yD,
        scores$score_cross_eff,
        use = "everything",
        method = "pearson"
      )
    )
    
    simulaciones$corr_spearman_yD_cross_eff[i] <- as.numeric (
      cor (
        scores$score_yD,
        scores$score_cross_eff,
        use = "everything",
        method = "spearman"
      )
    )
    
    simulaciones$corr_kendall_yD_cross_eff[i] <- as.numeric (
      cor (
        scores$score_yD,
        scores$score_cross_eff,
        use = "everything",
        method = "kendall"
      )
    )
    
    ###
    # corr yD vs score_super_efficiency
    ###
    simulaciones$corr_pearson_yD_super_eff[i] <- as.numeric (
      cor (
        scores$score_yD,
        scores$score_super_eff,
        use = "everything",
        method = "pearson"
      )
    )
    
    simulaciones$corr_spearman_yD_super_eff[i] <- as.numeric (
      cor (
        scores$score_yD,
        scores$score_super_eff,
        use = "everything",
        method = "spearman"
      )
    )
    
    simulaciones$corr_kendall_yD_super_eff[i] <- as.numeric (
      cor (
        scores$score_yD,
        scores$score_super_eff,
        use = "everything",
        method = "kendall"
      )
    )
    
    if ("additive" %in% label_type) {
      
      ###
      # corr yD vs score_cafee_DEA_50
      ###
      if (any(is.na(scores$score_cafee_DEA_50)) == FALSE) {
        
        # there are not NA cases
        filtered_data <- scores
        
      } else {
        
        # there are NA cases
        # all are missing
        if (length(which(is.na(scores$score_cafee_DEA_50))) == N) {
          
          filtered_data <- scores
          
        } else {
          
          idx_NA_cafee_DEA_50 <- which(is.na(scores$score_cafee_DEA_50))
          filtered_data <- scores[- idx_NA_cafee_DEA_50, ]
          
        }
        
      }
      
      simulaciones$corr_pearson_yD_score_cafee_DEA_50[i] <- as.numeric (
        cor (
          filtered_data$score_yD, 
          filtered_data$score_cafee_DEA_50, 
          use = "everything", 
          method = "pearson"
        )
      )
      
      simulaciones$corr_spearman_yD_score_cafee_DEA_50[i] <- as.numeric (
        cor (
          filtered_data$score_yD, 
          filtered_data$score_cafee_DEA_50, 
          use = "everything", 
          method = "spearman"
        )
      )
      
      simulaciones$corr_kendall_yD_score_cafee_DEA_50[i] <- as.numeric (
        cor (
          filtered_data$score_yD, 
          filtered_data$score_cafee_DEA_50, 
          use = "everything", 
          method = "kendall"
        )
      )
      
      ###
      # corr yD vs score_cafee_DEA_cut_off
      ###
      if (any(is.na(scores$score_cafee_DEA_cut_off)) == FALSE) {
        
        # there are not NA cases
        filtered_data <- scores
        
      } else {
        
        # there are NA cases
        # all are missing
        if (length(which(is.na(scores$score_cafee_DEA_cut_off))) == N) {
          
          filtered_data <- scores
          
        } else {
          
          idx_NA_cafee_DEA_cut_off <- which(is.na(scores$score_cafee_DEA_cut_off))
          filtered_data <- scores[- idx_NA_cafee_DEA_cut_off, ]
          
        }
        
      }
      
      simulaciones$corr_pearson_yD_score_cafee_DEA_cut_off[i] <- as.numeric (
        cor (
          filtered_data$score_yD, 
          filtered_data$score_cafee_DEA_cut_off, 
          use = "everything", 
          method = "pearson"
        )
      )
      
      simulaciones$corr_spearman_yD_score_cafee_DEA_cut_off[i] <- as.numeric (
        cor (
          filtered_data$score_yD, 
          filtered_data$score_cafee_DEA_cut_off, 
          use = "everything", 
          method = "spearman"
        )
      )
      
      simulaciones$corr_kendall_yD_score_cafee_DEA_cut_off[i] <- as.numeric (
        cor (
          filtered_data$score_yD, 
          filtered_data$score_cafee_DEA_cut_off, 
          use = "everything", 
          method = "kendall"
        )
      )
      
    }
    
    if ("bootstrapping_dea" %in% label_type) {
      
      ###
      # corr yD vs score_cafee_BDEA_50
      ###
      
      if (any(is.na(scores$score_cafee_BDEA_50)) == FALSE) {
        
        # there are not NA cases
        filtered_data <- scores
        
      } else {
        
        # there are NA cases
        # all are missing
        if (length(which(is.na(scores$score_cafee_BDEA_50))) == N) {
          
          filtered_data <- scores
          
        } else {
          
          idx_NA_cafee_BDEA_50 <- which(is.na(scores$score_cafee_BDEA_50))
          filtered_data <- scores[- idx_NA_cafee_BDEA_50, ]
          
        }
        
      }
      
      simulaciones$corr_pearson_yD_score_cafee_BDEA_50[i] <- as.numeric (
        cor (
          filtered_data$score_yD,
          filtered_data$score_cafee_BDEA_50,
          use = "everything",
          method = "pearson"
        )
      )
      
      simulaciones$corr_spearman_yD_score_cafee_BDEA_50[i] <- as.numeric ( 
        cor (
          filtered_data$score_yD,
          filtered_data$score_cafee_BDEA_50,
          use = "everything",
          method = "spearman"
        )
      )
      
      simulaciones$corr_kendall_yD_score_cafee_BDEA_50[i] <- as.numeric (
        cor (
          filtered_data$score_yD,
          filtered_data$score_cafee_BDEA_50,
          use = "everything",
          method = "kendall"
        )
      )
      
      ###
      # corr yD vs score_cafee_BDEA_cut_off
      ###
      if (any(is.na(scores$score_cafee_BDEA_cut_off)) == FALSE) {
        
        # there are not NA cases
        filtered_data <- scores
        
      } else {
        
        # there are NA cases
        # all are missing
        if (length(which(is.na(scores$score_cafee_BDEA_cut_off))) == N) {
          
          filtered_data <- scores
          
        } else {
          
          idx_NA_cafee_BDEA_cut_off <- which(is.na(scores$score_cafee_BDEA_cut_off))
          filtered_data <- scores[- idx_NA_cafee_BDEA_cut_off, ]
          
        }
        
      }
      
      simulaciones$corr_pearson_yD_score_cafee_BDEA_cut_off[i] <- as.numeric (
        cor (
          filtered_data$score_yD,
          filtered_data$score_cafee_BDEA_cut_off,
          use = "everything",
          method = "pearson"
        )
      )
      
      simulaciones$corr_spearman_yD_score_cafee_BDEA_cut_off[i] <- as.numeric ( 
        cor (
          filtered_data$score_yD,
          filtered_data$score_cafee_BDEA_cut_off,
          use = "everything",
          method = "spearman"
        )
      )
      
      simulaciones$corr_kendall_yD_score_cafee_BDEA_cut_off[i] <- as.numeric (
        cor (
          filtered_data$score_yD,
          filtered_data$score_cafee_BDEA_cut_off,
          use = "everything",
          method = "kendall"
        )
      )
    }
    
    ###
    # corr yD vs p_cafee_DEA
    ###
    simulaciones$corr_pearson_yD_p_cafee_DEA[i] <- as.numeric (
      cor (
        scores$score_yD,
        p_cafee$p_cafee_DEA,
        use = "everything",
        method = "pearson"
      )
    )
    
    simulaciones$corr_spearman_yD_p_cafee_DEA[i] <- as.numeric (
      cor (
        scores$score_yD,
        p_cafee$p_cafee_DEA,
        use = "everything",
        method = "spearman"
      )
    )
    
    simulaciones$corr_kendall_yD_p_cafee_DEA[i] <- as.numeric (
      cor (
        scores$score_yD,
        p_cafee$p_cafee_DEA,
        use = "everything",
        method = "kendall"
      )
    )
    
    ###
    # corr yD vs p_cafee_BDEA
    ###
    simulaciones$corr_pearson_yD_p_cafee_BDEA[i] <- as.numeric (
      cor (
        scores$score_yD,
        p_cafee$p_cafee_BDEA,
        use = "everything",
        method = "pearson"
      )
    )
    
    simulaciones$corr_spearman_yD_p_cafee_BDEA[i] <- as.numeric (
      cor (
        scores$score_yD,
        p_cafee$p_cafee_BDEA,
        use = "everything",
        method = "spearman"
      )
    )
    
    simulaciones$corr_kendall_yD_p_cafee_BDEA[i] <- as.numeric (
      cor (
        scores$score_yD,
        p_cafee$p_cafee_BDEA,
        use = "everything",
        method = "kendall"
      )
    )
    
    # ============ #
    # MSE and bias #
    # ============ #
    
    # DEA measures
    diff_error <- scores[, "score_yD"] - scores[, "score_DEA"]
    
    simulaciones$mse_DEA[i] <- round(mean(diff_error ^ 2), 3)
    simulaciones$bias_DEA[i] <- abs(round(mean(diff_error), 3))
    
    # BDEA measures
    diff_error <- scores[, "score_yD"] - scores[, "score_BDEA"]
    
    simulaciones$mse_BDEA[i] <- round(mean(diff_error ^ 2), 3)
    simulaciones$bias_BDEA[i] <- abs(round(mean(diff_error), 3))
    
    # cross-efficiency measures
    diff_error <- scores[, "score_yD"] - scores[, "score_cross_eff"]
    
    simulaciones$mse_cross_eff[i] <- round(mean(diff_error ^ 2), 3)
    simulaciones$bias_cross_eff[i] <- abs(round(mean(diff_error), 3))
    
    # cross-efficiency measures
    diff_error <- scores[, "score_yD"] - scores[, "score_super_eff"]
    
    simulaciones$mse_super_eff[i] <- round(mean(diff_error ^ 2), 3)
    simulaciones$bias_super_eff[i] <- abs(round(mean(diff_error), 3))
    
    if ("additive" %in% label_type) {
      
      # cafee_DEA_50 measures
      if (any(is.na(scores$score_cafee_DEA_50)) == FALSE) {
        
        # there are not NA cases
        diff_error <- scores[, "score_yD"] - scores[, "score_cafee_DEA_50"]
        
      } else {
        
        # there are NA cases
        # all are missing
        if (length(which(is.na(scores$score_cafee_DEA_50))) == N) {
          
          diff_error <- as.vector(matrix(NA, nrow = N, ncol = 1))
          
        } else {
          
          diff_error <- scores[- idx_NA_cafee_DEA_50, "score_yD"] - scores[- idx_NA_cafee_DEA_50, "score_cafee_DEA_50"]
          
        }
        
      }
      
      simulaciones$mse_cafee_DEA_50[i] <- round(mean(diff_error ^ 2), 3)
      simulaciones$bias_cafee_DEA_50[i] <- abs(round(mean(diff_error), 3))
      
      # cafee_DEA_cut_off measures
      if (any(is.na(scores$score_cafee_DEA_cut_off)) == FALSE) {
        
        # there are not NA cases
        diff_error <- scores[, "score_yD"] - scores[, "score_cafee_DEA_cut_off"]
        
      } else {
        
        # there are NA cases
        # all are missing
        if (length(which(is.na(scores$score_cafee_DEA_cut_off))) == N) {
          
          diff_error <- as.vector(matrix(NA, nrow = N, ncol = 1))
          
        } else {
          
          diff_error <- scores[- idx_NA_cafee_DEA_cut_off, "score_yD"] - scores[- idx_NA_cafee_DEA_cut_off, "score_cafee_DEA_cut_off"]
          
        }
        
      }
      
      simulaciones$mse_cafee_DEA_cut_off[i] <- round(mean(diff_error ^ 2), 3)
      simulaciones$bias_cafee_DEA_cut_off[i] <- abs(round(mean(diff_error), 3))
      
    }
    
    if ("bootstrapping_dea" %in% label_type) {
      
      # cafee_BDEA_50 measures
      
      if (any(is.na(scores$score_cafee_BDEA_50)) == FALSE) {
        
        # there are not NA cases
        diff_error <- scores[, "score_yD"] - scores[, "score_cafee_BDEA_50"]
        
      } else {
        
        # there are NA cases
        # all are missing
        if (length(which(is.na(scores$score_cafee_BDEA_50))) == N) {
          
          diff_error <- as.vector(matrix(NA, nrow = N, ncol = 1))
          
        } else {
          
          diff_error <- scores[- idx_NA_cafee_BDEA_50, "score_yD"] - scores[- idx_NA_cafee_BDEA_50, "score_cafee_BDEA_50"]
          
        }
        
      }
      
      simulaciones$mse_cafee_BDEA_50[i] <- round(mean(diff_error ^ 2), 3)
      simulaciones$bias_cafee_BDEA_50[i] <- abs(round(mean(diff_error), 3))
      
      # cafee_BDEA_cut_off measures
      
      if (any(is.na(scores$score_cafee_BDEA_cut_off)) == FALSE) {
        
        # there are not NA cases
        diff_error <- scores[, "score_yD"] - scores[, "score_cafee_BDEA_cut_off"]
        
      } else {
        
        # there are NA cases
        # all are missing
        if (length(which(is.na(scores$score_cafee_DEA))) == N) {
          
          diff_error <- as.vector(matrix(NA, nrow = N, ncol = 1))
          
        } else {
          
          diff_error <- scores[- idx_NA_cafee_BDEA_cut_off, "score_yD"] - scores[- idx_NA_cafee_BDEA_cut_off, "score_cafee_BDEA_cut_off"]
          
        }
        
      }
      
      simulaciones$mse_cafee_BDEA_cut_off[i] <- round(mean(diff_error ^ 2), 3)
      simulaciones$bias_cafee_BDEA_cut_off[i] <- abs(round(mean(diff_error), 3))
      
    }
    # round results
    simulaciones[, 12:ncol(simulaciones)] <- round(simulaciones[, 12:ncol(simulaciones)], 3)
    
    # save scores and pvalues
    # second level of the list
    list <- list()
    
    list$scores <- scores
    list$p_cafee <- p_cafee
    list$pvalues_spearman <- pvalues_spearman
    list$pvalues_kendall <- pvalues_kendall
    
    
    # first level of the list
    list_information[[i]] <- list
    
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
  
  file_information <- paste("information_", DGP, "_", scenario_char, "_", N_char, "_", noise_char, ".RData", sep = "")
  save(list_information, file = file_information)
  
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

