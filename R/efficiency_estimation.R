#' @title Efficiency Estimation Using Classification Algorithms
#'
#' @description This function uses classification algorithms to estimate the efficiency of a set of DMUs (Decision Making Units).
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column indexes of input variables in \code{data}.
#' @param y Column indexes of output variables in \code{data}.
#' @param z Column indexes of environment variables in \code{data}.
#' @param balance_data Indicate the number of efficient and not efficient units.
#' @param target_method Methodology for labeling the data.
#' @param trControl Parameters for controlling the training process (from the \code{'caret'} package).
#' @param methods A \code{list} of selected machine learning models and their hyperparameters.
#' @param metric A \code{string} specifying the summary metric for classification to select the optimal model. Default includes \code{"Balanced_accuracy"} due to (normally) unbalanced data.
#' @param hold_out A \code{number} value (5-20) for validation data percentage during training (default: 0.2).
#' @param convexity A \code{logical} value indicating if a convex technology is assumed.
#' @param returns Type of returns to scale.
#' @param scenarios Level of efficiency to peer.
#'
#' @importFrom caret trainControl train createDataPartition defaultSummary prSummary
#' @importFrom dplyr select_if %>% arrange top_n sample_n
#' @importFrom Benchmarking dea.boot
#'
#' @return A \code{"cafee"} object.
#'
#' @export

efficiency_estimation <- function (
    data, x, y, z = NULL, orientation, target_method,
    trControl, methods, metric, hold_out, balance_data = c(3, 1),
    scenarios = 0.75, convexity = TRUE, returns = "variable"
    ) {

  # save factor variables
  data_factor <- data[, z]
  
  # pre-processing
  data <- preprocessing (
    data = data, 
    x = x, 
    y = y
  )

  # reorder index 'x' and 'y' in data
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)
  
  if (is.null(z)) {
    z <- NULL
  } else {
    z <- (ncol(data) + 1):(ncol(data) + ncol(data_factor))
  }
  
  # number of inputs / outputs as inputs and number of outputs
  nX <- length(x)
  nY <- length(y)
  nZ <- length(z)
  
  if (target_method == "bootstrapping_dea") {
    
    # ========================== #
    # Label by bootstrapping_dea #
    # ========================== #
    
    bootstrapping_dea <- dea.boot (
      X = as.matrix(data[, x]),
      Y = as.matrix(data[, y]),
      NREP = 200,
      ORIENTATION = "out",
      alpha = 0.01
      # CONTROL = list(scaling = c("curtisreid", "equilibrate"))
    )

    data <- as.data.frame(data)
    
    # 3 labelling as not efficient
    data$class_efficiency <- rep("not_efficient", nrow(data))
    
    # labelling as efficient
    data_opt <- as.data.frame(cbind(data[, x], y = bootstrapping_dea[["eff.bc"]] * data[, y]))
    data_opt$class_efficiency <- "efficient"
    names(data_opt) <- names(data)

    # join data
    data <- rbind(data, data_opt)
    data$class_efficiency <- as.factor(data$class_efficiency)

    # class_efficiency as factor
    levels(data$class_efficiency) <- c("efficient", "not_efficient")

  } else if (target_method == "BCC") {
    
    # ======================= #
    # Label by BCC model DEA  #
    # ======================= #
     
    # compute DEA scores through a BCC model
     add_scores <-  rad_out (
       tech_xmat = as.matrix(data[, x]),
       tech_ymat = as.matrix(data[, y]),
       eval_xmat = as.matrix(data[, x]),
       eval_ymat = as.matrix(data[, y]),
       convexity = convexity,
       returns = returns
     ) 
    
    # determine efficient and inefficient DMUs
    class_efficiency <- ifelse(add_scores[, 1] <= 1.0001, 1, 0)

    data <- as.data.frame (
      cbind(data, class_efficiency)
    )
    
    data$class_efficiency <- factor(data$class_efficiency)
    data$class_efficiency <- factor (
      data$class_efficiency,
      levels = rev(levels(data$class_efficiency))
    )

    levels(data$class_efficiency) <- c("efficient", "not_efficient")
    
  } else if (target_method == "additive") {
    
    # ============================ #
    # Label by additive model DEA  #
    # ============================ #
    
    # compute DEA scores through a BCC model
    add_scores <-  compute_scores_additive (
      data = data,
      x = x,
      y = y
    ) 
    
    # determine efficient and inefficient DMUs
    class_efficiency <- ifelse(add_scores[, 1] <= 0.0001, 1, 0)
    
    data <- as.data.frame (
      cbind(data, class_efficiency)
    )
    
    data$class_efficiency <- factor(data$class_efficiency)
    data$class_efficiency <- factor (
      data$class_efficiency,
      levels = rev(levels(data$class_efficiency))
    )
    
    levels(data$class_efficiency) <- c("efficient", "not_efficient")
    
  }
  
  pre_data <- data
  
  # add factor variables
  data <- cbind(data, data_factor)
  
  # save a copy of the original data
  eval_data <- data
  
  # observed proportion of efficient and inefficient DMUs
  obs_prop <- prop.table(table(data$class_efficiency))
  
  # set sub frontier
  sub_frontier <- balance_data[["sub_frontier"]]
  
  save_models_balance <- vector("list", length = length(balance_data$balance_proportions))
  names(save_models_balance) <- balance_data$balance_proportions
  
  save_confusion_matrix <- vector("list", length = length(balance_data$balance_proportions))
  names(save_confusion_matrix) <- balance_data$balance_proportions
  
  compare_confusion_matrix <- vector("list", length = length(balance_data$balance_proportions))
  names(compare_confusion_matrix) <- balance_data$balance_proportions
  
  for (balance in 1:length(balance_data$balance_proportions)) {
    
    balance_data_prop <- balance_data[["balance_proportions"]][balance]
    data <- eval_data
    
    # check presence of imbalanced data
    if (max(obs_prop[1], obs_prop[2]) > 0.50) {

      if (balance_data[["balance_proportions"]][balance] != 0) {
        data <- SMOTE_convex_balance_data(
          data = data,
          data_factor = data_factor,
          x = x,
          y = y,
          z = z,
          balance_data = balance_data_prop,
          sub_frontier = sub_frontier
        )

        library(cxhull)
        data_hull <- as.matrix(eval_data[which(eval_data$class_efficiency == "efficient"), c(x,y)])
        
        hull <- cxhull(data_hull)
        
        facets <- matrix(
          data = NA,
          ncol = ncol(eval_data[, c(x,y)]),
          nrow = length(hull[["facets"]])
        )
 
        lambda <- 1/ncol(eval_data[,c(x,y)])
        
        facets_detect <- lapply(hull[["facets"]], function(facet) {
          
          all_facets <- facet[["vertices"]]
          
          # if (length(all_facets) == 5) {
          #   
          #   all_facets <- data_hull[all_facets, ]
          #   colSums(all_facets * lambda)
          #   
          # } else {
          #   
          #   NULL  
          #   
          # }
          
        })
        
        
        
        facets_filtered <- facets_detect[!sapply(facets_detect, is.null)]
        
        facets <- do.call(rbind, facets_filtered) 
        
        
        browser()
        
        data_test_hull <- rbind(data_hull, facets)
        
        test_add_hull <- compute_scores_additive(
          data = data_test_hull,
          x = x,
          y = y
        )
        n_eff_org <- length(which(eval_data$class_efficiency == "efficient"))
        
        idx_convex <- which(as.matrix(test_add_hull[-(1:n_eff_org),]) < 0.0001)
        convex_combinations <- facets[idx_convex, ]
    
      }
      
    } # end balance data
    
    data_after_balance <- data
    
    if (hold_out != 0) {
      
      # Create train and validation data
      valid_index <- createDataPartition (
        data$class_efficiency,
        p = hold_out,
        list = FALSE
      )
      
      # divide dataset
      valid_data <- data[valid_index, ]
      train_data <- data[- valid_index, ]
      
    } else {
      
      valid_data <- data
      train_data <- data
      
    }
    
    # ====================== #
    # SELECT HYPERPARAMETERS #
    # ====================== #

    ml_model <- train_ml (
      data = train_data,
      trControl = trControl,
      methods = methods,
      metric = metric
    )
    
    # Best training 
    # n_matrix <- length(methods) * length(balance_data$balance_proportions)
    confusion_matrix <- vector("list", length = length(methods))
    names(confusion_matrix) <- names(methods)
    
    parms_vals <- vector("list", length = length(methods))
    names(parms_vals) <- names(methods)
    
    if (ml_model[[1]][["method"]] %in% c("rf")) {
      
      option_vals <- vector("list", length = length(methods$nnet$options))
      names(option_vals) <- names(methods$nnet$options)
      
    }
    
    # ================= #
    # SELECT BEST MODEL #
    # ================= #
    
    for (i in 1:length(methods)) {
      
      # parameter position
      parms_posn <- which(names(ml_model[[i]]) %in% names(methods[[i]]$hyparams)) 
      
      # parameter values
      parms_vals[[i]] <- as.data.frame(ml_model[[i]])[, parms_posn]
      
      if (ml_model[[1]]["method"] %in% c("rf")) {
        
        # option position
        option_posn <- which(names(ml_model[[i]]) %in% names(methods[[i]]$options)) 
        
        # optin values
        option_vals[[i]] <- as.data.frame(ml_model[[i]])[, option_posn]
        
      }
      
      # rename parameters if is null
      if (is.null(names(parms_vals[[i]]))) {
        parms_vals[[i]] <- as.data.frame(parms_vals[[i]])
        names(parms_vals[[i]]) <- names(methods[[i]]$hyparams)
      }
      
      # avoid messages for some methods
      verb_methods <- c("gbm", "svmPoly")
      
      if (names(methods[i]) == "rf") {
        
        best_ml_model <- train (
          form = class_efficiency ~.,
          data = train_data,
          method = names(methods[i]),
          tuneGrid = parms_vals[[i]],
          trControl = trainControl(method = "none", classProbs = TRUE),
          ntree = option_vals[[i]]
        )
        
      } else if (names(methods[i]) == "nnet") {
        
        best_ml_model <- train (
          form = class_efficiency ~.,
          data = train_data,
          method = names(methods[i]),
          tuneGrid = parms_vals[[i]],
          trControl = trainControl(method = "none", classProbs = TRUE),
          maxit = methods$nnet$options$maxit
        )
        
      }
      
      if (names(methods[i]) %in% verb_methods) {
        
        # tune models
        best_ml_model <- train (
          form = class_efficiency ~.,
          data = train_data,
          method = names(methods[i]),
          tuneGrid = parms_vals[[i]],
          verbose = FALSE
        )
        
      } else {
        # Tune models
        best_ml_model <- train (
          form = class_efficiency ~.,
          data = train_data,
          method = names(methods[i]),
          tuneGrid = parms_vals[[i]]
        )
      }

      y_obs <- valid_data$class_efficiency
      y_hat <- predict(best_ml_model, valid_data) # 0.5 but can it change? 0.75, 0.85, 0.95

      #create confusion matrix and calculate metrics related to confusion matrix
      confusion_matrix[[i]] <- confusionMatrix (
        data = y_hat,
        reference = y_obs,
        mode = "everything",
        positive = "efficient"
      )[["byClass"]]
      
    }
    
    # matrix for model evaluation
    precision_models <- matrix (
      nrow = length(methods),
      ncol = length(names(confusion_matrix[[1]]))
    )
    
    precision_models <- as.data.frame(precision_models)
    
    # names of precision_models matrix
    colnames(precision_models) <- names(confusion_matrix[[1]])
    rownames(precision_models) <- names(methods)
    
    names(precision_models)[ncol(precision_models)] <- "Balanced_Accuracy"
   
    for (i in 1:length(methods)) {
      precision_models[i, ] <- confusion_matrix[[i]]
    }
    
    # select the best model by metric
    selected_model <- precision_models %>%
      arrange(desc("Balanced_Accuracy"), desc(F1), desc(Precision), desc(Sensitivity))
    selected_model <- selected_model[1, ]
    
    save_confusion_matrix[[balance]] <- selected_model
    
    # index of the best model in ml_model
    best_model_index <- which(row.names(selected_model) == ml_model[[i]]["method"]) # names(ml_model)
    
    # ============== #
    # FIT BEST MODEL #
    # ============== #
    
    # avoid messages for some methods
    verb_methods <- c("gbm", "svmPoly")
    
    repeat {
      if (names(methods[best_model_index]) == "rf") {
        final_model <- train (
          form = class_efficiency ~.,
          data = data,
          method = row.names(selected_model),
          tuneGrid = parms_vals[[best_model_index]],
          ntree = option_vals[[i]],
          # verbose = FALSE,
          #trControl = trainControl(method = "none", classProbs = TRUE)
          trControl = trainControl(method = "oob")
        )
        
      } else if (names(methods[best_model_index]) == "nnet") {
        
        final_model <- train (
          form = class_efficiency ~.,
          data = data,
          method = row.names(selected_model),
          tuneGrid = parms_vals[[best_model_index]],
          verbose = FALSE,
          trControl = trainControl(method = "none", classProbs = TRUE),
          maxit = methods$nnet$options$maxit
        )
        
      } else {
        
        # generic ml model: svm...
        final_model <- train (
          form = class_efficiency ~.,
          data = data,
          method = row.names(selected_model),
          tuneGrid = parms_vals[[best_model_index]],
          trControl = trainControl(method = "none", classProbs = TRUE)
        )
        
      }
      
      try_cut_off <- tryCatch (
        {
          cut_off <- select_cut_off (
            data = valid_data,
            final_model = final_model
          )
        },
        error = function(e) NULL
      )
      
      if (!is.null(try_cut_off)) {
        
        cut_off <- try_cut_off
        final_model$cut_off <- cut_off
        break
      }
    }
    
    save_models_balance[[balance]] <- final_model
    save_confusion_matrix[[balance]] <- confusion_matrix

  } # loop balance_data

  decision_balance <- as.data.frame(matrix(
    data = NA,
    ncol = length(c("Method", "Balance", names(save_confusion_matrix[[1]][[1]]))),
    nrow = length(balance_data$balance_proportions)
  ))
  
  names(decision_balance) <- c("Method", "Balance", names(save_confusion_matrix[[1]][[1]]))
  
  for (n in 1:length(balance_data$balance_proportions)) {
    decision_balance[n, 1] <- names(save_confusion_matrix[[n]])
    decision_balance[n, 2] <-  names(save_confusion_matrix[n])
    
    decision_balance[n, 3:ncol(decision_balance)] <- save_confusion_matrix[[n]][[names(save_confusion_matrix[[n]])]]
  }

  train_decision_balance <- decision_balance
  
  train_decision_balance[, 3:ncol(decision_balance)] <- round(decision_balance[, 3:ncol(decision_balance)], 2)

  # change name to arrange correct
  names(train_decision_balance)[13] <- "Balanced_Accuracy"
  
  train_decision_balance <- train_decision_balance %>% 
    arrange(desc(Balanced_Accuracy), desc(F1), desc(Precision), desc(Specificity))
  
  ###
  # test with real data
  ###
  test_decision_balance <- as.data.frame(matrix(
    data = NA,
    ncol = length(c("Method", "Balance", names(save_confusion_matrix[[1]][[1]]))),
    nrow = length(balance_data$balance_proportions)
  ))
  
  names(test_decision_balance) <- c("Method", "Balance", names(save_confusion_matrix[[1]][[1]]))
  
  # test confusion matrix
  test_confmatrix <- list()
  y_obs_test <- eval_data$class_efficiency
  
  for (m in 1:length(save_models_balance)) {
    
    y_pre_test <- predict(save_models_balance[m], eval_data)[[1]]
    
    test_confmatrix[[m]] <- confusionMatrix (
      data = y_pre_test,
      reference = y_obs_test,
      mode = "everything",
      positive = "efficient"
    )[["byClass"]]
  }
  names(test_confmatrix) <- balance_data$balance_proportions
  
  for (n in 1:length(balance_data$balance_proportions)) {
    test_decision_balance[n, 1] <- save_models_balance[[n]][["method"]]
    test_decision_balance[n, 2] <-  names(test_confmatrix[n])
    
    test_decision_balance[n, 3:ncol(decision_balance)] <- round(test_confmatrix[[n]], 2)
  }
  
  selected_real_balance <- test_decision_balance
  
  names(selected_real_balance)[13] <- "Balanced_Accuracy"

  selected_real_balance <- selected_real_balance %>% 
    arrange(desc(Balanced_Accuracy), desc(F1), desc(Precision), desc(Sensitivity), Balance)
  
  # select best balance hyperparameter
  first <- selected_real_balance[1, 3:ncol(selected_real_balance)]
  
  duplicated_metrics <- as.numeric(names(which(apply(selected_real_balance[2:3, 3:ncol(selected_real_balance)], 1, function(x) all(x == first)) == TRUE)))
  
  if (length(duplicated_metrics) == 0) {
    duplicated_metrics <- 0
    
    # the correct balance on real data
    best_balance <- selected_real_balance[1, 2]
    
  } else {
    
    best_real_balance <- selected_real_balance[c(1, duplicated_metrics),2]
    
    best_train_balance <- train_decision_balance[which(train_decision_balance$Balance %in% best_real_balance), ]
    
    best_balance <- best_train_balance[1, 2]
  }

  # load final model and train data
  final_model <- save_models_balance[[best_balance]]
  
  # ============================== #
  # detecting importance variables #
  # ============================== #
  
  # necesary data to calculate importance in rminer
  train_data <- final_model[["trainingData"]]
  names(train_data)[1] <- "ClassEfficiency"
  
  dataset_dummy <- model.matrix(ClassEfficiency~ . - 1, data = train_data)
  train_data <- cbind(train_data[1], dataset_dummy)
  
  train_data <- train_data[,c(2:length(train_data),1)]
  
  # importance with our model of Caret
  mypred <- function(M, data) {
    return (predict(M, data[-length(data)], type = "prob"))
  }
  
  # Define methods and measures
  methods_SA <- c("1D-SA") # c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
  measures_SA <- c("AAD") #  c("AAD", "gradient", "variance", "range")
  
  levels <- 7
  
  if (names(methods)[i] == "nnet") {
    # with rminer
    m <- rminer::fit(
      ClassEfficiency ~.,
      data = train_data,
      model = "mlp",
      scale = "none",
      size = final_model$bestTune$size,
      decay = final_model$bestTune$decay
      #entropy = FALSE
      #softmax = TRUE
    )
    
    # Calculate the importance for the current method and measure
    importance <- Importance(
      M = m,
      RealL = levels, # Levels
      data = train_data,
      method = methods_SA,
      measure = measures_SA,
      baseline = "mean", # mean, median, with the baseline example (should have the same attribute names as data).
      responses = TRUE
    )
    
  } else {
    # Calculate the importance for the current method and measure
    importance <- Importance(
      M = final_model$final_model$finalModel,
      RealL = levels, # Levels
      data = train_data, # data
      method = methods_SA,
      measure = measures_SA,
      baseline = "mean", # mean, median, with the baseline example (should have the same attribute names as data).
      responses = TRUE,
      PRED = mypred,
      outindex = length(train_data) # length(train_data)
    )
  }
  
  result_SA <- as.data.frame(t((round(importance$imp, 3))))[, -length(importance$imp)]
  rownames(result_SA) <- NULL
  names(result_SA) <- names(train_data)[-length(train_data)]
  
  if (names(methods) == "nnet") {
    final_model_p <- final_model
  } else {
    final_model_p <- final_model$final_model
  }
  
  print(paste("Inputs importance: ", sum(result_SA[1:length(x)])))
  print(paste("Outputs importance: ", sum(result_SA[(length(x)+1):(length(x)+length(y))])))
  print(paste("Seed: ", seed))
  
  # =========== #
  # get ranking #
  # =========== #
  
  eff_vector <- apply(eval_data[, c(x,y)], 1, function(row) {
    
    row_df <- as.data.frame(t(row))
    colnames(row_df) <- names(data[, c(x,y)])
    
    pred <- unlist(predict(final_model_p, row_df, type = "prob")[1])
    
    return(pred)
  })
  
  eff_vector <- as.data.frame(eff_vector)
  
  id <- as.data.frame(c(1:nrow(eval_data)))
  names(id) <- "id"
  eff_vector <- cbind(id, eff_vector)
  
  ranking_order <- eff_vector[order(eff_vector$eff_vector, decreasing = TRUE), ]
  
  # ============================= #
  # to get probabilities senarios #
  # ============================= #
  
  data_scenario_list <- list()
  metrics_list <- list()
  peer_list <- list()
  na_count_list <- list()
  n_not_prob_list <- list()
  
  for (e in 1:length(scenarios)) {
    print(paste("scenario: ", scenarios[e]))
    print(final_model)
    data_scenario <- compute_target (
      data = eval_data[,c(x,y)],
      x = x,
      y = y,
      #z = z,
      final_model = final_model,
      cut_off = scenarios[e],
      imp_vector = result_SA
    )

    if(all(is.na(data_scenario$data_scenario))) {
      print("all na")
      browser()
      
      # peer
      peer_restult <- NA
      
      # save_peer
      peer_list[[e]] <- peer_restult
      
      # main_metrics
      main_metrics <- NA
      
      # save main_metrics
      metrics_list[[e]] <- main_metrics 
      
      print("pause")
    } else {
      
      if(any(data_scenario$data_scenario[, c(x,y)] < 0)) {
        
        data_scenario$data_scenario[apply(data_scenario$data_scenario, 1, function(row) any(row < 0) || any(is.na(row))), ] <- NA
        
        na_idx <- which(apply(data_scenario$data_scenario, 1, function(row) any(is.na(row))))
        data_scenario$betas[na_idx,] <- NA
      }
      
      data_scenario_list[[e]] <- data_scenario
      
      # ================ #
      # determinate peer #
      # ================ #
      
      # first, determinate efficient units
      idx_eff <- which(eff_vector$eff_vector > scenarios[e])
      
      if (!length(idx_eff) == 0) {
        
        # save distances structure
        save_dist <- matrix(
          data = NA,
          ncol = length(idx_eff),
          nrow = nrow(eval_data)
        )
        
        # result_SA_matrix <- matrix(rep(result_SA, each = nrow(eval_data)), nrow = nrow(eval_data), byrow = FALSE)
        # 
        # w_eval_data <- eval_data[, c(x, y)] * result_SA_matrix
        # sweep(eval_data[, c(x, y)], 2, result_SA_matrix, "*")
        # sweep(eval_data[, c(x, y)], 2, result_SA, "*")
        # 
        
        # calculate distances
        for (unit_eff in idx_eff) {
          # set reference
          reference <- eval_data[unit_eff, c(x,y)]
          
          distance <- unname(apply(eval_data[, c(x,y)], 1, function(x) sqrt(sum((x - reference)^2))))
          
          # get position in save results
          idx_dis <- which(idx_eff == unit_eff)
          save_dist[,idx_dis] <- as.matrix(distance)
        }
        
        near_idx_eff <- apply(save_dist, 1, function(row) {
          
          which.min(abs(row))
          
        })
        
        peer_restult <- matrix(
          data = NA,
          ncol = 1,
          nrow = nrow(eval_data)
        )
        
        peer_restult[, 1] <- idx_eff[near_idx_eff]
        
        # save_peer
        peer_list[[e]] <- peer_restult
  
        # join data plus betas to metrics for scenario
        data_metrics <- cbind(data_scenario$data_scenario, round(data_scenario$betas, 5))
        
        # number not scenario 
        n_not_prob <- which(data_metrics$probability < scenarios[e])
        n_not_prob_list[[e]] <- n_not_prob
        
        # count na
        na_row <- which(apply(data_metrics, 1, function(row) all(is.na(row))))
        count_na <- length(na_row)
        na_count_list[[e]] <- count_na
        
        # metrics: mean, median, sd
        main_metrics <- as.data.frame(matrix(
          data = NA,
          ncol = ncol(data_metrics),
          nrow = 3
        ))

        # metrics
        main_metrics[1,] <- apply(data_metrics, 2, mean, na.rm = TRUE)
        main_metrics[2,] <- apply(data_metrics, 2, median, na.rm = TRUE)
        main_metrics[3,] <- apply(data_metrics, 2, sd, na.rm = TRUE)
        
        names(main_metrics) <- names(data_metrics)
        row.names(main_metrics) <- c("mean", "median", "sd")
        
        metrics_list[[e]] <- main_metrics
        
      } else {
        
        peer_list[[e]] <- NULL
        na_count_list[[e]] <- nrow(eval_data)
        metrics_list[[e]] <- NULL
      }
      
    }
    
  } # end loop scenarios
  
  names(data_scenario_list) <- scenarios
  names(metrics_list) <- scenarios
  names(peer_list) <- scenarios
  names(na_count_list) <- scenarios
  names(n_not_prob_list) <- scenarios
  
  # check real data performance
  y_obs <- eval_data$class_efficiency
  y_hat <- predict(final_model, eval_data)
  
  #create confusion matrix and calculate metrics related to confusion matrix
  performance_real_data <- confusionMatrix (
    data = y_hat,
    reference = y_obs,
    mode = "everything",
    positive = "efficient"
  )[["byClass"]]

  final_model <- list(
    train_decision_balance = train_decision_balance,
    real_decision_balance = selected_real_balance,
    best_balance = best_balance,
    final_model = final_model,
    performance_train_dataset = selected_model,
    performance_real_data = performance_real_data,
    importance = importance,
    result_SA = result_SA,
    eff_vector = eff_vector,
    ranking_order = ranking_order,
    peer_list = peer_list,
    data_scenario_list = data_scenario_list,
    metrics_list = metrics_list,
    count_na = na_count_list,
    n_not_prob_list = n_not_prob_list
  )
  
  return(final_model)
  
}

#' @title Select cut-off point in Classification Algorithm
#'
#' @description This function selects the cut-off point that minimizes the sum of false positives and false negatives.
#' 
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param final_model The best \code{train} object from \code{caret}.
#'
#' @return It returns the best cut-off point.

select_cut_off <- function (
    data, final_model
    ) {
  
  # cut-off points
  df_cp <- data.frame (
    cut_off_points = seq(0, 1, by = 0.01),
    false_pred = NA
  )
  
  # predictions
  y_hat <- predict(final_model, data, type = "prob")
  
  for (i in 1:nrow(df_cp)) {
    
    # cut-off point
    cp_point <- df_cp[i, "cut_off_points"]
    
    # predictions for a given cut-off point
    y_hat_cp <- ifelse(y_hat$efficient >= cp_point, "efficient", "not_efficient")
    y_hat_cp <- factor(y_hat_cp, levels = c("efficient", "not_efficient"))
    
    # confusion matrix
    cm_cp <- confusionMatrix (
      data = y_hat_cp,
      reference = data$class_efficiency,
      mode = "everything",
      positive = "efficient"
    )[["table"]]
    
    # compute false positive and false negative
    df_cp[i, "false_pred"] <- cm_cp[2, 1] + cm_cp[1, 2]
  }

  # minimum cost for the cut-off point
  min_value <- min(df_cp$false_pred)
  min_index <- which(df_cp$false_pred == min_value)
  cut_point <- df_cp[max(min_index), "cut_off_points"]
  
  return(cut_point)
}
