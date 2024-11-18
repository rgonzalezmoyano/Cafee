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
    trControl, methods, metric, hold_out, eff_level = 0.5, balance_data = c(3, 1),  convexity = TRUE, returns = "variable"
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

  # check presence of imbalanced data
  # if (max(obs_prop[1], obs_prop[2]) > 0.50) {
  #   data <- balance_data (
  #     data = data,
  #     data_factor = data_factor,
  #     x = x,
  #     y = y,
  #     z = z,
  #     convexity = convexity,
  #     returns = returns
  #   )
  #   
  #   data <- na.omit(data)
  # }
  
  if (max(obs_prop[1], obs_prop[2]) > 0.50) {
    
    if (balance_data[1] != 0) {
    data <- SMOTE_balance_data(
      data = data,
      data_factor = data_factor,
      x = x,
      y = y,
      z = z,
      balance_data = balance_data
    )
   }
  }
  
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
    y_hat <- predict(best_ml_model, valid_data)

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
  
  for (i in 1:length(methods)) {
    precision_models[i, ] <- confusion_matrix[[i]]
  }

  # select the best model by metric
  selected_model <- precision_models %>%
    arrange(desc("Balanced Accuracy"), desc(F1), desc(Sensitivity))
  selected_model <- selected_model[1, ]
  
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
  print(x)
  print(y)
  data <- data_after_balance
  
  data_list <- list() # all results have scenarios[e] probability
  data_real_list <- list()
  data_beta <- list()
  metrics_list <- list()
  peer_list <- list()
  
  for (e in 1:length(scenarios)) {
    print(paste("scenario: ", e))
    # new x and y in data_scenario
    # x_target <- 1:length(x)
    # y_target <- (length(x)+1):(length(x)+length(y))
    browser()
    data_scenario <- compute_target (
      data = data,
      x = x,
      y = y,
      #z = z,
      final_model = final_model,
      cut_off = scenarios[e],
      imp_vector = result_SA
    )
browser()
    if(any(data_scenario$data_scenario[, c(x,y)] < 0)) {
      seed <- seed + 1
      need_rep <- TRUE
      break
    }
    
    # ================ #
    # determinate peer #
    # ================ #
    
    eff_vector <- apply(data[, c(x,y)], 1, function(row) {
      
      row_df <- as.data.frame(t(row))
      colnames(row_df) <- names(data[, c(x,y)])
      
      pred <- unlist(predict(final_model, row_df, type = "prob")[1])
      
      return(pred)
    })
    
    # first, determinate efficient units
    idx_eff <- which(eff_vector > scenarios[e])
    
    # save distances structure
    save_dist <- matrix(
      data = NA,
      ncol = length(idx_eff),
      nrow = nrow(data)
    )
    
    # calculate distances
    for (unit_eff in idx_eff) {
      # set reference
      reference <- data[unit_eff, c(x,y)]
      
      distance <- unname(apply(data[, c(x,y)], 1, function(x) sqrt(sum((x - reference)^2))))
      
      # get position in save results
      idx_dis <- which(idx_eff == unit_eff)
      save_dist[,idx_dis] <- as.matrix(distance)
    }
    
    # # # change to dataframe
    # save_dist <- as.data.frame(save_dist)
    # names(save_dist) <- idx_eff
    
    near_idx_eff <- apply(save_dist, 1, function(row) {
      
      which.min(abs(row))
      
    })
    
    peer_restult <- matrix(
      data = NA,
      ncol = 1,
      nrow = nrow(data)
    )
    
    peer_restult[, 1] <- idx_eff[near_idx_eff]
    
    # # change data to not worst the efficeint units
    # data_real <- data_scenario$data_scenario
    # 
    # data_real[idx_eff,] <- data[idx_eff, c(x,y)]
    
    # save data_scenario
    #data_list[[e]] <- data_scenario$data_scenario
    
    # save data real
    #data_real_list[[e]] <- data_real
    
    # save beta
    #data_beta[[e]] <- data_scenario$betas
    
    #save_peer
    peer_list[[e]] <- peer_restult
    
    # metrics: mean, median, sd
    main_metrics <- as.data.frame(matrix(
      data = NA,
      ncol = ncol(data[,c(x,y)]),
      nrow = 3
    ))
    
    # metrics
    main_metrics[1,] <- apply(data_real, 2, mean, na.rm = TRUE)
    main_metrics[2,] <- apply(data_real, 2, median, na.rm = TRUE)
    main_metrics[3,] <- apply(data_real, 2, sd, na.rm = TRUE)

    names(main_metrics) <- names(data_scenario$data_scenario)
    row.names(main_metrics) <- c("mean", "median", "sd")

    metrics_list[[e]] <- main_metrics
    
  }
  
  
  return(list(
    final_model = final_model,
    selected_model_metrics = selected_model,
    importance = importance,
    result_SA = result_SA,
    eff_vector = eff_vector,
    ranking_order = ranking_order
    )
  )
  
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
