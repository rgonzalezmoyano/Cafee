#' @title Efficiency Estimation Using Classification Algorithms
#'
#' @description This function uses classification algorithms to estimate the efficiency of a set of DMUs (Decision Making Units).
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column indexes of input variables in \code{data}.
#' @param y Column indexes of output variables in \code{data}.
#' @param orientation A \code{string}, equal to \code{"input"} (input-oriented) or \code{"output} (output-oriented).
#' @param target_method Methodology for labeling the data.
#' @param trControl Parameters for controlling the training process (from the \code{'caret'} package).
#' @param methods A \code{list} of selected machine learning models and their hyperparameters.
#' @param metric A \code{string} specifying the summary metric for classification to select the optimal model. Default includes \code{"Balanced_accuracy"} due to (normally) unbalanced data.
#' @param hold_out A \code{number} value (5-20) for validation data percentage during training (default: 0.2).
#'
#' @importFrom caret trainControl train createDataPartition defaultSummary prSummary
#' @importFrom dplyr select_if %>% arrange top_n sample_n
#' @importFrom Benchmarking dea.boot
#'
#' @return A \code{"cafee"} object.
#'
#' @export

efficiency_estimation <- function (
    data, x, y, orientation, target_method,
    trControl, methods, metric, hold_out
    ) {

  # pre-processing
  data <- preprocessing (
    data = data, 
    x = x, 
    y = y
    )

  # reorder index 'x' and 'y' in data
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)
  
  # number of inputs / outputs as inputs and number of outputs
  nX <- length(x)
  nY <- length(y)
  
  if (target_method == "bootstrapping_dea") {
    
    # ========================== #
    # Label by bootstrapping_dea #
    # ========================== #
    
    # 1 compute DEA scores through bootstrapping
    bootstrapping_dea <- dea.boot (
      X = as.matrix(data[, x]),
      Y = as.matrix(data[, y]),
      NREP = 200,
      ORIENTATION = "out",
      alpha = 0.01
    )
    
    # 2 confidence interval bootstrapping_dea
    conf_int <- bootstrapping_dea[["conf.int"]]
    
    data <- as.data.frame(data)
    # 3 labeling as not efficient
    data$class_efficiency <- "not_efficient"
    
    # labeling as efficient
    data_opt <- as.data.frame(cbind(data[, x], bootstrapping_dea[["eff.bc"]] * data[, y]))
    data_opt$class_efficiency <- "efficient"
    names(data_opt) <- names(data)
    
    # join data
    data <- rbind(data, data_opt)
    data$class_efficiency <- as.factor(data$class_efficiency)
    
    # class_efficiency as factor
    levels(data$class_efficiency) <- c("efficient", "not_efficient")
    
  } else if (label_by == "additive") {
    
    # ============================ #
    # Label by additive modelo DEA #
    # ============================ #
    
    # compute DEA scores through an additive model
     add_scores <- compute_scores_additive (
       data = data,
       x = x,
       y = y
     )
     
    # determine efficient and inefficient DMUs
    class_efficiency <- ifelse(add_scores[, 1] <= 0.0001, 1, 0)

    data <- cbind(data, class_efficiency) %>% as.data.frame()
    data$class_efficiency <- factor(data$class_efficiency)
    data$class_efficiency <- factor (
      data$class_efficiency,
      levels = rev(levels(data$class_efficiency))
    )

    levels(data$class_efficiency) <- c("efficient", "not_efficient")
    
  }
  
  # save a copy of the original data
  eval_data <- data
  
  # observed proportion of efficient and inefficient DMUs.
  obs_prop <- prop.table(table(data$class_efficiency))

  ##### Importante descomentar ETIQUETEDO NORMAL
  # # check presence of imbalanced data
  # if (max(obs_prop[1], obs_prop[2]) > 0.50 | nrow(data) < 150) {
  #   data <- balance_data (
  #     data = data, 
  #     x = x, 
  #     y = y
  #   )
  # }

  # Create train and validation data
  valid_index <- createDataPartition (
    data$class_efficiency,
    p = hold_out,
    list = FALSE
  )
  
  valid_data <- data[valid_index, ]
  train_data <- data[- valid_index, ]
  
  # ====================== #
  # SELECT HYPERPARAMETERS #
  # ====================== #
  
  ml_model <- train_ml (
    data = train_data,
    trControl = trControl,
    methods = methods
    )

  # Best training 
  confusion_matrix <- vector("list", length = length(methods))
  names(confusion_matrix) <- names(methods)
  
  parms_vals <- vector("list", length = length(methods))
  names(parms_vals) <- names(methods)
  
  # ================= #
  # SELECT BEST MODEL #
  # ================= #
  
  for (i in 1:length(methods)) {
    
    # parameter position
    parms_posn <- which(names(ml_model[[i]]) %in% names(methods[[i]])) 
    
    # parameter values
    parms_vals[[i]] <- as.data.frame(ml_model[[i]])[, parms_posn]
    
    # rename parameters if is null
    if (is.null(names(parms_vals[[i]]))) {
      parms_vals[[i]] <- as.data.frame(parms_vals[[i]])
      names(parms_vals[[i]]) <- names(methods[[i]])
    }
    
    # avoid messages for some methods
    verb_methods <- c("gbm", "svmPoly")
    
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
    arrange(desc(F1), desc(Sensitivity), desc("Balanced Accuracy"))
  selected_model <- selected_model[1, ]
  
  # index of the best model in ml_model
  best_model_index <- which(row.names(selected_model) == names(ml_model))
  
  # ============== #
  # FIT BEST MODEL #
  # ============== #
  
  # avoid messages for some methods
  verb_methods <- c("gbm", "svmPoly")
  
  if (names(methods[best_model_index]) %in% verb_methods) {
    final_model <- train (
      form = class_efficiency ~.,
      data = data,
      method = row.names(selected_model),
      tuneGrid = parms_vals[[best_model_index]],
      verbose = FALSE,
      trControl = trainControl(method = "none", classProbs = TRUE)
    )
  } else {
    final_model <- train (
      form = class_efficiency ~.,
      data = data,
      method = row.names(selected_model),
      tuneGrid = parms_vals[[best_model_index]],
      trControl = trainControl(method = "none", classProbs = TRUE)
    )
  }

  cut_off <- select_cut_off (
    data = valid_data,
    final_model = final_model
  )
  
  final_model$cut_off <- cut_off
  
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
