#' @title Efficiency Estimation Using Classification Algorithms
#'
#' @description This function uses classification algorithms to estimate the efficiency of a set of DMUs (Decision Making Units).
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column indexes of input variables in \code{data}.
#' @param y Column indexes of output variables in \code{data}.
#' @param orientation A \code{string}, equal to \code{"input"} (input-oriented) or \code{"output} (output-oriented)
#' @param trControl Parameters for controlling the training process (from the \code{'caret'} package).
#' @param methods A \code{list} of selected machine learning models and their hyperparameters.
#' @param metric A \code{string} specifying the summary metric for classification to select the optimal model. Default includes \code{"Balanced_accuracy"} due to (normally) unbalanced data.
#' @param hold_out A \code{number} value (5-20) for validation data percentage during training (default: 0.2).
#'
#' @importFrom caret trainControl train createDataPartition defaultSummary prSummary
#' @importFrom dplyr select_if %>% arrange top_n sample_n
#'
#' @return A \code{"cafee"} object.
#'
#' @export

efficiency_estimation <- function (
    data, x, y, orientation,
    trControl, methods, metric, hold_out
    ) {

  # pre-processing
  data <- preprocessing (
    data = data, 
    x = x, 
    y = y
    )

  # Reorder index 'x' and 'y' in data
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)
  
  # Number of inputs / outputs as inputs and number of outputs
  nX <- length(x)
  nY <- length(y)
  
  # compute DEA scores through an additive model
  add_scores <- compute_scores_additive (
    data = data, x = x, y = y, nX = nX, nY = nY
    )
  
  # determine efficient and inefficient DMUs
  class_efficiency <- ifelse(add_scores[, 1] <= 0.0001, 1, 0)
  
  # efficient dmus indexes
  eff_dmus_idx <- which(class_efficiency == 1)
  ineff_dmus_idx <- which(class_efficiency == 0)
  
  data <- cbind(data, class_efficiency) %>% as.data.frame()
  data$class_efficiency <- factor(data$class_efficiency)
  data$class_efficiency <- factor (
    data$class_efficiency, 
    levels = rev(levels(data$class_efficiency))
  )
  
  levels(data$class_efficiency) <- c("efficient", "not_efficient")
  
  # number of efficient and inefficient DMUs
  number_eff_dmus <- length(eff_dmus_idx)
  number_ineff_dmus <- length(ineff_dmus_idx)
  
  prop_eff <- number_eff_dmus / nrow(data)
  prop_ineff <- number_ineff_dmus / nrow(data)
  
  # check for imbalanced data
  if (max(prop_eff, prop_ineff) > 0.65) {
    
    print("Existen datos desbalanceados")
    
    data <- balanced_data (
      data = data, x = x, y = y, nX = nX, nY = nY,
      number_eff_dmus = number_eff_dmus, number_ineff_dmus = number_ineff_dmus,
      prop_eff = prop_eff, prop_ineff = prop_ineff,
      eff_dmus_idx = eff_dmus_idx
    )
      
  }
  
  browser()

  # Create train and validation data
  valid_index <- createDataPartition (
    data$class_efficiency,
    p = hold_out,
    list = FALSE)

  valid_data <- data[valid_index, ]
  train_data <- data[- valid_index, ]
  
  # Function train model
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
    
    # Tune models
    best_ml_model <- train (
      form = class_efficiency ~.,
      data = train_data,
      method = names(methods[i]),
      tuneGrid = parms_vals[[i]],
      verbose = FALSE
      )
    
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
  
  # Final best model
  final_model <- train (
    form = class_efficiency ~.,
    data = data,
    method = row.names(selected_model),
    tuneGrid = parms_vals[[best_model_index]],
    verbose = FALSE
  )
  
  # # Optimization problem
  # solution <- optimization(data = data, x = x, y = y, final_model = final_model, orientation = orientation)
  # 
  # resume <- data.frame()
  # 
  # if (orientation == "output") {
  # 
  #   resume <- cbind(data[, c(min(x):max(x), min(y):max(y))], DEA.score = DEA$scores, SVM.Classifier.score = solution$solution_score)
  # 
  #   resume <- as.data.frame(resume)
  # 
  #   names <- colnames(data)[c(x, y)]
  # 
  #   colnames(resume)[c(x, y)] <- names
  # 
  #   rownames(resume) <- nameDMUs
  # 
  #   long <- max(c(max(resume$DEA.score), max(resume$SVM.Classifier.score)))
  # 
  #   graph <- ggplot(resume) +
  #     geom_point(aes(x = SVM.Classifier.score, y = DEA.score)) +
  #     scale_y_continuous(limits = c(1, long),
  #                        breaks = seq(1, long, by = 0.1)) +
  #     scale_x_continuous(limits = c(1, long),
  #                        breaks = seq(1, long, by = 0.1)) +
  #     geom_abline(intercept = 0, slope = 1)
  # 
  # } else { # orientation == "input"
  # 
  #   resume <- cbind(data[, c(min(x):max(x), min(y):max(y))], DEA.score = DEA$scores, SVM.Classifier.score = solution$solution_score)
  # 
  #   name <- colnames(data)[x]
  # 
  #   colnames(resume)[x] <- name
  # 
  #   rownames(resume) <- nameDMUs
  # 
  #   long <- min(c(min(resume$SVM.Classifier.score), min(resume$DEA.score)))
  # 
  #   graph <- ggplot(resume) +
  #     geom_point(aes(x = SVM.Classifier.score, y = DEA.score)) +
  #     scale_y_continuous(limits = c(long, 1),
  #                        breaks = seq(0, 1, by = 0.1)) +
  #     scale_x_continuous(limits = c(long, 1),
  #                        breaks = seq(0, 1, by = 0.1)) +
  #     geom_abline(intercept = 0, slope = 1)
  # 
  # }
  # 
  # correlation <- cor(resume$SVM.Classifier.score, resume$DEA.score , use = "everything",
  #                    method = "pearson")
  # 
  # return(list(data = data, train_models = train_svm, best_model = as.character(selected_SVM_model[1]), best_model_fit = final_model, solution_point = solution[["solution_point"]], score = solution[["solution_score"]], resume = resume, plot = graph, correlation_pearson = correlation))
  
  return(final_model)

}
