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
#' @importFrom caret trainControl train createDataPartition
#' @importFrom dplyr select_if %>% arrange top_n
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
  scores <- compute_scores_additive (
    data = data, x = x, y = y, nX = nX, nY = nY
    )
  
  # Determine efficient and inefficient DMUs
  class_efficiency <- ifelse(scores[, 1] <= 0.000001, 1, 0)
  
  # Add "efficient" class
  data <- cbind(data, class_efficiency) %>% as.data.frame()
  data$class_efficiency <- as.factor(class_efficiency)
  
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
  best_ml_model <- vector("list", length = length(methods))
  
  # Change names to ROC matrics in train
  levels(data$class_efficiency) <- c("efficient", "not_efficient")
  
  browser()
  
  for (i in 1:length(methods)) {
    
    # parameter position
    parms_posn <- which(names(ml_model$metric_information[[i]]) %in% names(methods[[i]])) 
    
    # parameter values
    parms_vals <- as.data.frame(ml_model$metric_information[[i]])[, parms_posn]
    
    # rename parameters if is null
    if (is.null(names(parms_vals))) {
      parms_vals <- as.data.frame(parms_vals)
      names(parms_vals) <- names(methods[[i]])
    }
    
    # Tune models
    best_ml_model[[i]][[1]] <- train (
      form = class_efficiency ~.,
      data = train_data,
      method = names(methods[i]),
      tuneGrid = parms_vals
      )
    
    names(best_ml_model)[i] <- names(methods[i])
    
    y_obs <- valid_data$class_efficiency
    y_hat <- predict(best_ml_model[[i]], valid_data)
    
    #create confusion matrix and calculate metrics related to confusion matrix
    best_ml_model[[i]][[2]] <- confusionMatrix (
      data = y_hat,
      reference = y_obs,
      mode = "everything"
      )
    
  }
  
  browser()
  
  
  
  
  
  
  
  # best configuration for each model
  precision_models <- data.frame (
    "model_name" = names(ml_model$metric_information),
    "model_balanced_accuracy" = unname(sapply(ml_model$metric_information, "[[", "Balanced_accuracy")),
    "model_roc" = unname(sapply(ml_model$metric_information, "[[", "ROC"))
  )
  
  
  
  
  # select the best model
  #ERROR
  selected_model <- best_ml_model %>%
    arrange(model_balanced_accuracy, model_roc) %>%
    top_n(1)
  
  # index of the best model in ml_model
  best_model_index <- which(selected_model[1, 1] == names(ml_model$metric_information))
  
  # name of the parameters of the best model
  parms <- names(methods[[best_model_index]])
  
  # values of the parameters of the best model
  parms_cols <- names(ml_model$metric_information[[best_model_index]]) %in% parms
  parms_vals <- ml_model$metric_information[[best_model_index]][, parms_cols]
  
  # Optimization problem
  solution <- optimization(data = data, x = x, y = y, final_model = final_model, orientation = orientation)

  resume <- data.frame()

  if (orientation == "output") {

    resume <- cbind(data[, c(min(x):max(x), min(y):max(y))], DEA.score = DEA$scores, SVM.Classifier.score = solution$solution_score)

    resume <- as.data.frame(resume)

    names <- colnames(data)[c(x, y)]

    colnames(resume)[c(x, y)] <- names

    rownames(resume) <- nameDMUs

    long <- max(c(max(resume$DEA.score), max(resume$SVM.Classifier.score)))

    graph <- ggplot(resume) +
      geom_point(aes(x = SVM.Classifier.score, y = DEA.score)) +
      scale_y_continuous(limits = c(1, long),
                         breaks = seq(1, long, by = 0.1)) +
      scale_x_continuous(limits = c(1, long),
                         breaks = seq(1, long, by = 0.1)) +
      geom_abline(intercept = 0, slope = 1)

  } else { # orientation == "input"

    resume <- cbind(data[, c(min(x):max(x), min(y):max(y))], DEA.score = DEA$scores, SVM.Classifier.score = solution$solution_score)

    name <- colnames(data)[x]

    colnames(resume)[x] <- name

    rownames(resume) <- nameDMUs

    long <- min(c(min(resume$SVM.Classifier.score), min(resume$DEA.score)))

    graph <- ggplot(resume) +
      geom_point(aes(x = SVM.Classifier.score, y = DEA.score)) +
      scale_y_continuous(limits = c(long, 1),
                         breaks = seq(0, 1, by = 0.1)) +
      scale_x_continuous(limits = c(long, 1),
                         breaks = seq(0, 1, by = 0.1)) +
      geom_abline(intercept = 0, slope = 1)

  }

  correlation <- cor(resume$SVM.Classifier.score, resume$DEA.score , use = "everything",
                     method = "pearson")

  return(list(data = data, train_models = train_svm, best_model = as.character(selected_SVM_model[1]), best_model_fit = final_model, solution_point = solution[["solution_point"]], score = solution[["solution_score"]], resume = resume, plot = graph, correlation_pearson = correlation))

}
