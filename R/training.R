#' @title Training a Classification Machine Learning Model
#'
#' @description This function trains a set of models and selects best hyperparameters for each of them. 
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param trControl Parameters for controlling the training process (from the \code{'caret'} package).
#' @param methods A \code{list} of selected machine learning models and their hyperparameters.
#' @param metric A \code{string} specifying the summary metric for classification to select the optimal model. Default includes "Kappa" due to (normally) unbalanced data.
#'
#' @importFrom caret train twoClassSummary
#'
#' @return It returns a \code{list} with the chosen model.

train_ml <- function (
    data, trControl, methods, metric
    ) {

  model_eval <- vector("list", length = length(methods))
  
  for (a in 1:length(methods)) {

      # Params grid
      tune_grid <- unique(expand.grid(methods[[a]]))
      
      # Tune models
      model <- train (
        form = class_efficiency ~ .,
        data = data,
        method = names(methods[a]),
        trControl = trControl,
        tuneGrid = tune_grid
        )
      

      browser()

      model_eval[[a]] <- model$results[which.max(model$results[, metric]),]
      names(model_eval)[a] <- names(methods[a])
  }

  return(model_eval)
  
}