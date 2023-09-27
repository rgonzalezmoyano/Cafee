#' @title Training a Classification Machine Learning Model
#'
#' @description This function trains a set of models and selects best hyperparameters for each of them. 
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param trControl Parameters for controlling the training process (from the \code{'caret'} package).
#' @param methods A \code{list} of selected machine learning models and their hyperparameters.
#'
#' @importFrom caret train twoClassSummary
#'
#' @return It returns a \code{list} with the chosen model.

train_ml <- function (
    data, trControl, methods
    ) {
  
  levels(data$class_efficiency) <- c("not_efficient", "efficient")

  model_best <- vector("list", length = 2)
  names(model_best) <- c("metric_information", "best_model_fit")
  
  model_best$metric_information <- vector("list", length = length(methods))
  model_best$best_model_fit <- vector("list", length = length(methods))
  
  for (a in 1:length(methods)) {

      # Params grid
      tune_grid <- unique(expand.grid(methods[[a]]))
      
      sink("nul")
      
      # Tune models
      model <- train (
        form = class_efficiency ~ .,
        data = data,
        method = names(methods[a]),
        trControl = trControl,
        tuneGrid = tune_grid,
        metric = "ROC"
        )
      
      sink()
      
      #model[["results"]][["Balanced_accuracy"]] <- (model[["results"]][["Sens"]] + model[["results"]][["Spec"]]) / 2

      model_best$metric_information[[a]] <- model$results[which.max(model$results[, "ROC"]),]
      names(model_best$metric_information)[a] <- names(methods[a])
      
      model_best$best_model_fit[[a]] <- model[["finalModel"]]
      names(model_best$best_model_fit)[a] <- names(methods[a])
    
  }

  return(model_best)
  
}