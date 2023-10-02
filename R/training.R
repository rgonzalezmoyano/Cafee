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
  
  # list with best configuration
  best_model_fit <- vector("list", length = length(methods))
  
  for (a in 1:length(methods)) {

      # parameters grid
      tune_grid <- unique(expand.grid(methods[[a]]))
      
      # avoid messages for some methods
      verb_methods <- c("gbm", "svmPoly")
      
      if (names(methods[a]) %in% verb_methods) {
        # Tune models
        model <- train (
          form = class_efficiency ~ .,
          data = data,
          method = names(methods[a]),
          trControl = trControl,
          tuneGrid = tune_grid,
          verbose = FALSE
        )
      } else {
        # Tune models
        model <- train (
          form = class_efficiency ~ .,
          data = data,
          method = names(methods[a]),
          trControl = trControl,
          tuneGrid = tune_grid
        )
      }
    

      
      # compute F1 score
      prec <- model[["results"]]["Precision"]
      sens <- model[["results"]]["Sens"]
      
      model[["results"]]["F1"] <- (2 * prec * sens) / (sens + prec)
      
      # select best configuration
      best_config <- model[["results"]] %>%
        arrange(desc(F1), desc(Spec), desc(AUC), desc(Kappa), desc(Accuracy))
      
      best_model_fit[[a]] <- best_config[1, ]
      names(best_model_fit)[a] <- names(methods[a])
      
  }

  return(best_model_fit)
}