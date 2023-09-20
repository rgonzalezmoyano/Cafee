#' @title Training the different machine Learning models.
#'
#' @description This function trains for each model, the different hyperparameters and returns the best model with its best hyperparameters.
#'
#' @param data Dataset \code{data}.
#' @param methods Different machine learning algorithms to train \code{data}.
#' @param trControl Training characteristics \code{data}.
#'
#' @importFrom caret train
#' @importFrom dplyr rename
#'
#' @return It returns a \code{list} with the chosen model.
dea_classification <- function(data, methods, trControl) {

  results <- vector("list", length = length(methods))

  for (a in 1:length(methods)) {

      # Params grid
      tune_grid <- unique(expand.grid(methods[[a]]))

      # Tune models
      model <- train(form = as.factor(class_efficiency) ~.,
                     data = data,
                     method = names(methods[a]),
                     trControl = trControl,
                     tuneGrid = tune_grid,
                     metric = "Kappa")

      results[[a]] <- model$results[which.max(model$results[, "Kappa"]),]
      names(results)[a] <- names(methods[a])

  }

  return(results)
  
}