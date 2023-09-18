#' @title Training the different Support Vector Machines.
#'
#' @description This function trains for each model, the different hyperparameters and returns the best model with its best hyperparameters.
#'
#' @param data Dataset \code{data}.
#' @param method_svm Different SVM algorithms to train \code{data}.
#' @param trControl Training characteristics \code{data}.
#'
#' @importFrom caret train
#' @importFrom dplyr rename
#'
#' @return It returns a \code{list} with the chosen model.
DEA.svm <- function(data, method_svm, trControl) {

  SVM_results <- vector("list", length = length(method_svm))

  for (a in 1:length(method_svm)) {

    # Linear
    if(names(method_svm[a]) == "svmLinear") {

      # Params only for Linear
      tuneGrid_Linear <- unique(expand.grid(method_svm$svmLinear))

      # Tune models
      modelLinear <- train(form = ClassEfficiency ~.,
                           data = data,
                           method = names(method_svm[a]),
                           trControl = trControl,
                           tuneGrid = tuneGrid_Linear,
                           metric = "Kappa")

      SVM_results[[a]] <- modelLinear$results[which.max(modelLinear$results[, 3]),]
      names(SVM_results)[a] <- names(method_svm[a])

    }

    # Radial
    else if(names(method_svm[a]) == "svmRadial") {

      # Params only for Radial
      tuneGrid_Radial <- unique(expand.grid(method_svm$svmRadial)) # todas las combinaciones posibles sin repetirse

      # Tune models
      modelRadial <- train(form = ClassEfficiency ~.,
                         data = data,
                         method = names(method_svm[a]),
                         trControl = trControl,
                         tuneGrid = tuneGrid_Radial,
                         metric = "Kappa")

      SVM_results[[a]] <- modelRadial$results[which.max(modelRadial$results[, 4]),]
      names(SVM_results)[a] <- names(method_svm[a])
    }

    # Polynomial
    else if(names(method_svm[a]) == "svmPoly") {

      # Aux variable Params only for Radial
      tuneGrid_Poly <- unique(expand.grid(method_svm$svmPoly)) # todas las combinaciones posibles sin repetirse

      # Tune models
      modelPoly <- train(form = ClassEfficiency ~.,
                         data = data,
                         method = names(method_svm[a]),
                         trControl = trControl,
                         tuneGrid = tuneGrid_Poly,
                         metric = "Kappa")

      SVM_results[[a]] <- modelPoly$results[which.max(modelPoly$results[, 5]),]
      names(SVM_results)[a] <- names(method_svm[a])
    }

  }

  return(SVM_results)
}
