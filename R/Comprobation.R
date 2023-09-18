#' @title Mistakes and warnings introduced by user.
#'
#' @description If user introduced a model not available or not insert the correct hyper-parameters, it will warn to user and it will recommend a solution.
#'
#' @param method models to train defined by user. \code{data}
#' @param params params to tune the model defined by user. \code{data}
#' @param AllMethods Available models. \code{data}
#' @param Allparams Available hyper-parameters. \code{data}
#'
#' @importFrom deaR read_data model_additive efficiencies
#' @importFrom dplyr %>% mutate
#'
#' @return It returns a \code{data.frame} with the final data.
Comprobation <- function(method, params, AllMethods, Allparams) {

  # Is corret "models"?
  if(is.null(method)) {
    stop("There isn't a method specificated.\n\n These are the methods availables:\n", list(AllMethods))
  }

  for(a in 1:length(method)) {

    if(!method[a] %in% AllMethods) {
      warning("Method: <", method[a], "> is not available. It won't be used.\n\n These are the methods availables:\n", list(AllMethods), "\n\n")

    }

  }

  for(hyapms in 1:length(params)) {

    if(!names(params)[hyapms] %in% Allparams) {
      warning("\nThe are hyperparameters that won't be used because they don't be needed in the available methods.\n\n --- The available methods are:\n", list(AllMethods), "\n\n", "--- The available hyper-parameters are:\n", list(Allparams),"\n")
    }

  }

}
