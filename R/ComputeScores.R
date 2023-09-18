#' @title Data envelopment analysis' clasification.
#'
#' @description This function trains for each model, the different hyperparameters and returns the best model with its best hyperparameters.
#'
#' @param data Dataset \code{data}.
#' @param nX Number of inputs \code{data}.
#'
#' @importFrom deaR make_deadata model_basic efficiencies
#' @importFrom dplyr %>% mutate
#'
#' @return It returns a \code{data.frame} with the final data.
ComputeScores <- function(data, nX, nY, orientation) {

  # DeaR PreProcess
  dataDEA <- make_deadata(datadea = data, dmus = NULL, inputs = x, outputs = y)

  if (orientation == "input") {

    orientation.DeaR <- "io"

  } else {

    orientation.DeaR <- "oo"

  }

  # Model
  model <- model_basic(datadea = dataDEA,
                       orientation = orientation.DeaR,
                       rts = "vrs")

  scores <- efficiencies(model)

  # Add efficient class
  data <- data %>%
    mutate(ClassEfficiency = as.factor(ifelse(scores == 1, 1, -1)))

  return(list(data = data, scores = scores))
}
