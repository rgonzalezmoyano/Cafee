#' @title Create New DMUs to Balance Data
#'
#' @description This function creates new DMUs to address data imbalances. It can generate both efficient and inefficient DMUs.
#'
#' @param data A data.frame or matrix containing the variables used in the model.
#' @param x Column indexes of the input variables in the \code{data}
#' @param y Column indexes of the output variables in the \code{data}
#' @param nX Number of inputs \code{data}.
#' @param nY Number of outputs \code{data}.
#' @param number_eff_dmus Number of efficient DMUs to be created in the \code{data}
#' @param number_ineff_dmus Number of inefficient DMUs to be generated in the \code{data}
#' @param prop_eff Proportion of efficient DMUs to be introduced in the \code{data}
#' @param prop_ineff Proportion of inefficient DMUs to be added to the \code{data}
#' @param eff_dmus_idx Index of the efficient dmus observed in \code{data}
#'
#' @return Fill

balanced_data <- function (
      data, x, y, nX, nY,
      number_eff_dmus, number_ineff_dmus, 
      prop_eff, prop_ineff,
      eff_dmus_idx
) {
  
    if (prop_eff > prop_ineff) {
    
    print("efficient imbalanced")
    
    new_dmus <- ceiling(((-0.65 * number_ineff_dmus) + (0.35 * number_eff_dmus)) / 0.65)
    print(paste("Se crean ", new_dmus, " dmus ineficientes"))
    
    # Create inefficients dmus
    index_dmu_change <- sample(1:nrow(data), size = new_dmus)
    
    # create a new matrix data
    new_dmus_value <- matrix(data = NA, nrow = new_dmus, ncol = nX + nY)  
    colnames(new_dmus_value) <- names(data)[c(x, y)]
    
    # maximum values to worsen
    max_value_x <- apply(X = data[, x], MARGIN = 2, FUN = max)
    
    for (i in 1:new_dmus) {
      min_unif <- 0
      dmu <- index_dmu_change[i]
      max_unif <- max_value_x - data[dmu, x]
      
      for (j in 1:nX) {
        make_inefficient <- runif(n = 1, min = min_unif, max = as.numeric(max_unif[j]))
        new_dmus_value[i, j] <- data[dmu, j] + make_inefficient
      }
    }
    
    # minimum values to worsen
    min_value_y <- apply(X = as.matrix(data[, y]), MARGIN = 2, FUN = min)
    
    min_value_y <- matrix(data = NA, ncol = length(y), nrow = 1)
    colnames(min_value_y) <- names(data[y])
    
    for (i in (nX + 1):max(y)) {
      min_value_y[i] <- min(data[, i])
    }
    
    for (i in 1:new_dmus) {
      min_unif <- 0
      dmu <- index_dmu_change[i]
      max_unif <- data[dmu, y] - min_value_y
      
      for (j in (max(x) + 1):max(y)) {
        make_inefficient <- runif(n = 1, min = as.numeric(min_unif), max = as.numeric(max_unif[j]))
        new_dmus_value[i, j] <- data[dmu, j] - make_inefficient
      }
    }
    
    # new dmus to data.frame
    new_dmus_value <- as.data.frame(new_dmus_value)
    
    # clasification "ineffcient"
    new_dmus_value$class_efficiency <- "not_efficient"
    
    # combinate new dmus to dataset
    data <- rbind(data, new_dmus_value)
    
  } else {
    
    # compute bcc_scores
    bcc_scores <- rad_out (
      tech_xmat = as.matrix(data[, x]),
      tech_ymat = as.matrix(data[, y]),
      eval_xmat = as.matrix(data[, x]),
      eval_ymat = as.matrix(data[, y]),
      convexity = TRUE,
      returns = "variable"
    ) 
    
    proj_data <- as.data.frame(cbind(data[, x], data[, y] * bcc_scores[, 1]))
    
    print("efficient imbalanced")
    
    names(proj_data) <- names(data[, c(x, y)])
    proj_data$class_efficiency <- "efficient"
    
    # drop duplicated indexes
    proj_data <- proj_data[- eff_dmus_idx, ]
    
    # Select the minimum number of additions required to balance the data
    new_dmus <- ceiling(((-0.65 * number_eff_dmus) + (0.35 * number_ineff_dmus)) / 0.65)
    print(paste("Se crean ", new_dmus, " dmus eficientes"))
    
    # Select the efficient dmus
    index_dmu_effi <- sample(1:nrow(proj_data), size = new_dmus)
    
    new_dmus_value <- as.data.frame(matrix(data = NA, nrow = new_dmus, ncol = nX + nY + 1))  
    names(new_dmus_value) <- names(data)
    
    for (i in 1:new_dmus) {
      dmu <- index_dmu_effi[i]
      new_dmus_value[i, ] <- proj_data[dmu, ]
      
    }
    
    # add proj_data to original data
    data <- as.data.frame(rbind(data, new_dmus_value))
    
  }

  return(data)

}