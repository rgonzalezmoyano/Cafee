#' @title Create New DMUs to Balance Data
#'
#' @description This function creates new DMUs to address data imbalances. If the majority class is efficient, it generates new inefficient DMUs by worsering the observed units. Conversely, if the majority class is inefficient, it projects inefficient DMUs to the frontier. Finally, a random selection if performed to keep a proportion of 0.65 for the majority class and 0.35 for the minority class.
#' 
#' @param data A \code{data.frame} containing the variables used in the model.
#' @param x Column indexes of the input variables in the \code{data}.
#' @param y Column indexes of the output variables in the \code{data}.
#' @param obs_prop A \code{table} with the observed proportions of efficient and inefficient DMUs.
#'
#' @return It returns a \code{data.frame} with the newly created set of DMUs incorporated.

balance_data <- function (
      data, x, y, obs_prop
    ) {
  
  # proportion of dmus efficient
  prop_eff <- obs_prop["efficient"]
  
  # proportion of dmus not efficient
  prop_ineff <- obs_prop["not_efficient"]
  
  # number of dmus efficient
  n_eff <- prop_eff * nrow(data)
  
  # number of dmus not efficient
  n_ineff <- prop_ineff * nrow(data)
  
  # index of dmus efficient
  idx_eff <- which(data$class_efficiency == "efficient")
  
  # index of dmus not efficient
  idx_ineff <- which(data$class_efficiency == "not_efficient")
  
  if (prop_eff > prop_ineff) {
    
    print("efficient imbalanced")
    
    new_dmus <- ceiling(((-0.65 * n_ineff) + (0.35 * n_eff)) / 0.65)
    print(paste("Se crean ", new_dmus, " dmus ineficientes"))
    
    # Create inefficients dmus
    index_dmu_change <- sample(1:nrow(data), size = new_dmus)
    
    # create a new matrix data
    new_dmus_value <- matrix(data = NA, nrow = new_dmus, ncol = length(x) + length(y))  
    colnames(new_dmus_value) <- names(data)[c(x, y)]
    
    # maximum values to worsen
    max_value_x <- apply(X = data[, x], MARGIN = 2, FUN = max)
    
    for (i in 1:new_dmus) {
      min_unif <- 0
      dmu <- index_dmu_change[i]
      max_unif <- max_value_x - data[dmu, x]
      
      for (j in 1:length(x)) {
        make_inefficient <- runif(n = 1, min = min_unif, max = as.numeric(max_unif[j]))
        new_dmus_value[i, j] <- data[dmu, j] + make_inefficient
      }
    }
    
    # minimum values to worsen
    min_value_y <- apply(X = as.matrix(data[, y]), MARGIN = 2, FUN = min)
    
    min_value_y <- matrix(data = NA, ncol = length(y), nrow = 1)
    colnames(min_value_y) <- names(data[y])
    
    for (i in (length(x) + 1):max(y)) {
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
    proj_data <- proj_data[- idx_eff, ]
    
    # Select the minimum number of additions required to balance the data
    new_dmus <- ceiling(((-0.65 * n_eff) + (0.35 * n_ineff)) / 0.65)
    print(paste("Se crean ", new_dmus, " dmus eficientes"))
    
    # Select the efficient dmus
    index_dmu_effi <- sample(1:nrow(proj_data), size = new_dmus)
    
    new_dmus_value <- as.data.frame(matrix(data = NA, nrow = new_dmus, ncol = length(x) + length(y) + 1))  
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