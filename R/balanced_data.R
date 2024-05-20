#' @title Create New DMUs to Balance Data
#'
#' @description This function creates new DMUs to address data imbalances. If the majority class is efficient, it generates new inefficient DMUs by worsering the observed units. Conversely, if the majority class is inefficient, it projects inefficient DMUs to the frontier. Finally, a random selection if performed to keep a proportion of 0.65 for the majority class and 0.35 for the minority class.
#' 
#' @param data A \code{data.frame} containing the variables used in the model.
#' @param x Column indexes of the input variables in the \code{data}.
#' @param y Column indexes of the output variables in the \code{data}.
#' @param convexity Assumption of returns to scale in \code{data}.
#'
#' @return It returns a \code{data.frame} with the newly created set of DMUs incorporated.

balance_data <- function (
      data, data_factor, x, y, z, convexity
    ) {
  
  # number of inputs
  nX <- length(x)
  
  # number of outputs
  nY <- length(y)
  
  nZ <- length(z)
  
  # number of samples
  N <- nrow(data)
  
  # returns of scale
  # by default
  rts <- "variable"
  
  if (convexity == FALSE) {
    rts == "constant"
  }
  
  # =================== #
  # balance proportions #
  # =================== #

  # proportions
  props <- prop.table(table(data$class_efficiency))

  # proportion of dmus efficient
  prop_eff <- props["efficient"]

  # proportion of dmus not efficient
  prop_ineff <- props["not_efficient"]

  # number of dmus efficient
  n_eff <- prop_eff * nrow(data)

  # number of dmus not efficient
  n_ineff <- prop_ineff * nrow(data)

  if (prop_eff > prop_ineff) {

    # ======================= #
    # create inefficient DMUs #
    # ======================= #

    # number of dmus to create
    new_dmus <- ceiling(((- 0.50 * n_ineff) + (0.50 * n_eff)) / 0.50)

    # create new inefficient observations
    ineff_dmu <- create_dmu (
      data = data,
      x = x,
      y = y,
      N = new_dmus,
      type = "inefficient"
    )

    data <- rbind(data, ineff_dmu)

  } else {

    # ===================== #
    # create efficient DMUs #
    # ===================== #

    # number of dmus to create
    new_dmus <- ceiling(((- 0.50 * n_eff) + (0.50 * n_ineff)) / 0.50)

    # create new efficient observations
    eff_dmu <- create_dmu (
      data = data,
      data_factor = data_factor,
      x = x,
      y = y,
      z = z,
      N = new_dmus,
      type = "efficient"
    )

    # ===================================================== #
    # make innefficient to determinate innefficient region  #
    # ===================================================== #
    
    # create new inefficient observations
    ineff_dmu <- create_dmu (
      data = data[data$class_efficiency  == "not_efficient",],
      data_factor = data_factor,
      x = x,
      y = y,
      z = z,
      N = n_ineff,
      type = "inefficient"
    )
    
    # rbind data
    data <- rbind(data, eff_dmu, ineff_dmu)
    
  }
  
  
  
  # # ================== #
  # # enough sample size #
  # # ================== #
  # 
  # if (N < 150) {
  #   
  #   # create new "n" observations
  #   grow_n <- 150 - N
  #   
  #   # create new inefficient observations
  #   ineff_dmu <- create_dmu (
  #     data = data,
  #     x = x,
  #     y = y,
  #     N = grow_n / 2,
  #     type = "inefficient"
  #   )
  #   
  #   # create efficient observations
  #   eff_dmu <- create_dmu (
  #     data = data,
  #     x = x,
  #     y = y,
  #     N = grow_n / 2,
  #     type = "efficient"
  #   )
  #   
  #   data <- rbind(data, ineff_dmu, eff_dmu)
  #   data <- data[complete.cases(data), ]
  # }
  # 
  # 
  # 
  # # ============== #
  # # sub - frontier #
  # # ============== #
  # 
  # # frontier
  # eff_data <- data[data$class_efficiency == "efficient", ]
  # 
  # # perturbations
  # perturbations <- data.frame(matrix(runif(nrow(eff_data) * (ncol(eff_data) - 1), 0, 0.1), nrow = nrow(eff_data)))
  # 
  # # sub - frontier data
  # sfd_data <- data.frame (
  #   eff_data[, x] + eff_data[, x] * perturbations[, x],
  #   eff_data[, y] - eff_data[, y] * perturbations[, y],
  #   "class_efficiency" = "not_efficient"
  # )
  # 
  # colnames(sfd_data) <- colnames(data)
  # 
  # # add to data
  # data <- rbind(data, sfd_data)
  
  return(data)
}

#' @title Create New DMUs to reach 150 dmus
#'
#' @description This function adds DMUs to reach a sample of 150.
#' 
#' @param data A \code{data.frame} containing the variables used in the model.
#' @param x number of inputs in the \code{data}.
#' @param y number of outputs in the \code{data}.
#' @param N number of dmus to create \code{data}.
#' @param type Class of dmu to create \code{data}.
#'
#' @return It returns a \code{data.frame} with the newly created set of DMUs incorporated.

create_dmu <- function (
    data, data_factor, x, y, z, N, type
    ) {
  
  # number of inputs
  nX <- length(x)
  
  # number of outputs
  nY <- length(y)
  
  # number of environment variables
  nZ <- length(z)
  
  if (type == "inefficient") {
    
    # ======================= #
    # create inefficient DMUs #
    # ======================= #
    
    new_dmus <- N
  
    # indexes of DMUs for worsening
    if (new_dmus > nrow(data)) {
      replace <- TRUE
    } else {
      replace <- FALSE
    }
    
    idx_dmu_change <- sample(1:nrow(data), size = new_dmus, replace = replace)
    
    # create a new matrix of data
    new_dmu_values <- matrix(data = NA, nrow = new_dmus, ncol = nX + nY)  
    colnames(new_dmu_values) <- names(data)[c(x, y)]
    
    # data is moved by a uniform distribution.
    # minimum parameter for the uniform distribution.
    min_unif <- 0
    
    # alteration of inputs
    # maximum values of inputs
    max_value_x <- apply(X = data[x], MARGIN = 2, FUN = max)
    
    for (i in 1:new_dmus) {
      
      # select a specific DMU
      dmu <- idx_dmu_change[i]
      
      # maximum parameter for the uniform distribution
      max_unif <- max_value_x - data[dmu, x]
      
      # create new DMUs
      for (j in x) {
        make_inefficient <- runif(n = 1, min = min_unif, max = as.numeric(max_unif[j]))
        new_dmu_values[i, j] <- data[dmu, j] + make_inefficient
      }
      
    }
    
    # alteration of outputs
    # minimum values of outputs
    min_value_y <- apply(X = as.matrix(data[, y]), MARGIN = 2, FUN = min)
    
    for (i in 1:new_dmus) {
      
      # select a specific DMU
      dmu <- idx_dmu_change[i]
      
      # maximum parameter for the uniform distribution
      max_unif <- data[dmu, y] - min_value_y
      
      for (j in 1:nY) {
        make_inefficient <- runif(n = 1, min = min_unif, max = as.numeric(max_unif[j]))
        new_dmu_values[i, nX + j] <- data[dmu, nX + j] - make_inefficient
      }
      
    }
    
    # new set of DMUs to data.frame
    new_dmu_values <- as.data.frame(new_dmu_values)
    
    # classification of the new dmus as "inefficient"
    new_dmu_values$class_efficiency <- "not_efficient"
    
    new_dmu_values <- cbind(new_dmu_values, data_factor[idx_dmu_change, ])
    
  } else {
    
    # ===================== #
    # create efficient DMUs #
    # ===================== #
    
    # select the minimum number of additions required to balance the data
    new_dmus <- N
    
    # compute bcc_scores
    bcc_scores_out <- rad_out (
      tech_xmat = as.matrix(data[, x]),
      tech_ymat = as.matrix(data[, y]),
      eval_xmat = as.matrix(data[, x]),
      eval_ymat = as.matrix(data[, y]),
      convexity = TRUE,
      returns = "variable"
    ) 
    
    bcc_scores_inp <- rad_inp (
      tech_xmat = as.matrix(data[, x]),
      tech_ymat = as.matrix(data[, y]),
      eval_xmat = as.matrix(data[, x]),
      eval_ymat = as.matrix(data[, y]),
      convexity = TRUE,
      returns = "variable"
    )
    
    # efficient DMUs
    
    idx_eff <- c(1:nrow(data))[bcc_scores_out < 1.001]
    idx_eff <- c(1:nrow(data))[bcc_scores_inp > 0.999] # 
  
    # compute 1st decile
    rows_fst_dec <- matrix(0, nrow = nrow(data), ncol = ncol(data) - 1 - length(z))
    
    for (i in 1:ncol(rows_fst_dec)) {
      rows_fst_dec[, i] <- data[, i] <= quantile(data[, i], probs = 0.25)
    }
    
    # observations in the 1st decil
    rows_fst_dec <- c(1:nrow(rows_fst_dec))[rowSums(rows_fst_dec) >= (ncol(data) - 1) / 2]
    
    # select the indexes to input projection
    idx_inp <- setdiff(rows_fst_dec, idx_eff)
    
    # select the indexes to output projection
    size <- min(length(setdiff(c(1:nrow(data)), c(idx_inp, idx_eff))), new_dmus - length(idx_inp))

    idx_out <- sample(setdiff(c(1:nrow(data)), c(idx_inp, idx_eff)), size = max(size, 0))
    
    # project inefficient data 
    proj_data_out <- as.data.frame(cbind(data[, x], data[, y] * bcc_scores_out[, 1]))
    proj_data_inp <- as.data.frame(cbind(data[, x] * bcc_scores_inp[, 1], data[, y]))
    
    names(proj_data_out) <- names(data[, c(x, y)])
    names(proj_data_inp) <- names(data[, c(x, y)])
    
    proj_data_out$class_efficiency <- "efficient"
    proj_data_inp$class_efficiency <- "efficient"
    
    # add factor variables
    proj_data_out <- cbind(proj_data_out, data_factor)
    proj_data_inp <- cbind(proj_data_inp, data_factor)
    
    # create DMUs by orientation
    new_dmu_values <- as.data.frame(matrix(data = NA, nrow = new_dmus, ncol = nX + nY + 1 + nZ)) 
    names(new_dmu_values) <- names(data)
    
    # projection
    for (i in 1:new_dmus) {
      dmu <- c(idx_inp, idx_out)[i]
      
      if (dmu %in% idx_out) {
        new_dmu_values[i, ] <- proj_data_out[dmu, ]
      } else {
        new_dmu_values[i, ] <- proj_data_inp[dmu, ]
      }
    }
  }
  
  # # # grafico
  # new_dmu_values <- as.data.frame(matrix(data = NA, nrow = 9, ncol = 3))
  # # projection borrar
  # for (i in 1:9) {
  #   dmu <- idx_inp[i]
  # 
  # 
  #     new_dmu_values[i, ] <- proj_data_inp[dmu, ]
  # 
  # }
  
  return(new_dmu_values)
}