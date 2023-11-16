#' @title Create New DMUs to reach 150 dmus
#'
#' @description This function creates new DMUs to address data imbalances. If the majority class is efficient, it generates new inefficient DMUs by worsering the observed units. Conversely, if the majority class is inefficient, it projects inefficient DMUs to the frontier. Finally, a random selection if performed to keep a proportion of 0.65 for the majority class and 0.35 for the minority class.
#' 
#' @param data A \code{data.frame} containing the variables used in the model.
#' @param x Column indexes of the input variables in the \code{data}.
#' @param y Column indexes of the output variables in the \code{data}.
#' @param obs_prop A \code{table} with the observed proportions of efficient and inefficient DMUs.
#'
#' @return It returns a \code{data.frame} with the newly created set of DMUs incorporated.

create_dmu <- function (
  data, nX, nY, N, type
) {
  
  if (type == "inefficient") {
    
    # ======================= #
    # create inefficient DMUs #
    # ======================= #
    
    # indexes of DMUs for worsening
    idx_dmu_change <- sample(1:nrow(data), size = N)
    
    # create a new matrix of data
    new_dmu_values <- matrix(data = NA, nrow = new_dmus, ncol = nX + nY)  
    colnames(new_dmu_values) <- names(data)[c(x, y)]
    
    # data is moved by a uniform distribution.
    # minimum parameter for the uniform distribution.
    min_unif <- 0
    
    # alteration of inputs
    # maximum values of inputs
    max_value_x <- apply(X = data[, x], MARGIN = 2, FUN = max)
    
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
    
    # add the new set of DMUs to the original data
    data <- rbind(data, new_dmu_values)
    
  } else {
    
    # ===================== #
    # create efficient DMUs #
    # ===================== #
    
    # select the minimum number of additions required to balance the data
    new_dmus <- ceiling(((- 0.50 * n_eff) + (0.50 * n_ineff)) / 0.50)
    
    # create efficiency observations
    eff_dmu <- create_dmu (
      data = data,
      nX = nX,
      nY = nY,
      N = new_dmus,
      type = "efficient"
    )
    
    # # create grid of data
    # grid <- matrix(ncol = nX + nY)
    # 
    # for (v in 1:nX) {
    #   dmu_v <- ceiling(new_dmus / nX)
    #   mat_v <- matrix(rep(0, dmu_v), nrow = dmu_v, ncol = nX + nY)
    #   
    #   # fill v-th column
    #   mat_v[, v] <- seq(min(data[, v]), max(data[, v]), length.out = dmu_v)
    #   
    #   # fill the other columns
    #   min_v <- apply(data[, c(1:(nX + nY))[- v], drop = FALSE], 2, min)
    #   mat_v[, c(1:(nX + nY))[- v]] <- matrix(rep(min_v, dmu_v), ncol = nX + nY - 1, byrow = TRUE)
    #   
    #   # Fill the grid
    #   grid <- rbind(grid, mat_v)
    # }
    # 
    # colnames(grid) <- names(data)[1:(ncol(data) - 1)]
    # grid <- rbind(grid[- 1, ], data[, - ncol(data)])
    
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
      tech_xmat = as.matrix(grid[, x]),
      tech_ymat = as.matrix(grid[, y]),
      eval_xmat = as.matrix(grid[, x]),
      eval_ymat = as.matrix(grid[, y]),
      convexity = TRUE,
      returns = "variable"
    )
    
    # project inefficient data 
    proj_data_out <- as.data.frame(cbind(data[, x], data[, y] * bcc_scores_out[, 1]))
    proj_data_inp <- as.data.frame(cbind(data[, x] * bcc_scores_inp[, 1], data[, y]))
    
    names(proj_data_out) <- names(data[, c(x, y)])
    names(proj_data_inp) <- names(data[, c(x, y)])
    
    proj_data_out$class_efficiency <- "efficient"
    proj_data_inp$class_efficiency <- "efficient"
    
    # drop duplicated indexes
    # proj_data_out <- proj_data_out[- (idx_eff + (nrow(grid) - nrow(data))), ]
    # proj_data_inp <- proj_data_inp[- (idx_eff + (nrow(grid) - nrow(data))), ]
    proj_data_out <- proj_data_out[- idx_eff, ]
    proj_data_inp <- proj_data_inp[- idx_eff, ]
    
    # select the index to improve dmus
    idx_eff <- sample(1:nrow(proj_data_out), size = new_dmus)
    
    # number of DMUs for output projection
    n_division <- ceiling(new_dmus / 2)
    
    # index to improve by orientation
    # output projection
    idx_eff_inp <- sample(idx_eff, size = n_division)
    
    # output projection
    idx_eff_out <- setdiff(idx_eff, idx_eff_inp)
    
    # create DMUs by orientation
    new_dmu_values <- as.data.frame(matrix(data = NA, nrow = new_dmus, ncol = nX + nY + 1)) 
    names(new_dmu_values) <- names(data)
    
    for (i in 1:new_dmus) {
      dmu <- idx_eff[i]
      
      if (dmu %in% idx_eff_out) {
        new_dmu_values[i, ] <- proj_data_out[dmu, ]
      } else {
        new_dmu_values[i, ] <- proj_data_inp[dmu, ]
      }
    }
    
    # add proj_data to original data
    data <- as.data.frame(rbind(data, new_dmu_values))
  }
}