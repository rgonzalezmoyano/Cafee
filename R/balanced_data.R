#' @title Create New DMUs to Balance Data
#'
#' @description This function creates new DMUs to address data imbalances. If the majority class is efficient, it generates new inefficient DMUs by worsering the observed units. Conversely, if the majority class is inefficient, it projects inefficient DMUs to the frontier. Finally, a random selection if performed to keep a proportion of 0.65 for the majority class and 0.35 for the minority class.
#' 
#' @param data A \code{data.frame} containing the variables used in the model.
#' @param data_factor A \code{data.frame} containing the factor variables used in the model.
#' @param x Column indexes of the input variables in the \code{data}.
#' @param y Column indexes of the output variables in the \code{data}.
#' @param z Column indexes of environment variables in \code{data}.
#' @param convexity Assumption of returns to scale in \code{data}.
#' @param returns Type of returns to scale.
#'
#' @return It returns a \code{data.frame} with the newly created set of DMUs incorporated.

balance_data <- function (
      data, data_factor, x, y, z = NULL, convexity, returns
    ) {
  
  # number of inputs
  nX <- length(x)
  
  # number of outputs
  nY <- length(y)
  
  nZ <- length(z)
  
  # number of samples
  N <- nrow(data)
  
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
      type = "inefficient",
      convexity = convexity,
      returns = returns
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
      type = "efficient",
      convexity = convexity,
      returns = returns
    )
    #data_gra <- rbind(data, eff_dmu)

    # ===================================================== #
    # make innefficient to determinate innefficient region  #
    # ===================================================== #
    
    # create new inefficient observations
    ineff_dmu <- create_dmu (
      data = data,
      data_factor = data_factor,
      x = x,
      y = y,
      z = z,
      N = N,
      type = "inefficient",
      convexity = convexity,
      returns = returns
    )
    #data_gra <- rbind(data, new_dmu_values)
    
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
#' @param type class of dmu to create \code{data}.
#' @param convexity Assumption of returns to scale in \code{data}.
#' @param returns Type of returns to scale.
#'
#' @return It returns a \code{data.frame} with the newly created set of DMUs incorporated.

create_dmu <- function (
    data, data_factor, x, y, z = NULL, N, type, convexity, returns
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
      convexity = convexity,
      returns = returns
    ) 
    
    bcc_scores_inp <- rad_inp (
      tech_xmat = as.matrix(data[, x]),
      tech_ymat = as.matrix(data[, y]),
      eval_xmat = as.matrix(data[, x]),
      eval_ymat = as.matrix(data[, y]),
      convexity = convexity,
      returns = returns
    )
    
    # efficient DMUs
    #idx_eff <- c(1:nrow(data))[bcc_scores_out < 1.001]
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
  
  return(new_dmu_values)
}

#' @title Create New SMOTE Units to Balance Data
#'
#' @description This function creates new DMUs to address data imbalances. If the majority class is efficient, it generates new inefficient DMUs by worsering the observed units. Conversely, if the majority class is inefficient, it projects inefficient DMUs to the frontier. Finally, a random selection if performed to keep a proportion of 0.65 for the majority class and 0.35 for the minority class.
#' 
#' @param data A \code{data.frame} containing the variables used in the model.
#' @param data_factor A \code{data.frame} containing the factor variables used in the model.
#' @param x Column indexes of the input variables in the \code{data}.
#' @param y Column indexes of the output variables in the \code{data}.
#' @param z Column indexes of environment variables in \code{data}.
#' @param balance_data Indicate Level of efficient units to achive and the number of efficient and not efficient units.

#' @return It returns a \code{data.frame} with the newly created set of DMUs incorporated.

SMOTE_balance_data <- function (
    data, data_factor, x, y, z = NULL, balance_data
) {

 
  
  data_eff <- data[data$class_efficiency == "efficient", ]
  
  idx_eff <- 1:nrow(data_eff)
  
  lambda <- rep(0.1666667, ncol(data))
  
  # 0.25 para 4
  # 0.2 para 5
  # 0.1666667 para 6
  
  combinations <- as.data.frame(t(combn(idx_eff, 6)))
  
  # create convex combintaions
  resultados_df <- t(apply(combinations, 1, function(indices) {
    
    # select row
    seleccion <- data[indices, c(x,y)]
    
    # calculate
    colSums(seleccion * lambda)
  }))
  
  
  add_test <- compute_scores_additive(resultados_df, x = x, y = y)
  
  length(which(add_test < 0.0000001))
  
  # para combinaciones de 4: 35
  # para combinaciones de 5: 45
  # para combinaciones de 6: 41
  
  
  browser()
  
  # number of inputs
  nX <- length(x)
  
  # number of outputs
  nY <- length(y)
  
  nZ <- length(z)
  
  # number of samples
  N <- nrow(data)
  
  # determinate numbre of efficient and ineeficient units
  n_real_eff <- nrow(data[data$class_efficiency == "efficient",])
  n_real_ineff <-nrow(data[data$class_efficiency == "not_efficient",])
  
  prop_real <- n_real_eff / (n_real_eff + n_real_ineff)
  
  n_new_eff <- n_real_eff
  n_new_ineff <- n_real_ineff
  prop <- prop_real
  
  add_eff <- balance_data[2]
  if (add_eff == 0){
    add_eff <- 1
  }
  
  eff_level <- balance_data[1]

  while (prop < eff_level) {
    n_new_eff <- n_new_eff + add_eff
    
    if (balance_data[2] != 0) {
      n_new_ineff <- n_new_ineff + 1
    }
    
    prop <- n_new_eff / (n_new_eff + n_new_ineff)
  }

  create_eff <- n_new_eff - n_real_eff
  create_ineff <- n_new_ineff - n_real_ineff
  
  data_eff <- data[data$class_efficiency == "efficient",]
  compute_scores_additive(data_eff, x = x, y = y)
  
  # determinate eff units
  n_eff <- nrow(data_eff)
  
  # select unit to compare
  final_data <- data
  
  # save new units
  new_eff_point <- as.data.frame(matrix(
    data = NA,
    ncol = ncol(data[, c(x,y)]),
    nrow = 0
  ))
  
  names(new_eff_point) <- names(data[, c(x,y)])
  
  new_ineff_point <- as.data.frame(matrix(
    data = NA,
    ncol = ncol(data[, c(x,y)]),
    nrow = 0
  ))
  
  names(new_ineff_point) <- names(data[, c(x,y)])
  
  # save conexions
  eff_conexion <- as.data.frame(matrix(
    data = NA,
    ncol = 2,
    nrow = 0
  ))
  
  names(eff_conexion) <- c("reference", "unit_conexion")
  
  ineff_conexion <- as.data.frame(matrix(
    data = NA,
    ncol = 2,
    nrow = 0
  ))
  
  names(ineff_conexion) <- c("reference", "unit_conexion")
  
  # =================== #
  # balance proportions #
  # =================== #
  # ---
  # eff units
  # ---
  
  n_save_eff <- 0
  
  while (n_save_eff < create_eff) {
    
    repeat {
      
      random_units <- sample(1:n_eff, 2, replace = FALSE)
      
      # select unit reference
      reference <- random_units[1]
      
      # # units to copare
      unit_conexion <- random_units[2]
      
      conexion_test <- data.frame(
        reference = reference,
        unit_conexion = unit_conexion
      )
      
      is_in_ineff_conexion <- any(apply(ineff_conexion, 1, function(row) all(row == unlist(conexion_test))))
      
      if (is_in_ineff_conexion == FALSE) {
        break
      }
      
    }
    
    delta <- runif(1, min = 0, max = 1)
    
    # generate SMOTE unit
    new_point <- data_eff[reference, c(x,y)] + (data_eff[unit_conexion, c(x,y)] - data_eff[reference, c(x,y)]) * delta
    
    test_eff <- rbind(data_eff[, c(x,y)], new_point)
    idx_new_point <- n_eff + 1
    
    test_additive <- compute_scores_additive(test_eff, x = x, y = y)

    if (test_additive[idx_new_point] > 0) {
      
      # this conexion makes inefficient units
      conexion_ineff <- data.frame(
        reference = reference,
        unit_conexion = unit_conexion
      )
      
      conexion_ineff2 <- data.frame(
        reference = conexion_ineff$unit_conexion,
        unit_conexion = conexion_ineff$reference
      )
      
      ineff_conexion <- rbind(ineff_conexion, conexion_ineff, conexion_ineff2)
      
    } else {
      
      # this conexion makes efficient units
      conexion_eff <- data.frame(
        reference = reference,
        unit_conexion = unit_conexion
      )

      conexion_eff2 <- data.frame(
        reference = conexion_eff$unit_conexion,
        unit_conexion = conexion_eff$reference
      )
      
      eff_conexion <- rbind(eff_conexion, conexion_eff, conexion_eff2)
      
      # delete duplicated
      eff_conexion <- eff_conexion[!duplicated(eff_conexion),]
      
      # add correct unit efficient
      new_eff_point <- rbind(new_eff_point, new_point)
      #new_point$class_efficiency <- "efficient"
      
    }

    #prop_eff <- prop.table(table(final_data$class_efficiency))[1]
    n_save_eff <- nrow(new_eff_point)
    
  }
  eff_conexion[seq(1, nrow(eff_conexion), by = 2),]
  
  # ---
  # not eff units
  # ---
  
  if (balance_data[2] != 0){
    n_inef <- 0
    
    while (n_inef < create_ineff) {
      
      # select a random inefficient conexion
      ineff_conexion_sim <- ineff_conexion[seq(1, nrow(ineff_conexion), by = 2), ]
      
      # select a random inefficient conexion
      idx_inef <-  sample(1:nrow(ineff_conexion_sim), 1)
      
      reference <- ineff_conexion_sim[idx_inef, 1]
      unit_conexion <- ineff_conexion_sim[idx_inef, 2]
      
      # generate SMOTE delta
      delta <- runif(1, min = 0, max = 1)
      
      # generate SMOTE unit
      new_point <- data_eff[reference, c(x,y)] + (data_eff[unit_conexion, c(x,y)] - data_eff[reference, c(x,y)]) * delta
      
      # test aditive
      test_ineff <- rbind(data_eff[, c(x,y)], new_point)
      idx_new_point <- n_eff + 1
      
      test_additive <- compute_scores_additive(test_ineff, x = x, y = y)
      
      if (test_additive[idx_new_point] > 0) {
        new_ineff_point <- rbind(new_ineff_point, new_point)
      }
      
      n_inef <- nrow(new_ineff_point)
      
    }
  }
  
  new_eff_point$class_efficiency <- "efficient"
  if (nrow(new_ineff_point) != 0) {
    new_ineff_point$class_efficiency <- "not_efficient"
  }
  
  final_data <- rbind(final_data, new_eff_point, new_ineff_point)

  return(final_data)
}

#' @title Create New SMOTE Units to Balance Data combinations of m + s 
#'
#' @description This function creates new DMUs to address data imbalances. If the majority class is efficient, it generates new inefficient DMUs by worsering the observed units. Conversely, if the majority class is inefficient, it projects inefficient DMUs to the frontier. Finally, a random selection if performed to keep a proportion of 0.65 for the majority class and 0.35 for the minority class.
#' 
#' @param data A \code{data.frame} containing the variables used in the model.
#' @param data_factor A \code{data.frame} containing the factor variables used in the model.
#' @param x Column indexes of the input variables in the \code{data}.
#' @param y Column indexes of the output variables in the \code{data}.
#' @param z Column indexes of environment variables in \code{data}.
#' @param balance_data Indicate level of efficient units to achive and the number of efficient and not efficient units.
#' @param sub_frontier Indicate how many units not efficient it must be created.

#' @return It returns a \code{data.frame} with the newly created set of DMUs incorporated.

SMOTE_convex_balance_data <- function (
    data, data_factor, x, y, z = NULL, balance_data, sub_frontier
) {

  # =========================================================== #
  # determinate number of efficient and not efficient to create #
  # =========================================================== #
  
  # determinate numbre of efficient and ineeficient units
  n_real_eff <- nrow(data[data$class_efficiency == "efficient",])
  n_real_ineff <-nrow(data[data$class_efficiency == "not_efficient",])
  
  prop_real <- n_real_eff / nrow(data)
  
  # n_new_eff <- n_real_eff
  n_new_eff <- 0
  
  #n_new_ineff <- n_real_ineff
  n_new_ineff <- 0
  
  prop <- prop_real
  
  if (is.null(sub_frontier)) {
    
    add_not_eff == 0
    
  } else {
    
    # save proprtion efficient
    numerator <- as.numeric(substr(sub_frontier, 1, 1))
    denominator <- as.numeric(substr(sub_frontier, 3, 3))
    
    add_not_eff <- numerator / denominator
  }
  
  make_ineff <- 0
  loop <- 0
  
  while (make_ineff < 1) {
    
    loop <- loop + 1
    make_ineff <- make_ineff + add_not_eff
    
  }
  
  add_eff <- loop - 1
  
  eff_level <- balance_data
  eff_level <- 0.4
  
  test_n_eff <- n_real_eff
  test_n_ineff <- n_real_ineff
  
  while (prop < eff_level) {
    
    test_n_eff <- test_n_eff + add_eff
    
    if (sub_frontier != 0) {
      test_n_ineff <- test_n_ineff + 1
    }
    
    prop <- test_n_eff / (test_n_eff + test_n_ineff)
  }
  
  # it is necessary to create create_eff units
  create_eff <- test_n_eff - n_real_eff
  
  # it is necessary to create create_ineff units
  create_ineff <- test_n_ineff - n_real_ineff
  
  # ====================================================== #
  # create convex combinations to achieve create_eff units #
  # ====================================================== #
  
  # determinate 
  data_eff <- data[data$class_efficiency == "efficient", ]
  
  idx_eff <- 1:nrow(data_eff)
  
  # proportion importance
  len <- length(c(x,y))
  
  prop_imp <- 1/len
  
  lambda <- rep(prop_imp, ncol(data_eff[, c(x,y)]))
  
  n_comb <- length(c(x,y))

  combinations <- as.data.frame(t(combn(idx_eff, n_comb)))
  
  eff_convex <- as.data.frame(matrix(
    data = NA,
    ncol = ncol(data_eff[, c(x,y)]),
    nrow = 0
  ))
  
  names(eff_convex) <- names(data_eff[, c(x,y)])
  
  ineff_convex <- as.data.frame(matrix(
    data = NA,
    ncol = ncol(data_eff[, c(x,y)]),
    nrow = 0
  ))
  
  names(ineff_convex) <- names(data_eff[, c(x,y)])
  
  if(nrow(combinations) > 5000) {
    
    # Partition size
    n_batch <- 5000
    
    # shuffle the data
    shuffle_data <- sample(1:nrow(combinations))
    
    # Create an index for each partition
    combinations$particion <- ceiling(seq_along(shuffle_data) / n_batch)
    
    batch_all <- split(combinations[shuffle_data, ], combinations$particion)
    
    n_total_batch <- ceiling(nrow(combinations) / 5000)
    
  } else {
    
    batch_all <- list()
    batch_all[[1]] <- combinations
    
    n_total_batch <- 1
    
  }
    
  # create convex combintaions
  print("calculate combinations points")
  
  iter <- 0
  count_batch <- 0
  
  save_idx_eff <- NULL
  
  while (nrow(eff_convex) < create_eff) {
    
    count_batch <- 1
    iter <- iter + 1
    print(iter)
    
    # units to classify
    results_convx <- t(apply(batch_all[[iter]][,c(x,y)], 1, function(indices) {
      
      # select row
      seleccion <- data_eff[unlist(as.vector(indices)), c(x,y)]
      
      # calculate
      colSums(seleccion * lambda)
    }))
    
    # change to dataframe 
    results_convx <- as.data.frame(results_convx)
    
    # change name
    names(results_convx) <- names(data_eff[, c(x,y)])
    
    # create test dataset additive
    test_eff <- rbind(data_eff[, c(x,y)], results_convx)
    
    # test additive
    test_add <- compute_scores_additive(test_eff, x = x, y = y)
    b <- compute_scores_additive(results_convx, x = x, y = y)
    which(b < 0.0001)
    which(test_add < 0.0001)
    # leave original eff units, get index 
    new_results_convx <- results_convx[(nrow(data_eff) + 1):nrow(test_eff),]
    idx_eff <- which(test_add[(nrow(data_eff) + 1):nrow(test_add),] < 0.0001)
    
    # save idx_eff
    save_idx_eff <- c(save_idx_eff, idx_eff)
    
    new_eff_conx_unit <- new_results_convx[idx_eff, ]
    
    # save unit efficient
    eff_convex <- rbind(eff_convex, new_eff_conx_unit)
    
    # save not efficient
    ineff_to_save <- new_results_convx
    
    if(nrow(new_eff_conx_unit) != 0) {
      
      ineff_to_save <- ineff_to_save[-idx_eff,]
      
    }
    
    ineff_convex <- rbind(ineff_convex, ineff_to_save)
    
    # get prop information
    print(paste("There are:", nrow(eff_convex)))
    print(paste("It must be created:", create_eff))
    print(paste(nrow(eff_convex)/create_eff * 100, "%"))
    
    true_eff <- nrow(eff_convex)
    
    # if there are not enough efficient units, use 
    if(count_batch == n_total_batch & true_eff < create_eff) {
      
      # need to create 
      need_eff <- create_eff - true_eff
      
      eff_combinations <- combinations[save_idx_eff,]
      
      save_lambda_eff <- as.data.frame(matrix(
        data = NA,
        ncol = length(c(x,y)),
        nrow = 0
      ))
      
      while (nrow(save_lambda_eff) < need_eff) {
        
        # process to generate lambda
        generate_lambda <- runif(length(c(x, y)), min = 0.05, max = 0.95) 
        
        normalize_lambda <- generate_lambda/sum(generate_lambda)
        
        # set lambda
        lambda_eff <- normalize_lambda
        
        # set combnation to make new unit
        idx_new_eff <- sample(1:nrow(eff_combinations), size = 1)
        selec_comb <- eff_combinations[idx_new_eff,]
        
        # units to classify
        seleccion <- data_eff[unlist(as.vector(selec_comb)), c(x,y)]
        
        # calculate
        new_unit <- colSums(seleccion * lambda_eff)
        
        # check 
        check_data <- rbind(data_eff[, c(x,y)], new_unit)
        
        check_test <- compute_scores_additive(check_data, x = x, y = y)
        
        # save if is correct
        if (check_test[nrow(data_eff) + 1,] < 0.0001) {
          
          save_lambda_eff <- rbind(save_lambda_eff, new_unit)
          
        }
        
      } # end loop for
      
      names(save_lambda_eff) <- names(data_eff[, c(x,y)])
      
      # join eff_data
      eff_convex <- rbind(eff_convex, save_lambda_eff)
      
    } # end case need more efficient units
    
    if (count_batch == n_total_batch & true_eff == create_eff) {break}
    
  } # end while
  
  select_ineff_idx <- sample(nrow(ineff_convex), size = create_ineff, replace = FALSE) 
  
  if (any(duplicated(select_ineff_idx))) {
    print("duplicated; not efficient number of units have been created")
    stop()
  }
  
  ineff_convex <- ineff_convex[select_ineff_idx,]
  
  # add class efficiency
  eff_convex$class_efficiency <- "efficient"
  ineff_convex$class_efficiency <- "not_efficient"
  
  final_data <- rbind(data, eff_convex, ineff_convex)

  return(final_data)
}