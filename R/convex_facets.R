#' @title Create New SMOTE Units to Balance Data combinations of m + s 
#'
#' @description This function creates new DMUs to address data imbalances. If the majority class is efficient, it generates new inefficient DMUs by worsering the observed units. Conversely, if the majority class is inefficient, it projects inefficient DMUs to the frontier. Finally, a random selection if performed to keep a proportion of 0.65 for the majority class and 0.35 for the minority class.
#' 
#' @param data A \code{data.frame} containing the variables used in the model.
#' @param x Column indexes of the input variables in the \code{data}.
#' @param y Column indexes of the output variables in the \code{data}.
#' @param z Column indexes of environment variables in \code{data}.
#' @param groups Number of sub groups in \code{data}.
#' 
#' @return It returns a \code{data.frame} with the newly created set of DMUs incorporated.

convex_facets <- function (
    data, x, y, z = NULL, groups
) {
 
  copy_data <- data
  
  # create the dataframe to save data
  save_facets <- vector("list", length = length(data))
  
  for (context in 1:length(save_facets)) {
    
    # select data
    data_context <- data[[context]]
    
    # first, create set combinations 
    # determinate 
    data_eff <- data_context[data_context$class_efficiency == "efficient", ]
    
    n_eff <- 1:nrow(data_eff)
    
    # proportion importance
    len <- length(c(x,y))
    
    prop_imp <- 1/len
    
    lambda <- rep(prop_imp, ncol(data_eff[, c(x,y)]))
    
    n_comb <- length(c(x,y))
    
    combinations <- as.data.frame(t(combn(n_eff, n_comb)))
    
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
    print("calculate combinations points:")
    print(nrow(combinations))
    
    print("Number of batches:")
    print(length(batch_all))
    
    iter <- 0
    count_batch <- 0
    
    save_idx_eff <- NULL
    save_idx_ineff <- NULL
    
    # ================================= #
    # create efficient units to balance #
    # ================================= #
  
    for (iter in 1:length(batch_all)) {
      
      print(paste("Batch:", iter))
      
      # units to classify
      results_convx <- t(apply(batch_all[[iter]], 1, function(indices) { #[,c(x,y)]
      
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
      
      # leave original eff units, get index
      new_results_convx <- test_eff[(nrow(data_eff) + 1):nrow(test_eff),]
      idx_eff <- which(test_add[(nrow(data_eff) + 1):nrow(test_add),] < 0.0001)
      
      if (length(idx_eff) == 0) {
        ineff_to_save <- new_results_convx
        next
      }
      
      idx_dmus_eff <- batch_all[[iter]][idx_eff, c(x,y)]
     
      # # save idx_eff
      # save_idx_eff <- c(save_idx_eff, idx_eff)
      # 
      # new_eff_conx_unit <- new_results_convx[idx_eff, ]
      # 
      # save efficient convex
      eff_convex <- rbind(eff_convex, idx_dmus_eff)
      # 
      # # save not efficient
      # ineff_to_save <- new_results_convx[-idx_eff, ]
      print(nrow(eff_convex))
      
    }
    
    # change to dataframe
    results_convx <- as.data.frame(eff_convex)
    
    # # change name
    # names(results_convx) <- names(data[, c(x,y)]) 
    
    save_facets[[context]] <- results_convx
  } # end loop save_facets
    
  browser()
  return(results_convx)
  
}