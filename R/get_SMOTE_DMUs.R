#' @title Create New SMOTE Units to Balance Data combinations of m + s 
#'
#' @description This function creates new DMUs to address data imbalances.
#' If the majority class is efficient, it generates new inefficient DMUs by worsering the observed units.
#' Conversely, if the majority class is inefficient, it projects inefficient DMUs to the frontier.
#' Finally, a random selection if performed to keep a proportion of 0.65 for the majority class and 0.35 for the minority class.
#' 
#' @param data A \code{list} of \code{data.frames}, where each element represents a dataset with labeled data.
#' @param facets A \code{list} where each element represents a subgroup containing index combinations that generate efficient units.
#' @param x Column indexes of the input variables in the \code{data}.
#' @param y Column indexes of the output variables in the \code{data}.
#' @param z Column indexes of environment variables in \code{data} (optional).
#' @param balance A numeric vector indicating the different levels of balance required (e.g., c(0.1, 0.45, 0.6)).
#' 
#' @importFrom dplyr anti_join
#' 
#' @return A \code{list} where each element corresponds to a balance level, containing a single \code{data.frame} 
#' with the real and synthetic DMUs, correctly labeled.

get_SMOTE_DMUs <- function (
    data, facets, x, y, z = NULL, balance_levels = NULL
) {

  copy_data <- data
  
  save_dataset_balanced <- vector("list", length = length(balance_levels))
  
  names(save_dataset_balanced) <- as.character(balance_levels)

  # we need to determine, for each balance level, the number of synthetic DMUs to create
  for (balance in balance_levels) {
    
    save_dataset <- as.data.frame(matrix(
      data = NA,
      ncol = ncol(copy_data[[1]]),
      nrow = 0
    ))
    
    for(sub_group in 1:length(copy_data)) {
    
      data <- copy_data
      # =========================================================== #
      # determinate number of efficient and not efficient to create #
      # =========================================================== #
      data <- data[[sub_group]]
      
      if(nrow(facets[[sub_group]]) == 0) {
        print("nrow(facets[[sub_group]]) == 0")
        
        save_dataset <- rbind(data, save_dataset)
        next
        
      }

      # determinate numbre of efficient and ineeficient units
      n_real_eff <- nrow(data[data$class_efficiency == "efficient",])
      n_real_ineff <- nrow(data[data$class_efficiency == "not_efficient",])
      
      prop_real <- n_real_eff / nrow(data)
      
      # n_new_eff <- n_real_eff
      n_new_eff <- 0
      
      #n_new_ineff <- n_real_ineff
      n_new_ineff <- 0
      
      # proportion of efficients
      prop <- prop_real
      
      sense_balance <- NULL
      
      # determinate the way to balance, create efficient or not efficient
      if (prop < balance) {
        
        # need to create efficient units
        sense_balance <- "efficient"
        
        # in each itaretion we create these DMUs
        add_eff <- 1
        add_not_eff <- 0
        
      } else {
       
        # need to create not efficient units
        sense_balance <- "not_efficient"
        
        # in each itaretion we create these DMUs
        add_eff <- 0
        add_not_eff <- 1
        
      }
      
      # determinate how many DMUs create PROPORTION
      eff_level <- balance
      
      test_n_eff <- n_real_eff
      test_n_ineff <- n_real_ineff
      
      if (sense_balance == "not_efficient") {
        
        while (prop > eff_level) {
          
          test_n_ineff <- test_n_ineff + add_not_eff
          
          prop <- test_n_eff / (test_n_eff + test_n_ineff)
        }
        
      } else {
        
        while (prop < eff_level) {
          
          test_n_eff <- test_n_eff + add_eff
          
          prop <- test_n_eff / (test_n_eff + test_n_ineff)
        }
        
      }
      
      # it is necessary to create create_eff units
      create_eff <- test_n_eff - n_real_eff
      
      # it is necessary to create create_ineff units
      create_ineff <- test_n_ineff - n_real_ineff
      
      # ============================================ #
      # get index to create efficient synthetic DMUs #
      # ============================================ #
      
      # total combinations 
      # eff data
      data_eff <- data[data$class_efficiency == "efficient", ]
      
      # real efficient combination
      idx <- facets[[sub_group]]
      n_idx <- 1:nrow(idx)
      
      # number of efficient DMUs
      n_eff <- nrow(data_eff)
      
      # create units
      # lambda
      
      # proportion importance
      len <- ncol(facets[[sub_group]])
      
      prop_imp <- 1/len
      
      lambda <- rep(prop_imp, ncol(facets[[sub_group]]))
      
      n_comb <- nrow(data_eff)
      
      combinations <- as.data.frame(t(combn(n_comb, len)))
      
      if (sense_balance == "not_efficient") {
        
        # delete combinations efficient
        #combinations <- anti_join(combinations, idx, by = names(idx))
        
        # select  k-create_ineff
        if (nrow(combinations) > (create_ineff * 3)) {
          idx_combinations <- sample(x = 1:nrow(combinations), size = (create_ineff * 3), replace = FALSE)
          
          idx_ineff <- combinations[idx_combinations,]
          
          idx <- anti_join(idx_ineff, idx, by = names(idx))
          
          idx <- na.omit(idx)
          
        } else if (nrow(combinations) == 1) {

          print("Next")
          print(sub_group)
          
          save_dataset <- rbind(data, save_dataset)
          next
          
        } 
        
      } # end not efficient case
      
      # units to classify
      results_convx <- t(apply(idx, 1, function(indices) { 
        
        # select row
        seleccion <- data_eff[unlist(as.vector(indices)), c(x,y)]
        
        # calculate
        colSums(seleccion * lambda)
        
      }))
    
      # as data.frame
      results_convx <- as.data.frame(results_convx)
      
      # create contex information
      vector_contex <- unique(data[,z])
      
      n_col <- length(vector_contex)
      
      contex <- as.data.frame(matrix(
        data = NA,
        nrow = nrow(results_convx),
        ncol = n_col
      ))
      
      names(contex) <- names(vector_contex)
      
      for (colum_contex in 1:length(contex)) {
        
        contex[, colum_contex] <- rep(as.numeric(vector_contex[,colum_contex]), nrow(results_convx)) 
        
      }
      
      
      new_data <- cbind(results_convx, contex)
      
      if(sense_balance == "not_efficient") {
        
        new_data$class_efficiency <- rep("not_efficient", nrow(results_convx))
        
      } else {
        
        new_data$class_efficiency <- rep("efficient", nrow(results_convx))
        
      }
      
      new_data <- rbind(data, new_data)
      
      save_dataset <- rbind(save_dataset, new_data)
      
    } # end loop sub_group
    
    save_dataset_balanced[[as.character(balance)]] <- save_dataset
  } # end loop balance
    
   
  browser()
    
  

  
  
  for (context in 1:length(save_datasets)) {
    
    print(context)
    # select data
    data_context <- data[[context]]
    
    # eff data
    data_eff <- data_context[data_context$class_efficiency == "efficient", ]
    
    idx <- facets[[context]]
    
    # create units
    # lambda
    n_idx <- 1:nrow(idx)
    
    # proportion importance
    len <- ncol(facets[[context]])
    
    prop_imp <- 1/len
    
    lambda <- rep(prop_imp, ncol(facets[[context]]))
    
   
    
    # new_units <- as.data.frame(results_convx)
    # browser()
    # # new_units[, z] <- 
    # 
    # new_units$class_efficiency <- "efficient"
    # rbind(data[[context]], new_units)
    # 
    # 
    # save_datasets[[context]] <- rbind(data[[context]], new_units)
    # save
    
  }
    
  return(new_data)
  
}