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
#' @return A \code{list} where each element corresponds to a balance level, containing a single \code{data.frame} 
#' with the real and synthetic DMUs, correctly labeled.

get_SMOTE_DMUs <- function (
    data, facets, x, y, z = NULL, balance_levels = NULL
) {
  
  copy_data <- data
  
  # we need to determine, for each balance level, the number of synthetic DMUs to create
  
  
  
  # create the dataframe to save data
  save_datasets <- vector("list", length = length(data))
  
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
    
    # units to classify
    results_convx <- t(apply(idx, 1, function(indices) { 
      
      # select row
      seleccion <- data_eff[unlist(as.vector(indices)), c(x,y)]
      
      # calculate
      colSums(seleccion * lambda)
      
    }))
    
    new_units <- as.data.frame(results_convx)
    browser()
    new_units[, z]
    
    new_units$class_efficiency <- "efficient"
    rbind(data[[context]], new_units)
    
    
    save_datasets[[context]] <- rbind(data[[context]], new_units)
    save
    
  }

  
 
    
  return(new_data)
  
}