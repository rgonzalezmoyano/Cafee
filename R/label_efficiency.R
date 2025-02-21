#' @title Data Preprocessing and Efficiency Labeling Using DEA
#'
#' @description This function uses classification algorithms to estimate the efficiency of a set of DMUs (Decision Making Units).
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column indexes of input variables in \code{data}.
#' @param y Column indexes of output variables in \code{data}.
#' @param z Column indexes of environment variables in \code{data}.
#'
#' @importFrom dplyr select_if %>% arrange top_n sample_n group_split
#'
#' @return A \code{"cafee"} object.
#'
#' @export

label_efficiency <- function (
    data, x, y, z = NULL, target_method, convexity = TRUE,
    returns = "variable"
    ) {
  
  # original data with all variables
  data_original <- data
 
  # Select only the variables set as x, y, and z in the dataframe
  # save factor variables
  data_factor <- data[, z]

  # pre-processing
  data <- preprocessing (
    data = data,
    x = x,
    y = y
  )
  
  # reorder index 'x' and 'y' in data
  x <- 1:length(x)
  y <- (length(x) + 1):ncol(data)

  if (is.null(z)) {
    z <- NULL
  } else {
  
    x_y <- length(c(x, y))
    z <- (x_y + 1):((x_y) + length(z))
  }

  # number of inputs / outputs as inputs and number of outputs
  nX <- length(x)
  nY <- length(y)
  nZ <- length(z)

  # save a copy from original data after preprocess
  copy_data_no_label <- data
  
  # divide the dataset by z categorical, if z = 0, not do it. After that, we employ DEA to label each dataset.
  if (nZ != 0) {
    
    # label data with z
    
    # get the names z
    contx_name <- names(data_factor)
    
    # join with data_factor before to divide
    data_to_divide <- cbind(data, data_factor)
    
    # save a copy with data_factor
    data_save <- data_to_divide
    
    # divide in groups by z
    dfs <- data_to_divide %>%
      group_split(across(all_of(contx_name)), .keep = TRUE)
    
  } else {
    
    # label data without z (only one group)
    
    # join with data_factor before to divide (same as z case)
    data_to_divide <- data
    
    # save a copy with data_factor (same as z case)
    data_save <- data_to_divide
    
    # divide in groups by z
    dfs <- list(data)
    
  }

  # save all data labeled
  data_labeled <- as.data.frame(matrix(
    data = NA,
    ncol = ncol(data_to_divide) + 1,
    nrow = 0
  ))
  
  # set names
  names(data_labeled) <- c(names(data_to_divide), "class_efficiency")
  
  for (sub_group in 1:length(dfs)) {
    #sub_group <- 41
    #print(paste("Sub_group",sub_group))
 
    data <- dfs[[sub_group]]
    
    # ============================ #
    # Label by additive model DEA  #
    # ============================ #
    
    # compute DEA scores through a additive model
    add_scores <-  compute_scores_additive (
      data = data,
      x = x,
      y = y
    )
    
    # determine efficient and inefficient DMUs
    class_efficiency <- ifelse(add_scores[, 1] <= 0.0001, 1, 0)
    
    data <- as.data.frame (
      cbind(data, class_efficiency)
    )
    
    data$class_efficiency <- factor(data$class_efficiency)
    data$class_efficiency <- factor (
      data$class_efficiency,
      levels = rev(levels(data$class_efficiency))
    )
    
    levels(data$class_efficiency) <- c("efficient", "not_efficient")
    
    obs_prop <- prop.table(table(data$class_efficiency))
    
    data_labeled <- rbind(data_labeled, data)
  
  }
  
  return(data_labeled)
  
}
