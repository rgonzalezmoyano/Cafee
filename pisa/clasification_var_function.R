### Function Clasification of variables

# ========= #
# Libreries #
# ========= #
library(readxl)
library(dplyr)

# ========= #
# Load data #
# ========= #
load("C:/Users/Ricardo/Downloads/PISA_2012.RData")
data <- a$data
info <- a$names

# assign factor variable
idx_change_factor <- c(4, 9, 10, 11, 12, 14, 21:26, 28:31, 34:37, 39, 40:60, 120:124,
                       133:136,  147:171, 284:301, 333:339, 348:376, 378:387, 389:390,
                       392, 408:410, 412:413)

# determinate likert variable
likert <- c(8, 13, 15:20, 27, 32:33, 38, 61:119, 125:132, 137:140, 180:283, 302:332,
            340:347, 377, 388, 391, 393:404)

# numeric variables 
idx_change_num <- c(141:146, 172:179, 405:407, 411, 414:420)

# Transform variable to YES == 1 and NO == 2
get_last_char <- function(row) {
  last_char <- substr(row, nchar(row), nchar(row))
  return(last_char)
}

data$ST26Q15 <- apply(as.matrix(data$ST26Q15), MARGIN = 1, FUN = get_last_char)
data$ST26Q16 <- apply(as.matrix(data$ST26Q16), MARGIN = 1, FUN = get_last_char)
data$ST26Q17 <- apply(as.matrix(data$ST26Q17), MARGIN = 1, FUN = get_last_char)

# =============== #
# group_by school #
# =============== #

# create ID_key
data$ID_PISA <- paste0(data$CNT, data$SCHOOLID)

# dimensions
nrow = length(unique(data$ID_PISA))
ncol <- as.numeric(length(names(data)))


# datafreme of results
new_data <- matrix (
  data = NA,
  ncol = ncol,
  nrow = nrow
)

new_data <- as.data.frame(new_data)

# names
names(new_data) <- names(data)

# bucle
for (i in 1:nrow(new_data)) {
  
  # ID
  new_data[i, 635] <- unique(data$ID_PISA)[i]
  
  i_data <- data %>% 
    filter(ID_PISA == new_data[i, 635])
  
  # CNT
  new_data[i, 1] <- unique(i_data$CNT)
  
  # SUBNATIO
  new_data[i, 2] <- unique(i_data$SUBNATIO)
  
  # STRATUM
  new_data[i, 3] <- unique(i_data$STRATUM)
  
  # OECD
  new_data[i, 4] <- unique(i_data$OECD)
  
  # NC
  new_data[i, 5] <- unique(i_data$NC)
  
  # SCHOOLID
  new_data[i, 6] <- unique(i_data$SCHOOLID)
  
  # STIDSTD
  # ID information, after group_by, delete
  new_data[i, 7] <- NA
  
  for (vble in 1:ncol(i_data)) {
    
    # determinate if exist strange range
    posible_value <- unique(i_data[[vble]])
    
    posible_value <- sort(as.numeric(posible_value))
    
    if (vble %in% c(409)) {
      
      strange_v <- 0
      
    } else {
      
      # by default
      strange_v <- 1
      
      if (posible_value[length(posible_value)] - posible_value[length(posible_value) - 1] == 1) {
        
        strange_v <- 2
        
        if (posible_value[length(posible_value)] - posible_value[length(posible_value) - 2] == 2) {
          
          strange_v <- 3
          
        }
        
      }
      
    }
    
    if (vble %in% likert) {
      
      # sort posible values
      values <- unique(i_data[[vble]])
      values <- sort(values)
      
      # delete NA, Invalid, Missing index
      correct_idx <- values[1:(length(values) - strange_v)]
      
      # Only correct index
      value_idx <- which(i_data[[vble]] %in% correct_idx)
      
      # Only correct value
      i_clean_data <- i_data[[vble]][value_idx]
      
      # majority class
      freq <- table(i_clean_data)
      max_class <- which.max(freq)
      
      # asign majority class
      new_data[i, vble] <- values[max_class]
      
    } else {
      
      # sort posible values
      values <- unique(i_data[[vble]])
      values <- sort(values)
      
      # delete NA, Invalid, Missing index
      correct_idx <- values[1:(length(values) - strange_v)]
      
      # Only correct index
      value_idx <- which(i_data[[vble]] %in% correct_idx)
      
      # Only correct value
      i_clean_data <- i_data[[vble]][value_idx]
      
      # mean 
      mean_value <- mean(as.numeric(i_clean_data))
      
      # assign value
      new_data[i, vble] <- mean_value
      
    } 
    
  }
  
  
  
}














# for (i in 6:7) {
#   data[[i]] <- as.numeric(data[[i]])
#   print(paste(info$vble[i], "is", info$name[i]))
# }
# 
# # in
# info_data <- data[, 1:7]
# 
# info_data <- info_data[-7] %>% 
#   group_by(SCHOOLID) %>% 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# value_detection <- function (data) {
#   
#   data_dim <- matrix (
#     ncol = ncol(data),
#     nrow = 1
#   )
#   
#   data_dim <- apply(data, MARGIN = 2, FUN = function(x) length(unique(x)))
#   
#   idx_liker <- which(data_dim < 10)
#   
#   data_lkr <- data[idx_liker]
#   
#   
#   
# }

