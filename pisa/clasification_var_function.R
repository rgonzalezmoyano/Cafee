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
idx_change_factor <- c(4, 9, 10, 11, 12, 14, 21:26, 28:31, 34:37, 39, 40:41, 43:60, 120:124,
                       133:136,  147:171, 284:301, 333:339, 348:376, 378:387, 389:390,
                       392, 408:410, 412:413, 421:423, 435, 439, 448, 456:459, 468:469,
                       483, 632, 634, 635)

# determinate likert variable
likert <- c(8, 13, 15:20, 27, 32:33, 38, 61:119, 125:132, 137:140, 180:283, 302:332,
            340:347, 377, 388, 391, 393:404, 436, 439, 454:455, 465)

# numeric variables 
idx_change_num <- c(42, 141:146, 172:179, 405:407, 411, 414:420, 424:434, 437:438, 440:447,
                    449:453, 460:464, 466, 467, 470:476, 477:482, 484:499, 500:631, 633)

# test and validation variables
all_vector <-c(idx_change_factor, likert, idx_change_num)
diff <- setdiff(1:635, all_vector)

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

# only SPAIN
data <- data %>% 
  filter(CNT == "ESP")

# create ID_key
data$ID_PISA <- paste0(data$CNT, data$SCHOOLID)
# create ID_key by student
data$ID_PISA <- paste0(data$SCHOOLID, data$STIDSTD)

# dimensions
nrow <- length(unique(data$ID_PISA))
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
  paste("Iteración:", print(i))
  print(i/nrow(new_data))
  print("")
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
  
  for (vble in 8:ncol(i_data)) {
    #copy_data <- i_data
    
    # determinate if exist strange range
    posible_value <- unique(data[[vble]])
    
    posible_value <- sort(as.numeric(posible_value))
    
    if (vble %in% c(11, 12, 34, 36:37, 120:124, 409, 410, 457, 459, 500:630, 634, 635)) {
      
      strange_v <- 0
      no_value <- 0

    } else {
      
      # by default
      strange_v <- 1
      
      # get value to delete per school
      no_value <- posible_value[length(posible_value)]
      
      if (length(posible_value) == 1) {
        # do nothing
        # comprobar si está bien cuando no sea para España solo
        strange_v <- 0
        no_value <- 0
        
      } else if (posible_value[length(posible_value)] - posible_value[length(posible_value) - 1] == 1 & posible_value[length(posible_value)] > 50) {
        
        strange_v <- 2
        
        no_value_1 <- posible_value[length(posible_value) - 1]
        no_value <- c(no_value, no_value_1)
        
        if (posible_value[length(posible_value)] - posible_value[length(posible_value) - 2] == 2) {
          
          strange_v <- 3
          
          no_value_2 <- posible_value[length(posible_value) - 2]
          no_value <- c(no_value, no_value_2)
          
        }
        
      }
      
    }
    
    # Transform to numeric if it is posible
    # Check if there are any elements containing characters
    if (!any(grepl("[a-zA-Z]", i_data[[vble]]))) {
      
      # Convert to numeric if there are no characters
      i_data[[vble]] <- as.numeric(i_data[[vble]])
      
    }
    
    # imput NA on strange value
    i_data[[vble]][i_data[[vble]] %in% no_value] <- NA
    
    # confirm no data error
    i_data[[vble]] <- ifelse(i_data[[vble]] %in% c(9999, 9998, 9997), NA, i_data[[vble]])
    
    # clean data: filter no_value
    if (min(no_value) > 0) {  # & unique(!(no_value == 0))
      
      # get only correct data
      # i_clean_data <- i_data %>% 
      #   filter(!(i_data[[vble]] %in% c(no_value)))
      
      i_clean_data <- na.omit(i_data[[vble]])
      
    } else { # There is not problematic data
        
      i_clean_data <- i_data[[vble]]
      
      }
    
    # sort posible values
    values <- unique(i_clean_data)
    
    if (!(vble %in% likert) & !(vble %in% idx_change_num)) {
      
      if (all(is.na(values))) {  ##### cambios np_value por values
        
        new_data[i, vble] <- NA
        
      } else {
       
        values <- sort(values)
        # 
        # # delete NA, Invalid, Missing index
        # correct_idx <- values[1:(length(values) - strange_v)]
        # 
        # # Only correct index
        # value_idx <- which(i_data[[vble]] %in% correct_idx)
        # 
        # # Only correct value
        # i_clean_data <- i_data[[vble]][value_idx]
        
        # majority class
        freq <- table(i_clean_data)
        max_class <- which.max(freq)
        
        
        # asign majority class
        new_data[i, vble] <- values[max_class]
        
      }
      
    } else {
      
      if (all(is.na(values))) { ###### cambio no_value por values
        
        new_data[i, vble] <- NA
        
      } else {
        
        # sort posible values
        # values <- unique(i_data[[vble]])
        values <- sort(values)
        
        # # delete NA, Invalid, Missing index
        # correct_idx <- values[1:(length(values) - strange_v)]
        # 
        # # Only correct index
        # value_idx <- which(i_data[[vble]] %in% correct_idx)
        # 
        # # Only correct value
        # i_clean_data <- i_data[[vble]][value_idx]
        
        # mean 
        mean_value <- mean(as.numeric(i_clean_data))
        
        # assign value
        new_data[i, vble] <- mean_value
        
      }
      
    } 
    
  }
  
}





# save data
file <- paste("pisa/data_PISA_2012_by_ESP_student", ".RData", sep = "")
save(new_data, file = file)
