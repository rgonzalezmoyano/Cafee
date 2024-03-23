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

# dimensions
nrow <- length(unique(data$SCHOOLID))
ncol <- as.numeric(length(names(data))) - 1 # No STIDSTD 

new_data <- matrix(
  data = NA,
  ncol = ncol,
  nrow = nrow
  )

new_data <- as.data.frame(new_data)

# First: ID information
for (i in 1:5) {
  data[[i]] <- as.factor(data[[i]])
  print(paste(info$vble[i], "is", info$name[i]))
}

for (i in 6:7) {
  data[[i]] <- as.numeric(data[[i]])
  print(paste(info$vble[i], "is", info$name[i]))
}

# in
info_data <- data[, 1:7]

info_data <- info_data[-7] %>% 
  group_by(SCHOOLID) %>% 
  








value_detection <- function (data) {
  
  data_dim <- matrix (
    ncol = ncol(data),
    nrow = 1
  )
  
  data_dim <- apply(data, MARGIN = 2, FUN = function(x) length(unique(x)))
  
  idx_liker <- which(data_dim < 10)
  
  data_lkr <- data[idx_liker]
  
  
  
}