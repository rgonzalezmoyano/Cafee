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

value_detection <- function (data) {
  
  data_dim <- matrix (
    ncol = ncol(data),
    nrow = 1
  )
  
  data_dim <- apply(data, MARGIN = 2, FUN = function(x) length(unique(x)))
  
  idx_liker <- which(data_dim < 10)
}