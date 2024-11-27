# Test convex with hull

library(cxhull)
library(ggplot2)

data <- as.data.frame(matrix(
  data = NA, 
  ncol = 2,
  nrow = 7
))

names(data) <- c("x", "y")


x <- 1
y <- 2

data$x <- c(2,3,6,8,11, 6, 10)
data$y <- c(1,4,8,10,12, 5, 6)

ggplot() +
  geom_point(data = data, aes(x = x, y = y)) +
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(legend.position = "bottom")
  
