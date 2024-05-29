### Table descriptive

scores <- information_region[[1]]
summary(scores)

DMU <- as.data.frame(
  matrix(
    data = NA,
    ncol = 1,
    nrow = nrow(scores)
  )
)
DMU[1] <- 1:999

names(DMU) <- "DMU"

data <- cbind(DMU, scores)

library(ggplot2)
ggplot() +
  geom_density(data = data, aes(x = svmPoly, color = "orange") ) +
  geom_density(data = data, aes(x = nnet, color = "cyan")) +
  theme_bw()
  


