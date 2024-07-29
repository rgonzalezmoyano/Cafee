# get DMUs are pareto efficient

idx <- which(data$class_efficiency == "efficient")

data_peff <- data[idx, ]

# new  dataset of scores result
scores <- matrix (
  ncol = length(methods),
  nrow = nrow(data_peff)
) 

ML_scores <- information_region[[1]][idx,]

library(openxlsx)
write.xlsx(ML_scores, "scores_peff.xlsx")

write.xlsx(as.data.frame(idx), "scores_peff_idx.xlsx")
min(na.omit(ML_scores$nnet))



idx_SMV <- which(ML_scores$svmPoly > 1)

idx_NN <- which(ML_scores$nnet > 1)

max(ML_scores$svmPoly[idx_SMV])
max(ML_scores$nnet[idx_NN])

min(na.omit(ML_scores$svmPoly))
min(ML_scores$nnet)
