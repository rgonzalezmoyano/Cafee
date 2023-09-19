library(deaR)

dataDEA <- make_deadata(datadea = as.data.frame(data), dmus = NULL, inputs = x, outputs = y)

# Model
model_additive <- model_additive(datadea = dataDEA,
                                 rts = "vrs")

scores_additive <- efficiencies(model_additive)
