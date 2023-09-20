library(caret)

tuneGrid_Linear <- unique(expand.grid(methods$svmPoly))



modelLinear <- train(form = as.factor(class_efficiency) ~.,
                     data = data,
                     method = "svmLinear",
                     trControl = trControl,
                     tuneGrid = tuneGrid_Linear,
                     metric = "Kappa")