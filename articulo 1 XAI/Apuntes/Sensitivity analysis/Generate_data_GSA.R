### Example simulation
devtools::load_all()

# libraries
library("ggplot2")
library("rminer")

# generate data
set.seed(1997)

# Simulated data
data <- reffcy (
  DGP = "cobb_douglas_XnY1",
  parms = list (
    N = 30,
    nX = 3
  )
)

# # plot
# ggplot() +
#   geom_point(data = data, aes(x = x1, y = y)) +
#   # exes
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept = 0) +
#   theme_bw() +
#   theme(legend.position = "bottom")

# 
# library(openxlsx)
# write.xlsx(data, "data_example.xlsx")

# x and y indexes
x <- 1
y <- 2
z <- NULL

# import


# different types to label
target_method <- "BCC"

set.seed(314)
methods <- list (
  # "svmPoly" = list(
  #   hyparams = list(
  #     "degree" = c( 2),
  #     "scale" = c(1),
  #     "C" = c(1, 10)
  #   )
  # )
  
  # svm
  "svmPoly" = list(
    hyparams = list(
      "degree" = c(1, 2, 3, 4, 5),
      "scale" = c(0.001, 0.1, 1, 10, 100),
      "C" = c(0.001, 0.1, 1, 10, 100)
    )
  )
  # "svmRadial" = list(
  #   hyparams = list(
  #     "sigma" = c(0.01, 0.1, 1, 10, 100),
  #     "C" = c(0.001, 0.1, 1, 10, 100)
  #   )
  # ),
  
  # # random forest
  # "rf" = list (
  #   options = list (
  #     ntree = c(500) # c(100, 500, 1000)
  #   ),
  #   hyparams = list(
  #     mtry = c(4)
  #   )
  # ),
  
  # # neuronal network
  # "nnet" = list(
  #   hyparams = list(
  #     "size" = c(1, 5, 10, 20),
  #     "decay" = c(0, 0.1, 0.01, 0.001, 0,0001)
  #   ),
  #   options = list (
  #     maxit = 1000
  #   )
  # )
  
)

# =========== #
# score cafee #
# =========== #    

# efficiency orientation
orientation <- "output"

# metrics for model evaluation
MySummary <- function (data, lev = NULL, model = NULL) {
  
  # accuracy and kappa
  acc_kpp <- defaultSummary(data, lev, model)
  
  # AUC, sensitivity and specificity
  auc_sen_spe <- twoClassSummary(data, lev, model)
  
  # precision and recall
  pre_rec <- prSummary(data, lev, model)
  
  c(acc_kpp, auc_sen_spe, pre_rec)
  
  
} 

# Parameters for controlling the training process
trControl <- trainControl (
  method = "cv",
  number = 5,
  summaryFunction = MySummary,
  classProbs = TRUE,
  savePredictions = "all"
)

hold_out <- 0.00
# https://topepo.github.io/caret/train-models-by-tag.html

metric = "Accuracy"

convexity <- TRUE
returns <- "variable"

# save scores region
list_region <- list()

# new  dataset of scores result
scores <- matrix (
  ncol = length(methods),
  nrow = nrow(data)
) 

# change to data.frame
scores <- as.data.frame(scores)

# change names
score_names <- names(methods)
names(scores) <- score_names

# save model information
list_method <- list()  

# bucle region
for (i in 1:length(methods)) {
  
  # console information
  print(paste("METODO:", i,  names(methods)[i]))
  print("")
  
  # model result
  final_model <- efficiency_estimation (
    data = data,
    x = x,
    y = y,
    #z = z,
    orientation = orientation,
    trControl = trControl,
    method = methods[i],
    target_method = target_method,
    metric = metric,
    hold_out = hold_out,
    convexity = convexity,
    returns = returns
  )
  
  #final_model <- information_region[[2]][[2]][[1]]
  
  # bset cut off is selected 
  scores_cafee <- compute_scores (
    data = data,
    x = x,
    y = y,
    #z = z,
    final_model = final_model$final_model,
    orientation = orientation,
    cut_off = final_model$final_model[["cut_off"]]
  )  
  
  scores[i] <- scores_cafee
  
  # # Importance of variables
  # # varImp Caret
  # importance <- varImp(object = final_model$final_model)
  # print(importance)
  # 
  # plot <- plot(importance)
  
  if (names(methods[i]) == "svmPoly") {
    
    # necesary data to calculate importance
    train_data <- final_model$final_model[["trainingData"]]
    names(train_data)[1] <- "ClassEfficiency"
    
    dataset_dummy <- model.matrix(ClassEfficiency~ . - 1, data = train_data)
    train_data <- cbind(train_data[1], dataset_dummy)
    
    train_data <- train_data[,c(2:length(train_data),1)]
    
    # # con rminer pero no escala
    # m <- fit(
    #   ClassEfficiency~.,
    #   data = train_data,
    #   model = "ksvm",
    #   kernel = "polydot",
    #   scale = "none",
    #   kpar = list(
    #     degree = final_model$final_model$bestTune$degree,
    #     scale = final_model$final_model$bestTune$scale
    #   ),
    #   C = final_model$final_model$bestTune$C
    # )
    
    # Define methods and measures
    methods_SA <- c("GSA") # c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
    measures_SA <- c("AAD") #  c("AAD", "gradient", "variance", "range")
    
    # importance with our model
    mypred <- function(M, data) {
      
      return (predict(M, data[-length(data)], type = "prob"))
      
    }
    
    # Calculate the importance for the current method and measure
    
    importance <- Importance(
      M = final_model$final_model$finalModel,
      RealL = 5, # Levels
      data = train_data, # data
      method = methods_SA,
      measure = "AAD",
      interactions = 1:(length(train_data) - 1),
      responses = TRUE,
      PRED = mypred,
      outindex = length(train_data),
      baseline = "mean" # "mean", # mean, median, with the baseline example (should have the same attribute names as data).
    )  
    
    Inps = length(importance$inputs)
    val = vector(length = Inps)
    
    # importance Cortez
    for (a in 1:Inps) {
      
      AT <- a
      x = importance$sresponses[[1]]$x
      y = importance$sresponses[[1]]$y
      X1 = unique(x[,AT[1]])
      
      if(length(AT) > 1) { # not our case I SUPOSE
        
        # X2=unique(x[,AT[2]])
        # LX=length(X2)*length(X1)
        # 
        # if(is.matrix(y)) my = matrix(ncol=NCOL(y),nrow=LX)
        # 
        # Im=vector(length=LX)
        # k=1;
        # for(i in X1)
        #   for(j in X2)
        #   {
        #     W=which(x[,AT[1]]==i & x[,AT[2]]==j)
        #     Im[k]=W[1]
        #     if(is.matrix(y)) my[k,]=colMeans(y[W,])
        #     else my[k]=mean_resp(y[W])
        #     k=k+1
        #   }
        
      } else if (length(AT)==1) {
        
        LX = length(X1)
        
        if(is.matrix(y)) {
          
          my = matrix(
            
            ncol = NCOL(y),
            nrow = LX
          
          )
          
        } else if(is.factor(y)) {
          
          my = factor(rep(levels(y)[1],LX),levels=levels(y))
          
        } else {
          
          my = vector(length=LX)
          
        }
        
        Im = vector(length = LX); k=1;
        
        for(e in X1) {
          
          W = which(x[,AT[1]] == e)
          
          Im[k] = W[1]
          
          if(is.matrix(y)){
            
            my[k,] = colMeans(y[W,])
          
          } else {
            
            my[k]=mean_resp(y[W])
            
          } 
          
          k = k+1
          
        }
        
      }
      
      val[a] <- s_measure(my, "AAD")
      
    }
    
    ximportance = val/sum(val) # vector with the % overall importance values for each inputs, from att. 2 to 5.
    
    importance$value <- val
    names(importance$value) <- names(train_data)[-length(train_data)]
    importance$imp <- as.data.frame(t(as.matrix(ximportance)))
    names(importance$imp) <- names(train_data)[-length(train_data)]
    
  } else if (names(methods[i]) == "nnet") {
    
    # necesary data to calculate importance
    train_data <- final_model$final_model[["trainingData"]]
    names(train_data)[1] <- "ClassEfficiency"
    
    dataset_dummy <- model.matrix(ClassEfficiency~ . - 1, data = train_data)
    train_data <- cbind(train_data[1], dataset_dummy)
    
    # con rminer
    m <- fit(
      ClassEfficiency ~.,
      data = train_data,
      model = "mlp",
      scale = "none",
      size = final_model$final_model$bestTune$size,
      decay = final_model$final_model$bestTune$decay
    )
    
    # Define methods and measures
    methods_SA <- c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
    measures_SA <- c("AAD", "gradient", "variance", "range")
    
    # make grid SA
    grid_SA <- expand.grid(method = methods_SA, measure = measures_SA)
    
    # save results
    results_SA <- data.frame(method = character(), measure = character())
    
    # Loop through each combination of method and measure
    for (a in 1:nrow(grid_SA)) {
      
      method <- as.character(grid_SA$method[a])
      measure <- as.character(grid_SA$measure[a])
      
      # Calculate the importance for the current method and measure
      importance <- Importance(m, data = train_data, method = method, measure = measure)
      
      # Extract the importance values (assuming 26 values)
      imp_values <- importance$imp
      
      # Create a row with method, measure, and the 26 importance values
      result_row <- data.frame(method = method, measure = measure, t(imp_values))
      
      # Append the row to results_SA
      results_SA <- rbind(results_SA, result_row)
    }
    
    names(results_SA)[3:ncol(results_SA)] <- names(train_data)
    
  }
  
  
  # information model
  list <- list()
  
  list[[1]] <- final_model
  list[[2]] <- importance
  
  names(list) <- c("final_model", "importance")
  
  
  list_method[[i]] <- list
  
} # end bucle for (methods)  

information_region <- list()
information_region[[1]] <- scores
information_region[[2]] <- list_method


names(information_region) <- c("scores","ML_models")

# names final object
names(information_region) <- c("scores","ML_models")

names(information_region[["ML_models"]]) <- names(methods)

# prove
importance_example <- information_region[["ML_models"]][["svmPoly"]][["importance"]]
summary(train_data)


load("~/Cafee/articulo 1 XAI/Apuntes/Sensitivity analysis/sa_ssin_n2p.rda")

# save(train_data, file = "train_data.RData")
