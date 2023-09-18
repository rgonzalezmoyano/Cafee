#' @title Fill
#'
#' @description Fill
#'
#' @param data \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column input indexes in \code{data}.
#' @param y Column output indexes in \code{data}.
#' @param orientation ...
#' @param trControl Parameters of the train \code{data}.
#' @param methods A list of the chosen ML models' and their hyper-parameters\code{data}.
#'
#' @importFrom caret trainControl train
#' @importFrom dplyr select select_if %>% arrange filter row_number
#'
#' @return Fill
#'
#' @export
efficiency_estimation <- function (
    data, x, y, orientation,
    trControl, methods
    ) {
  
  # pre-processing
  data <- preprocessing (
    data = data, 
    x = x, 
    y = y
    )

  # Reorder index 'x' and 'y' in data
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)
  
  # Number of inputs / outputs as inputs and number of outputs
  nX <- length(x)
  nY <- length(y)

  # compute DEA scores through an additive model
  DEA <- compute_scores_additive (
    data = data, nX = nX, nY = nY
    )

  print("Etiquetas DEA añadidas")

  data <- DEA$data

  # ML validation
  trControl <- trainControl(method = trControl[["method"]],
                            number = trControl[["number"]],
                            savePredictions = "all")

  # https://www.rdocumentation.org/packages/caret/versions/6.0-92/topics/trainControl

  # SVM methods to train
  method_svm <- list()

  # KNN methods to train (same structure than SVM)
  # method_KNN <- NULL

  # which ML models have to be trained
  for(a in method) {
    if(a %in% list_SVM) {
      method_svm[a] <- list(a = methods[[a]]) # svm methods
      }

    # if(a %in% list_KNN) {
      # method_kNN[a] <- list(a = methods[[a]]) <- a # KNN methods
    # }
  }

  print("Eligiendo mejores hiperparámtetros SVM")

  # Function SVM
  train_svm <- DEA.svm(
    data = data,
    method_svm = method_svm,
    trControl = trControl
    )

  print("Hiperparámetros SVM elegidos")

  # Function KNN
  #train_KNN <- knn(data = data, method_svm = method_svm, trControl = trControl)

  # the chosen one model
  precision_models <- data.frame(
    "Model.name" = rep(NA, length(names(train_svm))),
    "Model.Kappa" = rep(NA, length(names(train_svm))),
    "Model.Accuracy" = rep(NA, length(names(train_svm)))
    )

  precision_models$Model.name <- names(train_svm)
  precision_models$Model.Kappa <- sapply(train_svm, "[[", "Kappa")
  precision_models$Model.Accuracy <- sapply(train_svm, "[[", "Accuracy")

  # select the best
  selected_SVM_model <- precision_models %>%
    arrange(desc(Model.Kappa), desc(Model.Accuracy)) %>%
    filter(row_number() == 1L)

  # Params FINAL MODEL: C, sigma, degree...
  params <- NULL

  # Choose the params
  for(i in names(train_svm[[as.character(selected_SVM_model[1])]])){ # 1 to choose NameModel

    if(i %in% list_SVM.params){

      pos <- which(names(train_svm[[as.character(selected_SVM_model[1])]]) == i)
      params[pos] <- i
    }

  }

  # select the VALUES of the params
  params_final <- train_svm[[as.character(selected_SVM_model[1])]] %>%
    select_if(names(train_svm[[as.character(selected_SVM_model[1])]]) %in% params)

  tuneGrid_final <- expand.grid(params_final)

  # No train; FINAL MODEL FIT
  final_model <- train(form = ClassEfficiency ~.,
                       data = data,
                       method = as.character(selected_SVM_model[1]),
                       trControl = trainControl(method = "none"),
                       tuneGrid = params_final,
                       metric = "Kappa")

  print("Calculando SVM scores")
  # Optimization problem
  solution <- optimization(data = data, x = x, y = y, final_model = final_model, orientation = orientation)

  print("SVM scores calculados")

  # FALTA PONER EL NOMBRE DE LAS DMUs ORIGINALES names_DMUs, borrar el numerico
  #data <- data[-1]
  #data <- cbind(DMUs, data)

  #row.names(solution$solution_point) <- data[, 1]
  #solution$solution_point <- cbind(data[, 1], solution$solution_point)

  resume <- data.frame()

  #resume <- cbind(resume, data[, min(1 + y):max(1 + y)], DEA.score = DEA$scores, SVM.Classifier.score = solution$solution_score)

  if (orientation == "output") {

    resume <- cbind(data[, c(min(x):max(x), min(y):max(y))], DEA.score = DEA$scores, SVM.Classifier.score = solution$solution_score)

    resume <- as.data.frame(resume)

    names <- colnames(data)[c(x, y)]

    colnames(resume)[c(x, y)] <- names

    rownames(resume) <- nameDMUs

    long <- max(c(max(resume$DEA.score), max(resume$SVM.Classifier.score)))

    graph <- ggplot(resume) +
      geom_point(aes(x = SVM.Classifier.score, y = DEA.score)) +
      scale_y_continuous(limits = c(1, long),
                         breaks = seq(1, long, by = 0.1)) +
      scale_x_continuous(limits = c(1, long),
                         breaks = seq(1, long, by = 0.1)) +
      geom_abline(intercept = 0, slope = 1)

  } else { # orientation == "input"

    resume <- cbind(data[, c(min(x):max(x), min(y):max(y))], DEA.score = DEA$scores, SVM.Classifier.score = solution$solution_score)

    name <- colnames(data)[x]

    colnames(resume)[x] <- name

    rownames(resume) <- nameDMUs

    long <- min(c(min(resume$SVM.Classifier.score), min(resume$DEA.score)))

    graph <- ggplot(resume) +
      geom_point(aes(x = SVM.Classifier.score, y = DEA.score)) +
      scale_y_continuous(limits = c(long, 1),
                         breaks = seq(0, 1, by = 0.1)) +
      scale_x_continuous(limits = c(long, 1),
                         breaks = seq(0, 1, by = 0.1)) +
      geom_abline(intercept = 0, slope = 1)

  }

  correlation <- cor(resume$SVM.Classifier.score, resume$DEA.score , use = "everything",
                     method = "pearson")

  return(list(data = data, train_models = train_svm, best_model = as.character(selected_SVM_model[1]), best_model_fit = final_model, solution_point = solution[["solution_point"]], score = solution[["solution_score"]], resume = resume, plot = graph, correlation_pearson = correlation))

}
