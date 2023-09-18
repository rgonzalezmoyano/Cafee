#' @title de eficiencia de las DMUs
#'
#' @description Esta función devuelve los scores de eficiencia.
#'
#' @param data \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param DMUs Column index of DMUs (optional). If there is not any DMU column, then it must be NULL.\code{data}.
#' @param x Column input indexes in \code{data}.
#' @param y Column output indexes in \code{data}.
#' @param trControl Parameters of the train \code{data}.
#' @param methods A list of the chosen ML models' and their hyper-parameters\code{data}.
#'
#' @importFrom caret trainControl train
#' @importFrom dplyr select select_if %>% arrange filter row_number
#'
#' @return Train model
#'
#' @export
efficiency_estimation <- function(data, DMUs, x, y, trControl, methods, orientation) {

  # Available methods and parameters
  list_SVM <- c("svmLinear", "svmRadial", "svmPoly")
  list_SVM.params <- c("C", "sigma", "scale", "degree")

  # list_KNN <- c()
  # list_KNNparams <- c()

  # All available methods and parameters
  AllMethods <- c(list_SVM) # for more methods EXAMPLE: c(list_SVM, list_KNN)
  Allparams <- c(list_SVM.params) # EXAMPLE c(list_SVMparams, list_KNNparams)

  # User models'
  method <- NULL
  for(i in 1:length(names(methods))) {
    method[i] <- names(methods)[i]
  }

  # User params'
  #params <- NULL
  #for(i in 1:length(methods))
  # FALTA ES PARA LA COMPROBACION


  # Comparation all user inputs are OK
  #comprobation <- Comprobation(method = names(methods), params = params, AllMethods = AllMethods, Allparams = Allparams)

  if (!is.null(DMUs)) {

    nameDMUs <- data[, DMUs]

  } else {

    nameDMUs <- row.names(data)

  }

  # Preprocess
  data <- preProcess(data = data, DMUs = DMUs, x = x, y = y, na.rm = na.rm)

  # DMUs
  if (!is.null(DMUs)) {

    for (i in 1:length(x)) {
      if (x[i] > DMUs) {
        x[i] <- x[i] - 1
      }
    }

    for (i in 1:length(y)) {
      if (y[i] > DMUs) {
        y[i] <- y[i] - 1
      }
    }

    x <<- as.integer(x)
    y <<- as.integer(y)

    # Delete DMUs
    #data <- data[, -DMUs]

  } else {

    row.names(data) <- row.names(data) # if NA overwrite DMUs, else create new colum; char

  }

  print("Preprocesado ejecutado correctamente")

  row.names(data) <- nameDMUs

  # Number of inputs and outputs
  nX <- length(x)
  nY <- length(y)

  # DMUs' estimate efficiency
  DEA <- ComputeScores(data = data, nX = nX, nY = nY, orientation = orientation)

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
