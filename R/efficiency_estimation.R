#' @title Efficiency Estimation Through Classification Algorithms
#'
#' @description This function estimates the efficiency of a set of DMUs through a classification algorithm.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column indexes of input variables in \code{data}.
#' @param y Column indexes of output variables in \code{data}.
#' @param orientation ...
#' @param trControl Parameters of the train \code{data}.
#' @param methods A list of the chosen ML models' and their hyper-parameters\code{data}.
#'
#' @importFrom caret trainControl train
#' @importFrom dplyr select select_if %>% arrange filter row_number mutate
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
  scores <- compute_scores_additive (
    data = data, x = x, y = y, nX = nX, nY = nY
    )
  
  class_efficiency <- ifelse(scores[, 1] <= 0.000001, 1, - 1)
  
  # Add efficient class
  data <- cbind(data, class_efficiency)
  
  print("Etiquetas DEA añadidas")

  # ML validation
  trControl <- trainControl(method = trControl[["method"]],
                            number = trControl[["number"]],
                            savePredictions = "all")

  # https://www.rdocumentation.org/packages/caret/versions/6.0-92/topics/trainControl

  print("Eligiendo mejores hiperparámtetros SVM")

  # Function train model
  train_ml <- dea_classification(
    data = data,
    methods = methods,
    trControl = trControl
    )

  print("Hiperparámetros SVM elegidos")

  # the chosen one model
  precision_models <- data.frame(
    "Model.name" = rep(NA, length(names(train_ml))),
    "Model.Kappa" = rep(NA, length(names(train_ml))),
    "Model.Accuracy" = rep(NA, length(names(train_ml)))
    )

  precision_models$Model.name <- names(train_ml)
  precision_models$Model.Kappa <- sapply(train_ml, "[[", "Kappa")
  precision_models$Model.Accuracy <- sapply(train_ml, "[[", "Accuracy")

  # select the best
  selected_model <- precision_models %>%
    arrange(desc(Model.Kappa), desc(Model.Accuracy)) %>%
    filter(row_number() == 1L)

  # Params FINAL MODEL: C, sigma, degree...
  params <- NULL
  
  # Choose the params
  for(i in names(train_ml[[as.character(selected_model[1])]])) { # 1 to choose NameModel

    if(i %in% names(methods[[selected_model[[1]]]])) {

      pos <- which(names(train_ml[[as.character(selected_model[1])]]) == i)
      params[pos] <- i
      
    }

  }

  # select the VALUES of the params
  params_final <- train_ml[[as.character(selected_model[1])]] %>%
    select_if(names(train_ml[[as.character(selected_model[1])]]) %in% params)

  tuneGrid_final <- expand.grid(params_final)

  # No train; FINAL MODEL FIT
  final_model <- train(form = as.factor(class_efficiency) ~.,
                       data = data,
                       method = as.character(selected_model[1]),
                       trControl = trainControl(method = "none"),
                       tuneGrid = params_final,
                       metric = "Kappa")

  print("Calculando SVM scores")
  # Optimization problem
  solution <- optimization(data = data, x = x, y = y, final_model = final_model, orientation = orientation)

  print("SVM scores calculados")

  resume <- data.frame()

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