#' @title Efficiency Estimation Using Classification Algorithms
#'
#' @description This function uses classification algorithms to estimate the efficiency of a set of DMUs (Decision Making Units).
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column indexes of input variables in \code{data}.
#' @param y Column indexes of output variables in \code{data}.
#' @param orientation A \code{string}, equal to \code{"input"} (input-oriented) or \code{"output} (output-oriented)
#' @param trControl Parameters for controlling the training process (from the \code{'caret'} package).
#' @param methods A \code{list} of selected machine learning models and their hyperparameters.
#'
#' @importFrom caret trainControl train
#' @importFrom dplyr select_if %>% arrange filter row_number
#'
#' @return A \code{"cafee"} object.
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
    y = y,
    trControl = trControl
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
  
  # Determine efficient and inefficient DMUs
  class_efficiency <- ifelse(scores[, 1] <= 0.000001, 1, - 1)
  
  # Add "efficient" class
  data <- cbind(data, class_efficiency)

  # Parameters for controlling the training process
  trControl <- trainControl (
    method = trControl[["method"]],
    number = trControl[["number"]],
    savePredictions = "all"
    )
  
  browser()

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
