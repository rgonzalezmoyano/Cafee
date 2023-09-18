#' @title Additive Scenarios for 1 Output
#'
#' @description This function is used to simulate the data in different additive scenarios as in \insertCite{kuosmanen2010;textual}{mafs}.
#'
#' @param N Sample size.
#' @param scenario \code{"A"}, \code{"B"}, \code{"C"}, \code{"D"}, \code{"E"} or \code{"F"}. For details, check Table 2.
#'
#' @importFrom dplyr %>% filter
#' @importFrom stats runif rnorm
#' @importFrom Rdpack reprompt
#'
#' @return \code{data.frame} with the simulated data.
#'
#' \insertRef{kuosmanen2010}{mafs}
#'
#' @export
AddScenario <- function(N, scenario) {
  if(!(scenario %in% c("A", "B", "C", "D", "E", "F"))){
    stop(paste(scenario, "is not allowed"))
  }

  if (scenario %in% c("A", "B")) {
    nX <- 1

  } else if (scenario %in% c("C", "E")) {
    nX <- 2

  } else {
    nX <- 3
  }

  colnames <- c(paste("x", 1:nX, sep = ""), "y")

  data <- matrix(
    ncol = length(colnames),
    nrow = N,
    dimnames = list(NULL, colnames)
  ) %>% as.data.frame()

  for (x in 1:nX){
    data[, x] <- runif(n = N, min = 1, max = 10)
  }

  u <- abs(rnorm(n = N, mean = 0, sd = 0.4))

  if (scenario == "A"){
    x1 <- data[, "x1"]
    y  <- log(x1) + 3
    data[, "y"]  <- y - u
    data[, "yD"] <- y

  } else if (scenario == "B"){
    x1 <- data[, "x1"]
    y  <- 3 + sqrt(x1) + log(x1)
    data[, "y"]  <- y - u
    data[, "yD"] <- y

  } else if (scenario == "C"){
    x1 <- data[, "x1"]
    x2 <- data[, "x2"]
    y  <- 0.1 * x1 + 0.1 * x2 + 0.3 * sqrt(x1 * x2)
    data[, "y"]  <- y - u
    data[, "yD"] <- y

  } else if (scenario == "D"){
    x1 <- data[, "x1"]
    x2 <- data[, "x2"]
    x3 <- data[, "x3"]
    y  <- 0.1 * x1 + 0.1 * x2 + 0.1 * x3 + 0.3 * (x1 * x2 * x3) ^ (1 / 3)
    data["y"]  <- y - u
    data["yD"] <- y

  } else if (scenario == "E"){
    x1 <- data[, "x1"]
    x2 <- data[, "x2"]
    y  <- 0.1 * x1 + 0.1 * x2 + 0.3 * (x1 * x2) ^ (1 / 3)
    data["y"]  <- y - u
    data["yD"] <- y

  } else {
    x1 <- data[, "x1"]
    x2 <- data[, "x2"]
    x3 <- data[, "x3"]
    y  <- 0.1 * x1 + 0.1 * x2 + 0.1 * x3 + 0.3 * (x1 * x2 * x3) ^ (1 / 4)
    data["y"]  <- y - u
    data["yD"] <- y
  }

  data <- data %>% filter(y >= 0)

  return(data)
}

