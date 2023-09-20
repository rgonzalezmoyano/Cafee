#' @title Optimization problem.
#'
#' @description This function solve the optimization problem and return the solution for each DMU.
#'
#' @param data Dataset \code{data}.
#' @param x Input value \code{data}.
#' @param y Output value \code{data}.
#' @param final_model Best trained model \code{data}.
#' @param orientation Direction of the optimization. \code{data}.
#'
#' @importFrom dplyr %>% select_if between
#' @importFrom kernlab predict
#'
#' @return It returns a \code{list} with the chosen model.
optimization <- function(data, x, y, final_model, orientation) {

  # Grid Intelgente FINAL

  # Save optimal points
  result.int <- data.frame()

  col <- colnames(data)

  # Save scores
  # result.int <- matrix(data = NA, nrow = nrow(data) , ncol = length(x))
  scores.int <- rep(NA, nrow(data))

  #matrix(nrow = dmu, ncol = 1)

  for (i in 1:nrow(data)) {

    if (orientation == "input") {

      aux.x <- data.frame() # points
      iter <- c() # iteration
      incr_0 <- order(c(seq(0, 0.9, length.out = 10)), decreasing = TRUE)
      incr_0.0 <- c((incr_0/10) - 0.1)
      incr <- c()
      predict <- c()

      # PUNTO INICIAL
      incr[1] <- 1
      aux.x <- rbind(aux.x, data[i, x])
      colnames(aux.x) <- c(col)[x]
      grid.1 <- data.frame(aux.x, y = data[i, y])
      predict[1] <- predict(final_model$finalModel, grid.1, type = "decision")[, 1]

      if (predict[1] > 0) { # efficient by SVM prediction

        result.int <- rbind(result.int, aux.x)

        scores.int[i] <- 1

      } else { # inefficient

        j <- 2

        while (max(predict) < 0) {

          incr[j] <- incr_0.0[j - 1]
          aux.x[j,] <- data[i, x] * incr[j]
          grid <- data.frame(aux.x[j,], y = data[i, y])
          predict[j] <- predict(final_model$finalModel, grid, type = "decision")[, 1]

          # print(paste("Iteración while", j))

          j.ref <- incr[j]
          j <- j + 1

        }

        iter <- 1
        incr[j] <- j.ref + ((1 - incr_0.0[iter])/10)
        aux.x[j,] <- data[i, x] * incr[j]
        grid <- data.frame(x = aux.x[j,], y = data[i, y])
        predict[j] <- predict(final_model$finalModel, grid, type = "decision")[, 1]

        while (predict[j] > 0) {

          j <- j + 1
          iter <- iter + 1
          incr[j] <- j.ref + ((1 - incr_0.0[iter])/10)
          aux.x[j,] <- data[i, x] * incr[j]
          grid <- data.frame(x = aux.x[j,], y = data[i, y])
          predict[j] <- predict(final_model$finalModel, grid, type = "decision")[, 1]

          #print(paste("Iteracion while ", j))

        }

          aux.sol <- as.data.frame((aux.x[j,] + aux.x[j - 1,]) / 2)
          colnames(aux.sol) <- c(col)[x]

          result.int <- rbind(result.int, aux.sol)

          scores.int[i] <- aux.sol[1,1]/aux.x[1,1]

      # The end PUNTO INICIAL prediction is negetive
      }

    # The end if INPUT orientation

    } else { # OUTPUT ORIENTATION

      aux.y <- data.frame() # points
      iter <- c() # iteration
      incr_0 <- order(c(seq(0, 0.9, length.out = 10)), decreasing = TRUE)
      incr_0.0 <- c((incr_0/10) - 0.1)
      incr <- c()
      predict <- c()

      # PUNTO INICIAL
      incr[1] <- 1
      aux.y <- rbind(aux.y, data[i, y])
      colnames(aux.y) <- c(col)[y]
      grid.1 <- data.frame(data[i, x], y = aux.y)
      predict[1] <- predict(final_model$finalModel, grid.1, type = "decision")[, 1]

      if (predict[1] > 0) { # efficient by SVM prediction

        result.int <- rbind(result.int, aux.y)

        scores.int[i] <- 1

      } else { # inefficient

        j <- 2

        while (max(predict) < 0) {

          incr[j] <- incr[1] + (incr[1] * 0.1) * (j - 1)
          aux.y[j,] <- data[i, y] * incr[j]
          grid <- data.frame(data[i, x], y = aux.y[j,])
          predict[j] <- predict(final_model$finalModel, grid, type = "decision")[, 1]

          # print(paste("Iteración while", j))

          j.ref <- incr[j]
          j <- j + 1

        }

        iter <- 1
        incr[j] <- j.ref - ((1 - incr_0.0[iter])/10)
        aux.y[j,] <- data[i, y] * incr[j]
        grid <- data.frame(x = data[i, x], y = aux.y[j,])
        predict[j] <- predict(final_model$finalModel, grid, type = "decision")[, 1]

        while (predict[j] > 0) {

          j <- j + 1
          iter <- iter + 1
          incr[j] <- j.ref - ((1 - incr_0.0[iter])/10)
          aux.y[j,] <- data[i, y] * incr[j]
          grid <- data.frame(x = data[i, x], y = aux.y[j,])
          predict[j] <- predict(final_model$finalModel, grid, type = "decision")[, 1]

          #print(paste("Iteracion while ", j))

        }

        aux.sol <- as.data.frame((aux.y[j,] + aux.y[j - 1,]) / 2)
        colnames(aux.sol) <- c(col)[y]

        result.int <- rbind(result.int, aux.sol)

        scores.int[i] <- aux.sol[1,1]/aux.y[1,1]

        # The end PUNTO INICIAL prediction is negetive
      }

      # The end if INPUT orientation

    }

      #print(paste("iteration ", i))

  } # FIN bucle for

  return(list(solution_point = result.int, solution_score = scores.int))

}
