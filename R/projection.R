#' @title Compute Efficiency Scores
#'
#' @description This function computes the efficiency scores based on a given model.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column indexes of input variables in \code{data}.
#' @param y Column indexes of output variables in \code{data}.
#' @param final_model The best-fitted model used for efficiency score computation.
#' @param orientation The direction in which data should be projected to calculate efficiency scores.
#'
#' @return A numeric vector containing the efficiency scores for each observation in the input data.

compute_scores <- function (
    data, x, y, final_model, orientation
    ) {
  
  browser()

  # vector of optimal scores
  scores <- matrix(NA, nrow = nrow(data), ncol = 1)
  
  # maximum and minimum values posibles a esperar
  max_value_x <- apply(X = as.matrix(data[, x]), MARGIN = 2, FUN = max)
  min_value_y <- apply(X = as.matrix(data[, y]), MARGIN = 2, FUN = min)

  for (i in 1:nrow(data)) {
    
    # probability of being efficient
    prob_eff <- predict(final_model, data[i, ], type = "prob")[1]
    
    incr <- 0

    if (orientation == "input") {
      
      if (prob_eff > 0.5) {
        
        # ======================== #
        # case dmu super-efficient #
        # ======================== #
        
        if (data[i, x] * 1.01 > max_value_x) { # si te pasas de los valores maximos observados
          
          incr <- 0.1
          
          while (prob_eff > 0.5) {
            
            # decrease by 0.01
            incr <- incr - 0.01
            
            # the dmu with the increments
            new_point <- cbind(data[i, x] * (1 + incr), data[i, y])
            colnames(new_point) <- names(data[c(x, y)])
            
            prob_eff <- predict(final_model, new_point, type = "prob")[1]
            
            # CASO QUE NO CRUZA EL HIPERPLANO
            if (incr == 0) {
              
              scores[i] <- NA
              
              break
              
            }
            
          }
          
        } else {
            
          # calculate increments to make the efficient class the minority
          while (prob_eff > 0.5) {
              
            # Increase by 0.1
            incr <- incr + 0.1
              
            # the dmu with the increments
            new_point <- cbind(data[i, x] * (1 + incr), data[i, y])
            colnames(new_point) <- names(data[c(x, y)])
              
            prob_eff <- predict(final_model, new_point, type = "prob")[1]
          
        } else {
          
          # Once the threshold is crossed, make the majority class efficient again
          while (prob_eff < 0.5) {
            
            # Increase by 0.1
            incr <- incr - 0.01
            
            # the dmu with the increments
            new_point <- cbind(data[i, x] * (1 + incr), data[i, y])
            colnames(new_point) <- names(data[c(x, y)])
            
            prob_eff <- predict(final_model, new_point, type = "prob")[1]
            
          }
          
          scores[i] <- 1 + (incr - 0.005) 
          
        }
        
      } else {
        
        # ====================== #
        # case dmu not efficient #
        # ====================== #
        
        # calculate increments to make not efficient class the minority
        while (prob_eff < 0.5) {
          
          # increase by 0.1
          incr <- incr + 0.1
          
          # case inputs reduction not available
          if (incr > 1) {
            
            scores[i] <- NA
            
          }
          
          # the dmu with the increments of 
          new_point <- cbind(data[i, x] * (1 - incr), data[i, y])
          colnames(new_point) <- names(data[c(x, y)])
          
          prob_eff <- predict(final_model, new_point, type = "prob")[1]
          
        }
        
        # Once the threshold is crossed, make the majority class non-efficient again
        while (prob_eff > 0.5) {
          
          # Increase by 0.1
          incr <- incr - 0.01
          
          # the dmu with the increments
          new_point <- cbind(data[i, x] * (1 - incr), data[i, y])
          colnames(new_point) <- names(data[c(x, y)])
          
          prob_eff <- predict(final_model, new_point, type = "prob")[1]
          
        }
        
        scores[i] <- 1 - (incr + 0.005)
        
      }

    } else { # OUTPUT ORIENTATION
      
            
      if (prob_eff > 0.5) {
        
        # ======================== #
        # case dmu super-efficient #
        # ======================== #
        
        # calculate increments to make the efficient class the minority
        while (prob_eff > 0.5) {
          
          # Increase by 0.1
          incr <- incr + 0.1
          
          # the dmu with the increments
          new_point <- cbind(data[i, x], data[i, y] * (1 - incr))
          colnames(new_point) <- names(data[c(x, y)])
          
          prob_eff <- predict(final_model, new_point, type = "prob")[1]
          
          print(prob_eff)
          
        }
        
        # Once the threshold is crossed, make the majority class efficient again
        while (prob_eff < 0.5) {
          
          # Increase by 0.1
          incr <- incr - 0.01
          
          # the dmu with the increments
          new_point <- cbind(data[i, x] * (1 + incr), data[i, y])
          colnames(new_point) <- names(data[c(x, y)])
          
          prob_eff <- predict(final_model, new_point, type = "prob")[1]
          
        }
        
        scores[i] <- 1 + (incr - 0.005) 
        
      } else {
        
        # ====================== #
        # case dmu not efficient #
        # ====================== #
        
        # calculate increments to make not efficient class the minority
        while (prob_eff < 0.5) {
          
          # Increase by 0.1
          incr <- incr + 0.1
          
          # the dmu with the increments of 
          new_point <- cbind(data[i, x] * (1 - incr), data[i, y])
          colnames(new_point) <- names(data[c(x, y)])
          
          prob_eff <- predict(final_model, new_point, type = "prob")[1]
          
        }
        
        # Once the threshold is crossed, make the majority class non-efficient again
        while (prob_eff > 0.5) {
          
          # Increase by 0.1
          incr <- incr - 0.01
          
          # the dmu with the increments of 
          new_point <- cbind(data[i, x] * (1 - incr), data[i, y])
          colnames(new_point) <- names(data[c(x, y)])
          
          prob_eff <- predict(final_model, new_point, type = "prob")[1]
          
        }
        
        scores[i] <- 1 - (incr + 0.005)
        
      }

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
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

      if (predict[1] > 0) { # efficient by final_model

        result.int <- rbind(result.int, aux.y)

        scores.int[i] <- 1

      } else { # inefficient

        j <- 2

        while (max(predict) < 0) {

          incr[j] <- incr[1] + (incr[1] * 0.1) * (j - 1)
          aux.y[j,] <- data[i, y] * incr[j]
          grid <- data.frame(data[i, x], y = aux.y[j,])
          predict[j] <- predict(final_model$finalModel, grid, type = "decision")[, 1]

          print(paste("Iteración while 1 ", j))

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

          print(paste("Iteracion while 2 ", j))

        }

        aux.sol <- as.data.frame((aux.y[j,] + aux.y[j - 1,]) / 2)
        colnames(aux.sol) <- c(col)[y]

        result.int <- rbind(result.int, aux.sol)

        scores.int[i] <- aux.sol[1,1]/aux.y[1,1]

        # The end PUNTO INICIAL prediction is negetive
      }

      # The end if INPUT orientation

    }

      print(paste("iteration ", i))

  } # FIN bucle for

  return(list(solution_point = result.int, solution_score = scores.int))

}
