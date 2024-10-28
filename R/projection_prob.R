#' @title Projections to the Hyperplane
#'
#' @description This function computes the efficiency scores based on a given model.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column indexes of input variables in \code{data}.
#' @param y Column indexes of output variables in \code{data}.
#' @param z Column indexes of environment variables in \code{data}.
#' @param final_model The best-fitted model used for efficiency score computation.
#' @param orientation The direction in which data should be projected to calculate efficiency scores.
#' @param scenarios Probability levels for determining efficient class scores.
#'
#' @return A numeric vector containing the efficiency scores for each observation in the input data.
#'
#' @export

compute_target <- function (
    data, x, y, z = NULL, final_model, orientation, scenarios
) {
    
    # environment variable 
    if (is.null(z)) {
      z <- 0
    }
    
    # matrix with optimal values based on probability of being efficient
    n_scenarios <- length(scenarios)

    data_contr <- as.data.frame(
      matrix(
        data = NA,
        ncol = n_scenarios,
        nrow = nrow(data)
      )
    )
    
    names(data_contr) <- c(scenarios) 
    
    # obtain probability vector
    max_prob <- max(scenarios)
    
    # maximum and minimum values posibles a esperar
    max_value_x <- apply(X = as.matrix(data[, x]), MARGIN = 2, FUN = max)
    min_value_x <- apply(X = as.matrix(data[, x]), MARGIN = 2, FUN = min)
      
    max_value_y <- apply(X = as.matrix(data[, y]), MARGIN = 2, FUN = max)
    min_value_y <- apply(X = as.matrix(data[, y]), MARGIN = 2, FUN = min)
    
    # loop for each observation
    for (i in 1:nrow(data)) {

      print(paste("DMU: ", i))
      print(paste("En curso:", round((i/nrow(data)) * 100), 4))
      data[i,y] <- min_value_y
      #print(data[i,])
      
      # probability of being efficient
      prob_eff <- predict(final_model, data[i, ], type = "prob")[1]
      
      incr <- 0
          
      # ====================== #
      # case dmu not efficient #
      # ====================== #
            
      # calculate increments to make not efficient class the minority
      while (prob_eff < max_prob) {

        if (any(data[i, y] + (data[i, y] * incr) > max_value_y * 2)) {
                
          break
                
        } else {
                
          # Increase by 0.1
          incr <- incr + 0.1
                
          # the dmu with the increments of 
          new_point <- cbind(data[i, x], data[i, y] * (1 + incr), data[i, z])
          colnames(new_point) <- names(data[c(x, y, z)])
                
          prob_eff <- predict(final_model, new_point, type = "prob")[1]
                
          if (round(prob_eff,2) %in% scenarios) {
                  
            name <- as.character(round(prob_eff, 2))
                  
            data_contr[i, name] <- data[i, y] * (1 + incr)
                  
          }
                
        } # end normal iteration 
              
      } # end while
          
    } # end bucle for
      
  return(data_contr)
      
}
