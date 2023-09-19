#' @title Data envelopment analysis' clasification.
#'
#' @description This function trains for each model, the different hyperparameters and returns the best model with its best hyperparameters.
#'
#' @param data Dataset \code{data}.
#' @param nX Number of inputs \code{data}.
#' @param nY Number of outputs \code{data}.
#' 
#' @importFrom deaR make_deadata model_basic efficiencies
#' @importFrom dplyr %>% mutate
#' @importFrom lpSolveAPI make.lp lp.control set.objfn
#'
#' @return It returns a \code{data.frame} with the final data.
compute_scores_additive <- function(data, nX, nY) {
    
    # Matriz de inputs 
    xmat <- as.matrix(data[, x])
  
    # Matriz de outputs
    ymat <- as.matrix(data[, y])
    
    dmu <- nrow(data)
  
    scores <- matrix(nrow = data, ncol = nX + nY + dmu) # slcak_X + slack_y + lambdas
    
    for (d in 1:dmu) {
      
      objVal <- matrix(ncol = nX + nY + dmu, nrow = 1) 
      
      x.weight <- rep(1, nX)
      y.weight <- rep(1, nY)
      objVal[1:(nX + nY)] <- c(x.weight, y.weight)
      
      lps <- make.lp(nrow = 0, ncol = nX + nY + dmu)
      lp.control(lps, sense = "max")
      set.objfn(lps, objVal)
      
      for(xi in 1:nX) {
        
        # Slacks para inputs
        x.slack <- rep(0, nX)
        x.slack[xi] <- 1
        slacks  <- c(x.slack, rep(0, nY))
        
        add.constraint(lps, xt = c(slacks, xmat[,xi]), "=", rhs = xmat[d,xi])
        
      }
      
      for(yi in 1:nY) {
        
        # Slacks para outputs
        y.slack <- rep(0, nY)
        y.slack[yi] <- -1
        slacks  <- c(rep(0, nX), y.slack)
        
        add.constraint(lps, xt = c(slacks, ymat[,yi]), "=", rhs = ymat[d, yi])
        
      }
      
      # Convexitiy returns, variable
      add.constraint(lprec = lps, xt = c(rep(0, nX + nY), rep(1, dmu)), type = "=", rhs = 1)
      
      solve(lps)
      scores[d, ] <- c(get.objective(lps), get.variables(lps))
      
    }

  # Add efficient class
  data <- data %>%
    mutate(ClassEfficiency = as.factor(ifelse(scores == 1, 1, -1)))

  return(list(data = data, scores = scores))
}
