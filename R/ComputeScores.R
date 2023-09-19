#' @title Data envelopment analysis' clasification.
#'
#' @description This function trains for each model, the different hyperparameters and returns the best model with its best hyperparameters.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column indexes of input variables in \code{data}.
#' @param y Column indexes of output variables in \code{data}.
#' @param nX Number of inputs \code{data}.
#' @param nY Number of outputs \code{data}.
#' 
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint
#'
#' @return Fill
compute_scores_additive <- function(data, x, y, nX, nY) {
    
    # matrix of inputs 
    xmat <- as.matrix(data[, x])
  
    # matrix of outputs
    ymat <- as.matrix(data[, y])
    
    # number of dmus
    dmu <- nrow(data)
  
    # initialize vectors of scores
    scores <- matrix(nrow = dmu, ncol = 1) 
    
    for (d in 1:dmu) {
      
      # vector for variables: slack_X + slack_y + lambdas
      objVal <- matrix(ncol = nX + nY + dmu, nrow = 1) 
      
      x.weight <- rep(1, nX)
      y.weight <- rep(1, nY)
      objVal[1:(nX + nY)] <- c(x.weight, y.weight)
      
      lps <- make.lp(nrow = 0, ncol = nX + nY + dmu)
      lp.control(lps, sense = "max")
      set.objfn(lps, objVal)
      
      for(xi in 1:nX) {
        
        # slacks for inputs
        x_slack <- rep(0, nX)
        x_slack[xi] <- 1
        slacks  <- c(x_slack, rep(0, nY))
        
        add.constraint(lps, xt = c(slacks, xmat[, xi]), "=", rhs = xmat[d, xi])
        
      }
      
      for(yi in 1:nY) {
        
        # Slacks para outputs
        y_slack <- rep(0, nY)
        y_slack[yi] <- -1
        slacks  <- c(rep(0, nX), y_slack)
        
        add.constraint(lps, xt = c(slacks, ymat[,yi]), "=", rhs = ymat[d, yi])
        
      }
      
      # Convexitiy and variable returns to scale
      add.constraint(lprec = lps, xt = c(rep(0, nX + nY), rep(1, dmu)), type = "=", rhs = 1)
      
      solve(lps)
      scores[d] <- get.objective(lps)
      
    }

  return(scores)
}
