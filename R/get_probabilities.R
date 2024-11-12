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
#' @param cut_off Probability levels for determining efficient class scores.
#' @param imp_vector A \code{data.frame} with importance variables results 
#'
#' @return A numeric vector containing the efficiency scores for each observation in the input data.
#'
#' @export

compute_target <- function (
    data, x, y, z = NULL, final_model, orientation, cut_off, imp_vector
) {

    # environment variable 
    if (is.null(z)) {
      z <- 0
    }
    
    # save data points from scenario
    data_scenario <- as.data.frame(
      matrix(
        data = NA,
        ncol = length(c(x,y)),
        nrow = nrow(data)
      )
    )
    
    names(data_scenario) <- names(data)
    
    # JA aproach
    mean_x <- apply(as.matrix(data[,x]), 2, mean)
    mean_y <- apply(as.matrix(data[,y]), 2, mean)
    
    score_imp_x <- as.numeric(imp_vector[x])
    score_imp_y <- as.numeric(imp_vector[y])
    
    betas <- as.data.frame(matrix(
      data = NA,
      ncol = 1,
      nrow = nrow(data)
    ))

    # loop for each observation
    for (i in 1:nrow(data)) {
  
      print(paste("DMU: ", i))
      print(paste("En curso:", (round(i/nrow(data), 4) * 100)))
      #if(i == 4){browser()}
      # Inicializar el rango inicial de 'y'
      range_beta <- as.matrix(seq(from = -10, to = 20, length.out = 30))
      
      # Crear la matriz para aplicar predict()
      matrix_eff <- as.data.frame(matrix(
        data = NA,
        ncol = length(c(x, y)),
        nrow = length(range_beta)
      ))
      
      # Nombrar las columnas como en data original
      names(matrix_eff) <- names(data)
    
      change_x <- matrix(
        data = rep((-score_imp_x) * mean_x, each =  nrow(matrix_eff)),
        nrow = nrow(matrix_eff),
        ncol = length(mean_x)
      )
      
      change_y <- matrix(
        data = rep((score_imp_y) * mean_y, each = nrow(matrix_eff)),
        nrow = nrow(matrix_eff),
        ncol = length(mean_y)
      )
      
      found_cut_off <- FALSE
      iter_count <- 0
      
      while (!found_cut_off) {
        
        iter_count <- iter_count + 1
        print(iter_count)
        
        # Asignar valores para 'x' y 'y'
        matrix_eff[, x] <- data[i,x] 
        
        matrix_eff[, x] <- sweep(change_x, 1, range_beta, "*") + matrix_eff[, x]
        
        matrix_eff[, y] <- data[i, y] 
        
        matrix_eff[, y] <- sweep(change_y, 1, range_beta, "*") + matrix_eff[, y]
        
        # Calcular probabilidad de eficiencia para cada fila
        eff_vector <- apply(matrix_eff, 1, function(row) {
          
          row_df <- as.data.frame(t(row))
          colnames(row_df) <- names(data)
          
          pred <- unlist(predict(final_model, row_df, type = "prob")[1])
          
          return(pred)
        })
        
        if (length(eff_vector) == 0 | is.null(eff_vector)) {
          
          data_scenario[i, x] <- rep(NA, ncol(matrix_eff[,x]))
          data_scenario[i, y] <- rep(NA, ncol(matrix_eff[,y]))
          
          betas[i,] <- NA
          break
          
        } else {
          
          # Verificar si alguna predicción coincide con el cut_off
          if (any(round(eff_vector, 10) == round(cut_off, 10))) {
            idx <- which(round(eff_vector, 7) == round(cut_off, 7))[1]  # Tomamos la primera coincidencia
            found_cut_off <- TRUE
            
            # Guardar los valores de 'x' y 'y' que coinciden con el cut_off
            data_scenario[i, x] <- matrix_eff[idx, x]
            data_scenario[i, y] <- matrix_eff[idx, y]
            
            betas[i,] <- range_beta[idx]
            
            # print("end while")
            break
            
          } else {
            
            # Encontrar el intervalo donde se encuentra el cut_off
            pos <- which(eff_vector < cut_off & c(eff_vector[-1], Inf) > cut_off) # [1] 
            
            if (length(pos) == 2) {
              pos <- pos[1]
            } else if (length(pos) == 3) {
              pos <- pos[2]
            } else if (length(pos) > 3){
              pos <- pos[as.integer(length(pos)/2)]
            }
            
            if (is.na(pos)) {
              pos <- length(eff_vector)
            }
            
            if (pos == length(range_beta)) {
              
              data_scenario[i, x] <- rep(NA, ncol(matrix_eff[,x]))
              data_scenario[i, y] <- rep(NA, ncol(matrix_eff[,y]))
              
              betas[i,] <- 1
              break
            }
           
            
            # Refinar el rango de 'y' entre las posiciones pos y pos + 1
            range_beta <- seq(from = range_beta[pos], to = range_beta[pos + 1], length.out = length(range_beta))
            
          }
          
        }
        
        #browser()
        
        if (range_beta[30] - range_beta[1] < 0.0000000001) {
          
          data_scenario[i, x] <- matrix_eff[15, x]
          data_scenario[i, y] <- matrix_eff[15, y]
          
          betas[i,] <- range_beta[15]
          
          print("end while by dif")
          found_cut_off <- TRUE
          
        }
        
        if (iter_count == 20) {
          
          data_scenario[i, x] <- matrix_eff[15, x]
          data_scenario[i, y] <- matrix_eff[15, y]
          
          betas <- range_beta[15]
          
          print("end while by iter")
          found_cut_off <- TRUE
          
        }
        
      } # end while
      
    } # end for
    
    if(any(is.na(betas))) {
      browser()
    }
    names(betas) <- "beta"
    
    # data_scenario <- cbind(data_scenario, betas)
    
  return(list(data_scenario = data_scenario, betas = betas)) 
      
}
