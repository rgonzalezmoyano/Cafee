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
#'
#' @return A numeric vector containing the efficiency scores for each observation in the input data.
#'
#' @export

compute_target <- function (
    data, x, y, z = NULL, final_model, orientation, cut_off
) {
    
    # environment variable 
    if (is.null(z)) {
      z <- 0
    }
    
    # maximum and minimum values posibles a esperar
    max_value_x <- apply(X = as.matrix(data[, x]), MARGIN = 2, FUN = max)
    min_value_x <- apply(X = as.matrix(data[, x]), MARGIN = 2, FUN = min)
      
    max_value_y <- apply(X = as.matrix(data[, y]), MARGIN = 2, FUN = max)
    min_value_y <- apply(X = as.matrix(data[, y]), MARGIN = 2, FUN = min)
    
    # save data points from scenario
    data_scenario <- as.data.frame(
      matrix(
        data = NA,
        ncol = length(c(x,y)),
        nrow = nrow(data)
      )
    )
    
    names(data_scenario) <- names(data)
    
    # loop for each observation
    for (i in 1:nrow(data)) {
      
      print(paste("DMU: ", i))
      print(paste("En curso:", (round(i/nrow(data), 4) * 100)))
      
      # Inicializar prob_eff
      prob_eff <- predict(final_model, data[i, ], type = "prob")[1]
      
      # Inicializar el rango inicial de 'y'
      range_y <- seq(from = min_value_y, to = max_value_y, length.out = 10)
      found_cut_off <- FALSE
      
      while (!found_cut_off) {
        
        # Crear la matriz para aplicar predict()
        matrix_eff <- as.data.frame(matrix(
          data = NA,
          ncol = length(c(x, y)),
          nrow = 10
        ))
        
        # Nombrar las columnas como en data original
        names(matrix_eff) <- names(data)
        
        # Asignar valores para 'x' y 'y' 
        matrix_eff[, x] <- data[i, x]
        matrix_eff[, y] <- range_y
        
        # Calcular probabilidad de eficiencia para cada fila
        eff_vector <- apply(matrix_eff, 1, function(row) {
          row_df <- as.data.frame(t(row))
          colnames(row_df) <- names(data)
          predict(final_model, row_df, type = "prob")[1]
        })
        
        eff_vector <- unlist(eff_vector)
        
        # Verificar si alguna predicción coincide con el cut_off
        if (any(round(eff_vector, 3) == cut_off)) {
          idx <- which(round(eff_vector, 3) == cut_off)[1]  # Tomamos la primera coincidencia
          found_cut_off <- TRUE
          
          # Guardar los valores de 'x' y 'y' que coinciden con el cut_off
          matching_x <- matrix_eff[idx, x]
          matching_y <- matrix_eff[idx, y]
          
          data_scenario[i, x] <- matching_x
          data_scenario[i, y] <- matching_y
          
        } else {
          # Encontrar el intervalo donde se encuentra el cut_off
          pos <- findInterval(cut_off, eff_vector)
          
          # Si 'cut_off' está fuera del rango, romper el bucle
          if (pos == 0 || pos == length(eff_vector)) {
            print(paste("El valor cut_off está fuera del rango para DMU:", i))
            break
          }
          
          # Refinar el rango de 'y' entre las posiciones pos y pos + 1
          range_y <- seq(from = range_y[pos], to = range_y[pos + 1], length.out = 10)
          
        }
        
      }
      
    }
    
  return(data_scenario)
      
}
