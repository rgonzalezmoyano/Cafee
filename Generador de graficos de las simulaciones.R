library(readxl)
library(dplyr)
library(ggplot2)

# cargar datos
recopilacion_datos <- read_excel("simulations/cobb_douglas_XnY1/results/hold_out_0.10 v3/recopilacion_datos.xlsx")

data <- recopilacion_datos

data[, c(9:28)] <- round(data[, c(9:28)], 3)

noise <- unique(data$noise)

# filtar datos
# noise
for (ruido in noise) {
  
  data_1_25_0 <- data %>% 
    filter(noise == ruido) %>% 
    select(scenario, noise, N, corr_spearman_yD_DEA, corr_spearman_yD_BDEA, corr_spearman_yD_cafee_DEA, corr_spearman_yD_cafee_BDEA)
  #data_1_25_0$scenario <- as.factor(data_1_25_0$scenario)
  
  grafico <- ggplot(data = data_1_25_0, aes(x = scenario)) +
    geom_line(aes(y = corr_spearman_yD_DEA, color = "yD_DEA")) +
    geom_point(aes(y = corr_spearman_yD_DEA, color = "yD_DEA")) +
    
    geom_line(aes(y = corr_spearman_yD_BDEA, color = "yD_BDEA")) +
    geom_point(aes(y = corr_spearman_yD_BDEA, color = "yD_BDEA")) +
    
    geom_line(aes(y = corr_spearman_yD_cafee_DEA, color = "yD_cafee_DEA")) +
    geom_point(aes(y = corr_spearman_yD_cafee_DEA, color = "yD_cafee_DEA")) +
    
    geom_line(aes(y = corr_spearman_yD_cafee_BDEA, color = "yD_cafee_BDEA")) +
    geom_point(aes(y = corr_spearman_yD_cafee_BDEA, color = "yD_cafee_BDEA")) +
    
    scale_x_continuous(breaks = c(1, 3, 6, 9, 12)) +
    scale_y_continuous(limits = c(0, 1)) +
    
    facet_wrap(~N) +
    
    labs(
      title = paste("Correlaciones con", ruido, "al aumentar N"),
      y = "Correlación",
      x = "Inputs"
    ) +
    
    theme_bw()
  
  if (ruido == 0) {
    ruido_txt <- "0.00" 
  } else if (ruido == 0.02){
    ruido_txt <- "0.02" 
  } else if (ruido == 0.05) {
    ruido_txt <- "0.05" 
  }
  
  ggsave(paste("noise_",ruido_txt, ".png", sep = ""), plot = grafico)

}



# al aumentar N en un conjunto
scenario <- unique(data$scenario)
for (input in scenario) {
  
  data_1_25_0 <- data %>% 
    filter(scenario == input) %>% 
    select(scenario, noise, N, corr_spearman_yD_DEA, corr_spearman_yD_BDEA, corr_spearman_yD_cafee_DEA, corr_spearman_yD_cafee_BDEA)

  grafico <- ggplot(data = data_1_25_0, aes(x = N)) +
    geom_line(aes(y = corr_spearman_yD_DEA, color = "yD_DEA")) +
    geom_point(aes(y = corr_spearman_yD_DEA, color = "yD_DEA")) +
    
    geom_line(aes(y = corr_spearman_yD_BDEA, color = "yD_BDEA")) +
    geom_point(aes(y = corr_spearman_yD_BDEA, color = "yD_BDEA")) +
    
    geom_line(aes(y = corr_spearman_yD_cafee_DEA, color = "yD_cafee_DEA")) +
    geom_point(aes(y = corr_spearman_yD_cafee_DEA, color = "yD_cafee_DEA")) +
    
    geom_line(aes(y = corr_spearman_yD_cafee_BDEA, color = "yD_cafee_BDEA")) +
    geom_point(aes(y = corr_spearman_yD_cafee_BDEA, color = "yD_cafee_BDEA")) +
    
    scale_x_continuous(breaks = c(25, 50, 150, 200)) +
    scale_y_continuous(limits = c(0, 1)) +
    
    facet_wrap(~noise) +
    
    labs(
      title = paste("Correlaciones con inputs =", input, " aumentando N"),
      y = "Correlación",
      x = "Input"
    ) +
    
    theme_bw()
  
  if (input == 1) {
    input_txt <- "01" 
  } else if (input == 3){
    input_txt <- "03" 
  } else if (input == 6) {
    input_txt <- "06" 
  } else if (input == 9){
    input_txt <- "09" 
  } else if (input == 12){
    input_txt <- "12" 
  }
  
  ggsave(paste("input_",input_txt, ".png", sep = ""), plot = grafico)
  
}
