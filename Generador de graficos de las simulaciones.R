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
      title = paste("Correlaciones con noise =", ruido, "al aumentar N"),
      y = "Correlación",
      x = "Inputs"
    ) +
    
    theme_bw() +
    theme(
      axis.title.x = element_text (
        size = 12, face = "bold", color = "#921F30",
        margin = margin(t = 10)),
      axis.title.y = element_text (
        size = 12, face = "bold", color = "#921F30",
        margin = margin(r = 10)),
      axis.text = element_text (
        size = 12, color = "black"),
      plot.margin = unit(c(1.25, 1.25, 1.25, 1.25), "lines"),
      plot.title = element_text (
        size = 12, face = "bold", color = "#921F30",
        margin = margin(b = 10)
      ),
      legend.position = "right"
    )
  
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
    scale_y_continuous(limits = c(0.8, 1)) +
    
    facet_wrap(~noise) +
    
    labs(
      title = paste("Correlaciones con inputs =", input, " aumentando N"),
      y = "Correlación",
      x = "N"
    ) +
    
    theme_bw() +
    theme(
      axis.title.x = element_text (
        size = 12, face = "bold", color = "#921F30",
        margin = margin(t = 10)),
      axis.title.y = element_text (
        size = 12, face = "bold", color = "#921F30",
        margin = margin(r = 10)),
      axis.text = element_text (
        size = 12, color = "black"),
      plot.margin = unit(c(1.25, 1.25, 1.25, 1.25), "lines"),
      plot.title = element_text (
        size = 12, face = "bold", color = "#921F30",
        margin = margin(b = 10)
      ),
      legend.position = "right"
    )
  
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


# por ratio_obs_por_input
# al aumentar N en un conjunto

data2 <- data
data2$ratio_obs_por_input <- data2$N/data2$scenario
data2$scenario <- as.factor(data2$scenario)

data2$ratio_obs_por_input <- round(data2$ratio_obs_por_input, 3)
data2 <- data2[data2$noise == 0.05, ]

data_filtrado <- data2[c("scenario", "N", "ratio_obs_por_input", "corr_spearman_yD_DEA", "corr_spearman_yD_BDEA", "corr_spearman_yD_cafee_DEA", "corr_spearman_yD_cafee_BDEA")]

data_DEA <- data2[c("scenario", "N", "ratio_obs_por_input", "corr_spearman_yD_DEA")]
data_DEA$corr <- "corr_yD_DEA"
names(data_DEA)[4] <- "correlation"

data_BDEA <- data2[c("scenario", "N", "ratio_obs_por_input", "corr_spearman_yD_BDEA")]
data_BDEA$corr <- "corr_yD_BDEA"
names(data_BDEA)[4] <- "correlation"

data_cafee_DEA <- data2[c("scenario", "N", "ratio_obs_por_input", "corr_spearman_yD_cafee_DEA")]
data_cafee_DEA$corr <- "corr_yD_cafee_DEA"
names(data_cafee_DEA)[4] <- "correlation"

data_cafee_BDEA <- data2[c("scenario", "N", "ratio_obs_por_input", "corr_spearman_yD_cafee_BDEA")]
data_cafee_BDEA$corr <- "corr_yD_cafee_BDEA"
names(data_cafee_BDEA)[4] <- "correlation"

new_data <- rbind(data_DEA, data_BDEA, data_cafee_DEA, data_cafee_BDEA)

grafico <- ggplot(data = new_data) +
  geom_point(aes(x = ratio_obs_por_input, y = correlation, colour = scenario)) +
  geom_line(aes(x = ratio_obs_por_input, y = correlation, group = scenario, colour = scenario)) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~corr) +
  labs(
    title = paste("Correlaciones al aumentar observaciones por input con noise = 0.05"),
    y = "Correlación",
    x = "Obsevaciones por input"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text (
      size = 12, face = "bold", color = "#921F30",
      margin = margin(t = 10)),
    axis.title.y = element_text (
      size = 12, face = "bold", color = "#921F30",
      margin = margin(r = 10)),
    axis.text = element_text (
      size = 12, color = "black"),
    plot.margin = unit(c(1.25, 1.25, 1.25, 1.25), "lines"),
    plot.title = element_text (
      size = 12, face = "bold", color = "#921F30",
      margin = margin(b = 10)
    ),
    legend.position = "right"
  )

for (ruido in noise) {
  
  data_1_25_0 <- data2 %>% 
    filter(noise == ruido) %>% 
    select(scenario, noise, N, corr_spearman_yD_DEA, corr_spearmea_yD_BDEA, corr_spearman_yD_cafee_DEA, corr_spearman_yD_cafee_BDEA, obs_per_inpt)
  #data_1_25_0$scenario <- as.factor(data_1_25_0$scenario)
  
  grafico <- ggplot(data = data_1_25_0, aes(x = obs_per_inpt)) +
    geom_line(aes(y = corr_spearman_yD_DEA, color = "yD_DEA")) +
    geom_point(aes(y = corr_spearman_yD_DEA, color = "yD_DEA")) +
    
    geom_line(aes(y = corr_spearmea_yD_BDEA, color = "yD_BDEA")) +
    geom_point(aes(y = corr_spearmea_yD_BDEA, color = "yD_BDEA")) +
    
    geom_line(aes(y = corr_spearman_yD_cafee_DEA, color = "yD_cafee_DEA")) +
    geom_point(aes(y = corr_spearman_yD_cafee_DEA, color = "yD_cafee_DEA")) +
    
    geom_line(aes(y = corr_spearman_yD_cafee_BDEA, color = "yD_cafee_BDEA")) +
    geom_point(aes(y = corr_spearman_yD_cafee_BDEA, color = "yD_cafee_BDEA")) +
    
    scale_y_continuous(limits = c(0, 1)) +
    
    facet_wrap(~ruido) +
    
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
