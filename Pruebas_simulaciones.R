library(readxl)
simulaciones <- read_excel (
  "simulations/cobb_douglas_XnY1/results/hold_out_0.10/recopilacion_datos_final_mod.xlsx", 
  sheet = "Hoja1")

simulaciones$ratio_obs_por_input <- round(simulaciones$ratio_obs_por_input, 3)

#simulaciones$ratio_obs_por_input <- as.factor(simulaciones$ratio_obs_por_input)
simulaciones$noise <- as.factor(simulaciones$noise)
simulaciones$scenario <- as.factor(simulaciones$scenario)
simulaciones$N <- as.factor(simulaciones$N)


library(ggplot2)

data2 <- simulaciones[simulaciones$noise == 0, ]

data_filtrado <- data2[c("scenario", "N", "ratio_obs_por_input", "corr_yD_DEA", "corr_yD_BDEA", "corr_yD_cafee_DEA", "corr_yD_cafee_BDEA")]

data_DEA <- data2[c("scenario", "N", "ratio_obs_por_input", "corr_yD_DEA")]
data_DEA$corr <- "corr_yD_DEA"
names(data_DEA)[4] <- "value"

data_BDEA <- data2[c("scenario", "N", "ratio_obs_por_input", "corr_yD_BDEA")]
data_BDEA$corr <- "corr_yD_BDEA"
names(data_BDEA)[4] <- "value"

data_cafee_DEA <- data2[c("scenario", "N", "ratio_obs_por_input", "corr_yD_cafee_DEA")]
data_cafee_DEA$corr <- "corr_yD_cafee_DEA"
names(data_cafee_DEA)[4] <- "value"

data_cafee_BDEA <- data2[c("scenario", "N", "ratio_obs_por_input", "corr_yD_cafee_BDEA")]
data_cafee_BDEA$corr <- "corr_yD_cafee_BDEA"
names(data_cafee_BDEA)[4] <- "value"

new_data <- rbind(data_DEA, data_BDEA, data_cafee_DEA, data_cafee_BDEA)

par(mfrow = c(1,1))
  
  ggplot(data = new_data) +
    geom_point(aes(x = ratio_obs_por_input, y = value, colour = scenario)) +
    geom_line(aes(x = ratio_obs_por_input, y = value, group = scenario, colour = scenario)) +
    scale_y_continuous(limits = c(0, 1)) +
    facet_wrap(~corr) +
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



ggplot(data = data_filtrado) +
  geom_point(aes(x = ratio_obs_por_input, y = corr_yD_BDEA, colour = scenario)) +
  geom_line(aes(x = ratio_obs_por_input, y = corr_yD_BDEA, group = scenario, colour = scenario)) +
  scale_y_continuous(limits = c(0, 1)) +
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




ggplot() + 
  geom_line(data = data2, aes(x = scenario, y = corr_yD_BDEA, group = noise, colour = noise)) +
  geom_point(data = data2, aes(x = scenario, y = corr_yD_BDEA, group = noise, colour = noise)) +
  geom_line(data = data2, aes(x = scenario, y = corr_yD_DEA, group = noise, colour = noise)) +
  geom_point(data = data2, aes(x = scenario, y = corr_yD_DEA, group = noise, colour = noise)) +
  geom_line(data = data2, aes(x = scenario, y = corr_yD_cafee_BDEA, group = noise, colour = noise)) +
  geom_point(data = data2, aes(x = scenario, y = corr_yD_cafee_BDEA, group = noise, colour = noise)) +
  geom_line(data = data2, aes(x = scenario, y = corr_yD_cafee_DEA, group = noise, colour = noise)) +
  geom_point(data = data2, aes(x = scenario, y = corr_yD_cafee_DEA, group = noise, colour = noise)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw()



  geom_point(data = simulaciones, aes(x = simulaciones$ratio_obs_por_input, y = simulaciones$corr_yD_BDEA), colour = simulaciones$ratio_obs_por_input) 
  geom_point(data = simulaciones, aes(x = simulaciones$ratio_obs_por_input, y = simulaciones$corr_yD_cafee_DEA), colour = simulaciones$ratio_obs_por_input) 

  library(readxl)
