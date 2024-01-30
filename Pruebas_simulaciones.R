simulaciones <- read_excel (
  "simulations/cobb_douglas_XnY1/results/hold_out_0.10/recopilacion_datos_final_mod.xlsx", 
  sheet = "Hoja1")

simulaciones$ratio_obs_por_input <- as.factor(simulaciones$ratio_obs_por_input)
simulaciones$noise <- as.factor(simulaciones$noise)
simulaciones$scenario <- as.factor(simulaciones$scenario)
library(ggplot2)

data2 <- simulaciones[simulaciones$N == 25, ]

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
