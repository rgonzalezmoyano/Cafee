simulaciones <- read_excel (
  "simulations/cobb_douglas_XnY1/results/hold_out_0.10/recopilacion_datos_final_mod.xlsx", 
  sheet = "Hoja1")

simulaciones$ratio_obs_por_input <- as.factor(simulaciones$ratio_obs_por_input)
library(ggplot2)

ggplot() + 
  geom_point(data = simulaciones, aes(x = simulaciones$ratio_obs_por_input, y = simulaciones$corr_yD_DEA), colour = simulaciones$ratio_obs_por_input) +
  geom_point(data = simulaciones, aes(x = simulaciones$ratio_obs_por_input, y = simulaciones$corr_yD_BDEA), colour = simulaciones$ratio_obs_por_input) +
  geom_point(data = simulaciones, aes(x = simulaciones$ratio_obs_por_input, y = simulaciones$corr_yD_cafee_DEA), colour = simulaciones$ratio_obs_por_input) +
  