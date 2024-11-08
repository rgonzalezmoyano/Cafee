# need to load "new_dataset_toy_lebeled.Rdata"

### determinate efficient class

plot <- ggplot() +
  geom_point(data = data, aes(x = x1, y = y, color = class_efficiency)) +
  scale_color_manual(values = c("green4", "red"), name = "Class", labels = c("efficient", "inefficient")) +
  labs(x = "input", y = "output") +
  
  # name DMUs
  geom_text(data = data[data$class_efficiency == "efficient", ],
            aes(x = x1, y = y, label = row.names(data[data$class_efficiency == "efficient", ])),
            vjust = -1, hjust = 1) + 
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  xlim(0, 10) +
  ylim(0, 10) +
  
  theme_bw() +
  theme(legend.position = "bottom")

plot

ggsave(plot = plot, dpi = 600, filename = "DEA_add_label.png")
