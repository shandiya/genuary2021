library(tidyverse)
library(viridis)
library(here)

df <- data.frame(
  x = 1:100,
  y = c(
    runif(10, 1, 1.2), 
    runif(10, 0.9, 1.3), 
    runif(10, 0.8, 1.4), 
    runif(10, 0.7, 1.5),
    runif(10, 0.6, 1.6),
    runif(10, 0.5, 1.7),
    runif(10, 0.4, 1.8),
    runif(10, 0.3, 1.9),
    runif(10, 0.2, 2.0),
    runif(10, 0.1, 2.1)),
  group = 1:100
)

ggplot(df) +
  geom_line(aes(x = x, y = y),
            colour = "#666666") +
  coord_flip() +
  theme_void() +
  labs(caption = "Shandiya Balasubramaniam | @ShandiyaB") +
  theme(plot.caption = element_text(color = "#aaaaaa", size = 7)) +
  ggsave(here("plots", "genuary_19.png"), width = 8, height = 5, units = "in")


ggplot(df) +
  geom_line(aes(x = x, y = y, colour = group)) +
  scale_colour_viridis(option = "inferno") +
  coord_flip() +
  theme_void() +
  labs(caption = "Shandiya Balasubramaniam | @ShandiyaB") +
  theme(
    plot.caption = element_text(color = "#aaaaaa", size = 7),
    legend.position = "none") +
  ggsave(here("plots", "genuary_17.png"), width = 8, height = 5, units = "in") 
   


