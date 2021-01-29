# load libraries-------------------

library(tidyverse)
library(viridis)
library(here)
library(palmerpenguins)
library(feather)


# 16: Circles only---------------------

oriole <- get_pal("oriole")
colours <- oriole[c(3, 6, 4, 5, 2)]

ggplot(penguins) +
  geom_point(
    aes(
      x = flipper_length_mm,
      y = body_mass_g,
      colour = species:island,
      size = year,
    ),
    alpha = 0.7,
    shape = 16
  ) +
  xlim(172, 231) +
  ylim(2700, 6300) +
  coord_polar(start = -1) +
  scale_color_manual(values = colours) +
  scale_size(range = c(1, 3)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "black", colour = "black")
  ) +
  ggsave(here("plots", "genuary_16.png"), height = 3, width = 3, units = "in")


# 17: Draw a line, pick a new color, move a bit---------------------

df_17 <- data.frame(
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

ggplot(df_17) +
  geom_line(aes(x = x, y = y, colour = group)) +
  scale_colour_viridis(option = "inferno") +
  coord_flip() +
  theme_void() +
  labs(caption = "Shandiya Balasubramaniam | @ShandiyaB") +
  theme(
    plot.caption = element_text(color = "#aaaaaa", size = 7),
    legend.position = "none") +
  ggsave(here("plots", "genuary_17.png"), width = 8, height = 5, units = "in")   


# 19: Increase the randomness along the Y-axis---------------------

ggplot(df_17) +
  geom_line(aes(x = x, y = y),
            colour = "#666666") +
  coord_flip() +
  theme_void() +
  labs(caption = "Shandiya Balasubramaniam | @ShandiyaB") +
  theme(plot.caption = element_text(color = "#aaaaaa", size = 7)) +
  ggsave(here("plots", "genuary_19.png"), width = 8, height = 5, units = "in")


# 24: 500 lines-------------------------
# murmuration

df_24 <- data.frame(x = sample(500), y = sample(500), group = 1:20)

ggplot(df_24) +
  geom_curve(aes(x = x, xend = x + 8, y = y, yend = y + 4, colour = group)) +
  scale_colour_gradient(low = "darkolivegreen1", high = "darkolivegreen4") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "lightblue", colour = "lightblue")) +
  ggsave(here("plots", "genuary_24.png"), height = 4, width = 4, units = "in")

# 25: Make a grid of permutations of something-----------------
# permutations in pink

df_25 <- data.frame(
  x = rep(c(1:25), 25),
  y = rep(c(1:25), each = 25))

rows <- sample(nrow(df_25))
df_shuffled <- df_25[rows, ]
binom <- rbinom(25, 20, 0.2)
df_shuffled$alpha <- binom/10

ggplot(df_shuffled, aes(x, y, alpha = alpha)) +
  geom_tile(fill = rgb(0.9,0.5,0.9), colour = "white") +
  theme_void() +
  theme(legend.position = "none") +
  ggsave(here("plots", "genuary_25.png"), width = 4, height = 4, units = "in")

# attempt at animation (incomplete)
# 
# library(gganimate)
# library(gifski)
# 
# p <- ggplot(df_shuffled, aes(x, y, alpha = alpha)) +
#   geom_tile(fill = rgb(0.9,0.5,0.9), colour = "white") +
#   theme_void() +
#   theme(legend.position = "none")
# 
# anim <- p + transition_states(alpha, transition_length = 3, state_length = 1) 
# animate(anim, duration = 5, fps = 20, width = 400, height = 400, renderer = gifski_renderer())
# anim_save("output.gif")



