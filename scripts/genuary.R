# load libraries-------------------

library(tidyverse)
library(viridis)
library(here)
library(palmerpenguins)
library(feather)
library(tuneR)
library(patchwork)


# 16: Circles only---------------------

oriole <- get_pal("oriole")
colours <- oriole[c(3, 6, 4, 5, 2)]

p16 <- ggplot(penguins) +
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
    plot.background = element_rect(fill = "black", colour = "black"))

p16 + ggsave(here("plots", "genuary_16.png"), height = 2, width = 2, units = "in")


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

p17 <- ggplot(df_17) +
  geom_line(aes(x = x, y = y, colour = group)) +
  scale_colour_viridis(option = "inferno") +
  coord_flip() +
  theme_void() +
  #labs(caption = "Shandiya Balasubramaniam | @ShandiyaB") +
  theme(
    plot.caption = element_text(color = "#aaaaaa", size = 7),
    legend.position = "none") 

p17 + ggsave(here("plots", "genuary_17.png"), width = 2, height = 2, units = "in")   


# 19: Increase the randomness along the Y-axis---------------------

p19 <- ggplot(df_17) +
  geom_line(aes(x = x, y = y),
            colour = "#666666") +
  coord_flip() +
  theme_void() +
  #labs(caption = "Shandiya Balasubramaniam | @ShandiyaB") +
  theme(plot.caption = element_text(color = "#aaaaaa", size = 7))

p19 + ggsave(here("plots", "genuary_19.png"), width = 2, height = 2, units = "in")


# 24: 500 lines-------------------------
# murmuration

df_24 <- data.frame(x = sample(500), y = sample(500), group = 1:20)

p24 <- ggplot(df_24) +
  geom_curve(aes(x = x, xend = x + 8, y = y, yend = y + 4, colour = group)) +
  scale_colour_gradient(low = "darkolivegreen1", high = "darkolivegreen4") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "lightblue", colour = "lightblue")) 

p24 + ggsave(here("plots", "genuary_24.png"), height = 2, width = 2, units = "in")


# 25: Make a grid of permutations of something-----------------
# permutations in pink

df_25 <- data.frame(
  x = rep(c(1:25), 25),
  y = rep(c(1:25), each = 25))

rows <- sample(nrow(df_25))
df_shuffled <- df_25[rows, ]
binom <- rbinom(25, 20, 0.2)
df_shuffled$alpha <- binom/10

p25 <- ggplot(df_shuffled, aes(x, y, alpha = alpha)) +
  geom_tile(fill = rgb(0.9,0.5,0.9), colour = "white") +
  theme_void() +
  theme(legend.position = "none") 

p25 + ggsave(here("plots", "genuary_25.png"), width = 2, height = 2, units = "in")


# 28: Use sound-----------------------

# read in file, wrangle df
c_tibicen <- readMidi(here("data", "c_tibicen.mid"))
c_tibicen_notes <- c_tibicen %>% 
  getMidiNotes() %>% 
  mutate(notename_abbr = str_sub(notename, 1, 1))

# plot!
p28 <- ggplot(c_tibicen_notes) +
  geom_point(aes(
    x = time,
    y = note,
    size = length,
    alpha = velocity,
    colour = notename_abbr),
  shape = 16) +
  ylim((min(c_tibicen_notes$note) - 8), max(c_tibicen_notes$note)) +
  scale_size(range = c(3, 10)) +
  scale_alpha(range = c(0.4, 0.9)) +
  scale_color_manual(values = c(
    "#2AA09A",
    "#9BD7D7",
    "#3B643F",
    "#8C9E5A",
    "#DBA62D",
    "#F7C9BD",
    "#C12B38")) +
  annotate(
    "text", 
    x = 600, 
    y = 69, 
    label = "Australian \nmagpie", 
    size = 5, 
    fontface = "bold",
    colour = "#777777") +
  coord_polar() +
  theme_void() +
  labs(caption = "Shandiya Balasubramaniam | @ShandiyaB") +
  theme(
    plot.caption = element_text(size = 8, colour = "#555555"),
    legend.position = "none") 

p28 + ggsave(here("plots", "genuary_28.png"), width = 5, height = 5, units = "in")
  

# combine all plots------------

all_plots <- (p25 + p28 + p24) / (p17 + p16 + p19)
ggsave('plots/all_plots.png', all_plots, width = 12, height = 8, units  = "in")  
  





