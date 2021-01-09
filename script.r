library(tidyverse)
library(readr)
library(ggrepel)
library(ggimage)
library(gghighlight)


source("src/plot.r")

## Data Loading
pokemon <- read_csv("data/pokemon.csv", col_types = cols(capture_rate = col_double()))
image <- read_csv("data/image.csv") %>%
  select(Number, Pokemon, PNG) %>%
  mutate(local_img=glue("data/pokemon_img/{Number}.png"))

first_generation <- pokemon %>% 
  filter(generation==1) %>%
  left_join(., image, by=c("pokedex_number" ="Number")) %>%
  mutate(
    att_def_ratio=attack/defense,
    sum_att_def=attack+defense,
    hp_def_ratio=hp/defense
  )



plot_family <- function(data, family){
  ratio_quantiles <- quantile(data$attack/data$defense, probs=seq(0, 1, 0.25))
  plot <- ggplot(data) + 
    geom_gridline_ratio(max_x=max(data$defense), ratio_quantiles) +
    geom_image(aes(x=defense, y=attack, label=name, image=PNG), size=.05) + 
    gghighlight(pokedex_number %in% family) +
    coord_cartesian(xlim = c(0, 200), ylim = c(0, 200)) +
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
    labs(x="Defense", y="Attack") +
    custom_theme()
}

## Attack Defense plot
ratio_quantiles <- quantile(first_generation$att_def_ratio, probs=seq(0, 1, 0.1))
attack_defense_plot <- ggplot(first_generation) + 
  geom_gridline_ratio(max_x=max(first_generation$defense), ratio_quantiles) +
  geom_image(aes(x=defense, y=attack, label=name, image=PNG), size=.05) +
  coord_cartesian(xlim = c(0, 200), ylim = c(0, 200)) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  labs(x="Defense", y="Attack") +
  custom_theme()

attack_defense_plot + ggsave("img/attack_defense_plot.png", bg="white", width = 50, height = 30, units = "cm", dpi = 300)

ratio_quantiles <- quantile(first_generation$sum_att_def, probs=seq(0, 1, 0.1))
attack_defense_plot_bis <- ggplot(first_generation) + 
  geom_gridline_cumul(ratio_quantiles) +
  geom_image(aes(x=defense, y=attack, label=name, image=PNG), size=.05) +
  coord_cartesian(xlim = c(0, 200), ylim = c(0, 200)) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  labs(x="Defense", y="Attack") +
  custom_theme()

attack_defense_plot_bis + ggsave("img/attack_defense_plot_bis.png", bg="white", width = 50, height = 30, units = "cm", dpi = 300)



## Attack defenses filter on family
attack_defense_plot_filter_poli_family <- plot_family(first_generation, c(60, 61, 62))
attack_defense_plot_filter_poli_family + ggsave("img/attack_defense_plot_filter_poli_family.png", bg="white", width = 50, height = 30, units = "cm", dpi = 300)


values <- quantile(first_generation$capture_rate/first_generation$att_def_ratio, probs=seq(0, 1, 0.1))
test_plot <- ggplot(first_generation) + 
  geom_image(aes(x=att_def_ratio, y=capture_rate, label=name, image=PNG), size=.05) +
  # gghighlight(pokedex_number %in% c(1,2,3,4,5,6,7,8,9)) + 
  labs(x="Att/Def ratio", y="Capture Rate") +
  geom_gridline_ratio(max(first_generation$att_def_ratio), values) +
  custom_theme()

test_plot + ggsave("img/test_plot.png", bg="white", width = 50, height = 30, units = "cm", dpi = 300)
