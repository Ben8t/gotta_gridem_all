library(tidyverse)
library(glue)
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
  ratio_quantiles <- seq(0, 4, 0.25)
  plot <- ggplot(data) + 
    geom_gridline_ratio(max_x=max(data$defense), ratio_quantiles) +
    geom_image(aes(x=defense, y=attack, label=name, image=PNG), size=.06) + 
    gghighlight(pokedex_number %in% family) +
    coord_cartesian(xlim = c(0, 185), ylim = c(0, 180)) +
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
    labs(x="Defense", y="Attack") +
    custom_theme()
}

## Attack Defense plot

### Classic grid
attack_defense_plot <- ggplot(first_generation) +
  geom_image(aes(x=defense, y=attack, label=name, image=PNG), size=.06) +
  coord_cartesian(xlim = c(0, 185), ylim = c(0, 180)) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  labs(x="Defense", y="Attack") +
  custom_theme_with_grid()
  
attack_defense_plot + ggsave("img/attack_defense_classic_plot.png", bg="white", width = 50, height = 30, units = "cm", dpi = 300)

### Cumul
ratio_quantiles <- quantile(first_generation$sum_att_def, probs=seq(0, 1, 0.1))
# ratio_quantiles <- seq(0, 400, 25)
attack_defense_plot_bis <- ggplot(first_generation) + 
  geom_gridline_cumul(ratio_quantiles) +
  geom_image(aes(x=defense, y=attack, label=name, image=PNG), size=.06) +
  coord_cartesian(xlim = c(0, 185), ylim = c(0, 180)) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  labs(x="Defense", y="Attack") +
  custom_theme()

attack_defense_plot_bis + ggsave("img/attack_defense_plot_bis.png", bg="white", width = 50, height = 30, units = "cm", dpi = 300)


### Ratio
# ratio_quantiles <- quantile(first_generation$att_def_ratio, probs=seq(0, 1, 0.1))
ratio_quantiles <- seq(0, 4, 0.25)
attack_defense_plot <- ggplot(first_generation) + 
  geom_gridline_ratio(max_x=max(first_generation$defense), ratio_quantiles) +
  geom_image(aes(x=defense, y=attack, label=name, image=PNG), size=.06) +
  coord_cartesian(xlim = c(0, 185), ylim = c(0, 180)) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  labs(x="Defense", y="Attack") +
  custom_theme()

attack_defense_plot + ggsave("img/attack_defense_plot.png", bg="white", width = 50, height = 30, units = "cm", dpi = 300)


### Family ratio
attack_defense_plot_filter_poli_family <- plot_family(first_generation, c(1, 2, 3, 4, 5, 6, 7, 8, 9))
attack_defense_plot_filter_poli_family + ggsave("img/attack_defense_plot_filter_poli_family.png", bg="white", width = 50, height = 30, units = "cm", dpi = 300)


values <- quantile(first_generation$capture_rate/first_generation$att_def_ratio, probs=seq(0, 1, 0.1))
test_plot <- ggplot(first_generation) + 
  geom_image(aes(x=att_def_ratio, y=capture_rate, label=name, image=PNG), size=.05) +
  # gghighlight(pokedex_number %in% c(1,2,3,4,5,6,7,8,9)) + 
  labs(x="Att/Def ratio", y="Capture Rate") +
  geom_gridline_ratio(max(first_generation$att_def_ratio), values) +
  custom_theme()

test_plot + ggsave("img/test_plot.png", bg="white", width = 50, height = 30, units = "cm", dpi = 300)



test_data <- ggplot(first_generation) + 
  geom_image(aes(x=base_total, y=attack, label=name, image=PNG), size=.05) + 
  labs(x="Base", y="Attack")