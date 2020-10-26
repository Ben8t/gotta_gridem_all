library(tidyverse)
library(readr)
library(ggrepel)
library(ggimage)
library(gghighlight)

## Data Loading
pokemon <- read_csv("data/pokemon.csv")
image <- read_csv("data/image.csv") %>%
  select(Number, Pokemon, PNG)

first_generation <- pokemon %>% 
  filter(generation==1) %>%
  left_join(., image, by=c("pokedex_number" ="Number"))


## Function
geom_gridline <- function(max_x, quantiles){
  list(
    map(quantiles, function(v){
      geom_segment(x = 0, y = 0, xend = max_x*2, yend = v*max_x*2, linetype = "dashed", colour = "dimgrey")
      }
    )
  )
}

custom_theme <- function(){
  list(
    theme(plot.margin=unit(c(1,2,1,1), "cm")),
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_line(),
      axis.line.x = element_line(size = 0.2, linetype = "solid", colour = "dimgrey"),
      axis.line.y = element_line(size = 0.2, linetype = "solid", colour = "dimgrey"),
      axis.text.x = element_text(size=25), 
      axis.text.y = element_text(size=25),
      axis.title.x = element_text(size=30),
      axis.title.y = element_text(size=30),
      legend.title = element_text(size=20),
      legend.text = element_text(size=20),
      strip.text.x = element_text(size=30)
    ),
    theme(text=element_text(family="Recursive Sans Linear Light", face="bold", size=12))
  )
}

plot_family <- function(data, family){
  ratio_quantiles <- quantile(data$attack/data$defense, probs=seq(0, 1, 0.25))
  plot <- ggplot(data) + 
    geom_gridline(max_x=max(data$defense), ratio_quantiles) +
    geom_image(aes(x=defense, y=attack, label=name, image=PNG), size=.1) + 
    gghighlight(pokedex_number %in% family) +
    coord_cartesian(xlim = c(0, 200), ylim = c(0, 200)) +
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
    labs(x="Defense", y="Attack") +
    custom_theme()
}

## Plot
ratio_quantiles <- quantile(first_generation$attack/first_generation$defense, probs=seq(0, 1, 0.25))
attack_defense_plot <- ggplot(first_generation) + 
  geom_gridline(max_x=max(first_generation$defense), ratio_quantiles) +
  geom_image(aes(x=defense, y=attack, label=name, image=PNG), size=.1) +
  coord_cartesian(xlim = c(0, 200), ylim = c(0, 200)) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  labs(x="Defense", y="Attack") +
  custom_theme()

attack_defense_plot + ggsave("img/attack_defense_plot.png", bg="white", width = 50, height = 30, units = "cm", dpi = 300)

attack_defense_plot_filter_poli_family <- plot_family(first_generation, c(60, 61, 62))
attack_defense_plot_filter_poli_family + ggsave("img/attack_defense_plot_filter_poli_family.png", bg="white", width = 50, height = 30, units = "cm", dpi = 300)
