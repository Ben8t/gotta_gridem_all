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

geom_gridline <- function(max_x, quantiles){
  list(
    map(quantiles, function(v){
      geom_segment(x = 0, y = 0, xend = max_x*2, yend = v*max_x*2, linetype = "dashed", colour = "lightgray")
      }
    )
  )
}

## Plot
filtered_data <- first_generation %>% filter(type1=="water" | type1=="fire" | type1=="grass")
ratio_quantiles <- quantile(filtered_data$attack/filtered_data$defense, probs=seq(0, 1, 0.25))
ggplot(filtered_data) + 
  geom_gridline(max_x=max(filtered_data$defense), ratio_quantiles) +
  geom_image(aes(x=defense, y=attack, label=name, image=PNG), size=.1) + 
  gghighlight(pokedex_number %in% c(60, 61, 62)) +
  coord_cartesian(xlim = c(0, NA), ylim = c(0, NA)) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  labs(x="Defense", y="Attack") +
  theme(plot.margin=unit(c(1,2,1,1), "cm")) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_line(),
    axis.line.x = element_line(size = 0.2, linetype = "solid", colour = "dimgrey"),
    axis.line.y = element_line(size = 0.2, linetype = "solid", colour = "dimgrey"),
  )
