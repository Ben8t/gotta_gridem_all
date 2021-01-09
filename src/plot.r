library(ggplot2)


## Function
geom_gridline_ratio <- function(max_x, quantiles){
  list(
    map(quantiles, function(v){
      geom_segment(x = 0, y = 0, xend = max_x*2, yend = v*max_x*2, linetype = "dashed", colour = "dimgrey")
      }
    )
  )
}

geom_gridline_cumul <- function(values){
  list(
    map(values, function(v){
      geom_segment(x = 0, y = v, xend = v, yend = 0, linetype = "dashed", colour = "dimgrey")
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
      axis.text.x = element_text(size=15), 
      axis.text.y = element_text(size=15),
      axis.title.x = element_text(size=20),
      axis.title.y = element_text(size=20),
      legend.title = element_text(size=20),
      legend.text = element_text(size=20),
      strip.text.x = element_text(size=20)
    ),
    theme(text=element_text(family="Object Sans", face="bold", size=12))
  )
}