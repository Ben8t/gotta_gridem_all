library(ggplot2)


## Function
geom_gridline_ratio <- function(max_x, quantiles){
  list(
    map(quantiles, function(v){
      geom_segment(x = 0, y = 0, xend = max_x*2, yend = v*max_x*2, linetype = "dashed", colour = "#8F9194")
      }
    )
  )
}

geom_gridline_cumul <- function(values){
  list(
    map(values, function(v){
      geom_segment(x = 0, y = v, xend = v, yend = 0, linetype = "dashed", colour = "#8F9194")
      }
    )
  )
}

custom_theme <- function(){
  list(
    theme(plot.margin=unit(c(1,2,1,1), "cm")),
    theme(
      panel.grid.minor = element_line(color="#DCDCDC"),
      panel.grid.major = element_line(color="#DCDCDC"),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_line(),
      axis.line.x = element_line(size = 0.2, linetype = "solid", colour = "black"),
      axis.line.y = element_line(size = 0.2, linetype = "solid", colour = "black"),
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


custom_theme_with_grid <- function(){
  list(
    theme(plot.margin=unit(c(1,2,1,1), "cm")),
    theme(
      panel.grid.minor = element_line(color="#DCDCDC"),
      panel.grid.major = element_line(color="#DCDCDC"),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_line(),
      axis.line.x = element_line(size = 0.2, linetype = "solid", colour = "black"),
      axis.line.y = element_line(size = 0.2, linetype = "solid", colour = "black"),
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