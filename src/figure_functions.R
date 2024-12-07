# functions for figure creation

theme_custom1 <- function() {
  theme_bw() %+replace%
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          strip.background = element_blank())
}

