#' Set ggplot theme for ggBrain package
#'
#' @import ggplot2
#'
#' @export ggBrainTheme
#'
#' @param base_size Sets general size for text elements.
#' @param base_family Sets font familiy to use
#' @return A ggplot Theme - list of class theme, gg.
#' @examples
#' ggplot() + ggBrainTheme()


ggBrainTheme <- function(base_size = 12,
                     base_family = "", background = "pink") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Specify axis options
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # Specify legend options
      legend.position = "none",
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0, "cm"),
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),
      strip.text.x = element_text(size = base_size * 0.8, color = "white"),
      strip.text.y = element_text(
        size = base_size * 0.8,
        color = "white",
        angle = -90
      ),
      # Specify plot options
      plot.background = element_rect(fill = background, color = background),
      plot.title = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    )
}
