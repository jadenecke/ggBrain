overlayPlot <- function(plot, overlay, ratio, palette) {
  plot <- plot + ggnewscale::new_scale("fill")
  if (ratio == 1) {
    plot <-
      plot + geom_raster(
        data = image,
        aes(x = x, y = y, fill = value),
        color = NA,
        linewidth = 0,
        width = 1,
        alpha = alpha
      )
  } else {
    plot <-
      plot + geom_tile(
        data = image,
        aes(x = x, y = y, fill = value),
        color = NA,
        linewidth = 0,
        width = 1,
        alpha = alpha
      )
  }

  plot <-
    plot + scale_fill_gradientn("wScoreMap", colours = palette, limits = maskRange)
  return(plot)
}
