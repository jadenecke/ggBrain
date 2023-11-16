#' Function to check to images for compatability and mybe in the future to scale the second to the first.
#'
#' @noRd

basePlot <- function(image, ratio, palette, alpha = 1){
  if(!is.data.frame(image) | c("x", "y", "value") %in% names(image)){
    stop("Input image is not the correct data type. Please Input a dataframe containing x and y as coordinates and value as voxel value.")
  }
  g <- ggplot() +
    scale_fill_gradientn("base", palette) +
    guides(fill = "none") +
    coord_fixed(
      ratio = ratio,
      xlim = c(1, xDim),
      ylim = c(1, yDim),
      expand = TRUE,
      clip = "on"
    )

  if (ratio == 1) {
    g <- g + geom_raster(data = image, aes(x = x, y = y, fill = value),
              color = NA,
              linewidth = 0,
              width = 1,
              alpha = alpha)
  } else {
    g <- g + geom_tile(data = image, aes(x = x, y = y, fill = value),
              color = NA,
              linewidth = 0,
              width=1,
              alpha = alpha)
  }
  return(g)
}
