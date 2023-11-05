#' Function to reduce any n-Dimensional image to a 2D slice with some safety checks.
#'
#' @noRd
#'
#'
clampImage <- function(image, min, max){
  if(min > max){stop("Cant apply image boundaries, min > max. Presumably the paletteRange is not [min, max]")}
  return(pmin(pmax(image, min), max))
}
