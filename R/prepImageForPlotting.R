#' Function to check to images for compatability and mybe in the future to scale the second to the first.
#'
#' @noRd

prepImageForPlotting <- function(input, dimIndex = NULL, dimValue = NULL, orientation = "RAS", paletteRange = NULL, inputPixdim = NULL){
  if((!is.null(paletteRange)) && length(paletteRange) != 2 ){
    stop("Legnth of paletteRange != 2. Please give lower and upper boundary.")
  }
  #read input
  input <- processImageInput(input, inputPixdim)
  if (RNifti::orientation(input) != orientation) {
    cat(paste0("Info: Reorienting Image to ", orientation))
    RNifti::orientation(input) <- orientation
  }
  input <- imageDimReduction(input, dimIndex=dimIndex, dimValue=dimValue)
  #create Palette if not present
  if(is.null(paletteRange)) {
    paletteRange <- c(quantile(input, .02),
                            quantile(input, .98))
  } else {
    if(length(paletteRange) != 2){stop("paletteRange must be of length 2")}
    if(paletteRange[1] > paletteRange[2]){stop("first value of paletteRange must be <= then the second value (first lower, then upper bound)")}
  }
  #clamp input range
  input <- clampImage(input, paletteRange[1], paletteRange[2])
  return(data.frame(x = rep(seq(nrow(input)), ncol(input)),
                    y = rep(seq(ncol(input)), each = nrow(input)),
                    value = as.numeric(input)))
}


