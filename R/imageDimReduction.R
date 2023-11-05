#' Function to reduce any n-Dimensional image to a 2D slice with some safety checks.
#'
#' @noRd

imageDimReduction <-
  function(image,
           dimIndex = NULL,
           dimValue = NULL,
           isMask = FALSE) {
    imageDim <- dim(image)
    if(isMask){
      dimMaskCheck <- dimIndex <= length(imageDim)
      dimIndex <- dimIndex[dimMaskCheck]
      dimValue <- dimValue[dimMaskCheck]
    }

    if (length(imageDim) < 2) {
      stop(paste0(
        "Image must be have at least two dimensions, yours has ",
        length(imageDim)
      ))
    }
    if (length(dimIndex) != length(dimValue)) {
      stop(
        "dimIndex and dimValue have different lengths. Please supply dimensions to slice to dimIndex and where to slice to dimValue"
      )
    }
    if (length(imageDim) - 2 != length(dimIndex)) {
      stop(
        paste0(
          "Non-fitting dimIndex and dimValue. In order to reduce the image to two dimensions you need to provide ",
          length(imageDim) - 2,
          " slice positions to dimIndex and dimValue each."
        )
      )
    }
    return(eval(parse(text = paste0(
      "image[", paste0(unlist(lapply(seq_along(imageDim), function(i) {
        paste0("", dimValue[dimIndex == i])
      })), collapse = ",") , "]"
    ))))
  }
