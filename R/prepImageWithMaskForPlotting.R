#' Function to check to images for compatability and mybe in the future to scale the second to the first.
#'
#' @noRd

prepImageWithMaskForPlotting <-
  function(image,
           mask,
           dimIndex = NULL,
           dimValue = NULL,
           orientation = "RAS",
           paletteImageRange = NULL,
           paletteMaskRange = NULL,
           inputPixdim = NULL) {
    #read Image
    image <- processImageInput(image, inputPixdim)

    #reorientation if required
    if (RNifti::orientation(image) != orientation) {
      cat(paste0("Info: Reorienting Image to ", orientation))
      RNifti::orientation(image) <- orientation
    }
    if (RNifti::orientation(mask) != orientation) {
      cat(paste0("Info: Reorienting Image to ", orientation))
      RNifti::orientation(mask) <- orientation
    }

    checkImageCompatability(image, mask)

    image <- prepImageForPlotting(
      input = image,
      dimIndex = dimIndex,
      paletteRange = paletteImageRange,
      inputPixdim = imagePixdim
    )

    mask <- prepImageForPlotting(
      input = mask,
      dimIndex = dimIndex,
      paletteRange = palette_maskRange,
      inputPixdim = mask_pixdim
    )
    return(list(image = image,  mask = mask))
  }
