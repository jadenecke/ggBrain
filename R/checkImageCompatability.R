#' Function to check to images for compatability and mybe in the future to scale the second to the first.
#'
#' @noRd

checkImageCompatability <- function(image, mask, dimIndex) {
  pixdimImageConsidered <- attr(image, "pixdim")
  pixdimMask <- attr(mask, "pixdim")
  if (length(dim(mask)) < length(dim(image))) {
    pixdimImageConsidered <-
      pixdimImageConsidered[seq_along(pixdimMask)]
  } else if (length(dim(mask)) > length(dim(image))) {
    stop(
      "Mask is of higher dimension than image, no means to handle this case. If this is an issue for you, consider writing a feature request on github. For now, please reduce you mask dimensions by hand."
    )
  }
  if (pixdimImageConsidered != pixdimMask) {
    stop(
      paste(
        "Mask and image have different voxel dimensions; Image:",
        paste(attr(image, "pixdim"), collapse = ","),
        "Mask:",
        paste(attr(mask, "pixdim"), collapse = ",")
      )
    )
  }
  if (RNifti::orientation(image) != RNifti::orientation(mask)) {
    warning("Mask and Image have different orientations, reorienting Mask")
    RNifti::orientation(mask) <- RNifti::orientation(image)
  }
  # if(any(dim(image) != dim(mask))){
  #   stop(paste("Mask and image have different dimensions; Image:", paste(dim(image), collapse = ","), "Mask:", paste(dim(mask), collapse = ",")))
  # }
  retrun(TRUE)
}
