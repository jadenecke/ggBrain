#' Function to process the image input, whether its Path, niftiImage or matrix
#'
#' @noRd

processImageInput <-
  function(input,
           pixdim = NULL,
           reference = NULL) {
    if (is(input, "character")) {
      if (length(input) != 1) {
        stop("Currently only a single image path is supported.")
      }
      # if(!requireNamespace("RNifti", quietly = TRUE)){
      #   stop("Package `RNifti` not availabel, no means of reading nifti file from path. Please `install.packages('RNifti')`")
      # }
      if (difftime(Sys.time(),
                   file.info(input)$atime, units = "secs") < 5) {
        warning(
          paste0(
            "Presumably loading the ",
            input,
            " multiple times from disk (last access time is < 5 seconds). Consider loading the image in you're Environment and supply as variable to reduce disk and computational load."
          )
        )
      }
      return(RNifti::readNifti(input))
    } else if (is(input, "niftiImage")) {
      return(input)
    } else if (is(input, "array")) {
      if (is.null(pixdim)) {
        if (is.null(reference)) {
          stop("Array supplied as input, but no pixel dimensions nor reference image supplied.")
        }
        # when reference
        return(RNifti::asNifti(input, reference = reference))
      } else if (is.null(reference)) {
        #when pixdim
        if (length(pixdim) != length(dim(input))) {
          stop(
            "Specified pixel dimensions dont fit array dimensions, supply one pixel size value per array dimension."
          )
        }
        if ((!is.numeric(pixdim)) |
            (!is.null(dim(pixdim)))){
          stop("pixdim must be a numeric vector.")
        }
        attr(input, "pixdim") <- pixdim # TODO: the first number in pixdim sets nifti-1 header style RAS/LAS information. With nifti-2 headers it is set to 0 and the information is extracted from sfrom/qfrom. However it would be nice to set the 1 / -1 to indicate RAS/LAS easily with custom matrices. However, RNifti ignores the given information when using RNifti::asNifti. Once I figure out how to set this value, I can add a parameter which indicates RAS/LAS and then reorienting the image should work as well.
        return(RNifti::asNifti(input))

      } else {
        stop(
          "Pixdim and reference image supplied, please use either one for consistency. reference is recommended over pixdim."
        )
      }
    } else{
      stop(
        "Cant interpret image input, please specifiy either a path, a niftiImage object or a array"
      )
    }
  }
