#' Set ggplot theme for ggBrain package
#'
#' @import ggplot2
#'
#' @export plotBrainSlice
#'
#' @param base_size Sets general size for text elements.
#' @param base_family Sets font familiy to use
#' @return A ggplot Theme - list of class theme, gg.
#' @examples
#' I()


plotBrainSlice <- function(image,
                           dimIndex,
                           dimValue,
                           mask = NULL,
                           orientation = "RAS",
                           palette_mask = palette_heat,
                           imagePixdim = NULL,
                           maskPixdim = NULL,
                           paletteMaskRange = NULL,
                           paletteImage = palette_gray,
                           paletteImageRange = NULL,
                           alpha = 1) {

  if(is.null(mask)){
    image <- prepImageForPlotting(input = image,
                                  dimIndex = dimIndex,
                                  paletteRange = paletteImageRange,
                                  inputPixdim = imagePixdim
    )
  } else {
    li <- prepImageWithMaskForPlotting(image = image,
                                       mask = mask,
                                       dimIndex = dimIndex,
                                       dimValue = dimValue,
                                       paletteImageRange = paletteImageRange,
                                       paletteMaskRange = paletteMaskRange,
                                       orientation = orientation)
  }


  g <- ggplot() +
    geom_tile(data = image, aes(Var1, Var2, fill = value),
              color = NA,
              linewidth = 0,
              width=1,
              alpha = alpha) +
    scale_fill_gradient("base", low = "black", high = "white") +
    guides(fill = "none") +
    new_scale("fill") +
    geom_tile(data = mask,
              aes(Var1, Var2, fill = value), color = NA,
              alpha = 1,
              linewidth = 0,
              width=1) +
    scale_fill_gradientn("wScoreMap", colours = palette, limits = maskRange) +
    theme_MRI() +
    coord_fixed(
      ratio = ratio,
      xlim = c(1, xDim),
      ylim = c(1, yDim),
      expand = TRUE,
      clip = "on"
    )
  return(g)
}


iterBrain <-
  function(dim,
           slice,
           img,
           mask,
           alpha,
           scale.col,
           upperMaskThresh,
           add_orientation_slice,
           lowerMaskThresh,
           maskRange) {
    print("preparing brain Slice")
    if (!all(dim(img) == dim(mask))) {
      stop("Image and mask have different dimensions")
    }
    if (!all(attr(mask, "pixdim") == attr(mask, "pixdim"))) {
      stop("Image and mask have different dimensions")
    }
    voxelSize <- attr(img, "pixdim")[-dim]

    xDim <- attr(img, "dim")[-dim][1]
    yDim <- attr(img, "dim")[-dim][2]


    imgSlice <- abind::asub(img, slice, dim)
    imgSliceLong <- reshape2::melt(imgSlice)
    imgSliceLong <- imgSliceLong[!is.na(imgSliceLong$value),]
    imgSliceLong <- imgSliceLong[imgSliceLong$value != 0,]

    maskSliceLong <- reshape2::melt(abind::asub(mask, slice, dim))
    maskSliceLong <- maskSliceLong[!is.na(maskSliceLong$value),]
    if (!is.na(upperMaskThresh)) {
      maskSliceLong <-
        maskSliceLong[maskSliceLong$value < upperMaskThresh,]
    }
    if (!is.na(lowerMaskThresh)) {
      maskSliceLong <-
        maskSliceLong[maskSliceLong$value > lowerMaskThresh,]
    }

    ratio <- do.call('/', as.list(voxelSize))
    print(paste0("Image slice Ratio: ", ratio, " with ", paste0(voxelSize, collapse = " / "), " as voxel sizes"))
    g <-
      plotBrain(
        image = imgSliceLong,
        alpha = alpha,
        mask = maskSliceLong,
        ratio = ratio,
        xDim = xDim,
        yDim = yDim,
        scale.col = scale.col,
        maskRange = maskRange
      )

    if(add_orientation_slice & dim %in% c(3,2)){
      positiveLabels <- unlist(strsplit(orientation(img), ""))
      negativeLabels <- c(R="L", A="P", S="I", L="R", P="A", I="S")[positiveLabels]
      print(dim)
      g <- g + annotate("text", x = round(xDim * 0.02), y = round(yDim * 0.5), label = negativeLabels[1], colour = "white")
      g <- g + annotate("text", x = round(xDim * 0.98), y = round(yDim * 0.5), label = positiveLabels[1], colour = "white")
    }
    return(g)
  }
