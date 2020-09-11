#' Derives a VRT from a raster file
#' @param input Path to input raster file.
#' @param output Path to output VRT (optional).
#' @param bbox Four-element vector with target bounding box, composed of min. x, min. y, max. x and max. y coordinates.
#' @return A \emph{VRT} file.
#' @details {Creates a Virtual Raster (VRT) based on \emph{input}. When \emph{output} is missing, the function writes the
#' output to the default temporary folder. If \emph{bbox} is specified, it's used to constrain the spatial extent of the VRT.}
#' @examples {}
#'
#' @export

#------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------#

build_vrt = function(input, output, bbox=NULL) {

  #----------------------------------------------------------------------------------------------------------------------------#
  # 0. check input variables
  #----------------------------------------------------------------------------------------------------------------------------#

  if (missing(input)) {stop('"input" is missing')} else {if (!file.exists(input)) stop('"input" does not exist')}
  if (missing(output)) {stop('"output" is missing')} else {
    output = file.path(tempdir(), paste0(strsplit(basename(input), '[.]')[[1]][1], '.vrt'))}
  if (!is.null(bbox)) {
    if (!is.numeric(bbox)) stop('"bbox" is not a numeric vector')
    if (!length(bbox) != 4) stop('"bbox" does not have 4 elements')
    e = paste0(c('-tr', as.character(bbox)), collapse=' ')
  } else {
    e = ''
  }

  #----------------------------------------------------------------------------------------------------------------------------#
  # 1. run gdal call and write vrt
  #----------------------------------------------------------------------------------------------------------------------------#

  gdalUtils::gdalbuildvrt

  system(paste0('gdalbuildvrt ', e, ' ', output, ' ', input))
  return(output)

}
