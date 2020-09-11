#' Converts polygons into points and reports on their raster overlap
#' @param x Object of class \emph{SpatVector} and of type \emph{polygon}.
#' @param y Single or two-element vector of class \emph{numeric} with the target resolution.
#' @param id \emph{Numeric} or \emph{character} vector with unique identifiers, one for each row of \emph{x}.
#' @importFrom terra rast aggregate rasterize xyFromCell geomtype
#' @return {A \emph{data.frame} object, containing:
#' \itemize{
#' \item{\emph{id}: Unique identifier of the reference polygon}
#' \item{\emph{x}: x coordinate}
#' \item{\emph{y}: y coordinate}
#' \item{\emph{overlap}: percent overlap between the pixel and the polygon}
#' }}
#' @details {The function iterates through each element of \emph{x}, converting the corresponding polygon into points. For
#' each point, the function returns an estimate of the percent overlap between the corresponding pixel and the reference
#' polygon. Each entry in the output \emph{data.frame} will be related to the original reference polygon through a unique
#' identifier. Without an \emph{id} argument, the identifier will reflect the order by which \emph{x} was sorted.}
#' @examples {}
#'
#' @export
#' @keywords sampling

#------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------#

poly2sample = function(x, y, id) {

  #----------------------------------------------------------------------------------------------------------------------------#
  # 0. check input variables
  #----------------------------------------------------------------------------------------------------------------------------#

  if (class(x)[1] != 'SpatVector') stop('"x" is not a SpatVector object')
  if (geomtype(x) != 'polygons') stop('"x" does not contain polygons')
  if (class(y)[1] != 'numeric') stop('"y" is not a numeric object')
  if (length(y) > 2) stop('"y" has more than 1 element')
  if (length(y) == 1) y = c(y, y)
  if (!missing(id)) {id = 1:nrow(x)} else {
    if (!is.vector(id)) stop('"id" is not a vector')
    if (length(id) != nrow(x)) stop('"id" has a different length than "x"')
  }

  #----------------------------------------------------------------------------------------------------------------------------#
  # 1. extract samples
  #----------------------------------------------------------------------------------------------------------------------------#

  # iterate through each polygon
  point.samples = do.call(rbind, lapply(1:nrow(x), function(i) {

    # create reference raster over which to rasterize polygon
    reference = rast(x[i,], res=y/100)

    # rasterize polygon to 1/100 of the original resolution, and sum overlapping pixels to get percent overlap
    rasterized = aggregate(rasterize(x[i,], reference, field=1, touches=T), fact=100, fun='sum', na.rm=T) / 100

    # extract raster values
    values = as.matrix(rasterized)

    # find non-NA pixels
    ind = which(!is.na(values[,1]))

    # derive x/y coordinates from selected pixels
    xy = xyFromCell(rasterized, ind)

    # build data.frame with pixel coordinates and percent overlap
    odf = data.frame(id=i, x=xy[,1], y=xy[,2], overlap=values[ind,1])

    return(odf)

    rm(reference, rasterized, values, ind, xy, odf)

  }))

  return(point.samples)

}
