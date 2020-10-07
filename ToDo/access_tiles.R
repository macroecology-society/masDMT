#' Access georreferenced tiles to concurrent processing
#' @param scale.id {Keyword to select relevant tiles. One of: \itemize{
#'   \item{\emph{high-resolution}: tiles most adequate when dealing with
#'   datasets with resolutions below 300-m} \item{\emph{mid-resolution}: tiles
#'   most adequate when dealing with datasets with resolutions between 300 and
#'   1000-m} \item{\emph{low-resolution}: tiles most adequate when dealing with
#'   datasets with resolutions above 1000-m} \item{\emph{gee-land}: tiles used
#'   to extract data from Google Earth Engine(GEE) over the terrestrial realm.}
#'   }}
#' @param bbox Output of function \link[sf]{st_bbox} with the desired spatial
#'   bounding box.
#' @importFrom sf st_read st_intersection st_as_sfc st_crs st_crs<-
#' @return {A \emph{spatRaster} object}
#' @details {Function used to access global tiling systems. When dealing with
#'   large-scale processing, these tiles help split tasks into manageable chunks
#'   that can be run concurrently in a High-Performance Computing system such as
#'   EVE.}
#' @examples {}
#'
#' @export

#------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------#

access_tiles = function(scale.id, bbox) {

  # find required tiles and check if they exist
  iname = paste0(getOption('dmt.base'), getOption('dmt.apps'), scale.id, '.json')
  if (!file.exists(iname)) stop("couldn't find any tiles (is 'scale' written correctly?)")
  s = st_read(iname) # read tile

  # apply bbox, if applicable
  if (!missing(bbox)) {
    bbox = st_as_sfc(bbox)
    st_crs(bbox) = st_crs(s)
    if (class(bbox)[1] == 'bbox') stop('"bbox" is not of a valid format') else s = st_intersection(s, bbox)
  }

  return(s)

}
