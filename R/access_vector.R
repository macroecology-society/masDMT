#' Access to vector data following the MAS data structure.
#' @param data.id Element of class \emph{character}.
#' @param query SQL query used to thematically subset a vector dataset.
#'@param bbox Four-element vector with target bounding box, composed of min. x, min. y, max. x and max. y coordinates.
#' @param limit \emph{Numeric} element specifying how many features to read from requested vector data.
#' @importFrom sf st_read st_as_text st_as_sfc st_bbox
#' @return {The output consists of the following:
#' \itemize{
#' \item{A \emph{data.frame} with dataset table but without geometries, if \emph{geometry} is set to FALSE}
#' \item{An \emph{sf} object, if \emph{geometry} is set to TRUE (constrained by  \emph{query}, and \emph{bbox}, if provided)}
#' }}
#' @details {This function provides access to vector data available in the MAS database. All vector datasets shared by the
#' group are stored as SQLite databases. This facilitates the access to large vector datasets, and the current function
#' simplifies that process. The function looks for the dataset specific through \emph{data.id}, and returns a subset,
#' depending on user defined criteria. Using \emph{query}, users can limit the amount of data to read using an SQL query,
#' allowing to e.g. select features based on categorical information in a given data column. Note that these queries are
#' case specific, and should be customized to each dataset. In addition, when provided with a \emph{bbox} argument, the
#' function will select features that exist within the specified bounding box. Note that all vector datasets within the
#' MAS database is stored in WGS 84, and thus the \emph{bbox} argument should be composed by latitude/longitude coordinates.
#' To get acquainted with a dataset, users should use the \emph{limit} argument. This argument determine how many features
#' will be read, allowing users to work with a subset of the dataset.}
#' @examples {}
#'
#' @export

#------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------#

access_vector <- function(data.id, query=query, bbox=bbox, limit=limit) {

  #----------------------------------------------------------------------------------------------------------------------------#
  # 1. access database and test input variables
  #----------------------------------------------------------------------------------------------------------------------------#

  if (missing(data.id)) {stop('"data.id" is missing, please provide a valid character')}

  if (!missing(query)) {if (!is.character(query)) {stop('"query" should be a character string')}} else {query=NULL}

  if (!missing(bbox)) {
    bbox = st_bbox(c(xmin=bbox[1], ymin=bbox[2], xmax=bbox[3], ymax=bbox[4]))
    bbox = tryCatch(st_as_text(st_as_sfc(bbox)), error=function(x) return(FALSE))
    if (isFALSE(bbox)) {stop('"bbox" is not a valid argument (see st_bbox() from sf package)')}
  } else {
    bbox = character(0)
  }

  if (!missing(limit)) {if (!is.numeric(limit)) {stop('"limit" should be a numeric argument')}} else {limit=NULL}

  #----------------------------------------------------------------------------------------------------------------------------#
  # 2. process data query
  #============================================================================================================================#
  # 2.1. find path to target dataset/subdataset
  #============================================================================================================================#

  data = list_data(data.id)

  if (data$format != 'vector') {stop('target dataset is not a vector')}

  #============================================================================================================================#
  # 2.2. build SQL query, if one is not provided by the user
  #============================================================================================================================#

  if (is.null(limit) & is.null(query)) {query=paste0('select * from ', data$subdataset)}
  if (!is.null(limit) & is.null(query)) {query=paste0('select * from ', data$subdataset, ' limit ', as.character(limit))}

  #============================================================================================================================#
  # 2.3. access data
  #============================================================================================================================#

  s <- st_read(paste0(getOption('dmt.data'), data$path), query=query, wkt_filter=bbox)

  return(s)

}
