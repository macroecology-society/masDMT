#' Access to gridded data following the MAS data structure.
#'
#' @param data.id Element of class \emph{character}.
#' @param range Two-element vector of class \emph{character} with desired start
#'   and end dates (optional, in yyyy-mm-dd format).
#' @param bbox Four-element vector with target bounding box, composed of min. x,
#'   min. y, max. x and max. y coordinates.
#' @details {The function provides access to gridded data in the MAS database,
#'   returning as a \emph{SpatRaster} object, native to the \emph{terra} R
#'   package. If \emph{range} is provided, the function will select data files
#'   within the given temporal range and, if \emph{bbox is provided}, those
#'   files will be constrained to the given spatial bounding box. If the query
#'   encounters multiple files (i.e. time-series), the output will be a
#'   multi-layered \emph{spatRaster} object. Note that independently of the
#'   number of layers in the output, the function \link[terra]{time} can be used
#'   to determine the date to which the given layer corresponds to. However, if
#'   the target layer is a composite of multiple dates, this information will be
#'   set as NA. Whenever this function is ran, it will look for an existing
#'   temporary directory and store virtual rasters (VRT's) corresponding to the
#'   read-in layers. The temporary directory is created the first time this
#'   function is ran, and its path will be accessible using
#'   \emph{getOption('rast.temp')}.}
#' @return A \emph{spatRaster} object
#' @examples {}
#' @importFrom terra rast time<-
#' @importFrom gdalUtils gdalbuildvrt
#' @importFrom utils write.table
#' @export

#------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------#

access_grid <- function(data.id, range=range, bbox=bbox) {

  #----------------------------------------------------------------------------------------------------------------------------#
  # 1. access database and test input variables ----
  #----------------------------------------------------------------------------------------------------------------------------#

  # test dataset
  if (!is.character(data.id)) stop('"data.id" is not in a numeric vector')

  # test start date
  if (!missing(range)) {
    if (length(range) != 2) {stop('"range" does should be a 2-element character vector')}
    if (!is.character(range)) {stop('"range" is not a character vector')}
    range = tryCatch(as.Date(range), error=function(x) return(FALSE))
    if (isFALSE(range)) stop('format of "range" not valid (should be yyy-mm-dd)')
  }

  # test bounding box
  if (!missing(bbox)) {
    if (!is.numeric(bbox)) stop('"bbox" is not a numeric vector')
    if (length(bbox) != 4) stop('"bbox" does not have 4 elements')
    subset_img = TRUE
  } else {
    subset_img = FALSE
  }

  #----------------------------------------------------------------------------------------------------------------------------#
  # 2. process data query ----
  #============================================================================================================================#
  # 2.1. find index of datasets/subdatasets
  #============================================================================================================================#

  # access database
  data = list_data(data.id)
  data$start = as.Date(data$start)
  data$end = as.Date(data$end)

  # test if the selected dataset is a raster
  if (data$format[1] != 'grid') {stop('target dataset is not a raster')}

  #============================================================================================================================#
  # 2.2. apply temporal constraints
  #============================================================================================================================#

  if (!missing(range)) {

    data = data[(data$start >= range[1]) & (data$end <= range[2]),]
    if (nrow(data) == 0) {stop('no data found within the specified temporal range')}

  }

  #----------------------------------------------------------------------------------------------------------------------------#
  # 3. read data (and, if required, apply spatial bounding box) ---
  #----------------------------------------------------------------------------------------------------------------------------#

  # create file list
  bname = paste0(tempfile(), '_', Sys.getpid()) # session-dependent base name
  ifile = paste0(bname, '.txt') # input list of files
  idf = data.frame(files=paste0(getOption("dmt.data"), data$path), stringsAsFactors=F) # table with file names
  write.table(idf, ifile, quote=F, col.names=FALSE, row.names=FALSE)
  ofile = paste0(bname, '.vrt')

  # create vrt with
  if (!subset_img) gdalbuildvrt(input_file_list=ifile, output.vrt=ofile, separate=TRUE)
  if (subset_img) gdalbuildvrt(input_file_list=ifile, output.vrt=ofile, te=bbox, separate=TRUE)

  # read file list as spatRaste object
  x = rast(ofile)

  #----------------------------------------------------------------------------------------------------------------------------#
  # 4. add metadata to raster when appropriate ----
  #----------------------------------------------------------------------------------------------------------------------------#

  # assign time information tp raster object
  dates = data$start # retrieves
  dates[dates != data$end] = NA # don't assign a date to images with a time range
  dates = as.Date(dates) # convert date information to actual dates
  time(x) = dates # assign dates to raster object
  names(x) = data$path # file paths as names

  return(x)

}
