#' Register legend file
#' @param data_id Dataset id, composed of \emph{dataset} and \emph{subdataset} (e.g. 'CCI_landCover/landCover').
#' @param value Numeric vector with unique grid value.
#' @param label Character vector with labels matching input of \emph{value}.
#' @param r Numeric vector with red color intensity (0-255).
#' @param g Numeric vector with green color intensity (0-255).
#' @param b Numeric vector with blue color intensity (0-255).
#' @param overwrite Logical. If a legend already exists, should it be modified?
#' @importFrom lubridate ymd years
#' @importFrom tools md5sum
#' @importFrom grDevices rgb
#' @importFrom utils write.table
#' @importFrom filelock lock unlock
#' @return Writes file into standardized file structure.
#' @details {Registers a the legend for a gridded dataset, 
#' which can be used for standardized mapping and plotting.}
#' @seealso \code{\link{build_descriptor}}
#'
#' @export
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

register_legend = function(data_id, label, value, r, g, b, overwrite=FALSE) {
  
  #---------------------------------------------------------------------------#
  # test arguments ----
  #---------------------------------------------------------------------------#
  
  # test data_id and extract needed variables
  if (!is.character(data_id)) stop('"data_id" is not a character element')
  tmp = strsplit(data_id, '/')
  if (length(tmp) != 2) stop('"data_id" is not composed valid')
  dataset = tmp[1]
  subdataset = tmp[2]
  
  # test format of legend inputs
  if (!is.character(label)) stop('"label" is not a character vector')
  if (!is.numeric(value)) stop('"value" is not a numeric vector')
  if (!is.numeric(r)) stop('"r" is not a numeric vector')
  if (!is.numeric(g)) stop('"g" is not a numeric vector')
  if (!is.numeric(b)) stop('"b" is not a numeric vector')
  
  # test length of legend inputs
  nr = unique(c(length(label), length(value), length(r), length(g), length(b)))
  if (length(nr) > 1) stop('"label", "value", "r", "g", and "b" must have the same length')
  
  # test if overwrite command is valid
  if (!is.logical(overwrite)) stop('"overwrite" is not a logical argument')
  
  #---------------------------------------------------------------------------#
  # test output directory ----
  #---------------------------------------------------------------------------#
  
  # construct path to output directory
  dir = paste0(getOption('dmt.data'), 
               .Platform$file.sep, dataset, 
               .Platform$file.sepm, 'info')
  
  # check if output directory exists; if not, create
  if (!dir.exists(dir)) stop('dataset does not exist')
  
  #---------------------------------------------------------------------------#
  # write legend (if possible)
  #---------------------------------------------------------------------------#
  
  # build path to csv that will contain legend file
  file_path = file.path(dir, paste0(tmp[1], '-', tmp[2], '.csv'))
  
  # stop function if output file exists but overwriting is disabled
  if (file.exists(file_path) & (overwrite=FALSE)) {
    stop('legend already exists, but "overwrite" is set to FALSE')
  }
  
  # if file exists and overwriting is allowed, unlock file
  if (file.exists(file_path) & (overwrite==TRUE)) lockpick(file_path, 'open')
  
  # make color hexadecimal code
  hex = rgb(r,g,b, maxColorValue=255)
  
  # compile legend parameters into single data.frame
  odf = data.frame(value=value, label=label, r=r, g=g, b=b, hex=hex)
  
  # write file
  write.table(odf, file_path, row.names=FALSE)
  
  # enable file lock
  lockpick(file_path, 'close')
  
}