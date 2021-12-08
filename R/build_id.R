#' Build unique file identifier
#' @param file Path to file, or files, to be registered in the database.
#' @param dataset Name of dataset (e.g. 'CCI_landCover').
#' @param subdataset Name of subdataset (e.g. 'landCover').
#' @param start_date Character element with start of dataset (as yyyy-mm-dd). When month/day is unknown, set to '00'.
#' @param end_date Character element with end of dataset (as yyyy-mm-dd). When month/day is unknown, set to '00'.
#' @param resolution Character element with resolution of dataset (e.g. '10km').
#' @param verbose Logical. Should the function report on the progress?
#' @return Returns standardized file identifier.
#' @details {Builds an unique file identifier, 
#' needed to register a file in the database.}
#' @seealso \code{\link{build_descriptor}}
#'
#' @export
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

build_id = function(dataset, subdataset, start_date, end_date, resolution) {
  
  #---------------------------------------------------------------------------#
  # 0. test input variables ----
  #---------------------------------------------------------------------------#
  
  if (!is.character(dataset)) stop('"dataset" is not a character element')
  if (!is.character(subdataset)) stop('"subdataset" is not a character element')
  if (!is.character(start_date)) stop('"start_date" is not a character element')
  if (nchar(start_date) != 10) stop('"start_date" not in yyyy-mm-dd format')
  if (!is.character(end_date)) stop('"end_date" is not a character element')
  if (nchar(end_date) != 10) stop('"end_date" not in yyyy-mm-dd format')
  if (!is.character(resolution)) stop('"resolution" is not a character element')
  
  #---------------------------------------------------------------------------#
  # 1. build standardized data entry ----
  #---------------------------------------------------------------------------#
  
  # configure "start_date" string
  start_date = paste0(substr(start_date,1,4), 
                      substr(start_date,6,7), 
                      substr(start_date,9,10))
  
  # configure "end_date" string
  end_date = paste0(substr(end_date,1,4), 
                    substr(end_date,6,7), 
                    substr(end_date,9,10))
  
  # build string to report on date range
  if (start_date == end_date) {
    date = start_date
  } else {
    date = paste0(start_date, '-', end_date)
  }
  
  # test resolution
  tmp = strsplit('10km', "(?<=[A-Za-z])(?=[0-9])|(?<=[0-9])(?=[A-Za-z])", perl = TRUE)[[1]]
  if (is.na(is.numeric(tmp[1]))) stop('"resolution" is not a valid argument')
  if (length(tmp) < 2) stop('"resolution" is not valid, should be e.g. 10km (i.e. NUMERIC + TEXT')
  
  # return file ID
  return(paste0(dataset, '-', subdataset, '_', date, '_', resolution))
  
}
