#' Build unique file identifier
#' @param dataset Name of dataset (e.g. 'CCI_landCover').
#' @param subdataset Name of subdataset (e.g. 'landCover').
#' @param start_date Date object with start date of the dataset.
#' @param end_date Date object with end date of the dataset.
#' @param resolution Character element with resolution of dataset (e.g. '10km').
#' @importFrom methods setClass
#' @return Returns standardized file identifier.
#' @details {Builds an unique file identifier, 
#' needed to register a file in the database.}
#' @seealso \code{\link{register_data}}
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
  if (!is.Date(start_date)) stop('"start_date" is not a date element')
  if (!is.Date(end_date)) stop('"end_date" is not a date element')
  if (!is.character(resolution)) stop('"resolution" is not a character element')
  
  #---------------------------------------------------------------------------#
  # 1. build standardized data entry ----
  #---------------------------------------------------------------------------#
  
  # test resolution string
  tmp = strsplit(resolution, 
                 "(?<=[A-Za-z])(?=[0-9])|(?<=[0-9])(?=[A-Za-z])", 
                 perl=TRUE)[[1]]
  if (is.na(is.numeric(tmp[1]))) stop('"resolution" is not a valid argument')
  if (length(tmp) < 2) stop(paste0('"resolution" is not valid, ', 
                                   'should be e.g. 10km (', 
                                   'i.e. string with NUMBER + UNIT)'))
  
  # return file ID object
  obj = setClass("mas_id", slots=list(dataset="character", 
                                      subdataset="character", 
                                      start="character", 
                                      end='character', 
                                      resolution='character'))
  
  
  return(obj(dataset=dataset, 
             subdataset=subdataset, 
             start=as.character(start_date), 
             end=as.character(end_date), 
             resolution=resolution))
  
}
