#' Create a sandbox containing small subsets of target datasets.
#' @param data_id \emph{Character} vector with target dataset ID's.
#' @param range Two-element \emph{Date} vector with start/end date range..
#' @param bbox Four-element vector with target bounding box (min x, min y, max x, max y).
#' @param path \emph{Character} element with path to directory where to construct database. 
#' @param virtual Logical. Should the output be a Virtual Raster (VRT) or a real image?
#' @param verbose Logical. Should the function communicate it's progress?
#' @return A copy of a parent database.
#' @importFrom lubridate is.Date
#' @importFrom gdalUtilities gdalbuildvrt gdal_translate
#' @details {The function creates a copy of the datasets given by \emph{data_id} 
#' within the area defined by \emph{bbox} and the temporal range determined by 
#' \emph{range}. Currently, the function only handles raster objects, and it 
#' provides the output either as a  \emph{GeoTiff}, if \emph{virtual} is FALSE, 
#' or a \emph{VRT}, if \emph{virtual} is TRUE.}
#'
#' @export

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

build_sandbox <- function(data_id, bbox, range, path, virtual=FALSE, verbose=FALSE) {

  #---------------------------------------------------------------------------#
  # 0. test input arguments ---
  #---------------------------------------------------------------------------#
  
  if (!is.character(data_id)) stop('"data_id" is not a character vector')
  if (!is.numeric(bbox)) stop('"bbox" is not a numeric vector')
  if (length(bbox) != 4) stop('"bbox" does not have 4 elements')
  if (!is.Date(range)) stop('"range" should be a date object (yyy-mm-dd)')
  if (length(range) != 2) stop('"range" should be a 2-element vector')
  if (!is.character(path)) {stop('"path" is not a character element')}
  if (!dir.exists(path)) {
    if (verbose) warning('creating output path')
    dir.create(path)
  }
  
  #---------------------------------------------------------------------------#
  # 1. find and analyze vector datasets ---
  #---------------------------------------------------------------------------#
  
  database = list_data(data_id)
  database = database[
    which((database$start >= range[1]) & 
            (database$end <= range[2])),]
  
  for (i in 1:nrow(database)) {
    
    odir = file.path(path, database$dataset[i])
    if (!file.exists(odir)) dir.create(odir)
    
    # input file
    input = file.path(getOption('dmt.data'), 
                      .Platform$file.sep, database$path[i])
    
    # output file
    output = file.path(odir, paste0(.Platform$file.sep, 
                       strsplit(basename(database$path[i]), '[.]')[[1]][1], 
                       '.tif'))
    
    # create VRT file
    tmp = paste0(strsplit(output, '[.]')[[1]][1], '.vrt')
    vrt = tryCatch(gdalbuildvrt(input, tmp, te=bbox), error=function(e) return(FALSE))
    
    if ((!virtual) & (!is.logical(vrt))) {
      
      gdal_translate(tmp, output, co=list("COMPRESS=deflate", "ZLEVEL=9", "PREDICTOR=2"))
      file.remove(tmp)
      
    }
    
    # apply access restrictions
    Sys.chmod(output, '444', use_umask=FALSE)
    
  }
  
  #---------------------------------------------------------------------------#
  # 2. copy auxiliary information (e.g. legends, bibtex) ---
  #---------------------------------------------------------------------------#
  
  paths = unique(dirname(database$path))
  
  for (p in paths) {
    
    idir = file.path(getOption('dmt.data'), 
                     .Platform$file.sep, p, 
                     .Platform$file.sep, 'info')
    
    if (dir.exists(idir)) {
      
      files = list.files(idir, full.names=T)
      odir = file.path(path, paste0(p, .Platform$file.sep, 'info'))
      if (!dir.exists(odir)) dir.create(odir)
      file.copy(files, file.path(odir, basename(files)), overwrite=T)
      
      
    }
    
  }
  
}
