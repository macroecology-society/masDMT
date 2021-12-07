#' register dataset
#' @param file Path to file, or files, to be registered in the database.
#' @param dataset Name of dataset (e.g. 'CCI_landCover').
#' @param subdataset Name of subdataset (e.g. 'landCover').
#' @param start_date Character element with start of dataset (as yyyy-mm-dd). When month/day is unknown, set to '00'.
#' @param start_date Character element with end of dataset (as yyyy-mm-dd). When month/day is unknown, set to '00'.
#' @param resolution Character element with resolution of dataset (e.g. '10km').
#' @param catalog Logical. Should a new metadata entry be added to the database?
#' @param replace Logical. Replace file in catalog if the name is repeated?
#' @param delete Logical. Delete original file when finished?
#' @param verbose Logical. Should the function report on the progress?
#' @importFrom tools md5sum
#' @importFrom raster extension
#' @return Writes file into standardized file structure.
#' @details {When file is set to NULL, the function will return a standardized 
#' file path, without extension, where the file would need to be stored. If 
#' \emph{file} is not NULL, then the function will search for the specified 
#' file in system, and make a copy of it in the data structure using the 
#' standardized name. The fill will in addition be registered in the existing 
#' flat database, given that \emph{catalog} is set to TRUE.}
#' @seealso \code{\link{build_descriptor}}
#'
#' @export
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

register_data = function(dataset, subdataset, start_date, end_date, 
                         resolution, catalog=TRUE, replace=TRUE, 
                         delete=FALSE, verbose=TRUE, file=NULL) {
  
  #---------------------------------------------------------------------------#
  # 0. test input variabbles ----
  #---------------------------------------------------------------------------#
  
  if (!is.null(file)) {
    if (!file.exists(file)) stop('"file" does not exist')
    external = TRUE
  } else {
    external = FALSE
  }
  
  if (!is.character(dataset)) stop('"dataset" is not a character element')
  if (!is.character(variable)) stop('"variable" is not a character element')
  if (!is.character(start_date)) stop('"start_date" is not a character element')
  if (nchar(start_date) != 10) stop('"start_date" not in yyyy-mm-dd format')
  if (!is.character(end_date)) stop('"end_date" is not a character element')
  if (nchar(end_date) != 10) stop('"end_date" not in yyyy-mm-dd format')
  if (!is.character(resolution)) stop('"resolution" is not a character element')
  if (!is.logical(catalog)) stop('"catalog" is not a logical argument')
  if (!is.logical(replace)) stop('"replace" is not a logical argument')
  if (!is.logical(delete)) stop('"delete" is not a logical argument')
  if (!is.logical(verbose)) stop('"vebose" is not a logical argument')
  
  # define metadata file
  output_file = paste0(getOption('dmt.data'), 'database.rds')
  
  #---------------------------------------------------------------------------#
  # 1. build standardized data entry ----
  if (verbose) warning('standardizing file naming')
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
  
  # run if "file" is provided
  if (external) {
    
    # construct standardized file name
    path = path
    oname = file.path(getOptions('dmt.data'), 
                      paste0(dataset, .Platform$file.sep, dataset, 
                             '-', subdataset, '_', date, '_', resolution))
    
    # create output directory
    output = paste0(getOption('dmt.data'), dataset, .Platform$file.sep, oname)
    if (!dir.exists(diname(output))) dir.create(dirname(output))
    
    #---------------------------------------------------------------------------#
    # 2. write copy of file ----
    if (verbose) warning('making file copy')
    #---------------------------------------------------------------------------#
    
    fe = file.exists(output)
    if (fe) stop('output file already exists and replacement was not allowed')
    file.copy(file, output)
    
    if (delete) {
      
      a = as.vector(tools::md5sum(file))
      b = as.vector(tools::md5sum(output))
      
      if (a != b) stop('copy failed: input file not the same as the output')
      if (a == b) {
        file.remove(file)
        if (verbose) warning('original file was deleted after a successful md5sum check')
      }
      
    }
    
    if (catalog) {
      
      path = file.path(paste0(getOption('dmt.data'), .Platform$file.sep))
      output_file = paste0(path, 'database.rds')
      if (!file.exists(output_file)){
        warning('no database file available')
        stop('run "compile_metadata()" to create a flat database file')
      } else {
        if (verbose) warning('cataloging dataset')
      }
      
      # end date
      year = substr(start_date, 1, 4)
      if (as.numeric(substr(start_date,5,6)) == 0) month='01' else month=substr(start_date,5,6)
      if (as.numeric(substr(start_date,7,8)) == 0) day='01' else day=substr(start_date,7,8)
      start_date = as.Date(paste0(year, '-', month, '-', day))
      
      rm(ds, month, year)
      
      # end date
      year = substr(end_date, 1, 4)
      if (as.numeric(substr(end_date,5,6)) == 0) month='12' else month=substr(end_date,5,6)
      if (as.numeric(substr(end_date,7,8)) == 0) {
        end_date = as.Date(paste (1, as.numeric(month)+1, year, sep='-'), format='%d-%m-%Y') - 1
        if (month == '12') end_date = as.Date(paste0(year, '-', month, '-31'))
      } else {
        end_date = as.Date(paste0(year, '-', month, '-', day))
      }
      
      rm(month, year)
      
      ads = data.frame(dataset=dataset, subdataset=subdataset, 
                       resolution=resolution, path=oname, 
                       format=strsplit(file, '[.]')[[1]][2], 
                       start=start_date, end=end_date, 
                       modified=file.mtime(output))
      
      database = rbind(readRDS(output_file), ads)
      
    }
    
  
  # run if "file" is not provided
  } else {
    
    # return standardized file name
    return(file.path(getOptions('dmt.data'), 
                     paste0(dataset, '/', dataset, 
                            '-', subdataset, '_', date, 
                            '_', resolution)))
    
  }
  
}
