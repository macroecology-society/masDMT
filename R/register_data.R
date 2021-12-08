#' Build unique file identifier
#' @param file Path to file, to be registered in the database.
#' @param dataset Name of dataset (e.g. 'CCI_landCover').
#' @param subdataset Name of subdataset (e.g. 'landCover').
#' @param start_date Character element with start of dataset (as yyyy-mm-dd). When month/day is unknown, set to '00'.
#' @param end_date Character element with end of dataset (as yyyy-mm-dd). When month/day is unknown, set to '00'.
#' @param resolution Character element with resolution of dataset (e.g. '10km').
#' @importFrom raster extension
#' @return Writes file into standardized file structure.
#' @details {Builds an unique file identifier, 
#' needed to register a file in the database.}
#' @seealso \code{\link{build_descriptor}}
#'
#' @export
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

register_data = function(file, dataset, subdataset, start_date, end_date, resolution) {
  
  #---------------------------------------------------------------------------#
  # 0. test input variables ----
  #---------------------------------------------------------------------------#
  
  if (!file.exists(file)) stop('"file" missing from file system')
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
  
  # locate metadata file
  path = file.path(paste0(getOption('dmt.data'), .Platform$file.sep))
  output_file = paste0(path, 'database.rds')
  
  if (!file.exists(output_file)){
    warning('no database file available')
    stop('run "compile_metadata()" to create a flat database file')
  }
  
  # end date
  year = substr(start_date, 1, 4)
  if (as.numeric(substr(start_date,6,7)) == 0) month='01' else month=substr(start_date,6,7)
  if (as.numeric(substr(start_date,9,10)) == 0) day='01' else day=substr(start_date,9,10)
  start_date = as.Date(paste0(year, '-', month, '-', day))
  
  rm(month, year)
  
  # end date
  year = substr(end_date, 1, 4)
  if (as.numeric(substr(end_date,6,7)) == 0) month='12' else month=substr(end_date,6,7)
  if (as.numeric(substr(end_date,9,10)) == 0) {
    end_date = as.Date(paste (1, as.numeric(month)+1, year, sep='-'), format='%d-%m-%Y') - 1
    if (month == '12') end_date = as.Date(paste0(year, '-', month, '-31'))
  } else {
    end_date = as.Date(paste0(year, '-', month, '-', day))
  }
  
  rm(month, year)
  
  ads = data.frame(dataset=dataset, subdataset=subdataset, 
                   resolution=resolution, path=paste0(dataset, '/', basename(file)), 
                   format=strsplit(file, '[.]')[[1]][2], 
                   start=start_date, end=end_date, 
                   modified=file.mtime(output))
  
  saveRDS(rbind(readRDS(output_file), ads), file=output_file)
  
  odir = file.path(path, dataset)
  if (!dir.exists(odir)) dir.create(odir)
  file.copy(file, paste0(file.path(odir, basename(file))))

}