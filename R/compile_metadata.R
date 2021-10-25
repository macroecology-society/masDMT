#' MAS database builder.
#' @param verbose Logical argument. Should the progress be reported? Default is FALSE.
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#' @importFrom raster raster extent dataType res
#' @importFrom RSQLite SQLite
#' @importFrom utils write.csv
#' @return RDS compiling relevant metadata for every file in the database
#' @details {Updates, or creates, RDS with metadata for every file in the 
#' database. the function will first search an existing file in the database, 
#' following the path set in \emph{getOptions('dmt.data')}. If no file exists, 
#' a new one will be created. If a file exists, the function will read it, 
#' list all files currently in the database, and update the list if the last 
#' modification date differs, or if new files were added.}
#' @seealso \code{\link{collect_metadata}}
#'
#' @export
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

compile_metadata <- function(verbose=FALSE) {
  
  #-------------------------------------------------------------------------#
  if (verbose) print('located data files') # ----
  #-------------------------------------------------------------------------#
  
  # list files in database
  dirs = list.dirs(getOption('dmt.data'), full.names=T, recursive=F)
  files = do.call('c', lapply(dirs, function(d) {
    f1 = list.files(d, '.tif')
    f2 = list.files(d, '.vrt')
    f3 = list.files(d, '.sqlite')
    f4 = list.files(d, '.spatialite')
    return(paste0(basename(d), '/', c(f1,f2,f3,f4)))
  }))
  
  #---------------------------------------------------------------------------#
  if (verbose) print('checking for existing database file') # ----
  #---------------------------------------------------------------------------#
  
  output_file = paste0(getOption('dmt.data'), 'database.rds')
  
  if (file.exists(output_file)) {
    if (verbose) print('found file (updating)')
    
    database = readRDS(output_file)
    
    ind = which(files %in% database$path)
    finfo = sapply(
      paste0(getOption('dmt.data'), files[ind]), function(i) file.info(i)$mtime)
    ind = ind[which(finfo > database$modified)]
    
    files = c(files[ind], files[which(!files %in% database$path)])
    
  } else {
    
    if (verbose) print('no existing file (creating)')
    database = data.frame(dataset=as.character(), subdataset=as.character(), 
                          resolution=as.character(), pixel_size=as.numeric(), 
                          path=as.character(), format=as.character(), 
                          type=as.character(), nr_bytes=as.numeric(), 
                          nr_files=as.integer(), 
                          start=as.Date(as.integer()), 
                          end=as.Date(as.integer()), 
                          modified=as.POSIXct(as.integer()))
    
  }
  
  #---------------------------------------------------------------------------#
  if (verbose) print('extracting metadata') # ----
  #---------------------------------------------------------------------------#
  
  odf = collect_metadata(files)
  
  #---------------------------------------------------------------------------#
  if (verbose) print('writting metadata') # ----
  #---------------------------------------------------------------------------#
  
  saveRDS(odf, file=output_file)
  
  if (verbose) print('I am done! Enjoy!')
  
}
