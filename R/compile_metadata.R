#' MAS database builder.
#' @param verbose Logical argument. Should the progress be reported?
#' @importFrom raster extension
#' @importFrom lubridate ymd years
#' @importFrom filelock unlock
#' @return SQLite compiling relevant metadata for every file in the database
#' @details {Updates, or creates an SQLite file with metadata for every file 
#' in the database. The function will first search for an existing database 
#' file following the path set in \emph{getOption(dmt.data)}. If no file 
#' exists, a new one will be created. If a file exists, the function will 
#' read it, list all files currently in the database, and update the list 
#' if the last modification date differs, or if new files were added.}
#'
#' @export
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

compile_metadata <- function(verbose=FALSE) {
  
  #---------------------------------------------------------------------------#
  if (verbose) print('located database file and lock it in place') # ----
  #---------------------------------------------------------------------------#
  
  # base data path
  path = file.path(paste0(getOption('dmt.data'), .Platform$file.sep))
  
  # open/create database registry
  database = .access_database()
  
  # apply file lock
  output_file = file.path(getOption('dmt.data'), 'database.rds')
  
  file_lock = .lock_database()
  on.exit({unlock(file_lock$lock); file.remove(file_lock$path)}, add=TRUE)
  
  # allow a lock (unix and windows)
  Sys.chmod(output_file, '777')
  on.exit(Sys.chmod(output_file, '444'), add=TRUE)
  
  # allow a lock (windows)
  fe = file.exists(output_file)
  if (fe & (.Platform$OS.type == 'windows')) {
    shell(paste0('icacls ', fileSnapshot(x)$path, ' /grant Everyone:(D,M)'))}
  
  on.exit(if (.Platform$OS.type == 'windows') {
    shell(paste0('icacls ', fileSnapshot(output_file)$path, 
                 ' /deny Everyone:(D,M)'))}, add=TRUE)
  
  #---------------------------------------------------------------------------#
  if (verbose) print('find potential input files') # ----
  #---------------------------------------------------------------------------#
  
  # list files in database
  dirs = list.dirs(path, full.names=T, recursive=F)
  files = do.call('c', lapply(dirs, function(d) 
    paste0(basename(d), '/', list.files(d, '[.]', recursive=F))))
  
  # identify new files
  ind = which(files %in% database$path)
  finfo = as.character(file.mtime(paste0(path, files[ind])))
  ind = ind[which(finfo > database$modified[match(files[ind],database$path)])]
  files = c(files[ind], files[which(!files %in% database$path)])
  
  #---------------------------------------------------------------------------#
  if (verbose) print('extracting metadata') # ----
  #---------------------------------------------------------------------------#
  
  if (length(files) > 0) {
    
    odf = vector('list', length(files))
    
    for (i in 1:length(files)) {
      
      # main dataset name
      dataset = dirname(files[i])
      
      tmp = strsplit(files[i], paste0(dataset, '-'), fixed=T)[[1]][2]
      tmp = strsplit(tmp, '_')[[1]]
      
      subdataset = tmp[1]
      
      # extract date and variable strings
      dates = strsplit(tmp[2], '-')[[1]]
      
      # translate date string into data object (needed for database)
      dates = do.call('c', lapply(strsplit(dates, '-')[[1]], function(d) {
        year = substr(d, 1, 4)
        if (as.numeric(substr(d,5,6)) == 0) month='01' else month=substr(d,5,6)
        if (as.numeric(substr(d,7,8)) == 0) day='01' else day=substr(date,7,8)
        return(as.Date(paste0(year, '-', month, '-', day)))
      }))
      
      # if there is only one date available, add a second
      # NOTE: needed to fill metadata, which reports on start/end dates
      if (length(dates) == 1) dates = c(dates,dates)
      
      # infer end date (should be the last day of year to make queries flexible)
      dates[2] = (ymd(dates[1])+years(1))-1
      
      # check when file was last modified
      modification_time = file.mtime(paste0(path,files[i]))
      
      resolution = strsplit(tmp[3], '[.]')[[1]][1]
      
      # construct data frame with metadata and add it to the list
      odf[[i]] = data.frame(
        dataset=dataset, subdataset=subdataset, 
        resolution=resolution, path=files[i], 
        format=strsplit(extension(files[i], maxchar=20), '[.]')[[1]][2],
        start=dates[2], end=dates[2], 
        modified=modification_time, 
        stringsAsFactors=F)
      
    }
    
    # combine original database with new data
    database = rbind(database, do.call(rbind, odf))
    
    #-------------------------------------------------------------------------#
    if (verbose) print('writting metadata') # ----
    #-------------------------------------------------------------------------#
    
    .access_database(database)
    
    if (verbose) print('I am done! Enjoy!')
    
  } else {
    
    stop('no new files to add')
    
  }
  
}
