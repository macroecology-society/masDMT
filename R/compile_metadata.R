#' MAS database builder.
#' @param verbose Logical argument. Should the progress be reported?
#' @importFrom raster extension
#' @return RDS compiling relevant metadata for every file in the database
#' @details {Updates, or creates, RDS with metadata for every file in the 
#' database. the function will first search an existing file in the database, 
#' following the path set in \emph{getOptions('dmt.data')}. If no file exists, 
#' a new one will be created. If a file exists, the function will read it, 
#' list all files currently in the database, and update the list if the last 
#' modification date differs, or if new files were added.}
#'
#' @export
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

compile_metadata <- function(verbose=FALSE) {
  
  #-------------------------------------------------------------------------#
  if (verbose) print('located data files') # ----
  #-------------------------------------------------------------------------#
  
  # base data path
  path = file.path(paste0(getOption('dmt.data'), .Platform$file.sep))
  
  # list files in database
  dirs = list.dirs(path, full.names=T, recursive=F)
  files = do.call('c', lapply(dirs, function(d) 
    paste0(basename(d), '/', list.files(d, '[.]', recursive=F))))
  
  files = files[extension(files) != '.xml']
  
  #---------------------------------------------------------------------------#
  if (verbose) print('checking for existing database file') # ----
  #---------------------------------------------------------------------------#
  
  output_file = paste0(path, 'database.rds')
  
  if (file.exists(output_file)) {
    if (verbose) print('found file (updating)')
    
    database = readRDS(output_file)
    
    ind = which(files %in% database$path)
    finfo = file.mtime(paste0(path, files[ind])[ind])
    ind = ind[which(finfo > database$modified[match(files[ind], database$path)])]
    
    files = c(files[ind], files[which(!files %in% database$path)])
    
  } else {
    
    if (verbose) print('no existing file (creating)')
    database = data.frame(dataset=as.character(), subdataset=as.character(), 
                          resolution=as.character(), path=as.character(), 
                          format=as.character(), start=as.Date(as.integer()), 
                          end=as.Date(as.integer()), 
                          modified=as.POSIXct(as.integer()))
    
  }
  
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
      dates = strsplit(tmp[2], '–')[[1]]
      
      if (dates[1] != "NA") {
        
        # end date
        ds = dates[1]
        year = substr(ds, 1, 4)
        if (as.numeric(substr(ds,5,6)) == 0) month='01' else month=substr(ds,5,6)
        if (as.numeric(substr(ds,7,8)) == 0) day='01' else day=substr(ds,7,8)
        start_date = as.Date(paste0(year, '-', month, '-', day))
        
        rm(ds, month, year)
        
        # end date
        ds = dates[length(dates)]
        year = substr(ds, 1, 4)
        if (as.numeric(substr(ds,5,6)) == 0) month='12' else month=substr(ds,5,6)
        if (as.numeric(substr(ds,7,8)) == 0) {
          end_date = as.Date(paste (1, as.numeric(month)+1, year, sep='-'), format='%d-%m-%Y') - 1
          if (month == '12') end_date = as.Date(paste0(year, '-', month, '-31'))
        } else {
          end_date = as.Date(paste0(year, '-', month, '-', day))
        }
        
        rm(ds, month, year)
        
      }
      
      # check when file was last modified
      modification_time = file.mtime(paste0(path,files[i]))
      
      resolution = strsplit(tmp[3], '[.]')[[1]][1]
      
      # construct data frame with metadata and add it to the list
      odf[[i]] = data.frame(dataset=dataset, subdataset=subdataset, 
                            resolution=resolution, path=files[i], 
                            format=strsplit(extension(files[i]), '[.]')[[1]][2],
                            start=start_date, end=end_date, 
                            modified=modification_time, 
                            stringsAsFactors=F)
      
    }
    
    # combine original database with new data
    database = rbind(database, do.call(rbind, odf))
    
    #-------------------------------------------------------------------------#
    if (verbose) print('writting metadata') # ----
    #-------------------------------------------------------------------------#
    
    saveRDS(database, file=output_file)
    if (verbose) print('I am done! Enjoy!')
    
  } else {
    
    stop('no new files to add')
    
  }
  
}
