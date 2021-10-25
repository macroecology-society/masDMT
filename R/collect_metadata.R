#' MAS database builder.
#' @param files \emph{Vector} listing files to evaluate
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
#' @seealso \code{\link{compile_metadata}}
#'
#' @export
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

collect_metadata = function(files) {
  
  # find combinations of dataset/resolution
  fid = do.call(rbind, lapply(files, function(i) {
    tmp = paste0(
      strsplit(
        strsplit(
          basename(i), paste0(dirname(i), '-'), fixed=T)[[1]][2], '_'
      )[[1]][3], collapse='_')
    
    tmp = strsplit(tmp, '[.]')[[1]]
    return(data.frame(fid=paste0(dirname(i), '_', tmp[1]), format=tmp[2]))
  }))
  
  # treat vector files as unique entities
  ind = which(fid$format %in% c('sqlite','spatialite'))
  fid$fid[ind] = paste0(fid$fid[ind],as.character(1:length(ind)))
  
  #  extract metadata for reference files
  # assign output to every related file
  odf = vector('list', length(files))
  ufid = unique(fid$fid) # unique ID's
  for (f in ufid) {
    
    # list related files
    ind = which(fid$fid == f)
    
    # full file path
    path = paste0(getOption('dmt.data'), files[ind[1]])
    
    # file extension
    extension = strsplit(basename(path), '[.]')[[1]][2]
    
    # access first file (used as reference)
    ids = files[ind[1]]
    
    # process raster data
    if (extension %in% c('vrt','tif')) {
      data_format = 'grid'
      img = raster(path)
      data_type = dataType(img)
      grid_res = res(img)[1]
      nr_bytes = as.numeric(substr(data_type, 4, 4))
      nr_files = NA
      rm(img)
    } else {
      data_type = NA
      nr_bytes = NA
    }
    
    # process vector data
    if (extension == 'spatialite') {
      grid_res = NA
      s = strsplit(basename(ids), '-')[[1]]
      s = paste0(s[2:length(s)], collapse='-')
      s = strsplit(s, '[_.]')[[1]]
      v = s[1] # variable name
      data_format = 'vector'
      db = dbConnect(SQLite(), path)
      nr_files = dbGetQuery(db, paste0('select COUNT(*) from ', v))[1,1]
      dbDisconnect(db)
      rm(db)
    }
    
    # process tabular data
    if (extension == 'sqlite') {
      grid_res = NA
      s = strsplit(basename(ids), '-')[[1]]
      s = paste0(s[2:length(s)], collapse='-')
      s = strsplit(s, '[_.]')[[1]]
      v = s[1] # variable name
      data_format = 'table'
      db = dbConnect(SQLite(), path)
      nr_files = dbGetQuery(db, paste0('select COUNT(*) from ', v))[1,1]
      dbDisconnect(db)
      rm(db)
    }
    
    # prepare output table
    for (i in ind) {
      
      # target file
      ids = files[i]
      
      dataset = dirname(ids)
      
      tmp = strsplit(ids, paste0(dataset, '-'), fixed=T)[[1]][2]
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
      modification_time = file.mtime(paste0(getOption('dmt.data'),files[i]))
      
      pixel_resolution = strsplit(tmp[3], '[.]')[[1]][1]
      
      # construct data frame with metadata and add it to the list
      odf[[i]] = data.frame(dataset=dataset, subdataset=subdataset, 
                            resolution=pixel_resolution, pixel_size=grid_res, 
                            path=files[i], format=data_format, type=data_type, 
                            nr_bytes=nr_bytes, nr_files=nr_files, 
                            start=start_date, end=end_date, 
                            modified=modification_time, 
                            stringsAsFactors=F)
      
    }
    
  }
  
  return(do.call(rbind, odf))
  
}
