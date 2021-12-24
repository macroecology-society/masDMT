#' Tool to open database file
#' @param database Optional. data.frame with metadata.
#' @return a data.frame, if \emph{database} is provided.
#' @details {Returns an existing database registry as a data.frame if 
#' \emph{database} is missing, the function updates the registry.}
#'
#' @export
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

.access_database = function(database) {
  
  # path to database
  path = file.path(getOption('dmt.data'), 'database.rds')
  fe = file.exists(path)
  if (!fe & missing(database)) {
    
    database = data.frame(dataset=as.character(), subdataset=as.character(), 
                          resolution=as.character(), path=as.character(), 
                          format=as.character(), start=as.Date(as.integer()), 
                          end=as.Date(as.integer()), 
                          modified=as.POSIXct(as.integer()))
    
    return(database)
    
  }
  
  #---------------------------------------------------------------------------#
  # open existing database
  #---------------------------------------------------------------------------#
  
  if (missing(database) & fe) return(readRDS(path))
  
  #---------------------------------------------------------------------------#
  # test new database file
  #---------------------------------------------------------------------------#
  
  # test input before adding it to the output
  if (!missing(database)) {
    
    # test format
    if (!is.data.frame(database)) stop('"database" not a data.frame')
    
    # check for existence of data
    if (nrow(database) == 0) stop('"database" has no data')
    
    # variables in input
    cn = colnames(database)
    
    # variables that should exist in input
    tn = c('dataset','subdataset','resolution',
           'path','format','start','end','modified')
    
    # which variables are missing?
    mi = which(!tn %in% cn)
    
    # inform used of missing variables (exit when missing)
    if (length(mi) > 0) stop(paste0('columns ', 
                                    paste0(tn[mi], collapse=', '), 
                                    ', missing'))
    
    # sort database columns
    database = database[,tn]
    
    # check format of variables
    ff = unname(unlist(sapply(colnames(database), 
                              function(i) class(database[,i]))))
    
    # formats that should exist in input
    tf = c('character','character','character',
           'character','character','Date','Date',
           'POSIXct','POSIXt')
    
    # identify variables with mismatching formats
    mi = which(tf != tf)
    
    # report which variables have the wrong format
    if (length(mi) > 0) stop(paste0('columns ', 
                                    paste0(tn[mi], collapse=', '), 
                                    ', have invalid formats'))
    
    # update database
    saveRDS(database, path)
    
  }
  
}
