#' Create a sandbox containing small subsets of target datasets.
#' @param data_id Element or vector of class \emph{character}.
#' @importFrom dplyr tbl filter pull slice as_data_frame data_frame
#' @importFrom stats complete.cases
#' @return A \emph{data.frame}.
#' @details {When no argument is used, the function will return a 
#' \emph{data.frame} with a summary of the existing datasets and 
#' their general characteristics. When \emph{data.id} is provided, 
#' the function will search for the corresponding dataset and return 
#' a \emph{data.frame} reporting on the individual files that compose 
#' each of the target datasets.}
#'
#' @export

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

list_data <- function(data_id) {

  # access database
  database = readRDS(paste0(getOption('dmt.data'), 'database.rds'))
  
  # evaluate dataset argument
  if (missing(data_id)) {
    
    return(database)
  
  } else {
    
    ind = do.call(c, lapply(data_id, function(i) {
      tmp = strsplit(i, '/')[[1]]
      nr = length(tmp)
      colnames(database)[nr]
      colnames(database)
      
      # if only dataset name
      if (nr == 1) ind = which(database$dataset == tmp[1])
      
      # if dataset and subdaset names
      if (nr == 2) ind = which(database$dataset == tmp[1] & 
                           database$subdataset == tmp[2])
      
      # if dataset and subdaset names, and resolution
      if (nr == 3) ind = which(database$dataset == tmp[1] & 
                           database$subdataset == tmp[2] & 
                           database$resolution == tmp[3])
      
      return(ind)
      
    }))
    
    return(database[ind,])
    
  }

}
