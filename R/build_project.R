#' Build project data structure
#' @param out.path Output directory where data structure will be built.
#' @return A standardized file structure.
#' @details {This function returns a standardized data structure to ease data and 
#' code transferability. The final data structure includes the following folders:
#' \itemize{
#'   \item{\emph{00_data -} Standardized data storage}
#'   \item{\emph{01_analysis -} output of data analysis}
#'   \item{\emph{02_documents -} General documents folder}
#'   \item{\emph{03_code -} Storage of  project scripts}
#'   \item{\emph{temp - temporary folder (used e.g. for testing)}}
#' }}
#' @examples {}
#'
#' @export

#------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------#

build_project = function(out.path) {

  #----------------------------------------------------------------------------------------------------------------------------#
  # 0. check input argument
  #----------------------------------------------------------------------------------------------------------------------------#
  
  if (!is.character(out.path)) stop('"out.path" is not a string character')
  if (length(out.path) > 1) stop('"out.path" has more than 1 element')
  if (!dir.exists(out.path)) {
    check = tryCatch(dir.create(out.path), error=function(e) TRUE)
    if (check) stop('could not create folder with given "out.path"')
  }
  
  #----------------------------------------------------------------------------------------------------------------------------#
  # 1. create folder structure
  #----------------------------------------------------------------------------------------------------------------------------#
  
  dirs = list()
  
  dirs$data = file.path(out.path, '00_data', .Platform$file.sep)
  if (!dir.exists(dirs$data)) dir.create(dirs$data)
  
  dirs$analysis = file.path(out.path, '01_analysis', .Platform$file.sep)
  if (!dir.exists(dirs$analysis)) dir.create(dirs$analysis)
  
  dirs$documents = file.path(out.path, '02_documents', .Platform$file.sep)
  if (!dir.exists(dirs$documents)) dir.create(dirs$documents)
  
  dirs$code = file.path(out.path, '03_code', .Platform$file.sep)
  if (!dir.exists(dirs$code)) dir.create(dirs$code)
  
  dirs$temp = file.path(out.path, 'temp', .Platform$file.sep)
  if (!dir.exists(dirs$temp)) dir.create(dirs$temp)
  
  # write readme
  sink(file.path(out.path, 'README.txt'))
  cat("Description of data folder structure/content:")
  cat("\n")
  cat("00_data - Standardized database (should contain project data following strict data structure/standards)")
  cat("\n")
  cat("01_analysis - Contains analysis using content of 00_data, including stats, plots, images, ...")
  cat("\n")
  cat("02_documents - Stores documents (e.g. literature, manuscripts),  reference information (e.g. CSV with info. needed for scripts), QGis mapping projects, ...")
  cat("\n")
  cat("03_code -  Folder for scripts")
  cat("\n")
  cat("temp - General purpose folder; useful to test code, check new downloaded data,")
  sink()
  
  warning('Done! Enjoy :)')
  return(dirs)
  
}