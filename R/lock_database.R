#' Lock database to allow for unobstructed reading/writing
#' @return A lock file
#' @details Creates a file lock that avoids changes to a datavase registry.
#' @importFrom filelock lock
#'
#' @export

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

.lock_database = function() {
  
  file_lock = file.path(getOption('dmt.data'), 'database.lock')
  
  f = function(x) {
    connection <- file(file_lock)
    writeLines(c('locked'), connection)
    close(connection)
    file.exists(file_lock)
  }
  
  fc = FALSE
  
  while(!fc) {
    if (!file.exists(file_lock)) {
      fc = tryCatch(f(file_lock), error=function(e) return(FALSE))}
  }
  
  return(list(path=file_lock, lock=lock(file_lock)))
  
}
