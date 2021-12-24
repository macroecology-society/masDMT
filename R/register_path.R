#' Register relevant paths
#' @param paths List of parameters.
#' @importFrom yaml read_yaml write_yaml
#' @return Writes file into standardized file structure.
#' @details {Registers paths of different material of a project. By default, 
#' the function accepts a the following keywords, provided in a named list:
#' \itemize{
#'  \item{"dmt.data"}{Path to database (default set to iDiv's MAS drive)}
#'  \item{"dmt.catalog"}{Path to data catalog (default set to 
#'  \href{https://macroecology-society.github.io/data-catalog/}{MAS data catalog})}
#'  \item{"dmt.toolbox"}{Path to the documentation of a toolbox for a project}
#' }
#' The provided paths will be recorded in a configuration file within 
#' \emph{masDMT}, meaning they will set as defaults in future R sessions.}
#' @importFrom yaml read_yaml write_yaml
#' @seealso \code{\link{list_data}} \code{\link{documentation}}
#'
#' @export
#' 
#' 

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

register_path <- function(paths) {
  
  if (!is.list(paths)) stop('"paths" is not a list')
  if (is.null(names(paths))) stop('"paths" are missing santioned named')
  
  input = file.path(system.file(package = 'masDMT', 'extdata'), 'config.yml')
  config = read_yaml(input)
  pnames = names(paths)
  
  # register system variables
  options(paths)
  
  # update registration file
  for (p in pnames) config[[p]] = paths[[p]]
  
  # update yaml file
  write_yaml(config, input)
  
}
