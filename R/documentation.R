#' Access documentation websites
#'
#' @param source What documentation to access? 'Choose `catalog` or `toolbox`. 
#' @details {Helper tool to access documentation websites of the MAS database. 
#' If the user chooses `catalog`, the function will open the website for the 
#' data catalog. If the user chooses `toolbox`, the function will open the 
#' website for the masDMT R package.}
#' @importFrom utils browseURL
#' @export

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

documentation <- function(source) {
  
  # test input
  if (!is.character(source)) stop('input not valid (should be a character)')
  if (!length(source) > 1) stop('cannot open more than 1 entry at once')
  if (!source %in% c('catalog','toolbox')) stop('entry is not a valid keyword')
  
  # launch requested page
  if (source == 'catalog') browseURL(getOption('dmt.catalog'))
  if (source == 'toolbox') browseURL(getOption('dmt.toolbox'))
  
}
