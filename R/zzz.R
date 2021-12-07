.onLoad <- function(libname, pkgname) {
  options(dmt.data='/data/idiv_meyer/00_data/processed/')
  options(dmt.catalog='https://macroecology-society.github.io/data-catalog/')
  options(dmt.toolbox='https://macroecology-society.github.io/masDMT/')
  #globalVariables(c('dataset', 'subdataset'))
  #if (Sys.which('gdalbuildvrt') == '') warning('missing GDAL')
  #if (Sys.which('python') == '') warning('missing python')
}
