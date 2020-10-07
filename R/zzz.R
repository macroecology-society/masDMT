.onLoad <- function(libname, pkgname) {
  options(dmt.data='/data/idiv_meyer/00_data/processed/')
  #globalVariables(c('dataset', 'subdataset'))
  if (Sys.which('gdalbuildvrt') == '') warning('missing GDAL')
  if (Sys.which('python') == '') warning('missing python')
}
