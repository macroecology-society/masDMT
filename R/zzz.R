.onLoad <- function(libname, pkgname) {
  
  # read configuration file
  config = file.path(system.file(package = 'masDMT', 'extdata'), 'config.yml')
  config = yaml::read_yaml(config)
  
  # register system variables
  options(dmt.data=config$dmt.data)
  options(dmt.catalog=config$dmt.catalog)
  options(dmt.toolbox=config$dmt.tools)
  
  #globalVariables(c('dataset', 'subdataset'))
  #if (Sys.which('gdalbuildvrt') == '') warning('missing GDAL')
  #if (Sys.which('python') == '') warning('missing python')
}
globalVariables(c("x", "a"))
