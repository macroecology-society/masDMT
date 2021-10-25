#' Create metadata entry for the MAS database
#' @param params \emph{Yaml} file with required metadata.
#' @return A Rmarkdown file that can be fed into the MAS data catalog.
#' @importFrom yaml read_yaml
#' @importFrom raster extension
#' @importFrom leaflet leaflet setView addPolygons addTiles
#' @importFrom mapview mapshot
#' @importFrom rmarkdown render
#' @importFrom methods as
#' @details {The function parses a yaml file with metadata on a given dataset 
#' to create a standardized record of it. This record can then be integrated 
#' in the data catalog of the MAS research group. If no yaml is provided, the 
#' function will return a template as a variable, which can be saved with 
#' \link[yaml]{write_yaml}}.
#'
#' @export
#'
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

compile_dataset_metadata <- function(params='params.yml') {
  
  if (missing(params)) {
    
    # when missing user-provided parameters, return example file 
    input = paste0(system.file(package='masDMT'), '/inst/extdata/meta_example.yml')
    return(read_yaml(input))
  
  } else {
    
    #---------------------------------------------------------------------------#
    # test quality of input
    #---------------------------------------------------------------------------#
    
    # define output directory
    output = file.path(dirname(params), .Platform$file.sep)
    
    # try to read parameter file
    params = tryCatch(read_yaml(params), error=function(e) return(FALSE))
    if (is.logical(params)) {
      stop('Could not open the input file. Is it a yaml?')
    }
    
    # get names of parameters and report missing ones
    n = names(params)
    i = which(!n %in% c('title', 'preview_image', 'short_description', 
                        'long_description', 'metadata', 'categories',
                        'subdatasets', 'author', 'documentation', 
                        'license','version'))
    
    # check if parameters are missing and, when so, report which ones
    if (length(i) > 0) {
      stop(paste0('the following parameters are missing from the yaml: ', 
                  paste0(n[i], collapse=', ')))
    }
    
    # check if "subdatasets can be turned into a data.frame
    subdatasets = tryCatch(
      data.frame(data_id=params$subdatasets$data_id, 
                 description=params$subdatasets$description, 
                 stringsAsFactors=F), 
      error=function(e) return(FALSE)
    )
    
    # check if "subdatasets" contains all relevant information
    if (is.logical(subdatasets)) {
      stop(cat('\ncould not read subdatasets parameter. Potential reasons:\n
         i) lack of relevant fields (i.e. data_id, description)\n
         ii) the fields are not of the same length'))
    }
    #-------------------------------------------------------------------------#
    # create map with spatial extent of dataset
    #-------------------------------------------------------------------------#
    
    s = as(extent(params$metadata$spatial_extent), 'SpatialPolygons')
    
    # create global map with leaflet
    map = setView(
      addPolygons(
        addTiles(
          leaflet(s, width=510, height=350)
          ), color="red", opacity=0.3, fillColor="red"), 0,0, zoom=1)
    
    # save map as image
    extentImg = paste0(output, params$title, '_extent.png')
    mapshot(map, file=extentImg, cliprect='viewport', vheight=350, vwidth=510)
    
    #-------------------------------------------------------------------------#
    # format parameters with multiple entries
    #-------------------------------------------------------------------------#
    
    # list of categories
    categories = unname(sapply(params$categories, function(i) paste0('  - ', i)))
    
    # entries of table with subdataset names and descriptions
    subdatasets = sapply(1:nrow(subdatasets), function(i) 
      paste0('|`', subdatasets$data_id[i], '`|', 
             subdatasets$description[i], '|'))
    
    # determine if citation is present
    reference = paste0(params$title, '.bib')
    if (!file.exists(paste0(output, reference))) {
      reference = NULL
    } else {
      reference = paste0('bibliography: ', reference)
    }
    
    preview_image = paste0(params$title, '.png')
    if (!file.exists(paste0(output, preview_image))) {
      preview_image = NULL
    } else {
      preview_image = paste0('preview: ', preview_image)
    }
    
    #-------------------------------------------------------------------------#
    # write data into file
    #-------------------------------------------------------------------------#
    
    ifile = paste0(output, params$title, '.Rmd')
    
    connection <- file(ifile)
    
    id1 = params$title
    id2 = paste0(id1, '/', params$subdatasets$data_id[1])
    id3 = paste0(id2, '/', paste0(strsplit(params$metadata$spatial_resolution[1], ' ')[[1]], collapse=''))
    
    writeLines(
      c(
        '---', 
        paste0('title: "', params$title, '"'), 
        'description: |', 
        paste0('  ', params$short_description), 
        preview_image, 
        paste0('slug: ', params$title), 
        'categories:', 
        categories, 
        paste0('date: ', Sys.Date()), 
        'output:', 
        '  distill::distill_article:', 
        '    highlight_downlit: false', 
        reference, 
        '---', 
        '  ', 
        '```{r setup, include=FALSE}', 
        'knitr::opts_chunk$set(echo = FALSE)', 
        '```', 
        '', 
        '<div width="80%;">', 
        '<h3>Details</h3>', 
        '<p align="justify">', 
        params$long_description, 
        '</p>', 
        '', 
        '<h3>Subdatsets</h3>', 
        '| name                              | description                 |', 
        '|---------------------------------- |-----------------------------|', 
        subdatasets, 
        '</div>', 
        '', 
        '<aside>', 
        '<p>**Format**</p>', 
        paste0('<p>', params$metadata$format, '</p>'), 
        '<p>**Spatial extent:**</p>', 
        paste0('<img src="', extentImg, '" width=120, height=90>'), 
        '<p>**Temporal frequency:**</p>', 
        paste0('<p>', params$metadata$temporal_resolution, '</p>'), 
        '<p>**Documentation**</p>', 
        paste0('<p><a href="', params$documentation, '">Click here for more</a></p>'), 
        '<p>**license:**</p>', 
        paste0('<p>', params$license, '</p>'), 
        '<hr style="margin-top:0;margin-bottom:0;">', 
        '<p>**Version**</p>', 
        paste0('<p>', params$version, '</p>'), 
        '<p>**Resolution**</p>', 
        paste0('<p>', params$metadata$spatial_resolution, '</p>'), 
        '<p>**Time frame**</p>', 
        paste0('<p>', params$metadata$temporal_range[1], ' to ', 
               params$metadata$temporal_range[2], '</p>'), 
        '</aside>', 
        '', 
        '<hr>', 
        '', 
        '```{r eval=FALSE, echo=TRUE}', 
        '# data call without specifying subdataset and resoluton', 
        paste0('list_data("', id1, '")'), 
        '', 
        '', 
        '# data call for specific subdataset ', 
        paste0('list_data("', id2, '")'),
        '', 
        '', 
        '# data call to subdataset with a specific resolution', 
        paste0('list_data("', id3, '")'), 
        '```', 
        '', 
        '<aside>', 
        '<img src="https://raw.githubusercontent.com/macroecology-society/masDMT/master/docs/sidebar_credits.png" width="100%;">', 
        '<p>**Where there  mistakes?**</p>', 
        paste0('[Propose an edit at our GitHub repository](https://github.com/macroecology-society/data-catalog/tree/main/_posts/', params$title, '/', params$title, '.Rmd)'), 
        '<p>**Interested in out work?**</p>', 
        '<p>[Find us through our group page](https://www.idiv.de/en/groups-and-people/core-groups/macroecosocial.html)</p>', 
        '</aside>', 
        '', 
        '<hr>'
      ), 
      connection
    )
    
    close(connection)
    render(ifile)
    
  }
  
}
