#' Knits a metadata entry for the MAS database
#' @param params \emph{Yaml} file with required metadata.
#' @return A Rmarkdown file that can be fed into the MAS data catalog.
#' @importFrom yaml read_yaml
#' @importFrom raster extension extent
#' @importFrom leaflet leaflet setView addPolygons addTiles
#' @importFrom mapview mapshot
#' @importFrom rmarkdown render
#' @importFrom methods as
#' @importFrom pingr ping
#' @importFrom RefManageR Citet ReadBib
#' @details {The function parses a yaml file with metadata on a given 
#' dataset to create a standardized, html record of it. This record can 
#' then be added to the data catalog of the MAS research group.}
#'
#' @export
#'
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

knit_descriptor <- function(params) {
  
  #---------------------------------------------------------------------------#
  # test quality of input ----
  #---------------------------------------------------------------------------#
  
  # define output directory
  dname = dirname(params)
  output = file.path(dname, .Platform$file.sep)
  
  # try to read parameter file
  params = tryCatch(read_yaml(params), error=function(e) return(FALSE))
  if (is.logical(params)) {
    stop('Could not open the input file. Is it a yaml?')
  }
  
  # get names of parameters and report missing ones
  required_parameters = c('title', 'dataset_id',  'short_description', 'long_description', 
                          'documentation', 'data_access', 'license', 
                          'version', 'categories', 'status', 
                          'metadata', 'subdatasets')
  n = names(params)
  i = which(!required_parameters %in% n)
  
  # check if parameters are missing and, when so, report which ones
  if (length(i) > 0) {
    stop(paste0('the following parameters are missing from the yaml: ', 
                paste0(required_parameters[i], collapse=', ')))
  }
  
  # validate user-specified data format
  if (params$metadata$format %in% c('grid', 'vector', 'table')) {
    stop('"format" is not a valid keyword')
  } else {
    data_format = params$metadata$format
  }
  
  #---------------------------------------------------------------------------#
  # create map with spatial extent of dataset ----
  #---------------------------------------------------------------------------#
  
  # create spatial object from spatial extent of dataset
  s = as(extent(params$metadata$spatial_extent), 'SpatialPolygons')
  
  # create global map with leaflet
  map = setView(
    addPolygons(
      addTiles(
        leaflet(s, width=510, height=350)
        ), color="red", opacity=0.3, fillColor="red"), 0,0, zoom=1)
  
  # save map as image
  extentImg = paste0(output, params$data_id, '_extent.png')
  mapshot(map, file=extentImg, cliprect='viewport', vheight=350, vwidth=510)
  extentImg = basename(extentImg)
  
  #---------------------------------------------------------------------------#
  # format parameters with multiple entries ----
  #---------------------------------------------------------------------------#
  
  # list of categories
  categories = unname(sapply(params$categories, function(i) paste0('  - ', i)))
  
  #---------------------------------------------------------------------------#
  # compile subdataset entries ----
  #---------------------------------------------------------------------------#
  
  # list subdataset unique ID's
  data_id = names(params$subdatasets)
  
  # apply html formatting
  subdatasets = list()
  i = 0
  for (d in data_id) {
    
    # initiate subdataset container
    i = i + 1
    subdatasets[[i]] = paste0("<details style='cursor:pointer;border:none;", 
                              "box-shadow: rgba(0, 0, 0, 0.05) 0px 6px 24px ",
                              "0px, rgba(0, 0, 0, 0.08) 0px 0px 0px 1px;",
                              "padding:10px;margin:0;'>")
    
    # add subdataset name
    i = i + 1
    subdatasets[[i]] = paste0('<summary style="font-size:16px;">`', 
                              d, '`<span style="color:#C8C8C8;"> | </span>', 
                              params$subdatasets[[d]]$variable, '</summary>')
    
    # add table with details of variable's content
    i = i + 1
    subdatasets[[i]] = '<hr style="margin-top:0;margin-bottom:20px;">'
    i = i + 1
    subdatasets[[i]] = '|content|description|'
    i = i + 1
    subdatasets[[i]] = '|-------|-----------|'
    content = names(params$subdatasets[[d]]$content)
    for (v in content) {
      i = i + 1
      subdatasets[[i]] = paste0('|', v, '|', 
                                params$subdatasets[[d]]$content[[v]], '|')
    }
    
    # close details section
    i = i + 1
    subdatasets[[i]] = '</details>'
    
  }
  
  # make vector of parameter list
  subdatasets = unlist(subdatasets)
  
  #---------------------------------------------------------------------------#
  # detect preview image ----
  #---------------------------------------------------------------------------#
  
  preview_image = paste0(params$dataset_id, '.png')
  if (!file.exists(paste0(output, preview_image))) {
    preview_image = NULL
  } else {
    preview_image = paste0('preview: ', preview_image)
  }
  
  #---------------------------------------------------------------------------#
  # check data status ----
  #---------------------------------------------------------------------------#
  
  if (!params$status %in% c('planned','developing','available')) {
    stop(paste0('"', params$status, '" is not a valid keyword')) 
  }
  
  if (params$status == 'planned') status_color = '#4682B4'
  if (params$status == 'developing') status_color = '#2E8B57'
  if (params$status == 'available') status_color = '#228B22'
  
  #---------------------------------------------------------------------------#
  # check documentation ----
  #---------------------------------------------------------------------------#
  
  if (length(grep('@', params$documentation)) > 0) {
    documentation = paste0("<p style='margin-top:0;margin-bottom:0;'>",
                           '<a style="margin:0;padding:0;" href="mailto:', 
                           params$documentation, '?subject=Data information request: ', 
                           params$tile, '">documentation</a></p>')
    
  } else {
    p = ping(tryCatch(strsplit(params$documentation, 
                               '//', fixed=T)[[1]][2], 
                      error=function(e) return(FALSE)))
    
    if (max(p, na.rm=T) < 0) {
      documentation = paste0("<p style='margin-top:0;margin-bottom:0;'>",
                             "<a style='margin:0;padding:0;' href='", 
                             params$documentation, "'>documentation</a></p>")
    } else {
      documentation = 'Undocumented'
    }
  }
  
  #---------------------------------------------------------------------------#
  # check data access ----
  #---------------------------------------------------------------------------#
  
  if (length(grep('@', params$data_access)) > 0) {
    data_access = paste0("<p style='margin-top:0;margin-bottom:0;'>",
                           '<a style="margin:0;padding:0;" href="mailto:', 
                           params$data_access, '?subject=Data access request: ', 
                           params$tile, '">Data access</a></p>')
    
  } else {
    p = ping(tryCatch(strsplit(params$data_access, '//', fixed=T)[[1]][2], 
                      error=function(e) return(FALSE)))
    if (max(p, na.rm=T) < 0) {
      data_access = paste0("<p style='margin-top:0;margin-bottom:0;'>",
                           "<a style='margin:0;padding:0;' href='", 
                             params$data_access, "'>Data access</a></p>")
    } else {
      data_access = 'Restricted data acess'
    }
  }
  
  #---------------------------------------------------------------------------#
  # check bibtex ----
  #---------------------------------------------------------------------------#
  
  bib = file.path(dname, paste0(params$dataset_id, '.bib'))
  
  if (file.exists(bib)) {
    
    # read bibtex
    bib = ReadBib(bib)
    
    # compile citation strings
    citations = unlist(unname(lapply(1:length(bib), function(i) {
      return(paste0('<p>[', as.character(i), '] ', Citet(bib[i]), '</p>'))
    })))
    
    # specify where to find the file in the file system
    ifile = paste0("https://raw.githubusercontent.com/data-catalog/main/_posts/", 
                   params$dataset_id, "/", params$dataset_id, ".bib")
    
    # suggest output name
    ofile = paste0(params$dataset_id, "/", params$dataset_id, ".bibtex")
    
    # add HTML formatting
    citations = c(
      '<h3 style="margin-bottom:0;">References</h3>', 
      paste0("<p style='margin-top:0;margin-bottom:5;'>", "
             <a target='_blank' href='", ifile, "' download='", 
             ofile, "'><small>(bibtex)</small></a></p>"), 
      citations
      )
    
  } else {
    
    citations = ''
    
  }
  
  #---------------------------------------------------------------------------#
  # write data into file ----
  #---------------------------------------------------------------------------#
  
  ifile = paste0(output, params$dataset_id, '.Rmd')
  
  connection <- file(ifile)
  
  id1 = params$dataset_id
  id2 = paste0(id1, '/', names(params$subdatasets)[1])
  id3 = paste0(id2, '/', 
               paste0(strsplit(params$metadata$spatial_resolution[1], 
                               ' ')[[1]], collapse=''))
  
  writeLines(
    c(
      '---', 
      paste0('title: "', params$title, '"'), 
      'description: |', 
      paste0('  ', strsplit(params$short_description, '\n')[[1]]), 
      preview_image, 
      paste0('slug: ', params$dataset_id), 
      'categories:', 
      categories, 
      paste0('date: ', Sys.Date()), 
      'output:', 
      '  distill::distill_article:', 
      '    highlight_downlit: false', 
      '---', 
      '  ', 
      '```{r setup, include=FALSE}', 
      'knitr::opts_chunk$set(echo = FALSE)', 
      '```', 
      '', 
      '<div style="width=80%;margin-top:0;margin-bottom:0;">', 
      '<h3 style="margin-top:5;margin-bottom:5;">Details</h3>', 
      '<p align="justify">', 
      paste0(strsplit(params$long_description, '\n')[[1]], collapse=''), 
      '</p>', 
      '', 
      '<h3 style="margin-top:5;margin-bottom:5;">Subdatasets</h3>',
      subdatasets, 
      '<h3 style="margin-top:5;margin-bottom:5;">masDMT query</h3>', 
      '<div style="width=80%;margin-top:5;margin-bottom:5;background-color:#777777;">',
      '```{r eval=FALSE, echo=TRUE}', 
      '# data call without specifying subdataset and resolution', 
      paste0('list_data("', id1, '")'), 
      '', 
      '# data call for specific subdataset ', 
      paste0('list_data("', id2, '")'),
      '', 
      '# data call to subdataset with a specific resolution', 
      paste0('list_data("', id3, '")'), 
      '```', 
      '</div>', 
      '<hr style="margin-top:0;margin-bottom:0;">', 
      citations, 
      '<hr>', 
      '</div>', 
      '', 
      '<aside>', 
      paste0('<p style="padding:5px;margin-top:0;margin-bottom:20px;', 
             'border-radius:10px;', 'text-align:center;background:', 
             status_color,';color:#ffffff;','height:25px;width:70px;',
             'box-shadow: rgba(9, 30, 66, 0.25) 0px 4px 8px -2px, ',
             'rgba(9, 30, 66, 0.08) 0px 0px 0px 1px;">',
             params$status,'</p>'), 
      '<hr style="margin-top:0;margin-bottom:0;">', 
      '<p style="margin-top:0;margin-bottom:0;">**Format**</p>', 
      paste0('<p style="margin-top:0;">', data_format, '</p>'), 
      '<p style="margin-top:0;">**Spatial extent:**</p>', 
      paste0('<img src="', extentImg, '" width=120, height=90>'), 
      '<p style="margin-top:0;margin-bottom:0;">**Temporal frequency:**</p>', 
      paste0('<p style="margin-top:0;">', params$metadata$temporal_resolution, '</p>'), 
      '<p style="margin-top:0;margin-bottom:0;">**Resolution**</p>', 
      paste0('<p style="margin-top:0;">', params$metadata$spatial_resolution, '</p>'), 
      '<p style="margin-top:0;margin-bottom:0;">**Time frame**</p>', 
      paste0('<p style="margin-top:0;">', params$metadata$temporal_range[1], 
             ' to ', params$metadata$temporal_range[2], '</p>'), 
      '<hr style="margin-top:0;margin-bottom:0;">', 
      paste0('<p>', documentation, '</p>'), 
      paste0('<p>', data_access, '</p>'), 
      '<hr style="margin-top:0;margin-bottom:0;">', 
      '<p style="margin-top:0;margin-bottom:0;">**license:**</p>', 
      paste0('<p style="margin-top:0;">', params$license, '</p>'), 
      '<p style="margin-top:0;margin-bottom:0;">**Version**</p>', 
      paste0('<p style="margin-top:0;">', params$version, '</p>'), 
      '<br>', 
      '<br>', 
      '<img src="https://raw.githubusercontent.com/macroecology-society/masDMT/master/docs/sidebar_credits.png" width="120;">', 
      '<br>',
      '<br>', 
      '<p style="margin-bottom:0;padding:0;">**Where there  mistakes?**</p>', 
      paste0("<p><a href='https://github.com/macroecology-society/data-catalog/tree/main/_posts/", 
             params$dataset_id, "/", params$dataset_id, ".yml'>Propose an edit at our GitHub repository</a></p>"), 
      '</aside>'
    ), 
    connection
  )
  
  close(connection)
  render(ifile)
  
}
