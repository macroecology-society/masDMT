#' Create a sandbox containing small subsets of target datasets.
#' @param data.id Element or vector of class \emph{character}.
#' @param range Two-element vector of class \emph{character} with desired start and end dates (optional, in yyyy-mm-dd format).
#' @param bbox Four-element vector with target bounding box, composed of min. x, min. y, max. x and max. y coordinates.
#' @param report.only Logical. Should the function only report on storage requirements?
#' @param non.spatial {Optional argument. How should non-spatial data be treated? Provide one of the following:
#'   \itemize{
#'     \item{\emph{a character element} with an SQL query}
#'     \item{\emph{a numeric element} specifying how many records to randomly select}
#'    }
#' }
#' @return A mirror of the data structure on the cluster, containing a subset of the target datasets.
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#' @importFrom terra rast res
#' @importFrom sf st_write
#' @details {The function creates a copy of the datasets specified in \emph{data.id} within the area defined
#' by \emph{bbox} and temporal range determined by \emph{range}. When \emph{report.only} is set to TRUE, the
#' function will provide a \emph{data.frame} reporting on the amount of data per individual file that fits to
#' the search criteria. When set to false, the function will inform the user on the amount of data that will
#' be processed and request an output folder to write the data to. Note that the estimates are fairly accurate
#' for gridded and tabular data. For gridded data, the area within a \emph{bbox} will be consistently covered by pixels, and a data volume
#' is estimated from the amount of cells within it and the data format. For vector data, this is just an
#' estimate that assumes that the target area is consistently covered by all vector entries in the target
#' dataset. this does not consider vector complexity or the actual amount of overlapping vectors.}
#' @examples {}
#'
#' @export

#------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------#

build_sandbox <- function(data.id, bbox, range, non.spatial=NULL, report.only=FALSE) {

  #----------------------------------------------------------------------------------------------------------------------------#
  # 1. test input arguments ---
  #----------------------------------------------------------------------------------------------------------------------------#

  if (!is.character(data.id)) stop('"data.id" is not a character vector')
  if (!is.numeric(bbox)) stop('"bbox" is not a numeric vector')
  if (!length(bbox) != 4) stop('"bbox" does not have 4 elements')
  if (!is.character(range)) stop('"range" should be a character vector (format yyy-mm-dd)')
  if (length(range) != 2) stop('"range" should be a 2-element vector')
  if (!is.logical(report.only)) {stop('"report.only" is not a logical argument')}
  if (!is.null(non.spatial)) {
    if (is.character(non.spatial)) method = 'sql'
    if (is.numeric(non.spatial)) method = 'random'
  } else {
    method = NULL
  }

  #----------------------------------------------------------------------------------------------------------------------------#
  # 2. search for nonexistent datasets ---
  #----------------------------------------------------------------------------------------------------------------------------#

  data = list_data() # read reference metadata
  ind = which(data$data_id %in% data.id) # find requested datasets that don't exist

  # if there are missing datasets, report
  if (length(ind) == 0) stop(paste0(paste0(data.id[ind], collapse=','), ' not found in the database (maybe mispelled?)'))

  #----------------------------------------------------------------------------------------------------------------------------#
  # 2. find and analyze gridded datasets ---
  #----------------------------------------------------------------------------------------------------------------------------#

  file_report = do.call(rbind, lapply(data.id, function(id) {

    data = list_data(id) # find relevant files

    # subset data by date
    data$start = as.Date(data$start)
    data$end = as.Date(data$end)
    data = data[which(((data$start >= range[1]) & (data$end <= range[2])) | is.na(data$start)),]
    files = paste0(getOption('dmt.data'), data$path) # build file path

    if (data$format[1] == 'grid') {

      nr_records = 1 # count files associated to dataset
      pixel.res = res(rast(files[1]))[1] # pixel resolution
      xbytes = (bbox$xmax-bbox$xmin) / pixel.res * data$bytes[1] # nr. of bytes in x image range
      ybytes = (bbox$xmax-bbox$xmin) / pixel.res * data$bytes[1] # nr. of bytes in y image range
      pred_size = ((xbytes * ybytes) * 0.000001) # predicted size of subset in Mb

    }

    if (data$format[1] == 'vector') {

      idb = dbConnect(SQLite(), files) # connect to vector database
      nr_records = dbGetQuery(idb, paste0('select COUNT(*) from ', data$subdataset))[1,1] # count number of features
      dbDisconnect(idb) # disconnect from vector database
      original_size = file.info(files)$size/1000000 # account for original file size (in Mb)
      referenceArea = (data$xmax-data$xmin) * (data$ymax-data$ymin) # maximum area
      x_overlap = max(c(min(c(data$xmax, bbox$xmax)) - max(c(data$xmin, bbox$xmin)))) # overlap along the x coordinates
      y_overlap = max(c(min(c(data$ymax, bbox$ymax)) - max(c(data$ymin, bbox$ymin)))) # overlap along the y coordinates
      overlapArea = x_overlap * y_overlap # overlap area
      pred_size = (overlapArea/referenceArea) * original_size * nr_records # predicted size of subset in Mb

    }

    if (data$format[1] == 'table') {

      idb = dbConnect(SQLite(), files) # connect to table database

      # nr. records in database
      nr_records = dbGetQuery(idb, paste0('select COUNT(*) from ', data$subdataset))[1,1] # total
      if (is.null(method)) target_nr = nr_records
      if (method == 'sql') target_nr = dbGetRowCount(dbGetQuery(idb, non.spatial)) # records that fit the SQL query
      if (method == 'random') nr_records = non.spatial # pre-defined nr of records

      dbDisconnect(idb) # disconnect from vector database
      original_size = file.info(files)$size/1000000 # account for original file size (in Mb)
      pred_size = original_size * ((target_records)/nr_records) # predicted size of subset in Mb

    }

    # report on file composition
    return(data.frame(data.id=id, path=files, nr_records=nr_records, size_Mb=pred_size, format=data$format, stringsAsFactors=F))

  }))

  #----------------------------------------------------------------------------------------------------------------------------#
  # 3. find and analyze vector datasets ---
  #----------------------------------------------------------------------------------------------------------------------------#

  if (report.only) {

    return(file_report)

  } else {

    #==========================================================================================================================#
    # ask for use input ---
    #==========================================================================================================================#

    file.size = as.character(sum(file_report$size_Mb))
    r = paste0("Your output will consist of ", file.size, " Mb's. Shall I Proceed? (0=No, 1=Yes): ")
    r = as.numeric(readline(r))

    if (r == 1) {

      out.dir = readline('please provide an output directory: ')
      if (!dir.exists(out.dir)) {stop('provided directory does not exist :/')}

    #==========================================================================================================================#
    # build sandbox file structure
    #==========================================================================================================================#

      file_report$out_dir = paste0(out.dir, dirname(file_report$data.id), '/')
      unique.dirs = unique(file_report$out_dir)
      for (d in unique.dirs) if (!dir.exists(d)) dir.create(d)

    #==========================================================================================================================#
    # process gridded data
    #==========================================================================================================================#

      ind = which(file_report$format == 'grid')

      if (length(ind) > 0) {

        bname = paste0(tempfile(), '_', Sys.getpid()) # devise temporary file basename

        for (i in ind) {

          # make vrt of raster subset
          ifile = file_report$path[i]
          ofile = paste0(tmpDir, strsplit(basename(ifile), '[.]')[[1]][1], '.vrt')
          gdalbuildvrt(input_file_list=ifile, output.vrt=ofile, te=bbox, separate=TRUE)

          # translate subset into raster
          ifile = ofile
          ofile = paste0(file_report$out_dir, strsplit(basename(ifile), '[.]')[[1]][1], '.tif')
          gdal_translate(ifile, ofile, co=list('COMPRESS=deflate', 'ZLEVEL=9', 'PREDICTOR=2'))

        }

      }

      #==========================================================================================================================#
      # process vector data
      #==========================================================================================================================#

      ind = which(file_report$format == 'vector')

      if (length(ind) > 0) {

        for (i in ind) {

          s = access_vector(file_report$data.id[i], bbox=bbox)
          st_write(s, paste0(file_report$out_dir[i], basename(file_report$path)), driver='sqlite', append=F)

        }

      }

    #==========================================================================================================================#
    # process table data
    #==========================================================================================================================#

      ind = which(file_report$format == 'table')
  
      if (length(ind) > 0) {
  
        for (i in ind) {
  
          oname = paste0(file_report$out_dir[i], basename(file_report$path)) # output file name
  
          # without special requirements, simply copy data
          if (is.null(method)) file.copy(file_report$data.id[i], oname)
  
          # with special requirements, find relevant data
          if (!is.null(method)) {
  
            bn = basename(file_report$data.id[i]) # table name
  
            if (method == 'random') {
              non.spatial = paste0('SELECT TOP ', as.character(non.spatial), ' * FROM ', bn, ' ORDER BY NEWID()')
            }
  
            # access data in original database
            idb = dbConnect(SQLite(), file_report$data.id[i]) # connect to table database
            ods = dbFetch(dbGetRowCount(dbGetQuery(idb, non.spatial))) # parse SQL query
            dbDisconnect(idb) # disconnect from vector database
  
            # initiate copy
            idb = dbConnect(SQLite(), oname)
            dbWriteTable(ods, bn, overwrite=T)
            dbDisconnect(idb)
  
          }
  
        }
  
      }
      
    } else {
      
      warning('process interrupted :|')
      stop()
      
    }
    
  }

}
