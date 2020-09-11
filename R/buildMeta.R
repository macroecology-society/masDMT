#' MAS database builder.
#' @param verbose Logical argument. Should the progress be reported? Default is FALSE.
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#' @importFrom raster raster extent dataType
#' @importFrom terra rast ext res
#' @importFrom RSQLite SQLite
#' @importFrom utils write.csv
#' @importFrom sf st_read
#' @return An SQLite database
#' @details Updates, or creates, SQL database with metadata on MAS data products.
#' @seealso \code{\link{access_grid}} \code{\link{access_vector}} \code{\link{list_data}}
#' @examples {}
#'
#' @export

.build_meta <- function(verbose=FALSE) {

  # 0. find input files ----

  # list input files
  f1 = unname(sapply(list.files(getOption('dmt.data'), '.tif', recursive=TRUE), function(i) strsplit(i, '[.]')[[1]][1]))
  f2 = unname(sapply(list.files(getOption('dmt.data'), '.vrt', recursive=TRUE), function(i) strsplit(i, '[.]')[[1]][1]))
  f2 = paste0(f2[!f2 %in% f1], '.vrt')
  f1 = paste0(f1[!f1 %in% f2], '.tif')
  f3 = list.files(getOption('dmt.data'), '.sqlite', recursive=TRUE)
  files = c(f1, f2, f3)

  if (verbose) print('identified target data files')

  # 1. extract relevant metadata ----

  if (verbose) print('extracting metadata')

  odf <- do.call(rbind, lapply(files, function(i) {

    e = strsplit(basename(i), '[.]')[[1]][2] # extension

    # extract variables
    d <- dirname(i)
    if (e %in% c('vrt','tif')) {
      s <- strsplit(basename(i), '-')[[1]]
      s <- paste0(s[2:length(s)], collapse='-')
      s <- strsplit(s, '[_.]')[[1]]
      vn <- s[1] # variable name
      tmp <- strsplit(s[2], '[–_]')[[1]]
      st <- tmp[1] # start date
      st <- paste0(substr(st, 1, 4), '-', substr(st, 5, 6), '-', substr(st, 7, 8))
      ed <- tmp[length(tmp)] # end date
      ed <- paste0(substr(ed, 1, 4), '-', substr(ed, 5, 6), '-', substr(ed, 7, 8))
      r = rast(paste0(getOption('dmt.data'), i))
      pr = res(r)[1]
      e = ext(r)@ptr$vector
      rm(r)
      df = 'grid'
      dt = dataType(raster(paste0(getOption('dmt.data'), i)))
      nb = as.numeric(substr(dt, 4, 4))
    } else {
      vn = strsplit(basename(i), '[.]')[[1]][1]
      st = NA
      ed = NA
      pr = NA
      e = extent(st_read(paste0(getOption('dmt.data'), i)))
      df = 'vector'
      dt = NA
      nb = NA
    } # object extent

    # prepare output table
    tdf <- data.frame(path=i, dataset=d, subdataset=vn,
                     start=st, end=ed, resolution=pr, format=df, type=dt,
                     bytes=nb, xmin=e[1],xmax=e[2], ymin=e[3], ymax=e[4],
                     stringsAsFactors=F)

    return(tdf)

  }))

  odf$index = 1:nrow(odf) # add index

  # convert missing dates to NA
  odf$start[which(odf$start == 'NA--')] = NA
  odf$end[which(odf$end == 'NA--')] = NA

  # define data_id, which is used by the user to find data
  odf$data_id = paste0(odf$dataset, '/', odf$subdataset)

  # re-structure database
  odf = odf[,c('index','path','data_id','dataset','subdataset',
               'resolution','format','type','bytes','xmin','ymin',
               'xmax','ymax','start','end')]

  # 2. make database summary ----

  if (verbose) print('summarizing metadata')

  uv = unique(odf$data_id) # list unique dataset/subdataset combinations

  sdf = do.call(rbind, lapply(uv, function(d) {

    ind = which(odf$data_id == d) # find relevant files
    st = min(odf$start[ind]) # start time
    ed = max(odf$end[ind]) # end time
    ds = odf$dataset[ind[1]] # dataset name
    sds =odf$subdataset[ind[1]] # subdataset name
    bt = odf$bytes[ind[1]] # number of bytes per pixel
    dt = odf$type[ind[1]] # data type
    df = odf$format[ind[1]] # data format
    pr = odf$resolution[ind[1]] # pixel resolution
    xmin = min(odf$xmin[ind], na.rm=T) # starting x coordinate
    ymin = min(odf$ymin[ind], na.rm=T) # starting y coordinate
    xmax = max(odf$xmax[ind], na.rm=T) # ending x coordinate
    ymax = max(odf$ymax[ind], na.rm=T) # ending y coordinate

    # build output database
    tmp = data.frame(data_id=d, dataset=ds, subdataset=sds, resolution=pr,
                     format=df, type=dt, bytes=bt, start=st, end=ed,
                     xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax,
                     stringsAsFactors=F)

    return(tmp)

  }))

  # add spatial index and re-structure database
  sdf$index = 1:nrow(sdf)
  sdf = sdf[,c('index','data_id','dataset','subdataset',
            'resolution','format','type','bytes','xmin',
            'ymin','xmax','ymax','start','end')]

  #-------------------------------------------------------------------------------------------------#
  # 3. write csv files with metadata (overall and summary) and compile SQLite database
  #-------------------------------------------------------------------------------------------------#

  if (verbose) print('writting metadata')

  write.csv(sdf, paste0(getOption('dmt.data'), 'mas_datasets.csv'), row.names=F)
  write.csv(odf, paste0(getOption('dmt.data'), 'mas_subdatasets.csv'), row.names=F)

  odb <- dbConnect(SQLite(), paste0(getOption('dmt.data'), 'mas_database.sqlite'))
  dbWriteTable(odb, 'overview', sdf, overwrite=TRUE)
  dbWriteTable(odb, 'complete', odf, overwrite=TRUE)
  dbDisconnect(odb)

  print('SQLite database compiled!')

}
