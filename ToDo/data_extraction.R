require(raster)

#--------------------------------------------------------------------------------#
# read samples
#--------------------------------------------------------------------------------#

ddir = '/data/idiv_meyer/01_projects/Marina/00_data/'

samples = read.csv(paste0(ddir, 'toydata_2020-02-12_15_00.csv'), stringsAsFactors=F)
# variables = read.csv(paste0(ddir, 'target_covariates.csv'), stringsAsFactors=F)
variables = covariates # SE: I have included the table as the internal dataset 'covariates' (see ?covariates) so to make it easier to work with it. It will be loaded automatically wiht the package. If you want to update the object, do 'usethis::use_data(..., overwrite = TRUE)' and re-compile. Perhaps you can add more to the documentation in data.R

if (sum(!complete.cases(samples)) > 0) {
  samples = samples[complete.cases(samples),]
  print('Marina, there are missing values (!!!!!!)')
}

#--------------------------------------------------------------------------------#
# extract covariates
#--------------------------------------------------------------------------------#

bdir = '/data/idiv_meyer/00_data/processed/'

unique.years = sort(unique(samples$year))

covariates = lapply(unique.years, function(year) {

  ind = which(samples$year == year)

  tmp = do.call(cbind, lapply(1:nrow(variables), function(i) {

    # extract variable metadata
    prd = variables$product[i]
    var = variables$variable[i]
    res = variables$resolution[i]
    time = variables$time[i]

    # make file path
    if (time %in% c('yes', 'no')) {
      if (time == 'yes') {
        file = paste0(bdir, prd, '/', prd, '-', var, '_', as.character(year), '0101_', res, '.vrt')
      } else {
        file = list.files(paste0(bdir, prd, '/'), paste0(prd, '-', var, '_.*.', res, '.*.tif'), full.names=T)
      }
    } else {
      file = paste0(bdir, prd, '/', prd, '-', var, '_', time, '0101_', res, '.vrt')
    }

    r = raster(file)
    e = extract(r, samples[ind,c('long','lat')])

    df = data.frame(var=e)
    colnames(df) = var
    return(df)

  }))

  return(list(covariates=tmp, samples=samples[ind,]))

})

#--------------------------------------------------------------------------------#
# combine covariates and samples into single data frames
#--------------------------------------------------------------------------------#

samples_out = do.call(rbind, lapply(covariates, function(i) i$samples))
covariates_out = do.call(rbind, lapply(covariates, function(i) i$covariates))

# write result as an RDS file
trainingData = list(covariates=covariates_out, samples=samples_out)
oname = paste0(ddir, 'trainingData.rds')
saveRDS(covariates, oname)

write.csv(samples_out, paste0(ddir, 'samples.csv'), row.names=FALSE)
write.csv(covariates_out, paste0(ddir, 'covariates.csv'), row.names=FALSE)
