###############################################################################
# Mosaics MODIS MCD43A4 (500m Nadir BRDF-Adjusted Reflectance ) product to 
# cover the spatial extent of the ZOI/CSA/PA boundary of each team site.
# 
# Requires a GDAL installation that supports HDF4 files - on Windows, see 
# OSGEO4W to meet this dependency.
###############################################################################

library(rgdal)
library(raster)
library(stringr)

library(doParallel)
library(foreach)

EVI <- function(blue, red, nir, ...) {
    ret <- overlay(blue, red, nir, fun=function(blue, red, nir) {
            (2.5*(nir - red)) / (1 + nir + 6*red - 7.5*blue)
        }, ...)
    return(ret)
}

n_cpus <- 12
overwrite <- TRUE

registerDoParallel(n_cpus)

in_base_dir <- '/localdisk/home/azvoleff/MODIS_NBAR_Reflectance'
out_base_dir <- '/localdisk/home/azvoleff/MODIS_NBAR_Reflectance'
in_folder <- in_base_dir
out_folder <- out_base_dir

dats <- dir(in_folder, pattern='.$')
sitecodes <- gsub('_', '', str_extract(dats, '_[A-Z]{2,3}_'))
dates <- as.Date(gsub('[_.dat]', '', str_extract(dats, '_[0-9]{7}.dat')), '%Y%j')

for (sitecode in unique(sitecode)) {
    timestamp()
    message('Processing ', sitecode, '...')

    these_dats <- dats[sitecodes == sitecode]
    if (length(these_dats) == 0) {
        stop('no files found for ', sitecode)
    }

    product <- unique(gsub('[.]', '', str_extract(these_dats, '^[a-zA-Z0-9]*[.]')))
    if (length(product) != 1) {
        stop('files for ', sitecode, ' are from more than one MODIS product')
    }

    these_dats <- these_dats[order(these_dates)]
    these_dates <- these_dates[order(these_dates)]

    # Blue, red, and near-infrared reflectances in MODIS product are centered 
    # at 469-nanometers, 645-nanometers, and 858-nanometers, respectively.
    #	Red is MODIS band 1
    #	Near-IR is MODIS band 2
    #	Blue is MODIS band 3

    reds <- stack(these_dats, bands=1)
    nirs <- stack(these_dats, bands=2)
    blues <- stack(these_dats, bands=3)

    min_date_string <- format(min(these_dates), '%Y%j')
    max_date_string <- format(max(these_dates), '%Y%j')
    evi_filename <- file.path(out_folder,paste(product, sitecode, 
                                               paste(min_date_string, 
                                                     max_date_string, sep='-'), 
                                               sep='_'))
    modis_evi <- EVI(blue=blues, red=reds, nir=nirs, filename=evi_filename, 
                     overwrite=overwrite)
}
