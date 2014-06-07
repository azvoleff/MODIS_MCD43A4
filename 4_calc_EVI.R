###############################################################################
# Calculates EVI from cropped MODIS MCD43A4 (500m Nadir BRDF-Adjusted
# Reflectance) product tiles covering the spatial extent of the ZOI/CSA/PA
# boundary of each team site.
# 
# Requires a GDAL installation that supports HDF4 files - on Windows, see 
# OSGEO4W to meet this dependency.
###############################################################################

library(rgdal)
library(raster)
library(stringr)


library(doParallel)
library(foreach)
n_cpus <- 12
registerDoParallel(n_cpus)

overwrite <- TRUE

EVI <- function(blue, red, nir, ...) {
    ret <- overlay(blue, red, nir, fun=function(blue, red, nir) {
            evi <- (2.5*(nir - red)) / (1 + nir + 6*red - 7.5*blue)
            evi <- round(evi * 10000) # apply scaling
        }, ...)
    return(ret)
}

in_base_dir <- '/localdisk/home/azvoleff/MODIS_NBAR_Reflectance'
out_base_dir <- '/localdisk/home/azvoleff/MODIS_NBAR_Reflectance'
in_folder <- file.path(out_base_dir, 'ZOI_Crops')
out_folder <- file.path(out_base_dir, 'EVI')

dats <- dir(in_folder, pattern='.dat$')
product <- unique(gsub('_', '', str_extract(dats, '^[a-zA-Z0-9]*_')))
if (length(product) != 1) {
    stop('files are from more than one MODIS product')
}
sitecodes <- gsub('_', '', str_extract(dats, '_[A-Z]{2,3}_'))
dates <- as.Date(gsub('[_.dat]', '', str_extract(dats, '_[0-9]{7}.dat')), '%Y%j')

for (sitecode in unique(sitecodes)) {
    timestamp()
    message('Processing ', sitecode, '...')

    these_dats <- dats[sitecodes == sitecode]
    if (length(these_dats) == 0) {
        stop('no files found for ', sitecode)
    }

    these_dates <- dates[sitecodes == sitecode]
    these_dats <- these_dats[order(these_dates)]
    these_dates <- these_dates[order(these_dates)]
    
    out_base <- file.path(out_folder, paste(product, sitecode, sep='_'))

    evi_file_base <- paste0(out_base, '_EVI_')
    out_rasts <- foreach(datfile=iter(these_dats), date=iter(these_dates),
            .packages=c('raster', 'gdalUtils', 'stringr'),
            .inorder=FALSE) %dopar% {
        # Blue, red, and near-infrared reflectances in MODIS product are centered 
        # at 469-nanometers, 645-nanometers, and 858-nanometers, respectively.
        #	Red is MODIS band 1
        #	Near-IR is MODIS band 2
        #	Blue is MODIS band 3
        nbar <- stack(file.path(in_folder, datfile)) * .0001 # Apply scaling
        evi_filename <- paste0(evi_file_base, format(date, '%Y%j'), '.envi')
        modis_evi <- EVI(blue=nbar[[3]], red=nbar[[1]], nir=nbar[[2]],
                         filename=evi_filename, overwrite=overwrite,
                         datatype='INT2S')
    }
    
    min_date_string <- format(min(these_dates), '%Y%j')
    max_date_string <- format(max(these_dates), '%Y%j')
    evi_files <- dir(out_folder, pattern=paste0(basename(evi_file_base),
                                                '[_0-9]*.envi$'),
                     full.names=TRUE)
    vrt_filename <- paste0(out_base, '_EVI_stack_',  paste(min_date_string, 
                                                     max_date_string, sep='-'),
                           '_temp.vrt')
    gdalbuildvrt(evi_files, vrt_filename, separate=TRUE)
    
    # Stack files and output as a single image
    evi_stack_filename <- paste0(evi_file_base, 'stack_',
                                 paste(min_date_string,
                                       max_date_string, sep='-'),
                                 '.tif')
    gdal_translate(vrt_filename, evi_stack_filename, overwrite=overwrite)
    
    unlink(vrt_filename)
}
