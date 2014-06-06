library(RCurl)
library(foreach)
library(iterators)
library(stringr)

start_date <- as.Date('2000/2/18')
end_date <- as.Date('2014/2/18')

output_path <- '/localdisk/home/azvoleff'

MODIS_product_base_url <- 'http://e4ftl01.cr.usgs.gov/MOTA/MCD43A4.005'

desired_tiles <- read.csv('TEAM_Site_MODIS_Tiles.csv')
desired_tile_strings <- paste0('h', sprintf('%02i', desired_tiles$h),
                               'v', sprintf('%02i', desired_tiles$v))
desired_tile_strings <- unique(desired_tile_strings)

# Calculate dates for all desired MODIS tiles
ann_dates <- seq(as.Date('2000/1/1'), as.Date('2014/01/01'), by='years')
# Note the below hack with format and unlist to get the multiple lists to 
# concatenate properly 
dates <- lapply(ann_dates, function(start_date) {
               format(seq(start_date, length.out=46, by='8 days'), '%Y/%m/%d')
})
dates <- as.Date(unlist(dates))
dates <- dates[(dates >= start_date) & (dates <= end_date)]

# Calculate base URLs for these dates
base_urls <- paste0(MODIS_product_base_url, '/', format(dates, '%Y.%m.%d'))

foreach (base_url=iter(base_urls)) %do%  {
    usgs_page <- scan(base_url, what='character', quiet=TRUE)
    hdf_files <- str_extract(usgs_page, 'MCD43A4.A[0-9]{7}.h[0-9]{2}v[0-9]{2}.[0-9]{3}.[0-9]{13}.hdf')
    hdf_files <- unique(hdf_files)
    hdf_files <- hdf_files[!is.na(hdf_files)]
    tile_hv <- str_extract(hdf_files, 'h[0-9]{2}v[0-9]{2}')

    modis_filenames <- hdf_files[tile_hv %in% desired_tile_strings]
    if (length(modis_filenames) != length(desired_tile_strings)) {
        warning(length(modis_filenames), ' downloads found for ', 
                length(desired_tile_strings), ' desired tiles')
    }
    # Also download XML metadata files
    modis_filenames <- c(modis_filenames, paste0(modis_filenames, '.xml'))

    foreach (modis_filename=iter(modis_filenames), .inorder=FALSE) %dopar% {
        local_file <- file.path(output_path, 'MODIS_NBAR_Reflectance', download_url)
        remote_file <- file.path(base_url, modis_filename)
        ret_code <- download.file(remote_file, local_file, mode="w", quiet=TRUE)
    }

}
