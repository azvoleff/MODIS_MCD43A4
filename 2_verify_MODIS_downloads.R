library(stringr)

in_path <- '/localdisk/home/azvoleff/MODIS_NBAR_Reflectance'

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

hdf_files <- dir(in_path, 
                   pattern='^MCD43A4.A[0-9]{7}.h[0-9]{2}v[0-9]{2}.[0-9]{3}.[0-9]{13}.hdf$')
hdf_files <- hdf_files[!is.na(hdf_files)]
tile_hv <- str_extract(hdf_files, 'h[0-9]{2}v[0-9]{2}')
dates <- gsub('[.]', '', str_extract(hdf_files, '.A[0-9]{7}.'))
dates <- as.Date(dates, '%Y%j')

table(tile_hv)
