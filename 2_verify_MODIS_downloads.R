library(stringr)
library(plyr)

in_path <- '/localdisk/home/azvoleff/MODIS_NBAR_Reflectance'
start_date <- as.Date('2000/2/18')
end_date <- as.Date('2014/2/18')

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
file_dates <- gsub('[A.]', '', str_extract(hdf_files, '.A[0-9]{7}.'))
file_dates <- as.Date(file_dates, '%Y%j')

files <- data.frame(name=hdf_files, date=file_dates, tile=tile_hv)

table(tile_hv)

# One tile is missing. Figure out which tile that is:
num_tiles_by_date <- ddply(files, .(tile, date), summarize, num_tiles=length(name), .drop=FALSE)
num_tiles_by_date[num_tiles_by_date$num_tiles == 0, ]
# Missing tile: h09v07 2004-12-10
# Since that tile is missing and is unavilable, will need to use a linear projection between the two dates.