library(raster)
library(R.utils)
library(tidyverse)

# Define function for creating link to data
create_url <- function(date = '2014-01-01'){
  date <- as.Date(date)
  url <- paste0("ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_daily/tifs/p05/",
                format(date, '%Y'),
                "/chirps-v2.0.",
                format(date, '%Y'),
                ".",
                format(date, '%m'),
                ".",
                format(date, "%d"),
                ".tif.gz")
  return(url)
}

dates <- seq(as.Date('2014-01-01'),
             as.Date('2016-12-31'),
             by = 1)

# If no weather_data dir, create on
if(!dir.exists('weather_data')){
  dir.create('weather_data')
}

for (i in 1:length(dates)){
  this_date <- dates[i]
  start_time <- Sys.time()
  try({
    # Define a file name
    file_name <- paste0('weather_data/', this_date, '.tif')
    # Skip if the data is already there
    if(!file.exists(file_name)){
      this_url <- create_url(this_date)
      # Remove the old stuff
      file.remove('temp.tif')
      file.remove('temp.tif.gz')
      # Download file
      download.file(url = this_url,
                    destfile = 'temp.tif.gz')
      # Extract
      R.utils::gunzip('temp.tif.gz')
      # Move
      file.copy(from = 'temp.tif',
                to = file_name)
      message('---------------------------------')
      end_time <- Sys.time()
      message(paste0('That took ',
                     round(as.numeric(end_time - start_time), digits = 2),
                     ' seconds.'))
    }
  })
}

# Aggregate weather data
###########################

# Location for weather
location <- data_frame(x = -14.360311211,
                       y = 13.3102621)
location_sp <- location
coordinates(location_sp) <- ~x+y
proj4string(location_sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Get a place to store aggregated data
if(!dir.exists('weather_data/aggregated')){
  dir.create('weather_data/aggregated')
}

# Read each of the files
files <- dir('weather_data/')
files <- files[grepl('.tif', files, fixed = TRUE)]

# Go into weather data
setwd('weather_data/')
if(file.exists('aggregated/precipitation_daily.csv') &
   file.exists('aggregated/precipitation_weekly.csv') &
   file.exists('aggregated/precipitation_monthly.csv')){
  
  precipitation_daily <- read_csv('aggregated/precipitation_daily.csv')
  precipitation_weekly <- read_csv('aggregated/precipitation_weekly.csv')
  precipitation_monthly <- read_csv('aggregated/precipitation_monthly.csv')
} else {
  
  # Read in each file and combine
  results <- list()
  for (i in 1:length(files)){
    this_file <- files[i]
    this_date <- as.Date(gsub('.tif', '', this_file, fixed = TRUE))
    r <- raster(this_file)
    
    # Extract the values
    x <- raster::extract(r, location_sp)

    results[[i]] <- x
    message(this_date)
  }
  
  # Put into a dataframe
  x <- unlist(results)
  precipitation <- data_frame(date = dates,
                        precipitation = x)
  
  precipitation$year <- as.numeric(format(precipitation$date, '%Y'))
  precipitation$month <- as.numeric(format(precipitation$date, '%m'))
  precipitation$day <- as.numeric(format(precipitation$date, '%d'))
  precipitation$week <- as.numeric(format(precipitation$date, '%U')) + 1
  
  # Aggregate
  precipitation_daily <- precipitation
  precipitation_weekly <- 
    precipitation %>%
    group_by(year, week) %>%
    summarise(precipitation = sum(precipitation, na.rm = TRUE))
  precipitation_monthly <- 
    precipitation %>%
    group_by(year, month) %>%
    summarise(precipitation = sum(precipitation, na.rm = TRUE))
  
  # Store the results
  write_csv(precipitation_daily, 'aggregated/precipitation_daily.csv')
  write_csv(precipitation_weekly, 'aggregated/precipitation_weekly.csv')
  write_csv(precipitation_monthly, 'aggregated/precipitation_monthly.csv')
}

setwd('..')

# Remove unecessary objects
rm(location, location_sp,
   r, results, this_file, x,
   dates, file_name, files, i,
   start_time, this_date, create_url)

# Plot
