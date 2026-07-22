## Extract the year from the file name
year <- gsub(".*_(\\d{4})_.*", "\\1", nc_file)

## Load the .asc file as a raster layer
raster_layer <- terra::rast(nc_file)

## Assuming raster_layer is your SpatRaster object
num_layers <- dim(raster_layer)[3]  # Number of layers

## Create an empty list to store each layer individually
daily_layers <- vector("list", num_layers)

## Populate the list with each layer
for (i in 1:num_layers) {
  daily_layers[[i]] <- raster_layer[[i]]
}

start_date <- as.Date(paste0(year,"-01-01"))
dates <- seq.Date(start_date, by = "day", length.out = num_layers)
names(daily_layers) <- as.character(dates)

# ## Write raster to file, with the year as part of the filename
saveRDS(daily_layers, file = paste0(folder.path.output, weather.variable, "_", year, ".Rds"))
