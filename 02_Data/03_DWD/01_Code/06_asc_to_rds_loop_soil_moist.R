## Build a single virtual stack over all depths
x <- terra::rast(nc_files)
crs(x) <- crs("EPSG:31467")

## Sanity: number of days in a single file
Tday <- nlyr(rast(nc_files[1]))

## Group layers so that the same day across depths shares the same index
idx <- rep(seq_len(Tday), times = length(nc_files))

terra::terraOptions(
  threads = parallel::detectCores() - 3,
  memfrac = 0.8,
  progress = 1
)

## Name the layers by date
start_date <- as.Date(sprintf("%d-01-01", YEAR))
mean_by_day <- terra::tapp(
  x, index = idx, fun = "mean", na.rm = TRUE #,
  # filename = file.path(folder.path.output, paste0("mean_", YEAR, ".tif")),
  # overwrite = TRUE,
  # wopt = list(
  #   datatype = "FLT4S",
  #   gdal = c("TILED=YES", "COMPRESS=NONE", "BIGTIFF=YES",
  #            "BLOCKXSIZE=512", "BLOCKYSIZE=512")
  # )
)

names(mean_by_day) <- format(seq.Date(start_date, by = "day", length.out = nlyr(mean_by_day)),
                             "%Y-%m-%d")

## list of single-layer SpatRasters:
daily_layers <- as.list(mean_by_day)
saveRDS(daily_layers, file = paste0(folder.path.output, weather.variable, "_", YEAR, ".Rds"))
