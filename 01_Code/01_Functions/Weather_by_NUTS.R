## Function to process weather raster and NUTS shapefile using terra with package prefixes
process.weather.nuts.terra <- function(weather.object.path, nuts.regions, variable.names, YEAR) {
  
  ## Load the raster data with terra
  weather.raster <- terra::rast(weather.object.path)
  
  ## Check if "temperature" is part of any variable name
  if (any(grepl("temperature", variable.names, ignore.case = TRUE))) {
    values(weather.raster) <- values(weather.raster) / 10
  }
  
  ## Transform the projection of NUTS to match the raster data if necessary
  nuts.regions <- sf::st_transform(nuts.regions, terra::crs(weather.raster))
  
  ## Extract weather information for each NUTS region using terra
  weather_values <- terra::extract(weather.raster, nuts.regions, fun = mean, na.rm = TRUE)[,2]

  ## Convert the extracted data to a dataframe
  ## Assuming the first column of the result contains the extracted values
  df <- data.frame(NUTS.SELECTION = nuts.regions$NUTS_ID, Weather_Value = weather_values)
  colnames(df) <- c(variable.names)
  df$YEAR <- YEAR
  
  ## Return the dataframe
  return(df)
}
