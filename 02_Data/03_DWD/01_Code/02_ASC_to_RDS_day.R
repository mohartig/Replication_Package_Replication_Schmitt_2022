## Load the raster package
vars <- c("air_temperature_max", "air_temperature_min", "soil_moist")

## Iterate over each type of weather data
for (weather.name in vars) {

    if(weather.name %in% c("air_temperature_max", "air_temperature_min")){
      folder.path.input <- paste0("02_Data/03_DWD/03_RAW_WEATHER_DATA/daily/hyras_de/", weather.name, "/")
      folder.path.output <- paste0("02_Data/03_DWD/04_PROCESSED_WEATHER_DATA/daily/hyras_de/", weather.name, "/")    
    
      ## List all the .asc files in the folder
      nc_files <- list.files(path = folder.path.input, pattern = "\\.nc$", full.names = TRUE)
      
      ## Subset to years before 2021
      nc_files <- nc_files[grep("_(200[4-9]|201[0-9]|202[0-4])", nc_files)]
      
      ## Process each .asc file separately
      source("02_Data/03_DWD/01_Code/05_asc_to_rds_loop.R")
      }
    
    if(weather.name %in% c("soil_moist")){
      for (YEAR in c(2004:2020)) {
        
        print(YEAR)
        
        folder.path.input <- paste0("02_Data/03_DWD/03_RAW_WEATHER_DATA/daily/soil_moist_layers/winterwheat/", YEAR, "/")
        folder.path.output <- paste0("02_Data/03_DWD/04_PROCESSED_WEATHER_DATA/daily/soil_moist_layers/winterwheat/", YEAR, "/")

        ## List all the .asc files in the folder
        nc_files <- list.files(path = folder.path.input, pattern = "\\.nc$", full.names = TRUE)
        
        ## Process each .asc file separately
        source("02_Data/03_DWD/01_Code/06_asc_to_rds_loop_soil_moist.R")
      }
    }
}
