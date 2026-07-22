for(filtering in c("no_filter", "crop_map_filter")){

  for(crop.filter.threshold in thresholds){
    
    if(filtering == "no_filter" & crop.filter.threshold > thresholds[1]){next}
    
    CROPMAP <- readRDS(paste0("02_Data/05_Crop_Map_Baumert_et_al/EU_expected_crop_shares/Germany_Cropped_", crop.filter.threshold,".Rds"))
    source("01_Code/01_Functions/Weather_by_NUTS.R")
    
    ## Select weather variables
    vars <- c("air_temperature_min", "air_temperature_max", "soil_moist")

    for (weather.variable in vars) {
      for(YEAR in years){
        
        if(weather.variable %in% c("air_temperature_max", "air_temperature_min")){
          folder.path.input  <- paste0("02_Data/03_DWD/02_RAW_WEATHER_DATA/daily/hyras_de/", weather.variable, "/")
          folder.path.output <- paste0("02_Data/03_DWD/03_PROCESSED_WEATHER_DATA/daily/hyras_de/", weather.variable, "/")    
          
          ## List all the .asc files in the folder
          nc_files <- list.files(path = folder.path.input, pattern = "\\.nc$", full.names = TRUE)
          
          ## Subset to years before 2021
          nc_file <- nc_files[grep(as.character(YEAR), nc_files)]
          
          ## Process each .asc file separately
          source("02_Data/03_DWD/01_Code/05_asc_to_rds_loop.R")
          print(paste("Processed files for", YEAR, "/", weather.variable))
        }
        
        if(weather.variable %in% c("soil_moist")){
          folder.path.input  <- paste0("02_Data/03_DWD/02_RAW_WEATHER_DATA/daily/soil_moist_layers/", YEAR, "/")
          folder.path.output <- paste0("02_Data/03_DWD/03_PROCESSED_WEATHER_DATA/daily/soil_moist_layers/", YEAR, "/")
  
          ## List all the .asc files in the folder
          nc_files <- list.files(path = folder.path.input, pattern = "\\.nc$", full.names = TRUE)

          ## Process each .asc file separately
          source("02_Data/03_DWD/01_Code/06_asc_to_rds_loop_soil_moist.R")
          print(paste("Processed files for", YEAR, "/", weather.variable))
          
          daily_layers <- terra::rast(paste0(folder.path.output, "mean_", YEAR, ".tif"))
          crs(daily_layers) <- crs("EPSG:31467")
        }
        
        year <- YEAR
        source("01_Code/01_Functions/Load_NUTS.R")
        
        weather.list <- daily_layers
        
        LAU <- sf::st_transform(get("lau_data"), terra::crs(weather.list[[1]]))
        
        if(filtering == "crop_map_filter"){
          CROPMAP <- terra::project(CROPMAP, weather.list[[1]], method = "near")
        }
        
          weather.list.by.NUTS1 <- list()
          weather.list.by.NUTS2 <- list()
          weather.list.by.NUTS3 <- list()
          # weather.list.by.LAU <- list()
          
          dimension <- 1:length(weather.list)
          
          if(weather.variable %in% c("soil_moist")){dimension <- 1:nlyr(weather.list)}
          
          for (list.layers in dimension) {
            
            print(list.layers)
            
            ## Load the raster data with terra
            weather.raster <- weather.list[[list.layers]]
            
            if(filtering == "crop_map_filter"){
              ## Replace < X % wheat probability cells with NA
              weather.raster[CROPMAP == 0] <- NA
            }
            
            ## Transform the projection of NUTS to match the raster data if necessary
            nuts.regions1 <- sf::st_transform(get("NUTS1.germany.transformed"), terra::crs(weather.raster))
            nuts.regions2 <- sf::st_transform(get("NUTS2.germany.transformed"), terra::crs(weather.raster))
            nuts.regions3 <- sf::st_transform(get("NUTS3.germany.transformed"), terra::crs(weather.raster))
            LAU <- sf::st_transform(get("lau_data"), terra::crs(weather.raster))
            
            weather_values1 <- terra::extract(weather.raster, nuts.regions1, fun = "mean", na.rm = TRUE)
            weather_values2 <- terra::extract(weather.raster, nuts.regions2, fun = "mean", na.rm = TRUE)
            weather_values3 <- terra::extract(weather.raster, nuts.regions3, fun = "mean", na.rm = TRUE)
            weather_LAU <- exactextractr::exact_extract(weather.raster, LAU, "mean")
            
            weather_values1 <- weather_values1[,2]
            weather_values2 <- weather_values2[,2]
            weather_values3 <- weather_values3[,2]
            weather_LAU <- weather_LAU
            
            ## Convert the extracted data to a dataframe
            df1 <- data.frame(NUTS.SELECTION = nuts.regions1$NUTS_ID, Weather_Value = weather_values1)
            df2 <- data.frame(NUTS.SELECTION = nuts.regions2$NUTS_ID, Weather_Value = weather_values2)
            df3 <- data.frame(NUTS.SELECTION = nuts.regions3$NUTS_ID, Weather_Value = weather_values3)
            dflau <- data.frame(NUTS.SELECTION = LAU$LAU_ID, Weather_Value = weather_LAU)
            
            colnames(df1) <- c("NUTS1", weather.variable)
            colnames(df2) <- c("NUTS2", weather.variable)
            colnames(df3) <- c("NUTS3", weather.variable)
            colnames(dflau) <- c("LAU", weather.variable)

            df1$YEAR <- YEAR
            df2$YEAR <- YEAR
            df3$YEAR <- YEAR
            dflau$YEAR <- YEAR
            
            ## RUN FUNCTION
            weather.list.by.NUTS1[[list.layers]] <- df1
            weather.list.by.NUTS2[[list.layers]] <- df2
            weather.list.by.NUTS3[[list.layers]] <- df3
            weather.list.by.LAU[[list.layers]] <- dflau
          }
          
          start_date <- as.Date(paste0(YEAR, "-01-01"))
          dates <- seq.Date(start_date, by = "day", length.out = length(weather.list.by.NUTS1))
          dates <- seq.Date(start_date, by = "day", length.out = length(weather.list.by.NUTS2))
          dates <- seq.Date(start_date, by = "day", length.out = length(weather.list.by.NUTS3))
          dates <- seq.Date(start_date, by = "day", length.out = length(weather.list.by.LAU))

          names(weather.list.by.NUTS1) <- as.character(dates)
          names(weather.list.by.NUTS2) <- as.character(dates)
          names(weather.list.by.NUTS3) <- as.character(dates)
          names(weather.list.by.LAU) <- as.character(dates)
          
          saveRDS(weather.list.by.NUTS1, file = paste0(folder.path.output, weather.variable, "_NUTS_1_", YEAR, "_", filtering, "_", crop.filter.threshold, ".Rds"))
          saveRDS(weather.list.by.NUTS2, file = paste0(folder.path.output, weather.variable, "_NUTS_2_", YEAR, "_", filtering, "_", crop.filter.threshold, ".Rds"))
          saveRDS(weather.list.by.NUTS3, file = paste0(folder.path.output, weather.variable, "_NUTS_3_", YEAR, "_", filtering, "_", crop.filter.threshold, ".Rds"))
          saveRDS(weather.list.by.LAU, file = paste0(folder.path.output, weather.variable, "_LAU_", YEAR, "_", filtering, "_", crop.filter.threshold, ".Rds"))
          
          print(paste0("Finished threshold ", crop.filter.threshold, " at process files to NUTS"))
          
        # }
      }
    }
  }
}
