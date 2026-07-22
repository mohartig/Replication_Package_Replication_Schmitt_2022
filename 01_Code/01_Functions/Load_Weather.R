# ---------------------------------------------------------------------------- #
# Min Temperature
# ---------------------------------------------------------------------------- #
temperature.min.files <- list.files(path = "02_Data/03_DWD/04_PROCESSED_WEATHER_DATA/daily/hyras_de/air_temperature_min",
                                    pattern = "\\.Rds$", recursive = TRUE, full.names = TRUE)
temperature.min.files <- grep(pattern.setting, temperature.min.files, value = TRUE)
temperature.min.files <- grep(filtering, temperature.min.files, value = TRUE)
if(filtering == "crop_map_filter"){temperature.min.files <- grep(paste0(filtering, "_", crop.filter.threshold), 
                                                                        temperature.min.files, value = TRUE)}
temperature.min.files <- grep(year_pattern, temperature.min.files, value = TRUE)

# ---------------------------------------------------------------------------- #
# Max Temperature
# ---------------------------------------------------------------------------- #
temperature.max.files <- list.files(path = "02_Data/03_DWD/04_PROCESSED_WEATHER_DATA/daily/hyras_de/air_temperature_max",
                                    pattern = "\\.Rds$", recursive = TRUE, full.names = TRUE)
temperature.max.files <- grep(pattern.setting, temperature.max.files, value = TRUE)
temperature.max.files <- grep(filtering, temperature.max.files, value = TRUE)
if(filtering == "crop_map_filter"){temperature.max.files <- grep(paste0(filtering, "_", crop.filter.threshold), 
                                                                 temperature.max.files, value = TRUE)}
temperature.max.files <- grep(year_pattern, temperature.max.files, value = TRUE) 


# ---------------------------------------------------------------------------- #
# Field capacity
# ---------------------------------------------------------------------------- #
field.capacity.files <- list.files(path = "02_Data/03_DWD/04_PROCESSED_WEATHER_DATA/daily/soil_moist_layers/",
                                   pattern = "\\.Rds$", recursive = TRUE, full.names = TRUE)
field.capacity.files <- grep(pattern.setting, field.capacity.files, value = TRUE)
field.capacity.files <- grep(filtering, field.capacity.files, value = TRUE)
if(filtering == "crop_map_filter"){field.capacity.files <- grep(paste0(filtering , "_", crop.filter.threshold),
                                                                field.capacity.files, value = TRUE)}
field.capacity.files <- grep(year_pattern, field.capacity.files, value = TRUE)

# ---------------------------------------------------------------------------- #
# Snow
# ---------------------------------------------------------------------------- #
snow.files <- list.files(path = "02_Data/03_DWD/07_Snow_Data/03_Final_Data",
                         pattern = "\\.Rds$", recursive = TRUE, full.names = TRUE)
snow.files <- grep(pattern.setting, snow.files, value = TRUE)
snow.files <- grep(filtering, snow.files, value = TRUE)
if(filtering == "crop_map_filter"){snow.files <- grep(paste0(filtering, "_", crop.filter.threshold), 
                                                      snow.files, value = TRUE)}
snow.files <- grep(year_pattern, snow.files, value = TRUE)
