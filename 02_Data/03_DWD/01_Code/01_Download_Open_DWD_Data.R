
## Set the timeout to a higher value, for example, 300 seconds (5 minutes)
options(timeout = 500000)

download_recursive <- function(url, dest_folder, weather_variable, years_filter = years) {
  webpage <- read_html(url)
  links <- webpage %>% html_nodes("a") %>% html_attr("href")

  ## Filter out parent directory and query links
  links <- links[!links %in% c("../", "?C=N;O=D", "?C=M;O=A", "?C=S;O=A", "?C=D;O=A")]

  for (link in links) {
    
      if(link %in% c("radiation_global/", "humidity/", "precipitation/", "air_temperature_mean/")){
        next
      } else {
        if (str_ends(link, "/")) {
          if(weather_variable == "Soil_Moisture" & !any(str_detect(link, as.character(years_filter)))){next}
          new_folder <- file.path(dest_folder, link)
          dir.create(new_folder, recursive = TRUE, showWarnings = FALSE)
          download_recursive(url = paste0(url, link), dest_folder = new_folder, weather_variable = weather_variable)
          
        } else {
          if(weather_variable == "Temperature" & str_detect(link, "v6-1_de") == TRUE){next}
          if(weather_variable == "Soil_Moisture" & str_detect(link, "_0-60_") == FALSE){next}
          if(!any(str_detect(link, as.character(years_filter)))){next}
          
          download_url <- paste0(url, link)
          dest_path <- file.path(dest_folder, link)
          download.file(download_url, destfile = dest_path, mode = "wb")
          
          ## Unzip .gz files
          if (str_ends(link, ".gz")) {
            unzipped_content <- readLines(gzfile(dest_path))
            unzipped_file <- str_replace(dest_path, ".gz$", "")
            writeLines(unzipped_content, unzipped_file)
            file.remove(dest_path)
            }
        }   
      }
    }
}

# ---------------------------------------------------------------------------- #
#### 1. Temperature                                                         ####
# ---------------------------------------------------------------------------- #
dest_folder <- paste0(getwd(), "/02_Data/03_DWD/02_RAW_WEATHER_DATA/daily/hyras_de")
if (!dir.exists(dest_folder)) {
  dir.create(dest_folder, recursive = TRUE)
}
url <- "https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/hyras_de/"
download_recursive(url = url, dest_folder = dest_folder, weather_variable = "Temperature")


# ---------------------------------------------------------------------------- #
#### 2. Soil Moisture                                                       ####
# ---------------------------------------------------------------------------- #
dest_folder <- paste0(getwd(), "/02_Data/03_DWD/02_RAW_WEATHER_DATA/daily/soil_moist_layers")
if (!dir.exists(dest_folder)) {
  dir.create(dest_folder, recursive = TRUE)
}
url <- "https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/soil_moisture/wheat/"
download_recursive(url = url, dest_folder = dest_folder, weather_variable = "Soil_Moisture")















# ---------------------------------------------------------------------------- #
#### 3. Snow                                                                ####
# ---------------------------------------------------------------------------- #

# dest_folder <- paste0(getwd(), "/02_Data/03_DWD/07_Snow_Data/01_Data")
# if (!dir.exists(dest_folder)) {
#   dir.create(dest_folder, recursive = TRUE)
# }
# url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/water_equiv/historical/"
# download_recursive(url = url, dest_folder = dest_folder, weather_variable = "Snow")
# 






# dest_folder <- paste0(getwd(), "/02_Data/03_DWD/03_RAW_WEATHER_DATA/daily/soil_moist/")
# if (!dir.exists(dest_folder)) {
#   dir.create(dest_folder, recursive = TRUE)
# }
# url <- "https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/soil_moist/"
# download_recursive(url = url, dest_folder = dest_folder)




# dest_folder <- paste0(getwd(), "/02_Data/03_DWD/03_RAW_WEATHER_DATA/daily/soil_moisture/")
# if (!dir.exists(dest_folder)) {
#   dir.create(dest_folder, recursive = TRUE)
# }
# url <- "https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/soil_moisture/"
# download_recursive(url = url, dest_folder = dest_folder)
