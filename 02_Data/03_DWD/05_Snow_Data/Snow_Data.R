
## Set the timeout to a higher value, for example, 300 seconds (5 minutes)
options(timeout = 500000)

download_recursive <- function(url, dest_folder) {
  webpage <- read_html(url)
  links <- webpage %>% html_nodes("a") %>% html_attr("href")
  
  # Filter out parent directory and query links
  links <- links[!links %in% c("../", "?C=N;O=D", "?C=M;O=A", "?C=S;O=A", "?C=D;O=A")]
  
  for (link in links) {
    ## Check if link is a folder (ends with '/')
    if (str_ends(link, "/")) {
      new_folder <- file.path(dest_folder, link)
      dir.create(new_folder, recursive = TRUE, showWarnings = FALSE)
      download_recursive(paste0(url, link), new_folder)
    } else {
      download_url <- paste0(url, link)
      dest_path <- file.path(dest_folder, link)
      download.file(download_url, destfile = dest_path, mode = "wb")
      
      ## Unzip .gz files
      if (str_ends(link, ".gz")) {
        unzipped_content <- readLines(gzfile(dest_path))
        unzipped_file <- str_replace(dest_path, ".gz$", "")
        writeLines(unzipped_content, unzipped_file)
        file.remove(dest_path)  # Optionally remove the original .gz file
      }
    }
  }
}

dest_folder <- "02_Data/03_DWD/05_Snow_Data/01_Data/"
if (!dir.exists(dest_folder)) {
  dir.create(dest_folder, recursive = TRUE)
}
url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/water_equiv/historical/"
download_recursive(url, dest_folder)

## Unzip files
generate_save_snow_data <- function(filename){
  
  ## Use regular expressions to extract the station ID and dates
  matches <- regmatches(filename, regexec("_(\\d{5})_(\\d{8})_(\\d{8})_", filename))
  
  ## Extracted values
  station_id <- matches[[1]][2]
  start_date <- matches[[1]][3]
  end_date <- matches[[1]][4]  
  
  filename.txt <-  paste0("produkt_waequi_tag_", start_date, "_", end_date, "_", station_id , ".txt")
  filename.ID <- paste0("Metadaten_Geographie_", station_id,".txt")
  
  ## Define the path to the zip file and the directory to extract files
  zip_file_path <- paste0("02_Data/03_DWD/05_Snow_Data/01_Data/", filename)
  extracted_dir_path <- paste0("02_Data/03_DWD/05_Snow_Data/02_Processed/tageswerte_Wa_", station_id, "_", start_date, "_", end_date)
  
  if (!dir.exists(extracted_dir_path)) dir.create(extracted_dir_path, recursive = TRUE, showWarnings = FALSE)
  
  ## Unzip the file
  utils::unzip(zip_file_path, exdir = extracted_dir_path)

}

## LOAD FILE NAMES
filenames <- list.files("02_Data/03_DWD/05_Snow_Data/01_Data", pattern = ".zip")

## extract start and end dates from filenames
dates <- regmatches(filenames, gregexpr("\\d{8}", filenames))
start_dates <- as.Date(sapply(dates, `[`, 1), "%Y%m%d")
end_dates   <- as.Date(sapply(dates, `[`, 2), "%Y%m%d")

## target window
target_start <- as.Date(paste0(years[1],"-01-01"))
target_end   <- as.Date(paste0(years[length(years)],"-12-31"))

## keep files that overlap with 2004–2020
keep <- (start_dates <= target_end) & (end_dates >= target_start)
filtered_files <- filenames[keep]

## APPLY FUNCTION TO FILES
list.snow.data <- lapply(filtered_files, generate_save_snow_data)

## Load snow data
read_snow_data <- function(filename){
  
  ## take folder part only
  folder <- dirname(filename)
  
  ## Use regular expressions to extract the station ID and dates
  matches <- regmatches(folder, regexec("_(\\d{5})_(\\d{8})_(\\d{8})$", folder))

  ## Extracted values
  station_id <- matches[[1]][2]
  start_date <- matches[[1]][3]
  end_date <- matches[[1]][4]  
  
  ## Read the data file
  data <- fread(input = paste0("02_Data/03_DWD/05_Snow_Data/02_Processed/", filename), sep = ";", header = TRUE, na.strings = c("-999", "NA"))
  
  ## Select relevant columns: STATIONS_ID, MESS_DATUM, SH_TAG (Snow Height)
  snow_data <- data[, .(STATIONS_ID, MESS_DATUM, SH_TAG)]
  colnames(snow_data) <- c("Stations_id", "MESS_DATUM", "SH_TAG")
  
  ## Load the geographical data
  geo_file_path <- paste0("02_Data/03_DWD/05_Snow_Data/02_Processed/", folder, "/", "Metadaten_Geographie_", station_id,".txt")
  geo_data <- fread(geo_file_path, sep = ";", header = TRUE, na.strings = c("-999", "NA"))
  snow_data <- merge(snow_data, geo_data[1,], by = "Stations_id", all.x = TRUE)
  
  snow_data <- snow_data[, .(Stations_id, MESS_DATUM, SH_TAG, Geogr.Breite, Geogr.Laenge, Stationsname)]
  snow_data <- snow_data[snow_data$MESS_DATUM > "20031231",]
  
  print(filename)
  
  return(snow_data)
}

## LOAD FILE NAMES
data_file_path <- list.files("02_Data/03_DWD/05_Snow_Data/02_Processed/", pattern = "produkt_waequi_tag", recursive = TRUE)

## extract start and end dates from data_file_path
dates <- regmatches(data_file_path, gregexpr("\\d{8}", data_file_path))
start_dates <- as.Date(sapply(dates, `[`, 1), "%Y%m%d")
end_dates   <- as.Date(sapply(dates, `[`, 2), "%Y%m%d")

## target window
target_start <- as.Date(paste0(years[1],"-01-01"))
target_end   <- as.Date(paste0(years[length(years)],"-12-31"))

## keep files that overlap with 2004–2020
keep <- (start_dates <= target_end) & (end_dates >= target_start)
filtered_files <- data_file_path[keep]

## APPLY FUNCTION TO FILES
list.snow.data <- lapply(filtered_files, read_snow_data)
snow_data <- do.call(rbind, list.snow.data)
snow_data$year <- substr(snow_data$MESS_DATUM, 1, 4)
snow_data <- snow_data[snow_data$year < 2021, ]
snow_data <- snow_data[snow_data$year > 2003, ]

## SET CRS FOR ALL INPUTS
crs.EEA <- readRDS("02_Data/02_EEA_Grid/EEA_crs.rda")

for(filtering in c("no_filter", "crop_map_filter")){
  
  for(crop.filter.threshold in thresholds){
    
    if(filtering == "no_filter" & crop.filter.threshold > thresholds[1]){next}
    
    if(filtering == "crop_map_filter"){
      CROPMAP <- readRDS("02_Data/05_Crop_Map_Baumert_et_al/EU_expected_crop_shares/Germany_Cropped.Rds")
    }
    
    for (NUTS.SELECTION in NUTS.OPTIONS) {
      
      if(NUTS.SELECTION == "NUTS1"){NUTS <- "_NUTS_1_"}
      if(NUTS.SELECTION == "NUTS2"){NUTS <- "_NUTS_2_"}
      if(NUTS.SELECTION == "NUTS3"){NUTS <- "_NUTS_3_"}
      if(NUTS.SELECTION == "LAU"){NUTS <- "_LAU_"}
      
      for (YEAR in years) {
        
        ## LOAD NUTS FILES
        print(YEAR)
        year <- YEAR
        source("01_Code/01_Functions/Load_NUTS.R")
        
        snow_data_year <- snow_data[snow_data$year == YEAR,]
        
        ## Convert snow_data to a spatial data frame if it's not already
        snow_data_sf <- st_as_sf(snow_data_year, coords = c("Geogr.Laenge", "Geogr.Breite"), crs = 4326)
        
        ## Make geometries valid for both spatial data frames
        snow_data_sf <- st_make_valid(snow_data_sf)
        
        if(NUTS.SELECTION %in% c("NUTS1", "NUTS2", "NUTS3")){nuts_selection_germany <- st_make_valid(get(paste0(NUTS.SELECTION, ".germany")))}
        if(NUTS.SELECTION == "LAU"){nuts_selection_germany <- st_make_valid(get("lau_data"))}
        
        if(filtering == "crop_map_filter"){
          CROPMAP <- terra::project(CROPMAP, snow_data_sf, method = "near")
  
          ## Extract raster values at snow_data_sf point locations
          vals <- terra::extract(CROPMAP, vect(snow_data_sf))
          
          ## Bind extracted values back to your sf object
          snow_data_sf$crop_val <- vals$lyr.1
          
          ## Keep only points where CROPMAP is 1
          snow_data_sf <- snow_data_sf %>%
            filter(crop_val > 0)
          
        }
        
        sf::sf_use_s2(FALSE)
        
        ## Perform spatial join to get NUTS2 region for each snow data point
        snow_data_joined <- sf::st_join(snow_data_sf, nuts_selection_germany)
        
        ## Convert back to data.table for efficient computation
        snow_data_dt <- as.data.table(snow_data_joined)
        
        ## Compute the averages
        if(NUTS.SELECTION %in% c("NUTS1", "NUTS2", "NUTS3")){summary_snow_data <- snow_data_dt[, .(average_snow_depth = mean(SH_TAG, na.rm = TRUE)), by = .(id, MESS_DATUM)]}
        if(NUTS.SELECTION == "LAU"){summary_snow_data <- snow_data_dt[, .(average_snow_depth = mean(SH_TAG, na.rm = TRUE)), by = .(LAU_ID, MESS_DATUM)]}
        
        summary_snow_data$average_snow_depth[is.na(summary_snow_data$average_snow_depth)] <- 0
        summary_snow_data[, day := as.Date(as.character(MESS_DATUM), format = "%Y%m%d")]
        summary_snow_data <- data.frame(summary_snow_data)
        colnames(summary_snow_data) <- c(NUTS.SELECTION, "MESS_DATUM", "snow_layer", "day")
        
        ## Split the data.table into a list of data.frames, one for each day
        split_data <- split(summary_snow_data, f = summary_snow_data$day)
        
        ## Set names for each element in the list as YYYY-MM-DD
        names(split_data) <- sapply(split_data, function(x) format(x$day[1], "%Y-%m-%d"))
        
        for(list.object in 1:length(split_data)){
          df.save <- split_data[[list.object]]
          df.save <- df.save[,c(1,3)]
          df.save$YEAR <- YEAR
          split_data[[list.object]] <- df.save
        }
        
        sf::sf_use_s2(TRUE)
        
        saveRDS(split_data, file = paste0("02_Data/03_DWD/05_Snow_Data/03_Final_Data/snow_layer", NUTS, YEAR, "_", filtering, "_", crop.filter.threshold, ".Rds"))
        
        print(paste0("Finished threshold ", crop.filter.threshold, " at snow assimilation"))
        
      }
    }
  }
}

