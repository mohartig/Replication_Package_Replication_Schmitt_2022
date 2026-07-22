
NUTS.SELECTION <- "NUTS3"
phenological.stages <- data.frame(name = c("shooting", "flowering", "fruit_formation",
                                           "ripening", "harvest"),
                                  phase_id = c(15, 18, 18, 21, 24),
                                  BBCH = c(31, 51, 51, 87, 99))

## Select monthly climate data:
data("metaIndex")
m <- metaIndex
m <- metaIndex[metaIndex$res == "daily" &
                 metaIndex$per == "historical", ]

## Transform into spatial object:
msf <- sf::st_as_sf(m, coords = c("geoLaenge", "geoBreite"), crs = 4326)
msf$date <- as.Date(msf$bis_datum)

## Create new columns for year, month, and day
msf$year <- lubridate::year(msf$date)

station.location <- msf[,c(1,5,6,11,13)]
station.location <- station.location[station.location$year >= c(years[1]-1),]

## Keep only unique rows
station.location <- dplyr::distinct(station.location, Stationsname, .keep_all = TRUE)

# ----------------------- location ------------------------------------------- # 
location.base <- paste0(sub("climate$", "phenology", dwdbase), "/annual_reporters/farming/historical/")
location.urls <- "/PH_Beschreibung_Phaenologie_Stationen_Jahresmelder.txt"
location.station <- dataDWD(base = location.base,
                            url = location.urls,
                            joinbf = TRUE,
                            dir    = "02_Data/03_DWD/05_PHENOLOGICAL_STAGES/",
                            read = FALSE)

location.station <- read.table(location.station, sep = ";", header = TRUE)
location.station <- location.station[,c(1,2,3,4,10)]
location.station$Stations_id <- as.integer(location.station$Stations_id)

# ----------------------- planting date -------------------------------------- #
phenocrop.base <- paste0(sub("climate$", "phenology", dwdbase))

## Disable s2 geometry engine
sf::sf_use_s2(FALSE)

for (extreme.crop in "winter.wheat") {
  
  url.pheno <- "/annual_reporters/crops/historical/PH_Jahresmelder_Landwirtschaft_Kulturpflanze_Winterweizen_1925_2023_hist.txt"
  
  file.pheno <- dataDWD(base = phenocrop.base,
                        url = url.pheno,
                        joinbf = TRUE,
                        dir    = "02_Data/03_DWD/05_PHENOLOGICAL_STAGES/",
                        read = FALSE)
  
  crop <- read.table(file.pheno, sep = ";", header = TRUE)
  
  for (year in years) {  
    
    ## load NUTS files
    source("01_Code/01_Functions/Load_NUTS.R")
    NUTS.germany <- sf::st_make_valid(get(paste0(NUTS.SELECTION, ".germany")))
    
    for (stages in 1:5) {
      
      stage <- phenological.stages$phase_id[stages]
      stage.name <- phenological.stages$name[stages]
      
      crop.subset <- crop %>%
        filter(Referenzjahr == year,
               Phase_id == stage,
               Qualitaetsniveau == 10,
               Eintrittsdatum_QB == 1)
      
      crop.year <- crop.subset[, c("Stations_id", "Eintrittsdatum", "Jultag")] 
      crop.year$Stations_id <- as.integer(crop.year$Stations_id) 
      
      # ----------------------- merge ---------------------------------------------- # 
      joint.data <- dplyr::inner_join(location.station, crop.year, by = "Stations_id",
                                      relationship = "many-to-many")
      joint.data$Eintrittsdatum <- paste0(substr(joint.data$Eintrittsdatum, 1, 4), "-",
                                          substr(joint.data$Eintrittsdatum, 5, 6), "-",
                                          substr(joint.data$Eintrittsdatum, 7, 8))
      joint.data$Eintrittsdatum <- as.Date(joint.data$Eintrittsdatum)
      
      joint.data <- joint.data[complete.cases(joint.data$Eintrittsdatum),]
      
      # ----------------------- spatial points ------------------------------------- # 
      sp::coordinates(joint.data) <- ~geograph.Laenge+geograph.Breite
      sp::proj4string(joint.data) <- sp::CRS("+init=epsg:4326")
      
      # ----------------------- plot planting dates -------------------------------- # 
      ## Convert 'joint.data' to an 'sf' object 
      ## Assuming 'joint.data' is your SpatialPointsDataFrame
      joint.data.av.dates <- sf::st_as_sf(joint.data)
      joint.data.av.dates <- joint.data.av.dates[, c("Stations_id", "Eintrittsdatum", "Jultag", "geometry")]
      
      ## Perform a spatial join
      joint.data.av.dates <- sf::st_make_valid(joint.data.av.dates)
      joint.data.av.dates <- unique(joint.data.av.dates)
      joint.data.av.dates <- sf::st_join(joint.data.av.dates, NUTS.germany, join = sf::st_within)
      
      ## Remove rows with NA in the NUTS2 column
      joint.data.av.dates <- joint.data.av.dates[!is.na(joint.data.av.dates$geo), ]
      
      ## Convert the dates into numeric values
      dates <- as.numeric(format(joint.data.av.dates$Eintrittsdatum, "%Y%m%d"))
      
      ## Define a Mode function
      Mode <- function(x) {
        x <- na.omit(x)
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
      }
      
      ## Calculate the average date within each polygon
      avg_dates <- aggregate(dates, by = list(joint.data.av.dates$geo), FUN = Mode)
      
      ## Create a new column in NUTS.germany with the average dates
      NUTS.germany.dates <- sf::st_as_sf(NUTS.germany)
      NUTS.germany.dates$avg_date <- avg_dates$x[match(NUTS.germany.dates$geo, avg_dates$Group.1)]
      
      ## fill NA  
      NUTS.germany.dates$avg_date[is.na(NUTS.germany.dates$avg_date)] <- Mode(NUTS.germany.dates$avg_date)
      
      ## Calculate Jultag within each polygon
      ## Jultag
      jultag <- as.numeric(joint.data.av.dates$Jultag)
      avg_jultag <- aggregate(jultag, by = list(joint.data.av.dates$geo), FUN = Mode)
      
      ## Create a new column in NUTS.germany with the average dates
      NUTS.germany.dates$Jultag <- avg_jultag$x[match(NUTS.germany.dates$geo, avg_jultag$Group.1)]
      
      ## fill NA  
      NUTS.germany.dates$Jultag[is.na(NUTS.germany.dates$Jultag)] <- Mode(NUTS.germany.dates$Jultag)
      
      final.output.NUTS <- sf::st_drop_geometry(NUTS.germany.dates[,c("NUTS_ID", "Jultag")])
      final.output.NUTS <- as.data.frame(final.output.NUTS)
      colnames(final.output.NUTS) <- c(NUTS.SELECTION, stage.name)
      
      ## save output
      saveRDS(final.output.NUTS,
              file = paste0("02_Data/03_DWD/05_PHENOLOGICAL_STAGES/", stage.name, "/", stage.name, "_", year, "_", extreme.crop, "_NUTS" , ".Rds"))
      
      
      ## add to LAU
      idx <- st_intersects(lau_data, NUTS.germany.dates)
      lau_cent <- st_point_on_surface(st_make_valid(lau_data))
      m <- st_within(lau_cent, NUTS.germany.dates)
      best_match <- sapply(m, function(x) if (length(x)==0) NA_integer_ else x[1])
      
      # fallback for NAs
      na_i <- which(is.na(best_match))
      if (length(na_i) > 0) {
        best_match[na_i] <- st_nearest_feature(lau_data[na_i, ], NUTS.germany.dates)
      }
      
      lau_data_dates <- lau_data %>%
        mutate(.nuts_row = best_match) %>%
        left_join(
          NUTS.germany.dates %>%
            st_drop_geometry() %>%
            mutate(.nuts_row = row_number()) %>%
            select(.nuts_row, NUTS_ID, avg_date, Jultag),
          by = ".nuts_row"
        ) %>%
        select(-.nuts_row)
      
      final.output.LAU <- sf::st_drop_geometry(lau_data_dates[,c("LAU_ID", "Jultag")])
      final.output.LAU <- as.data.frame(final.output.LAU)
      colnames(final.output.LAU) <- c("LAU", stage.name)
      
      ## save output
      saveRDS(final.output.LAU,
              file = paste0("02_Data/03_DWD/05_PHENOLOGICAL_STAGES/", stage.name, "/", stage.name, "_", year, "_", extreme.crop, "_LAU", ".Rds"))
      
      print(paste0("Year: ", year, " and stage: ", stages))
      
    }
  }
}

## Enable engine
sf::sf_use_s2(TRUE)

