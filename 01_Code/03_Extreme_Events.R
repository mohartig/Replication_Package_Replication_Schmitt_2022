
for (extreme.event.crop in crops) {
  
  for(filtering in c("no_filter", "crop_map_filter")){
      
    for(crop.filter.threshold in thresholds){
      
      if(filtering == "no_filter" & crop.filter.threshold > thresholds[1]){next}
      
      for (NUTS.SELECTION in NUTS.OPTIONS) {
    
        if(NUTS.SELECTION == "NUTS1"){pattern.setting <- "NUTS_1"}
        if(NUTS.SELECTION == "NUTS2"){pattern.setting <- "NUTS_2"}
        if(NUTS.SELECTION == "NUTS3"){pattern.setting <- "NUTS_3"}
        if(NUTS.SELECTION == "LAU"){pattern.setting <- "LAU"}
        
        ## Define year pattern to match filenames containing any year
        year_pattern <- paste(years, collapse = "|")
        
        source("01_Code/01_Functions/Load_Weather.R")
        
        weather.list <- list()
        extreme.events.list <- list()
        
        ## Function to extract the year from the filename
        extract.year <- function(filename) {
          sub(".*_(\\d{4})_.*\\.Rds$", "\\1", filename)
          }
        
        ## Load and store the data
        for (file.count in 1:length(temperature.min.files)) {
          
          print(file.count)
          
          ## Extract year using the filename
          YEAR <- as.numeric(extract.year(temperature.min.files[file.count]))
          
          ## Read the .Rds files
          temp_min <- readRDS(temperature.min.files[file.count])
          temp_max <- readRDS(temperature.max.files[file.count])
          field_capacity <- readRDS(field.capacity.files[file.count])
          snow_layer <- readRDS(snow.files[file.count])
          
          if(NUTS.SELECTION == "NUTS2"){
            list.NUTS2 <- temp_min$`2020-01-01`$NUTS2
          }
          
          ## Initialize an empty list to store the daily dataframes
          daily_data <- list()
          
          ## Number of days in the datasets (assuming all lists are of the same length)
          num_days <- length(temp_min)
          
          ## Loop over each day
          for (day in 1:num_days) {
            
            ## Merge the data for the current day and YEAR
            merged_data <- merge(temp_min[[day]], temp_max[[day]], by = c(NUTS.SELECTION, "YEAR"))
            merged_data <- merge(merged_data, field_capacity[[day]], by = c(NUTS.SELECTION, "YEAR"), all = TRUE)
            merged_data <- merge(merged_data, snow_layer[[day]], by = c(NUTS.SELECTION, "YEAR"), all = TRUE)
            
            ## Calculate averages for NA values if needed
            merged_data$air_temperature_max[is.na(merged_data$air_temperature_max)] <- mean(merged_data$air_temperature_max, na.rm = TRUE)
            merged_data$air_temperature_min[is.na(merged_data$air_temperature_min)] <- mean(merged_data$air_temperature_min, na.rm = TRUE)
            merged_data$soil_moist[is.na(merged_data$soil_moist)] <- mean(merged_data$soil_moist, na.rm = TRUE)
            merged_data$snow_layer[is.na(merged_data$snow_layer)] <- mean(merged_data$snow_layer, na.rm = TRUE)
            
            ## Append data to the daily_data list
            daily_data[[day]] <- merged_data
          }
          
          ## Store the daily dataframes in the weather.list
          weather.list[[as.character(YEAR)]] <- daily_data
        }
        
        ## Initialize an empty dataframe with the structure expected for all years
        extreme_events_df <- data.frame(
          Year                 = integer(),
          NUTS                 = character(),
          Black_Frost          = integer(),
          Spring_Chill         = integer(),
          Heat                 = integer(),
          Spring_Drought       = integer(),
          Summer_Drought       = integer(),
          Spring_Waterlogging  = integer(),
          Summer_Waterlogging  = integer(),
          stringsAsFactors = FALSE
        )
        
        ## Store results in a list instead of using rbind()
        event_list <- list()
        
        ## Loop through specified year range
        for(year in years){
          print(year)
          
          source(paste0("02_Data/01_Extreme_events/", extreme.event.crop, ".R"))
 
          if(NUTS.SELECTION %in% c("NUTS1", "NUTS2")){
            
            if(NUTS.SELECTION == "NUTS1"){
              ## Create NUTS1 by removing the last digit from NUTS3
              shooting$NUTS1 <- substr(shooting$NUTS3, 1, nchar(shooting$NUTS3) - 2)
              flowering$NUTS1 <- substr(flowering$NUTS3, 1, nchar(flowering$NUTS3) - 2)
              fruit_formation$NUTS1 <- substr(fruit_formation$NUTS3, 1, nchar(fruit_formation$NUTS3) - 2)
              ripening$NUTS1 <- substr(ripening$NUTS3, 1, nchar(ripening$NUTS3) - 2)
              harvest$NUTS1 <- substr(harvest$NUTS3, 1, nchar(harvest$NUTS3) - 2)
            }
            
            if(NUTS.SELECTION == "NUTS2"){
              ## Create NUTS2 by removing the last digit from NUTS3
              shooting$NUTS2 <- substr(shooting$NUTS3, 1, nchar(shooting$NUTS3) - 1)
              flowering$NUTS2 <- substr(flowering$NUTS3, 1, nchar(flowering$NUTS3) - 1)
              fruit_formation$NUTS2 <- substr(fruit_formation$NUTS3, 1, nchar(fruit_formation$NUTS3) - 1)
              ripening$NUTS2 <- substr(ripening$NUTS3, 1, nchar(ripening$NUTS3) - 1)
              harvest$NUTS2 <- substr(harvest$NUTS3, 1, nchar(harvest$NUTS3) - 1)
            }
            
            ## Function to compute mean by NUTS
            average_by_NUTS <- function(df, variable, NUTS) {
        
              if(NUTS == "NUTS1"){
                return(aggregate(df[[variable]], by = list(NUTS1 = df$NUTS1), FUN = mean, na.rm = TRUE))
              }
                        
              if(NUTS == "NUTS2"){
                return(aggregate(df[[variable]], by = list(NUTS2 = df$NUTS2), FUN = mean, na.rm = TRUE))
              }
              
              if(NUTS == "LAU"){
                return(aggregate(df[[variable]], by = list(LAU = df$LAU_ID), FUN = mean, na.rm = TRUE))
              }
            }
            
            ## Compute averages for all relevant variables
            shooting <- average_by_NUTS(shooting, "shooting", NUTS.SELECTION)
            colnames(shooting) <- c(NUTS.SELECTION, "shooting")
            shooting$shooting <- round(shooting$shooting,)
            
            flowering <- average_by_NUTS(flowering, "flowering", NUTS.SELECTION)
            colnames(flowering) <- c(NUTS.SELECTION, "flowering")
            flowering$flowering <- round(flowering$flowering, 0) 
            
            fruit_formation <- average_by_NUTS(fruit_formation, "fruit_formation", NUTS.SELECTION)
            colnames(fruit_formation) <- c(NUTS.SELECTION, "fruit_formation")
            fruit_formation$fruit_formation <- round(fruit_formation$fruit_formation, 0)
            
            ripening <- average_by_NUTS(ripening, "ripening", NUTS.SELECTION)
            colnames(ripening) <- c(NUTS.SELECTION, "ripening")
            ripening$ripening <- round(ripening$ripening, 0)
            
            harvest <- average_by_NUTS(harvest, "harvest", NUTS.SELECTION)
            colnames(harvest) <- c(NUTS.SELECTION, "harvest")
            harvest$harvest <- round(harvest$harvest, 0)
          }
          
          ## Load combined data for the specified year from an .Rds file
          combined_data_year <- weather.list[[as.character(year)]]
          
          ## Loop through each day of combined data
          for (day in 1:length(combined_data_year)) {
          
            print(day)
            
            rm(daily_data)
            
            ## Access the data for each day
            daily_data <- combined_data_year[[day]]
            
            daily_data <- merge(daily_data, shooting, by = NUTS.SELECTION) 
            daily_data <- merge(daily_data, flowering, by = NUTS.SELECTION) 
            daily_data <- merge(daily_data, fruit_formation, by = NUTS.SELECTION) 
            daily_data <- merge(daily_data, ripening, by = NUTS.SELECTION) 
            daily_data <- merge(daily_data, harvest, by = NUTS.SELECTION) 
            
            # ---------------------------------------------------------------------- #
            ### Black Frost                                                        ###
            # ---------------------------------------------------------------------- #
            
            ## Function to calculate t_boundary (vectorized)
            calculate_t_boundary <- function(tmin, tmax, threshold) {
              result <- rep(NA, length(tmin))  ## Preallocate vector with NAs
              mask <- (tmin < threshold) & (tmax > threshold)  ## Identify valid indices
              
              result[mask] <- acos((2 * threshold - tmax[mask] - tmin[mask]) / (tmax[mask] - tmin[mask]))
              return(result)
            }
            
            ## Function to calculate CDD (vectorized)
            calculate_CDD <- function(tmin, tmax, snow_layer, threshold) {
              ## Compute t_boundary for all elements
              t_bar <- calculate_t_boundary(tmin, tmax, threshold)
              
              ## Preallocate output vector with zeros
              CDD <- rep(0, length(tmin))
              
              ## Case 1: If Tmin >= threshold and snow_layer < 5
              mask1 <- (!is.na(tmin) & tmin >= threshold & snow_layer < 5)
              CDD[mask1] <- 0
              
              ## Case 2: If Tmax <= threshold and snow_layer < 5
              mask2 <- (!is.na(tmax) & tmax <= threshold & snow_layer < 5)
              CDD[mask2] <- threshold - 0.5 * (tmax[mask2] + tmin[mask2])
              
              ## Case 3: If Tmin < threshold < Tmax and snow_layer < 5
              mask3 <- (!is.na(t_bar) & !is.na(tmin) & !is.na(tmax) & tmin < threshold & tmax > threshold & snow_layer < 5)
              CDD[mask3] <- (1 - (t_bar[mask3] / pi)) * (threshold - ((tmax[mask3] + tmin[mask3]) / 2)) +
                ((tmax[mask3] - tmin[mask3]) / (2 * pi)) * sin(t_bar[mask3])
              
              return(CDD)
            }
            
            ## Compute CDD for all rows in daily_data
            CDD <- calculate_CDD(
              tmin = daily_data$air_temperature_min,
              tmax = daily_data$air_temperature_max,
              snow_layer = daily_data$snow_layer,
              threshold = start_ww1
            )
            
            ## Calculate extreme events for each region
            daily_data$black_frost <- ifelse(day < daily_data$shooting &
                                               round(daily_data$air_temperature_min,1) <= start_ww1  &
                                               round(daily_data$snow_layer,1) < 5,
                                             CDD, 0)
            
            # ---------------------------------------------------------------------- #
            ### Heat                                                               ###
            # ---------------------------------------------------------------------- #      
            
            ## Function to calculate t_boundary (step 1)
            calculate_t_boundary_heat <- function(tmin, tmax, threshold) {
              result <- rep(NA, length(tmin))  ## Preallocate vector with NAs
              mask <- (tmin < threshold) & (tmax > threshold)  ## Identify valid indices
              
              result[mask] <- acos((2 * threshold - tmax[mask] - tmin[mask]) / (tmax[mask] - tmin[mask]))
              return(result)
            }
            
            ## Function to calculate Heat Stress (H_P99_ww)
            calculate_heat <- function(tmin, tmax, threshold) {
              ## Compute t_boundary for all elements
              t_bar <- calculate_t_boundary_heat(tmin, tmax, threshold)
              
              ## Preallocate output vector with zeros
              heat_stress <- rep(0, length(tmin))
              
              ## Case 1: If Tmax <= threshold → No heat stress
              mask1 <- (!is.na(tmax) & tmax <= threshold)
              heat_stress[mask1] <- 0
              
              ## Case 2: If Tmin >= threshold → Calculate temperature sum above threshold
              mask2 <- (!is.na(tmin) & tmin >= threshold)
              heat_stress[mask2] <- (0.5 * (tmax[mask2] + tmin[mask2])) - threshold
              
              ## Case 3: If Tmin < threshold < Tmax → Partial exceedance calculation
              mask3 <- (!is.na(t_bar) & !is.na(tmin) & !is.na(tmax) & tmin < threshold & tmax > threshold)
              heat_stress[mask3] <- (t_bar[mask3] / pi) * (((tmax[mask3] + tmin[mask3]) / 2) - threshold) +
                ((tmax[mask3] - tmin[mask3]) / (2 * pi)) * sin(t_bar[mask3])
              
              return(heat_stress)
            }
            
            HDD <- calculate_heat(
              tmin = daily_data$air_temperature_min,
              tmax = daily_data$air_temperature_max,
              threshold = flow_ww99
            )
            
            daily_data$heat <- ifelse(day >= daily_data$flowering &
                                        day < daily_data$fruit_formation &
                                        round(daily_data$air_temperature_max,1) >= flow_ww99,
                                      HDD, 0)
            
            # ---------------------------------------------------------------------- #
            ### Spring chill                                                       ###
            # ---------------------------------------------------------------------- # 
            daily_data$spring_chill <- 0  # Assuming no specific condition
            
            
            # ---------------------------------------------------------------------- #
            ### Spring drought                                                     ###
            # ---------------------------------------------------------------------- #  
            
            daily_data$spring_drought <- ifelse(day >= daily_data$shooting &
                                                  day < daily_data$fruit_formation & 
                                                  round(daily_data$soil_moist,1) <= shoot01,
                                                1, 0)
            
            # ---------------------------------------------------------------------- #
            ### Summer drought                                                     ###
            # ---------------------------------------------------------------------- #  
            
            daily_data$summer_drought <- ifelse(day >= daily_data$fruit_formation &
                                                  day < daily_data$harvest &
                                                  round(daily_data$soil_moist,1) <= fruit01,
                                                1, 0)
            
            # ---------------------------------------------------------------------- #
            ### Spring Waterlogging                                                ###
            # ---------------------------------------------------------------------- #  
            
            daily_data$spring_waterlogging <- ifelse(day >= daily_data$shooting &
                                                       day < daily_data$fruit_formation &
                                                       round(daily_data$soil_moist,1) >= shoot99,
                                                     1, 0)
            
            # ---------------------------------------------------------------------- #
            ### Summer Waterlogging                                                ###
            # ---------------------------------------------------------------------- #
            
            daily_data$summer_waterlogging <- ifelse(day >= daily_data$fruit_formation &
                                                       day < daily_data$harvest &
                                                       round(daily_data$soil_moist,1) >= fruit99,
                                                     1, 0)
            
            ## Convert daily_data to data.table for speed-up
            setDT(daily_data)
            
            ## Modify column name efficiently
            setnames(daily_data, NUTS.SELECTION, "NUTS")
            
            ## Aggregate using data.table (faster than aggregate())
            regional_summary <- daily_data[, .(
              Black_Frost = sum(black_frost, na.rm = TRUE),
              Spring_Chill = sum(spring_chill, na.rm = TRUE),
              Heat = sum(heat, na.rm = TRUE),
              Spring_Drought = sum(spring_drought, na.rm = TRUE),
              Summer_Drought = sum(summer_drought, na.rm = TRUE),
              Spring_Waterlogging = sum(spring_waterlogging, na.rm = TRUE),
              Summer_Waterlogging = sum(summer_waterlogging, na.rm = TRUE)
            ), by = NUTS]
            
            event_list[[day]] <- data.table(
              Year = year,
              NUTS = regional_summary$NUTS,
              Black_Frost = regional_summary$Black_Frost,
              Spring_Chill = regional_summary$Spring_Chill,
              Heat = regional_summary$Heat,
              Spring_Drought = regional_summary$Spring_Drought,
              Summer_Drought = regional_summary$Summer_Drought,
              Spring_Waterlogging = regional_summary$Spring_Waterlogging,
              Summer_Waterlogging = regional_summary$Summer_Waterlogging
            )
            
            ## Append the day's results (combine all at the end instead of row-wise rbind)
            extreme_events_df <- rbindlist(event_list, use.names = TRUE, fill = TRUE)
            
            ## Write modified daily data back to the original structure
            combined_data_year[[day]] <- data.frame(daily_data)
          }
          
          extreme.events.list[[as.character(year)]] <- data.frame(extreme_events_df)
          
          ## Save the updated yearly data back to the list
          weather.list[[as.character(year)]] <- combined_data_year
          
          }
        
        saveRDS(weather.list, file = paste0("03_Output/01_Extreme_Events/temp_weather_list_",
                                            NUTS.SELECTION,"_", extreme.event.crop, "_", filtering, "_", crop.filter.threshold, ".Rds"))
        saveRDS(extreme.events.list, file = paste0("03_Output/01_Extreme_Events/temp_extreme_events_list_",
                                                   NUTS.SELECTION,"_", extreme.event.crop, "_", filtering, "_", crop.filter.threshold, ".Rds"))
        
        
        extreme.events.list <- readRDS("03_Output/01_Extreme_Events/temp_extreme_events_list_LAU_winter.wheat_no_filter_10.Rds")
        
        ## Aggregate by Year for separate summary
        extreme.events.df <- as.data.frame(rbindlist(extreme.events.list))
        
        setDT(extreme.events.df)
        
        vars <- c("Black_Frost", "Heat", "Spring_Drought", "Summer_Drought",
                  "Spring_Waterlogging", "Summer_Waterlogging")
        
        yearly_summary <- extreme.events.df[
          , lapply(.SD, sum, na.rm = TRUE),
          by = Year,
          .SDcols = vars
        ]
        
        yearly_summary_by_NUTS <- extreme.events.df[
          , lapply(.SD, sum, na.rm = TRUE),
          by = .(Year, NUTS),
          .SDcols = vars
        ]
        
        yearly_summary_by_NUTS <- as.data.frame(yearly_summary_by_NUTS)
        
        
        write.csv(yearly_summary_by_NUTS,
                  file = paste0("03_Output/01_Extreme_Events/", NUTS.SELECTION, "_", 
                                extreme.event.crop, "_", filtering, "_", crop.filter.threshold, ".csv"),
                  row.names = FALSE)
      }
    }
  }
}
