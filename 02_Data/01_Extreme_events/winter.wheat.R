## Constants (thresholds and phenological days)
start_ww1 <- -12.8   # Threshold for Black Frost
flow_ww99 <- 32.6    # Threshold for Heat
springchill <- NA
shoot01 <- 14.4      # Threshold for Drought during Shooting & Flowering
fruit01 <- 7.8       # Threshold for Drought during Fruit Formation & Ripening
shoot99 <- 115.3     # Threshold for Waterlogging during Shooting & Flowering
fruit99 <- 111.6     # Threshold for Waterlogging during Fruit Formation & Ripening
snow <- 5

if(NUTS.SELECTION %in% c("NUTS1", "NUTS2", "NUTS3")){phenological.load <- "NUTS"}
if(NUTS.SELECTION %in% c("LAU")){phenological.load <- "LAU"}


shooting <- readRDS(paste0("02_Data/03_DWD/06_PHENOLOGICAL_STAGES/shooting/shooting_", year, "_", extreme.event.crop, "_", phenological.load, ".Rds"))
flowering <- readRDS(paste0("02_Data/03_DWD/06_PHENOLOGICAL_STAGES/flowering/flowering_", year, "_", extreme.event.crop, "_", phenological.load, ".Rds"))
flowering$flowering <- flowering$flowering + 12
fruit_formation <- readRDS(paste0("02_Data/03_DWD/06_PHENOLOGICAL_STAGES/fruit_formation/fruit_formation_", year, "_", extreme.event.crop, "_", phenological.load, ".Rds"))
fruit_formation$fruit_formation <- fruit_formation$fruit_formation + 23
ripening <- readRDS(paste0("02_Data/03_DWD/06_PHENOLOGICAL_STAGES/ripening/ripening_", year, "_", extreme.event.crop, "_", phenological.load, ".Rds"))
harvest <- readRDS(paste0("02_Data/03_DWD/06_PHENOLOGICAL_STAGES/harvest/harvest_", year, "_", extreme.event.crop, "_", phenological.load, ".Rds"))

