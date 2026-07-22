#------------------------------------------------------------------#
####--------------- DEFINE THE NUTS FILE TO BE USED ------------####
#------------------------------------------------------------------#

## load function for next lower year
source("01_Code/01_Functions/Next_year_function.R")

## get the next smaller year
NUTS.year <- getNextYear(year, c("2003", "2006", "2010", "2013", "2016" , "2021"))

#------------------------------------------------------------------#
####--------------- LOAD IN THE NUTS POLYGONS ------------------####
#------------------------------------------------------------------#

NUTS1 <- readRDS(paste0("02_Data/06_NUTS/", NUTS.year,"/NUTS1.rds"))
NUTS2 <- readRDS(paste0("02_Data/06_NUTS/", NUTS.year,"/NUTS2.rds"))
NUTS3 <- readRDS(paste0("02_Data/06_NUTS/", NUTS.year,"/NUTS3.rds"))
LAU <- readRDS(paste0("02_Data/06_NUTS/", NUTS.year,"/NUTS3.rds"))

#### --------------------------- GERMANY ---------------------- ####
NUTS1.germany <- NUTS1[NUTS1$CNTR_CODE %in% "DE", ]
NUTS2.germany <- NUTS2[NUTS2$CNTR_CODE %in% "DE", ]
NUTS3.germany <- NUTS3[NUTS3$CNTR_CODE %in% "DE", ]

NUTS1.germany.transformed <- sf::st_transform(NUTS1.germany, crs = crs.EEA)
NUTS2.germany.transformed <- sf::st_transform(NUTS2.germany, crs = crs.EEA)
NUTS3.germany.transformed <- sf::st_transform(NUTS3.germany, crs = crs.EEA)



## Define the URL for the LAU shapefiles
LAU_url <- "https://gisco-services.ec.europa.eu/distribution/v2/lau/shp/LAU_RG_01M_2020_4326.shp.zip"

## Define a local directory to save the files
save_dir <- paste0(getwd(), "/02_Data/06_NUTS/LAU")
zip_file <- file.path(save_dir, "LAU_RG_01M_2020_4326.shp.zip")  # Path for the zip file

## Download the file
# download.file(LAU_url, zip_file, mode = "wb")

## Unzip the downloaded file
# unzip(zip_file, exdir = save_dir)

## Find the shapefile (ends with .shp)
shp_file <- list.files(save_dir, pattern = "\\.shp$", full.names = TRUE)

## Load the shapefile into R as an sf object
lau_data <- st_read(shp_file)

## Filter for Germany
lau_data <- lau_data %>% filter(CNTR_CODE == "DE")
