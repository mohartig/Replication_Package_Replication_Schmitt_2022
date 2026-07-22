# ---------------------------------------------------------------------------- #
#### 1. Download & Unzip
# ---------------------------------------------------------------------------- #

url <- "https://zenodo.org/records/14409498/files/EU_expected_crop_shares.zip?download=1"
destfile <- "02_Data/05_Crop_Map_Baumert_et_al/EU_expected_crop_shares.zip"
unzip_dir <- "02_Data/05_Crop_Map_Baumert_et_al/EU_expected_crop_shares"

## Download the file
GET(url, write_disk(destfile, overwrite = TRUE))

## Unzip the contents
unzip(zipfile = destfile, exdir = unzip_dir)

# ---------------------------------------------------------------------------- #
#### 2. List and inspect extracted files
# ---------------------------------------------------------------------------- #

files <- list.files(unzip_dir, recursive = TRUE, full.names = TRUE,
                    pattern = "\\.tif$", ignore.case = TRUE)

## Load raster stack
raster_files <- files[grepl("\\.tif$", files)]
r_stack <- terra::rast(raster_files)

raster.names <- c("weight","BARL","CITR","DWHE","FARA","FRUI","GRAS","INDU",
                  "MAIZ","OATS","OCER","OLIV","PARI","POTA","PULS","RAPE",
                  "ROOF","RYEM","SOYA","SUGB","SUNF","SWHE","TEXT","TOBA",
                  "VEGE","VINY")

r_stack_22 <- r_stack[[grep("_22$", names(r_stack), value = TRUE)]]
r_stack_17 <- r_stack[[grep("_17$", names(r_stack), value = TRUE)]]

## Download NUTS
europe_3035 <- giscoR::gisco_get_countries(resolution = "01",  epsg = 3035, country = NULL)
germany_3035 <- giscoR::gisco_get_countries(resolution = "01",  epsg = 3035, country = "Germany")

r_stack_22_cropped_germany <- terra::crop(r_stack_22, germany_3035)

# ---------------------------------------------------------------------------- #
#### 3. Filter where agri area of wheat > 1%
# ---------------------------------------------------------------------------- #

for (percent in c(1:100)) {
  agri_mask_germany_percent <- terra::app(r_stack_22_cropped_germany,
                                  fun = function(x) as.integer(any(x >= percent, na.rm = TRUE)))
  saveRDS(agri_mask_germany_percent, file = paste0(unzip_dir, "/Germany_Cropped_", percent,".Rds"))
}

agri_mask_germany <- terra::app(r_stack_22_cropped_germany,
                                fun = function(x) as.integer(any(x >= 10, na.rm = TRUE)))
agri_mask <- terra::app(r_stack_22,
                        fun = function(x) as.integer(any(x >= 10, na.rm = TRUE)))

# ---------------------------------------------------------------------------- #
#### 4. Save files
# ---------------------------------------------------------------------------- #

saveRDS(agri_mask_germany, file = paste0(unzip_dir, "/Germany_Cropped.Rds"))
saveRDS(agri_mask, file = paste0(unzip_dir, "/Europe.Rds"))

