## Download NUTS files
for(YEAR in c(2003, 2006, 2010, 2013, 2016, 2021)){
  for (NUTS in c("1", "2", "3")) {
    file <- eurostat::get_eurostat_geospatial(resolution = "01", nuts_level = NUTS, year = YEAR)
    saveRDS(file, paste0("02_Data/06_NUTS/", YEAR, "/NUTS", NUTS,".rds"))
    print(paste0("Done with downloading NUTS ", NUTS))
  }
}

## Download & Unzip LAU file
download.file("https://gisco-services.ec.europa.eu/distribution/v2/lau/shp/LAU_RG_01M_2020_4326.shp.zip",
              destfile = file.path("02_Data/06_NUTS/LAU/LAU_RG_01M_2020_4326.shp.zip"),
              mode = "wb")
unzip(zip_file, exdir = "02_Data/06_NUTS/LAU/")
