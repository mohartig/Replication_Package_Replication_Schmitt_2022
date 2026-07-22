# ---------------------------- IMPORTANT NOTE -------------------------------- #
# The DWD constantly updates and improves versions of data products. With this,
# links and access options change. The current replication runs without issues
# on 15th of July 2026.
# ---------------------------------------------------------------------------- #


# ------------------------------- Description -------------------------------- #
# This R script initializes and executes the complete replication workflow. 
# It first clears the workspace, defines the working directory and FADN data 
# location, and specifies whether weather and crop-map data must be downloaded 
# and processed. It then sets key parameters, including graphical resolution, 
# font size, coordinate reference system, spatial aggregation levels, crop type,
# crop-map filtering options, study years, and filtering thresholds. 
# 
# Required R packages are installed and loaded through an external script.
# When download_and_prepare is set to TRUE, the script downloads and processes 
# crop-map, weather, phenological, and snow data and subsequently derives 
# extreme-weather indicators. Finally, it runs the statistical estimation 
# independently of the download setting and produces the figures,
# summary statistics, and output results.


# ---------------------------- R Version Information ------------------------- #
# RStudio
# 2024.12.1 Build 563
# © 2009-2025 Posit Software, PBC
# "Kousa Dogwood" Release (27771613, 2025-02-02) for windows
# ---------------------------------------------------------------------------- #


# ---------------------------- Contact --------------------------------------- #
# Contact: Moritz Hartig
# E-Mail : moritz.hartig@icloud.com
# ---------------------------------------------------------------------------- #

## We start fresh
rm(list = ls())

## Set working directory & FADN path - change to your respective folder
setwd("L:/02_Daten/01_DETECT/09_Replication_Schmitt_2022/04_Schmitt_Replication_Replication_Package")
FADN.path <- "L:/02_Daten/01_DETECT/01_FADN/"
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
## Did you already download and process the weather data?
## If Yes --> set download_and_prepare to FALSE
## If No --> set download_and_prepare to TRUE
download_and_prepare <- FALSE
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
## Set graphical settings
set.dpi = 900
font.size <- 12

## SET CRS FOR ALL INPUTS
crs.EEA <- readRDS("02_Data/02_EEA_Grid/EEA_crs.rda")

## Set spatial options
NUTS.OPTIONS <- c("NUTS3", "NUTS2", "NUTS1", "LAU")

## Set up crop
crops <- c("winter.wheat")

## Set whether crop map filter should be calculated or not
crop.map.settings <- c("TRUE", "FALSE")

## Set up years
years <- c(2004:2020)

## Set the thresholds for the crop map filtering
thresholds <- c(5:15)

# ---------------------------------------------------------------------------- #
#### 1. Load Packages                                                       ####
# ---------------------------------------------------------------------------- #

source("01_Code/02_Install_and_Load_Packages.R")

# ---------------------------------------------------------------------------- #
#### 2. Download & prepare weather data                                     ####
# ---------------------------------------------------------------------------- #

if(download_and_prepare == TRUE){
  source("01_Code/01_Functions/Download_NUTS.R")
  source("02_Data/05_Crop_Map_Baumert_et_al/download_Baumert_et_al_crop_map.R")
  source("02_Data/03_DWD/01_Code/01_Download_Open_DWD_Data.R")
  source("02_Data/03_DWD/01_Code/03_Process_Files_to_NUTS_day.R")
  source("02_Data/03_DWD/01_Code/04_Phenological_Stages.R")
  source("02_Data/03_DWD/05_Snow_Data/Snow_Data.R")
}

# ---------------------------------------------------------------------------- #
#### 3.Run extreme event derivation                                         ####
# ---------------------------------------------------------------------------- #

if(download_and_prepare == TRUE){
  source("01_Code/03_Extreme_Events.R")
}

# ---------------------------------------------------------------------------- #
#### 4. Run SOSFF estimation                                                ####
# ---------------------------------------------------------------------------- #

source("01_Code/04_Estimation.R")

# ---------------------------------------------------------------------------- #
#### 5. Make plots                                                          ####
# ---------------------------------------------------------------------------- #

source("01_Code/05_Graphics_and_Statistics.R")

