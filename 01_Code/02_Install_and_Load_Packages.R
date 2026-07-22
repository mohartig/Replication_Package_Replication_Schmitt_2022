## required packages vector
required_packages <- c(
  "dplyr", "sf", "terra", "ggplot2", "data.table", "rvest", "httr", "stringr",
  "ncdf4", "rdwd", "tidyverse", "fixest", "broom", "plm", "kableExtra", "glue",
  "xtable", "exactextractr", "fs", "utils", "eurostat")

## function for: install packages and load
install_and_load <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing package:", pkg))
    install.packages(pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

## run function
invisible(lapply(required_packages, install_and_load))