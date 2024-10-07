library(tidyverse)

options(tibble.width = Inf)

# SETUP #
#-------#

## Set data directory
data.dir <- "P:/12122261_rovdata_oppdrag_b_jerv"

## Source all functions in "R" folder
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
sourceDir('R')


# DATA PREPARATION & REFORMATTING #
#---------------------------------#

## Set paths, directories and filenames
path_rovbase <- paste0(data.dir, "/Data/DataRovbase/")
path_blacklist <- paste0(data.dir, "/Data/BlackList/")
dir_shapefile <- paste0(data.dir, "/GIS/")
  
data_CR_name <- "DNA_1701202407381349.xlsx"
data_dead_name <- "Dead_18012024150338874.xlsx"

blacklist_CR_name <- "Remove_samples.xlsx"
blacklist_dead_name <- "Remove_dead_recoveries.xlsx"

## Filter & clean Rovbase DNA data
data_CR <- wrangleData_RovbaseDNA(path_rovbase = path_rovbase, 
                                  data_CR_name = data_CR_name,
                                  path_blacklist = path_blacklist, 
                                  blacklist_CR_name = blacklist_CR_name,
                                  dir_shapefile = dir_shapefile)

## Filter & clean Rovbase dead recovery data
data_dead <- wrangleData_RovbaseDead(path_rovbase = path_rovbase, 
                                     data_dead_name = data_dead_name,
                                     path_blacklist = path_blacklist, 
                                     blacklist_dead_name = blacklist_dead_name,
                                     dir_shapefile = dir_shapefile)

