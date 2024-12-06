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


#-------------------------------#
# DATA WRANGLING & REFORMATTING #
#-------------------------------#

## Set paths, directories and filenames
path_rovbase <- paste0(data.dir, "/Data/DataRovbase/")
path_blacklist <- paste0(data.dir, "/Data/BlackList/")
path_carcass <- paste0(data.dir, "/Data/")
path_repro <- paste0(data.dir, "/Data/")
path_pedigree <- paste0(data.dir, "/Data/")

dir_shapefile <- paste0(data.dir, "/GIS/")
  
data_CR_name <- "DNA_1701202407381349.xlsx"
data_dead_name <- "Dead_18012024150338874.xlsx"
data_carcass_name <- "FellingOgSlakteTabell_20230117.xls"
data_repro_name <- "Reproductions2001_2023.xlsx"
data_pedigree_name <- "wolverine_pedigree_pr_09102024.xlsx"

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

## Filter & clean carcass data
data_carcass <- wrangleData_Carcass(path_carcass = path_carcass, 
                                    data_carcass_name = data_carcass_name)

## Filter & clean reproduction/litter data
data_repro <- wrangleData_Reproductions(path_repro = path_repro, 
                                        data_repro_name = data_repro_name,
                                        dir_shapefile = dir_shapefile)

## Load pedigree data (no filtering/cleaning needed)
data_pedigree <- readxl::read_xlsx(paste0(path_pedigree, data_pedigree_name))


#------------------------------#
# DATA FORMATTING & PREPARAION #
#------------------------------#

## Combine DNA and dead recovery data
data_CRR <- combineData_Rovbase(data_CR = data_CR,
                                data_dead = data_dead) 

## Collate individual-level information from CRR data
data_Ind <- extractIndInfo_CRR(data_CRR = data_CRR)

## Write traditional capture histories from CRR data
# TBA 

## Assemble simple kinship data (from pedigree and individual data)
data_kin <- assembleData_kin(data_Ind = data_Ind,
                             data_pedigree = data_pedigree,
                             use_biologicalYear = TRUE,
                             use_firstSample = TRUE)




