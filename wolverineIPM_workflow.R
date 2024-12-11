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


## Assemble simple close-kin mark-recapture data
assembleData_CKMR(KinPairType = "POP",
                  nYears_poolSamples = "all",
                  minYear = min(data_kin$SampleYear, na.rm = TRUE),
                  maxYear = max(data_kin$SampleYear, na.rm = TRUE))

## Create main pairwise comparison matrix (contains all comparisons)

# Create dataframe of pairwise comparisons with just individual offspring IDs
#pairwise.df <- data.frame(t(combn(data_kin$id, m = 2))) # Generates all the possible combinations. m = 2 means pair comparisons. t(x) gives the dimensions of the matrix (dataframe).
#colnames(pairwise.df) <- c("Ind_1", "id") #Rename columns so they can easily be joined

# Create dataframe that will be used to extract the birth years for the younger individual from each pairwise comparison using joins.
#Ind1_birthyears <- data_kin %>%
#  select(id, BirthYear, SampleYear, mom, dad, Sex) %>% 
#  dplyr::rename("Ind_1" = id, "Ind_1_birth" = BirthYear, "Ind_1_sampling" = SampleYear, "Ind_1_mom" = mom, "Ind_1_dad" = dad, "Ind_1_sex" = Sex) 

# Combine the two dataframes above to extract birth year and parents for each individual in the pairwise comparison matrix. 
#pairwise.df_all <- pairwise.df %>%
#  left_join(Ind1_birthyears, by = "Ind_1") %>%
#  left_join(data_kin, by = "id") %>%
#  dplyr::rename("Ind_2" = id, "Ind_2_birth" = BirthYear, "Ind_2_sampling" = SampleYear,
#                "Ind_2_mom" = mom, "Ind_2_dad" = dad, "Ind_2_sex" = Sex) %>% 
#  select(Ind_1, Ind_1_birth, Ind_1_sampling, Ind_1_sex, Ind_2, Ind_2_birth, Ind_2_sampling, Ind_2_sex, Ind_1_mom, Ind_1_dad, Ind_2_mom, Ind_2_dad) 


# Remove individuals (both offspring and parents) that are not in Rovbase / have no sampling year
data_kin <- data_kin %>%
  dplyr::mutate(id = ifelse(!is.na(SampleYear), id, NA),
                mom = ifelse(!is.na(mom_SampleYear), mom, NA),
                dad = ifelse(!is.na(dad_SampleYear), dad, NA)) %>%
  dplyr::filter(!is.na(id))

## Extract and add information for individuals appearing only as mothers/fathers
mom_data <- data_kin %>%
  dplyr::filter(!(mom %in% data_kin$id)) %>%
  dplyr::select(mom, mom_SampleYear, mom_BirthYear, mom_DeathYear) %>%
  dplyr::rename(id = mom, 
                SampleYear = mom_SampleYear,
                BirthYear = mom_BirthYear,
                DeathYear = mom_DeathYear) %>%
  dplyr::mutate(Sex = "female") %>%
  dplyr::distinct()

dad_data <- data_kin %>%
  dplyr::filter(!(dad %in% data_kin$id)) %>%
  dplyr::select(dad, dad_SampleYear, dad_BirthYear, dad_DeathYear) %>%
  dplyr::rename(id = dad, 
                SampleYear = dad_SampleYear,
                BirthYear = dad_BirthYear,
                DeathYear = dad_DeathYear) %>%
  dplyr::mutate(Sex = "male") %>%
  dplyr::distinct()

data_kin <- data_kin %>%
  dplyr::bind_rows(mom_data) %>%
  dplyr::bind_rows(dad_data)

## Create dataframe of pairwise comparisons with all combinations of all individual IDs (identified offspring and parents)
all_IDs <- unique(data_kin$id)
all_IDs <- all_IDs[-which(is.na(all_IDs))]

pairwise_IDs <- tidyr::crossing(all_IDs, all_IDs) %>%
  dplyr::rename(Ind_1 = "all_IDs...1",
                Ind_2 = "all_IDs...2") 

## Merge information on the first individual
data_pairs <- pairwise_IDs %>%
  dplyr::rename(id = Ind_1) %>% # Temporarily rename id column
  dplyr::left_join(data_kin, by = "id") %>%
  dplyr::rename(Ind_1 = id,
                Ind_1_mom = mom, Ind_1_dad = dad,
                Ind_1_SampleYear = SampleYear,
                Ind_1_BirthYear = BirthYear,
                Ind_1_DeathYear = DeathYear,
                Ind_1_Sex = Sex) %>%
  dplyr::select(-mom_SampleYear, -mom_BirthYear, -mom_DeathYear, 
                -dad_SampleYear, -dad_BirthYear, -dad_DeathYear)

## Merge information on the second individual
data_pairs <- data_pairs %>%
  dplyr::rename(id = Ind_2) %>% # Temporarily rename id column
  dplyr::left_join(data_kin, by = "id") %>%
  dplyr::rename(Ind_2 = id,
                Ind_2_mom = mom, Ind_2_dad = dad,
                Ind_2_SampleYear = SampleYear,
                Ind_2_BirthYear = BirthYear,
                Ind_2_DeathYear = DeathYear,
                Ind_2_Sex = Sex) %>%
  dplyr::select(-mom_SampleYear, -mom_BirthYear, -mom_DeathYear, 
                -dad_SampleYear, -dad_BirthYear, -dad_DeathYear)

# Drop all pairs that are impossible (i.e. where Ind_1 cannot be the parent of Ind_2)
# Criteria for exclusion:
# 0) No sampling year available (temporary criterion; these should not exist in the first place)
# 1) Ind_1 = Ind_2
# 2) Ind_1 was born in the same year as or after Ind_2 (requires birth years for both individuals)
# 3) Ind_1 died before reaching sexual maturity (requires birth and death date for Ind_1)
# 4) Ind_1 confirmed dead before Ind_2 was born (requires death date for Ind_1 & birth date for Ind_2 | that Ind_2 was sampled more than life span after death of Ind_1)
# 5) Ind_1 likely dead before Ind_2 was born (sampling interval of Ind_1 & Ind_2 > (lifespan - age at maturity) + lifespan)

lifespan <- 16
age_maturity <- 2 # This could be made sex-dependent... 

data_pairs <- data_pairs %>%
  dplyr::mutate(exclude = dplyr::case_when(
    is.na(Ind_1_SampleYear) | is.na(Ind_2_SampleYear) ~ TRUE, # 0
    Ind_1 == Ind_2 ~ TRUE, # 1
    !is.na(Ind_1_BirthYear) & !is.na(Ind_2_BirthYear) & Ind_1_BirthYear >= Ind_2_BirthYear ~ TRUE, # 2
    !is.na(Ind_1_DeathYear) & !is.na(Ind_1_BirthYear) & Ind_1_DeathYear - Ind_1_BirthYear < 2 ~ TRUE, # 3
    !is.na(Ind_1_DeathYear) & !is.na(Ind_2_BirthYear) & Ind_1_DeathYear < Ind_2_BirthYear ~ TRUE, # 4
    !is.na(Ind_1_DeathYear) & Ind_2_SampleYear > Ind_1_DeathYear + (lifespan - age_maturity) ~ TRUE, # 4
    abs(Ind_1_SampleYear - Ind_2_SampleYear) > (lifespan - age_maturity) + lifespan ~ TRUE, # 5
    TRUE ~ FALSE
  )) %>%
  dplyr::filter(!exclude)



## Subset data to desired time interval (also drops NA sample years)
data_kin <- data_kin %>%
  dplyr::filter(dplyr::between(SampleYear, minYear, maxYear))

## Set the number of years to pool
if(nYears_poolSamples == "all"){
  nYears <- length(min(data_kin$SampleYear, na.rm = TRUE):max(data_kin$SampleYear, na.rm = TRUE))
}else{
  nYears <- nYears_poolSamples
}

## List year ranges over which to pool samples
nYears_tot <- length(min(data_kin$SampleYear, na.rm = TRUE):max(data_kin$SampleYear, na.rm = TRUE))
nIntervals <- floor(nYears_tot/nYears)

yearRanges <- rep(NA, nIntervals)

for(t in 1:nIntervals){
  startYear <- minYear + (t-1)*nYears
  endYear <- startYear + nYears
  
  yearRanges[t] <- paste0(startYear, "-", endYear)
}
