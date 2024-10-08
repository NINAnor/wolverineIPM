#' Wrangle DNA (mark-recapture) data from Rovbase
#'
#' This function extracts necessary information from the Rovbase DNA data output,
#' renames relevant columns, drops blacklisted samples/observations, matches 
#' region and dead/alive status, and subsequently returns the cleaned and 
#' filtered data as a data frame. 
#' 
#' @param path_rovbase character string. Path to where Rovbase DNA data export 
#' (.xlsx format) is stored.
#' @param data_CR_name character string. Filename under which Rovbase DNA data 
#' export (.xlsx format) is stored.
#' @param path_blacklist character string. Path to where blacklist for
#' Rovbase DNA data (.xlsx format) is stored.
#' @param blacklist_CR_name character string. Filename under which blacklist for
#' Rovbase DNA data (.xlsx format) is stored. 
#' @param dir_shapefile character string. Directory where shapefiles with 
#' polygons reflecting the Norwegian large carnivore regions ("Rovviltregioner")
#' are stored. 
#'
#' @return a data frame containing DNA observations of Scandinavian wolverines
#' including relevant information on location (country, region) and dead/alive
#' state. 
#' @export
#'
#' @examples
#' 
wrangleData_RovbaseDNA <- function(path_rovbase, data_CR_name,
                                   path_blacklist, blacklist_CR_name,
                                   dir_shapefile){
  
  ## Read in DNA CR data and select/rename relevant columns
  data_CR <- suppressWarnings(readxl::read_xlsx(paste0(path_rovbase, data_CR_name))) %>%
    
    # Select relevant columns
    dplyr::select(`DNAID (Prøve)`,
                  `DNAID (Analyse)`,
                  `RovbaseID (Prøve)`,
                  `RovbaseID (Analyse)`,
                  `Art (Analyse)`,
                  `Prøvetype`,
                  `Prøvestatus`,
                  `Funnetdato`,
                  `Funnsted`,
                  `Nord (UTM33/SWEREF99 TM)`, `Øst (UTM33/SWEREF99 TM)`,
                  `Individ`,
                  `Kjønn`,
                  `Kommunenummer`, `Kommune`,
                  `Fylkenummer`, `Fylke`
    ) %>%
    
    # Rename relevant columns
    dplyr::rename(DNAID_Sample = `DNAID (Prøve)`,
                  DNAID_Analysis = `DNAID (Analyse)`,
                  RovbaseID_Sample = `RovbaseID (Prøve)`,
                  RovbaseID_Analysis = `RovbaseID (Analyse)`,
                  Species = `Art (Analyse)`,
                  SampleType = `Prøvetype`,
                  SampleStatus = `Prøvestatus`,
                  SampleDate = `Funnetdato`,
                  Location = `Funnsted`,
                  Coord_N = `Nord (UTM33/SWEREF99 TM)`, Coord_E = `Øst (UTM33/SWEREF99 TM)`,
                  IndID = `Individ`,
                  Sex = `Kjønn`,
                  MunicipalityNo = `Kommunenummer`, Municipality = `Kommune`,
                  CountyNo = `Fylkenummer`, County = `Fylke`)
  
  ## Read in corresponding blacklist
  blacklist_CR <- readxl::read_xlsx(paste0(path_blacklist, blacklist_CR_name))
  
  ## Remove blacklisted entries
  blacklist_count <- length(which(data_CR$DNAID_Analysis %in% blacklist_CR$DNAID_RB))
  message(paste0("Removed ", blacklist_count, " entries from CR data that have DNA IDs on the blacklist."))
  
  data_CR <- data_CR %>%
    dplyr::filter(!(DNAID_Analysis %in% blacklist_CR$DNAID_RB))
  
  ## Remove entries without coordinates
  coordNA_count <- length(which(is.na(data_CR$Coord_N)))
  message(paste0("Removed ", coordNA_count, " entries from CR data that are lacking coordinate information."))
  
  data_CR <- data_CR %>%
    dplyr::filter(!is.na(Coord_N))
  
  ## Check for (and remove) any "not-wolverines"
  otherSpp_count <- length(which(data_CR$Species != "Jerv"))
  
  if(otherSpp_count > 0){
    message(paste0("Removed ", otherSpp_count, " entries from CR data that were not wolverines."))
    
    data_CR <- data_CR %>%
      dplyr::filter(Species != "Jerv")
  }
  
  ## Add spatial information (country)
  data_CR <- data_CR %>%
    dplyr::mutate(CountryCode = stringi::stri_extract_first_regex(MunicipalityNo, "[0-9]+")) %>%
    dplyr::mutate(Country = dplyr::case_when(CountryCode == 1 ~ "Norway",
                                             CountryCode == 2 ~ "Sweden",
                                             CountryCode == 5 ~ "Finland"))
  message("Country assigned based on field MunicipalityNo (originally Kommunenummer).")
  
  ## Add spatial information (region)
  
  # Read in shapefile
  region_polygon <- sf::st_as_sf(sf::st_read(dir_shapefile))

  # Add geometry to raw data and match to regions shapefile
  sf_data_CR <- data_CR %>%
    sf::st_as_sf(coords = c("Coord_E", "Coord_N"), crs = 32633) %>%
    sf::st_join(region_polygon, join = sf::st_within)
  
  data_CR$Region <- sf_data_CR$Region
  
  # Implement manual overwrite to ensure correct matching (NOTE: May become redundant if shapefile is adjusted)
  data_CR <- data_CR %>%
    dplyr::rename(Region_shp = Region) %>%
    dplyr::mutate(Region = dplyr::case_when(
      Country %in% c("Sweden", "Finland") ~ 9, # This is a dummy code that we will use later
      
      County %in% c("Rogaland (N)", "Vestland (N)") ~ 1,
      County == "Buskerud (N)" ~ 2,
      County %in% c("Møre og Romsdal (N)", "Trøndelag (N)") ~ 6,
      County == "Nordland (N)" ~ 7,
      County %in% c("Finnmark (N)", "Troms (N)") ~ 8,
      
      County == "Innlandet (N)" & Region_shp == 1 ~ 3,
      
      Municipality %in% c("Lesja (N)", "Dovre (N)") ~ 3,
      Municipality %in% c("Engerdal (N)", "Os (N)", "Tynset (N)", "Folldal (N)") ~ 5,
      
      TRUE ~ Region_shp
    ))
  
  overwrite_count <- length(which(data_CR$Region != data_CR$Region_shp))
  
  message("Region initally assigned based on shapefile (1-8 = Norway, 9 = Sweden/Finland).") 
  message(paste0("Region for ", overwrite_count, " entries overwritten based on County and Municipality."))
  
  
  ## Split dead/alive status from individual ID & recode sex
  data_CR <- data_CR %>%
    dplyr::mutate(DeadRecovery = ifelse(grepl("[^A-Za-z0-9 ]", IndID), "yes", "no"),
                  IndID = stringr::str_trim(stringr::str_remove_all(IndID, pattern = "[^A-Za-z0-9 ]")),
                  DeadAlive = ifelse(substr(RovbaseID_Sample, start = 1, stop = 1) == "M", "dead", "alive"),
                  Sex = dplyr::case_when(Sex == "Hann" ~ "male",
                                         Sex == "Hunn" ~ "female",
                                         Sex == "Ukjent" | is.na(Sex) ~ NA))
  
  ## Return data
  return(data_CR)
}