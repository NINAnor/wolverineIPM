#' Wrangle dead recovery data from Rovbase
#'
#' This function extracts necessary information from the Rovbase dead recovery
#' data output, renames relevant columns, drops blacklisted samples/observations, 
#' matches regions, reformats/translates column content, and subsequently returns 
#' the cleaned and filtered data as a data frame. 
#' 
#' @param path_rovbase character string. Path to where Rovbase dead recovery
#'  data export (.xlsx format) is stored.
#' @param data_dead_name character string. Filename under which Rovbase dead
#' recovery data export (.xlsx format) is stored.
#' @param path_blacklist character string. Path to where blacklist for
#' Rovbase dead recovery data (.xlsx format) is stored.
#' @param blacklist_dead_name character string. Filename under which blacklist for
#' Rovbase dead recovery data (.xlsx format) is stored. 
#' @param dir_shapefile character string. Directory where shapefiles with 
#' polygons reflecting the Norwegian large carnivore regions ("Rovviltregioner")
#' are stored. 
#'
#' @return a data frame containing dead recoveries of Scandinavian wolverines
#' including relevant information on location (country, region) and cause of
#' death, harvest method, and outcome (killed vs. injured).
#' @export
#'
#' @examples
#' 
wrangleData_RovbaseDead <- function(path_rovbase, data_dead_name,
                                    path_blacklist, blacklist_dead_name,
                                    dir_shapefile){
  
  ## Read in dead data and select/rename relevant columns
  data_dead <- suppressWarnings(readxl::read_xlsx(paste0(path_rovbase, data_dead_name))) %>%
    
    # Select relevant columns
    dplyr::select(`RovbaseID`,
                  `DNAID`,
                  `Art`,
                  `Bakgrunn/årsak`,
                  `Bakgrunn/årsak metode`,
                  `Utfall`,
                  `Dødsdato`,
                  `Funnsted`,
                  `Nord (UTM33/SWEREF99 TM)`, `Øst (UTM33/SWEREF99 TM)`,
                  `Individ`,
                  `Kjønn`,
                  `Alder, vurdert`, `Alder, verifisert`,
                  `Helvekt`,
                  `Kommunenummer`, `Kommune`,
                  `Fylkenummer`, `Fylke`
    ) %>%
    
    # Rename relevant columns
    dplyr::rename(RovbaseID_Analysis = `RovbaseID`,
                  DNAID_Analysis = `DNAID`,
                  Species = `Art`,
                  DeathCause = `Bakgrunn/årsak`,
                  HarvestMethod = `Bakgrunn/årsak metode`,
                  HarvestOutcome = `Utfall`,
                  DeathDate = `Dødsdato`,
                  Location = `Funnsted`,
                  Coord_N = `Nord (UTM33/SWEREF99 TM)`, Coord_E = `Øst (UTM33/SWEREF99 TM)`,
                  IndID = `Individ`,
                  Sex = `Kjønn`,
                  Age_assumed = `Alder, vurdert`, Age_confirmed = `Alder, verifisert`,
                  Weight = `Helvekt`,
                  MunicipalityNo = `Kommunenummer`, Municipality = `Kommune`,
                  CountyNo = `Fylkenummer`, County = `Fylke`)
  
  ## Read in corresponding blacklist
  blacklist_dead <- readxl::read_xlsx(paste0(path_blacklist, blacklist_dead_name))
  
  ## Remove blacklisted entries
  blacklist_count <- length(which(data_dead$RovbaseID_Analysis %in% blacklist_dead$Rovbase_ID))
  message(paste0("Flagged ", blacklist_count, " entries in dead recovery data that have Rovbase IDs on the blacklist."))
  
  data_dead <- data_dead %>%
    dplyr::mutate(blacklisted_dead = ifelse(RovbaseID_Analysis %in% blacklist_dead$Rovbase_ID, 1, 0))
  
  ## Remove entries without coordinates
  coordNA_count <- length(which(is.na(data_dead$Coord_N)))
  
  if(coordNA_count > 0){
    message(paste0("Flagged ", coordNA_count, " entries in dead recovery data that are lacking coordinate information."))
  }
  
  data_dead <- data_dead %>%
    dplyr::mutate(noCoordinates_dead = ifelse(!is.na(Coord_N), 1, 0))
  
  ## Check for (and remove) any "not-wolverines"
  otherSpp_count <- length(which(data_dead$Species != "Jerv"))
  
  if(otherSpp_count > 0){
    message(paste0("Removed ", otherSpp_count, " entries from dead recovery data that were not wolverines."))
    
    data_dead <- data_dead %>%
      dplyr::filter(Species != "Jerv")
  }
  
  ## Add spatial information (country)
  data_dead <- data_dead %>%
    dplyr::mutate(CountryCode = stringi::stri_extract_first_regex(MunicipalityNo, "[0-9]+")) %>%
    dplyr::mutate(Country = dplyr::case_when(CountryCode == 1 ~ "Norway",
                                             CountryCode == 2 ~ "Sweden",
                                             CountryCode == 5 ~ "Finland"))
  message("Country assigned based on field MunicipalityNo (originally Kommunenummer).")
  
  ## Add spatial information (region)
  
  # Read in shapefile
  region_polygon <- sf::st_as_sf(sf::st_read(dir_shapefile))
  
  # Add geometry to raw data and match to regions shapefile
  sf_data_dead <- data_dead %>%
    sf::st_as_sf(coords = c("Coord_E", "Coord_N"), crs = 32633, na.fail = FALSE) %>%
    sf::st_join(region_polygon, join = sf::st_within)
  
  data_dead$Region <- sf_data_dead$Region
  
  # Implement manual overwrite to ensure correct matching (NOTE: May become redundant if shapefile is adjusted)
  data_dead <- data_dead %>%
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
  
  overwrite_count <- length(which(data_dead$Region != data_dead$Region_shp))
  
  message("Region initally assigned based on shapefile (1-8 = Norway, 9 = Sweden/Finland).") 
  message(paste0("Region for ", overwrite_count, " entries overwritten based on County and Municipality."))
  
  
  ## Reformat contents of columns
  data_dead <- data_dead %>%
    dplyr::mutate(
      
      # Remove "dead flag" from individual ID
      IndID = stringr::str_trim(stringr::str_remove_all(IndID, pattern = "[^A-Za-z0-9 ]")),
      
      # Code death cause
      DeathCause_orig = DeathCause,
      DeathCause = dplyr::case_when(
        is.na(DeathCause) | DeathCause == "Ukjent" ~ "unknown",
        DeathCause == "Oppdrag SNO" ~ "harvest_SNO",
        DeathCause == "Lisensfelling" ~ "harvest_quota",
        DeathCause %in% c("Skadefelling", "Nødverge tamdyr") ~ "harvest_nuisance",
        DeathCause == "Dyrevelferdshensyn" ~ "harvest_welfare",
        DeathCause == "Forskningsrelatert" ~ "harvest_research",
        DeathCause == "Illegal avlivning" ~ "poaching",
        DeathCause == "Påkjørsel bil" ~ "roadkill",
        DeathCause == "Påkjørsel bane" ~ "railkill",
        DeathCause == "Sykdom" ~ "illness",
        DeathCause == "Ulykke" ~ "accident",
        DeathCause == "Drept av annet dyr" ~ "other_animal",
        TRUE ~ "uncoded"
      ),
      
      # Code harvest method
      HarvestMethod_orig = HarvestMethod,
      HarvestMethod = dplyr::case_when(
        is.na(HarvestMethod) ~ NA,
        HarvestMethod %in% c("Hagle", "Rifle") ~ "firearm_regular",
        HarvestMethod == "Helikopter" ~ "firearm_helicopter",
        HarvestMethod == "Hiuttak" ~ "den_hunt",
        HarvestMethod == "Fellefangst" ~ "trap",
        HarvestMethod == "Immobilisering" ~ "immobilisation",
        TRUE ~ "uncoded"
      ),
      
      # Code harvest outcome
      HarvestOutcome_orig = HarvestOutcome,
      HarvestOutcome = dplyr::case_when(
        is.na(HarvestOutcome) ~ NA,
        HarvestOutcome == "Felt" ~ "killed",
        HarvestOutcome == "Påskutt, skade påvist" ~ "injured",
        TRUE ~ "uncoded"
      ),
      
      # Code sex
      Sex = dplyr::case_when(Sex == "Hann" ~ "male",
                             Sex == "Hunn" ~ "female",
                             Sex == "Ukjent" | is.na(Sex) ~ NA),
      
      # Format age and weight as numeric
      Age_assumed = as.numeric(Age_assumed),
      Age_confirmed = as.numeric(Age_confirmed),
      Weight = as.numeric(gsub(pattern = ",", replacement = ".", x = data_dead$Weight))
    )
  
  ## Check for uncoded death/harvest information
  uncodedDC_count <- length(which(data_dead$DeathCause == "uncoded")) 
  uncodedHM_count <- length(which(data_dead$HarvestMethod == "uncoded")) 
  uncodedHO_count <- length(which(data_dead$HarvestOutcome == "uncoded")) 
  
  if(uncodedDC_count > 0){
    uncodedDC <- unique(subset(data_dead, DeathCause == "uncoded")$DeathCause_orig)
    message(paste0("There are uncoded death causes (original field `Bakgrunn/årsak`): ", paste(uncodedDC, collapse = ", ")))
  }
  
  if(uncodedHM_count > 0){
    uncodedHM <- unique(subset(data_dead, HarvestMethod == "uncoded")$HarvestMethod_orig)
    message(paste0("There are uncoded harvest methods (original field `Bakgrunn/årsak metode`): ", paste(uncodedHM, collapse = ", ")))
  }
  
  if(uncodedHO_count > 0){
    uncodedHO <- unique(subset(data_dead, HarvestOutcome == "uncoded")$HarvestOutcome_orig)
    message(paste0("There are uncoded dharvest outcomes (original field `Utfall`): ", paste(uncodedHO, collapse = ", ")))
  }
  
  ## Fix any case mistakes in IDs
  data_dead <- data_dead %>%
    dplyr::mutate(IndID = stringi::stri_replace_all_regex(IndID,
                                                 pattern = c("ind", "INd"),
                                                 replacement = c("Ind"),
                                                 vectorize = FALSE))
  
  ## Return data
  return(data_dead)
}

