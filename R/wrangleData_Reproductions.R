#' Wrangle reproduction data
#'
#' This function extracts necessary information from the reproductions data file
#' (.xlsx format), renames relevant columns, matches regions, reformats/translates 
#' column content, and subsequently returns the cleaned and filtered data as a 
#' data frame. 
#' 
#' @param path_repro character string. Path to where reproductions data file
#' (.xlsx format) is stored.
#' @param data_repro_name character string. Filename under which reproductions
#' data file (.xlsx format) is stored.
#' @param path_blacklist character string. Path to where blacklist for
#' @param dir_shapefile character string. Directory where shapefiles with 
#' polygons reflecting the Norwegian large carnivore regions ("Rovviltregioner")
#' are stored. 
#'
#' @return a data frame containing records of reproductions and den hunts of 
#' Scandinavian wolverines including relevant information on region. 
#' @export
#'
#' @examples
#' 
wrangleData_Reproductions <- function(path_repro, data_repro_name,
                                      dir_shapefile){
  
  data_repro <- suppressWarnings(readxl::read_xlsx(paste0(path_repro, data_repro_name))) %>%
    
    # Select relevant columns
    dplyr::select(`Aar`, 
                  `Rovbasenr`,
                  `Lokalitetsnr`, `Lokalitetsnavn`,
                  `Xkoord_UTM33N`, `Ykoord_UTM33N`,
                  `Kommune`,
                  `Vurdering`,
                  `Hiuttak`,
                  `Ant_voksne`, `Ant_valper`
    ) %>%
    
    # Rename relevant columns¨
    dplyr::rename(Year = `Aar`, 
                  RovbaseID = `Rovbasenr`,
                  LocalityNo = `Lokalitetsnr`, Locality = `Lokalitetsnavn`,
                  Coord_N = `Xkoord_UTM33N`, Coord_E = `Ykoord_UTM33N`,
                  Municipality = `Kommune`,
                  ReproEvent = `Vurdering`,
                  DenHunt = `Hiuttak`,
                  NoAdultFs_killed = `Ant_voksne`, NoPups_killed = `Ant_valper`)
  
  ## Remove entries without coordinates
  coordNA_count <- length(which(is.na(data_repro$Coord_N)))
  
  if(coordNA_count > 0){
    message(paste0("Removed ", coordNA_count, " entries from reproduction data that are lacking coordinate information."))
    
    data_repro <- data_repro %>%
      dplyr::filter(!is.na(Coord_N))
  }
  
  
  ## Add spatial information (region)
  
  # Read in shapefile
  region_polygon <- sf::st_as_sf(sf::st_read(dir_shapefile))
  
  # Add geometry to raw data and match to regions shapefile
  sf_data_repro <- data_repro %>%
    sf::st_as_sf(coords = c("Coord_N", "Coord_E"), crs = 32633) %>%
    sf::st_join(region_polygon, join = sf::st_within)
  
  data_repro$Region <- sf_data_repro$Region
  
  message("Region assigned based on shapefile (1-8 = Norway).")
  message("Region assignments for this dataset are not surrently subject to overwrite according to municipality/county.")
  
  ## Recode information on reproductive events
  data_repro <- data_repro %>%
    dplyr::mutate(ReproEvent = dplyr::case_when(ReproEvent == "Antatt yngling" ~ "assumed",
                                                ReproEvent == "Dokumentert yngling" ~ "confirmed"))
  
  ## Return data
  return(data_repro)
}