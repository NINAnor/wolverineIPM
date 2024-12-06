#' Combine DNA, dead, and carcass data from Rovbase in capture-recapture-recovery data
#'
#' @param data_CR a tibble containing formatted and filtered DNA 
#' capture-recapture data. Output of wrangleData_RovbasDNA. 
#' @param data_dead a tibble containing formatted and filtered dead recovery 
#' data. Output of wrangleData_RovbaseDead(). 
#' @param data_carcass a tibble containing formatted and filtered carcass data.
#' Output of wrangleData_carcass().
#' @param mergeCarcassData logical. Whether (= TRUE) or not (= FALSE, default) to
#' merge information from carcass data by ID. As of now, I have to clarify whether
#' the ID's are compatible. 
#'
#' @return a tibble containing matched and unique DNA (re)capture and dead 
#' recovery events. 
#' @export
#'
#' @examples

combineData_Rovbase <- function(data_CR, data_dead, data_carcass, mergeCarcassData = FALSE){
  
  # Select columns from capture-recapture DNA data for joining
  data_CR_forJoin <- data_CR %>%
    dplyr::select(RovbaseID_Analysis,
                  SampleDate,
                  SampleType,
                  Region,
                  IndID, Sex,
                  DeadAlive, DeadRecovery) %>%
    dplyr::mutate(inData_CR = TRUE)
  
  # Select columns from dead data for joining
  data_dead_forJoin <- data_dead %>%
    dplyr::select(RovbaseID_Analysis,
                  DeathDate,
                  DeathCause, 
                  HarvestMethod, HarvestOutcome,
                  Region, 
                  IndID, Sex, 
                  Age_assumed, Age_confirmed) %>%
    dplyr::rename(IndID_dead = IndID, 
                  Sex_dead = Sex,
                  Region_dead = Region) %>%
    dplyr::mutate(inData_Dead = TRUE)
  
  if(mergeCarcassData){
    
    # Select columns and rows from carcass data for joining
    Region3_Municipalities <- stringr::str_split_fixed(subset(data_CR, Region == 3)$MunicipalityNo, pattern = "-", n = 2)[,2]
    Region5_Municipalities <- stringr::str_split_fixed(subset(data_CR, Region == 5)$MunicipalityNo, pattern = "-", n = 2)[,2]
    
    data_carcass_forJoin <- data_carcass %>%
      dplyr::filter(!is.na(RovbaseID_Analysis) & !(RovbaseID_Analysis %in% data_dead$RovbaseID_Analysis)) %>%
      dplyr::mutate(Region = dplyr::case_when(County %in% c("Rogaland", "Vestland") ~ 1,
                                              County %in% c("Agder", "Buskerud", "Telemark", "Vestfold", "Vestfold og Telemark", "Viken") ~ 2,
                                              County == "Oppland" ~ 3,
                                              County == "Hedmark" ~ 5,
                                              County == "Innlandet" & MunicipalityNo %in% Region3_Municipalities ~ 3,
                                              County == "Innlandet" & MunicipalityNo %in% Region5_Municipalities ~ 5,
                                              County %in% c("Møre og Romsdal", "Trøndelag", "Nord-Trøndelag", "Sør-Trøndelag") ~ 6,
                                              County == "Nordland" ~ 7,
                                              County == "Finnmark" ~ 8,
                                              TRUE ~ NA),
                    inData_Carcass = TRUE) %>%
      dplyr::select(RovbaseID_Analysis, 
                    DeathDate,
                    DeathCause,
                    Region,
                    ID, Sex,
                    Age_confirmed) %>%
      dplyr::rename(ID_carcass = ID,
                    Sex_dead = Sex,
                    Region_dead = Region)
    
    # Combine dead and carcass data
    data_dead_forJoin <- data_dead_forJoin %>%
      dplyr::bind_rows(data_carcass_forJoin)
  }
  
  # Join data by RovbaseID & drop all entries that are not linked to an individual
  if(mergeCarcassData){
    data_CRR_full <- data_CR_forJoin %>%
      dplyr::full_join(data_dead_forJoin, by = "RovbaseID_Analysis") %>%
      dplyr::filter(!is.na(IndID) | !is.na(IndID_dead) | !is.na(ID_carcass))
  }else{
    data_CRR_full <- data_CR_forJoin %>%
      dplyr::full_join(data_dead_forJoin, by = "RovbaseID_Analysis") %>%
      dplyr::filter(!is.na(IndID) | !is.na(IndID_dead))
  }
  
  # Check for and notify about conflicts between the two datasets
  test <- subset(data_CRR_full, inData_CR & inData_Dead)
  
  nConflicts_Date <- length(which(test$SampleDate != test$DeathDate))
  nConflicts_Region <- length(which(test$Region != test$Region_dead))
  nConflicts_IndID <- length(which(test$IndID != test$IndID_dead))
  nConflicts_Sex <- length(which(test$Sex != test$Sex_dead))
  
  message("Checking for conflicting information from both datasets...")
  message(paste0(nConflicts_Date, " entries with conflicting date."))
  message(paste0(nConflicts_Region, " entries with conflicting region."))
  message(paste0(nConflicts_IndID, " entries with conflicting individual ID."))
  message(paste0(nConflicts_Sex, " entries with conflicting sex."))
  
  conflict_list <- list(
    Date_conflicts = subset(test, SampleDate != DeathDate),
    Region_conflicts = subset(test, Region != Region_dead),
    IndID_conflicts = subset(test, IndID != IndID_dead),
    Sex_conflicts = subset(test, Sex != Sex_dead)
  )
  
  saveRDS(conflict_list, file = "RovbaseData_Conflicts_DNA_vs_Dead.rds")
  message("Conflicting entires stored in 'RovbaseData_Conflicts_DNA_vs_Dead.rds' for review.")
  
  # Return combined data
  return(data_CRR_full)
}