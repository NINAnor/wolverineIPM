#' Combine DNA, dead, and carcass data from Rovbase in capture-recapture-recovery data
#'
#' @param data_CR a tibble containing formatted and filtered DNA 
#' capture-recapture data. Output of wrangleData_RovbasDNA. 
#' @param data_dead a tibble containing formatted and filtered dead recovery 
#' data. Output of wrangleData_RovbaseDead(). 
#'
#' @return a tibble containing matched and unique DNA (re)capture and dead 
#' recovery events. 
#' @export
#'
#' @examples

combineData_Rovbase <- function(data_CR, data_dead, data_carcass){
  
  # Select columns from capture-recapture DNA data for joining
  data_CR_forJoin <- data_CR %>%
    dplyr::select(RovbaseID_Analysis,
                  SampleDate,
                  SampleType,
                  Region,
                  IndID, Sex,
                  DeadAlive, DeadRecovery,
                  blacklisted_CR) %>%
    dplyr::mutate(inData_CR = TRUE)
  
  # Select columns from dead data for joining
  data_dead_forJoin <- data_dead %>%
    dplyr::select(RovbaseID_Analysis,
                  DeathDate,
                  DeathCause, 
                  HarvestMethod, HarvestOutcome,
                  Region, 
                  IndID, Sex, 
                  Age_assumed, Age_confirmed,
                  blacklisted_dead) %>%
    dplyr::rename(IndID_dead = IndID, 
                  Sex_dead = Sex,
                  Region_dead = Region) %>%
    dplyr::mutate(inData_Dead = TRUE)
  
  # Join data by RovbaseID & drop all entries that are not linked to an individual
  data_CRR_full <- data_CR_forJoin %>%
    dplyr::full_join(data_dead_forJoin, by = "RovbaseID_Analysis") %>%
    dplyr::filter(!is.na(IndID) | !is.na(IndID_dead))
  
  # Consolidate overlapping information from both datasets
  data_CRR_full <- data_CRR_full %>%
    dplyr::mutate(
      # Use death date as sampling date if no sampling date specified
      SampleDate = dplyr::case_when(!is.na(SampleDate) ~ SampleDate,
                                              !is.na(DeathDate) ~ DeathDate,
                                              TRUE ~ NA),
      # Use dead ID if ID is not specified
      IndID = dplyr::case_when(!is.na(IndID) ~ IndID,
                               !is.na(IndID_dead) ~ IndID_dead,
                               TRUE ~ NA),
      # Use dead region if region is not specified
      Region = dplyr::case_when(!is.na(Region) ~ Region,
                                !is.na(Region_dead) ~ Region_dead,
                                TRUE ~ NA),
      # Use dead Sex if sex is not specified
      Sex = dplyr::case_when(!is.na(Sex) ~ Sex,
                             !is.na(Sex_dead) ~ Sex_dead,
                             TRUE ~ NA),
      # Set state to "dead" when origin is dead data
      DeadAlive = ifelse(inData_Dead, "dead", DeadAlive),
      # Set dead recovery to "yes" when origin is dead data
      DeadRecovery = ifelse(inData_Dead, "yes", DeadRecovery),
      # Update data origin flags
      inData_CR = ifelse(inData_CR, TRUE, FALSE),
      inData_Dead = ifelse(inData_Dead, TRUE, FALSE)
    )
    
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