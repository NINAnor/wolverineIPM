combineData_Rovbase <- function(data_CR, data_dead){
  
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
  
  # Join data by RovbaseID & drop all entries that are not linked to an individual
  data_CRR_full <- data_CR_forJoin %>%
    dplyr::full_join(data_dead_forJoin, by = "RovbaseID_Analysis") %>%
    dplyr::filter(!is.na(IndID) | !is.na(IndID_dead))
  
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