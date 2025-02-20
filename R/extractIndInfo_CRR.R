#' Extract individual-level information from capture-recapture-recovery data
#'
#' @param data_CRR a tibble containing matched and unique DNA (re)capture and dead 
#' recovery events. Output of combineData_Rovbase(). 
#'
#' @return a tibble containing one row per identified individual and listing
#' information on sex, estimated birth year(s), sampling years (first and last,
#' calendar years and "biological" years, which start with June and end with May),
#' and counts of observations (total, and unique years with observations).
#' @export
#'
#' @examples
#' 
extractIndInfo_CRR <- function(data_CRR = data_CRR){
  
  data_CRR <- data_CRR %>%
                    
    # Add sampling and birth years/months
    dplyr::mutate(SampleYear = lubridate::year(SampleDate),
                  SampleMonth = lubridate::month(SampleDate),
                  SampleYear_biological = ifelse(
                    lubridate::month(SampleDate) < 6, 
                    lubridate::year(SampleDate) - 1, 
                    lubridate::year(SampleDate)),
                  BirthYear_est = dplyr::case_when(
                    !is.na(Age_confirmed) ~ lubridate::year(SampleDate) - Age_confirmed, # Use confirmed age if available
                    Age_assumed == 0 ~ lubridate::year(SampleDate), # If confirmed age is not available, but it was assumed a YOY, set 0
                    TRUE ~ NA)) %>% # Otherwise NA
    
    # Add death years
    dplyr::mutate(DeathYear = ifelse(DeadAlive == "dead", SampleYear, NA),
                  DeathYear_biological = ifelse(DeadAlive == "dead", SampleYear_biological, NA)) %>%
    
    # Extract simple individual ID
    dplyr::mutate(id = dplyr::case_when(!is.na(IndID) ~ as.integer(stringr::str_split_fixed(IndID, pattern = "Ind", n = 2)[,2]),
                                        !is.na(IndID_dead) ~ as.integer(stringr::str_split_fixed(IndID_dead, pattern = "Ind", n = 2)[,2]),
                                        #!is.na(ID_carcass) ~ ID_carcass,
                                        TRUE ~ NA),
                  IndID = ifelse(!is.na(IndID), IndID, IndID_dead))
  
  # Summarise individual-level information
  data_Ind <- data_CRR %>%
    dplyr::filter(!is.na(id)) %>%
    dplyr::group_by(id, IndID) %>%
    
    dplyr::summarise(Sex = dplyr::case_when(
      all(is.na(Sex)) ~ NA,
      all(Sex %in% c("female", NA)) ~ "female",
      all(Sex %in% c("male", NA)) ~ "male",
      TRUE ~ "conflicting"
    ),
    BirthYear_est_mean = ifelse(all(is.na(BirthYear_est)), NA, mean(BirthYear_est, na.rm = TRUE)),
    BirthYear_est_min = ifelse(all(is.na(BirthYear_est)), NA, min(BirthYear_est, na.rm = TRUE)),
    BirthYear_est_max = ifelse(all(is.na(BirthYear_est)), NA, max(BirthYear_est, na.rm = TRUE)),
    FirstSampleYear = min(SampleYear),
    LastSampleYear = max(SampleYear),
    FirstSampleYear_biological = min(SampleYear_biological),
    LastSampleYear_biological = max(SampleYear_biological),
    DeathYear = ifelse(all(is.na(DeathYear)), NA, max(DeathYear, na.rm = TRUE)), # NOTE: I am using the max here but technically, every individual should only have one death year...
    DeathYear_biological = ifelse(all(is.na(DeathYear_biological)), NA, max(DeathYear_biological)),
    DeathYear_distinct = dplyr::n_distinct(DeathYear, na.rm = TRUE),
    Observations_n = dplyr::n_distinct(RovbaseID_Analysis),
    Observations_nbioyears = dplyr::n_distinct(SampleYear_biological),
    .groups = "keep") %>%
    dplyr::ungroup()
  
  # Check for conflicting individual information
  Sex_conflicts <- data_Ind %>%
    dplyr::filter(Sex == "conflicting")
  
  Sex_conflicts_n <- nrow(Sex_conflicts)
  
  BirthYear_conflicts <- data_Ind %>%
    dplyr::filter(!is.na(BirthYear_est_mean) & BirthYear_est_min != BirthYear_est_max)
  
  BirthYear_conflicts_n <- nrow(BirthYear_conflicts)
  
  if(Sex_conflicts_n > 0){
    message(paste0("The following ", Sex_conflicts_n, " individuals have conflicting sex information:"))
    print(cbind(Sex_conflicts$IndID))
  }else{
    message("There are no individuals with conflicting sex information.")
  }
  
  if(BirthYear_conflicts_n > 0){
    message(paste0("The following ", BirthYear_conflicts_n, " individuals have conflicting birth year information:"))
    print(cbind(BirthYear_conflicts$IndID))
  }else{
    message("There are no individuals with conflicting birth year information.")
  }
  
  DeathYear_conflicts <- data_Ind %>%
    dplyr::filter(DeathYear_distinct > 1)
  
  DeathYear_conflicts_n <- nrow(DeathYear_conflicts)
  
  if(DeathYear_conflicts_n > 0){
    message(paste0("The following ", DeathYear_conflicts_n, " individuals have conflicting death year information:"))
    print(cbind(DeathYear_conflicts$IndID))
  }else{
    message("There are no individuals with conflicting death year information.")
  }
  
  # Return individual-level data
  return(data_Ind)
}
