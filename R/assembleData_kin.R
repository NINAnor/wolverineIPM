#' Combine individual and pedigree data into kinship data
#'
#' @param data_Ind a tibble containing individual-level data from Rovbase.
#' Output of combineData_Rovbase(). 
#' @param data_pedigree a tibble containing pedigree data (offspring is, mother
#' id, father id). 
#' @param use_biologicalYear logical. If TRUE, uses biological year (start in 
#' June). If FALSE, uses calendar year (start in January).
#' @param use_firstSample logical. Uses The year of the first sample/detection
#' of the individual as sampling year.
#'
#' @return
#' @export
#'
#' @examples
#' 
assembleData_kin <- function(data_Ind, data_pedigree, use_biologicalYear, use_firstSample){
  
  # Add integer ID to individual data
  data_Ind <- data_Ind %>%
    dplyr::mutate(id = as.integer(stringr::str_split_fixed(IndID, pattern = "Ind", n = 2)[,2]))
  
  # Select relevant columns for individual data frame & duplicate columns for matching as parents
  data_Ind_sub <- data_Ind %>%
    dplyr::ungroup() %>%
    dplyr::mutate(SampleYear = dplyr::case_when(
      use_biologicalYear & use_firstSample ~ FirstSampleYear_biological,
      !use_biologicalYear & use_firstSample ~ FirstSampleYear,
      use_biologicalYear & !use_firstSample ~ LastSampleYear_biological,
      !use_biologicalYear & !use_firstSample ~ LastSampleYear),
      BirthYear = BirthYear_est_mean) %>%
    dplyr::select(id, SampleYear, BirthYear, Sex) %>%
    dplyr::mutate(mom_SampleYear = SampleYear,
                  dad_SampleYear = SampleYear,
                  mom_BirthYear = BirthYear,
                  dad_BirthYear = BirthYear)
  
  # Match individual data to pedigree data
  data_kin <- data_pedigree %>%
    dplyr::rename(mom = dam, 
                  dad = sire) %>%
    dplyr::left_join(data_Ind_sub[, c("id", "SampleYear", "BirthYear", "Sex")], 
                     by = "id") %>%
    dplyr::left_join(data_Ind_sub[, c("id", "mom_SampleYear", "mom_BirthYear")], 
                     by = dplyr::join_by(mom == id)) %>%
    dplyr::left_join(data_Ind_sub[, c("id", "dad_SampleYear", "dad_BirthYear")], 
                     by = dplyr::join_by(dad == id))
  
  # Return data
  return(data_kin)
}
