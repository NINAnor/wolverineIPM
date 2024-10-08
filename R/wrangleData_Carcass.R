#' Wrangle carcass data
#'
#' This function extracts necessary information from the carcass data file
#' (.xlsx format), renames relevant columns, reformats/translates column content, 
#' summarises and consolidates information on fetuses from the dedicated column
#' and mentions in comments columns, and subsequently returns the cleaned and 
#' filtered data as a data frame. 
#' 
#' @param path_carcass character string. Path to where carcass data file
#' (.xlsx format) is stored.
#' @param data_carcass_name character string. Filename under which carcass
#' data file (.xlsx format) is stored.
#'
#' @return a data frame containing comprehensive information from necropsies of
#' Scandinavian wolverines back in time. 
#' @export
#'
#' @examples
#' 
wrangleData_Carcass <- function(path_carcass, data_carcass_name){
  
  ## Read in data
  data_carcass <- suppressWarnings(readxl::read_xls(paste0(path_carcass, data_carcass_name))) %>%
    
    ## Select relevant columns
    dplyr::select(`Id`,
                  `Rovbaseno`,
                  `Art`,
                  `Fellingsdato`,
                  `Fellingssted`,
                  `Dodsaarsaker`,
                  `Kjonn`,
                  `Alder`,
                  `Total`, `Skrott`,
                  `Kjonnsmodenhet`,
                  `Laktasjon`,
                  `CorpusRubrum_H`, `CorpusRubrum_V`,
                  `CorpusAlbicans_H`, `CorpusAlbicans_V`,
                  `TotaltCorporaLutea_H`, `TotaltCorporaLutea_V`,
                  `TotaltAntArr`,
                  `AntallFoster_H`, `AntallFoster_V`,
                  `Merknad1c`, `Merknader2`, `Merknad3a`, `Merknader4`, `Merknader5`,
                  `Merknader6`, `GenerelleMerknader7`,
                  `Kommunenr`,
                  `Fylkesnavn`
    ) %>%
    
    ## Rename relevant columns
    dplyr::rename(ID = `Id`,
                  RovbaseID_Analysis = `Rovbaseno`,
                  Species = `Art`,
                  DeathDate = `Fellingsdato`,
                  Location = `Fellingssted`,
                  DeathCause = `Dodsaarsaker`,
                  Sex = `Kjonn`,
                  Age_confirmed = `Alder`,
                  Weight_total = `Total`, Weight_clean = `Skrott`,
                  SexuallyMature = `Kjonnsmodenhet`,
                  Lactation = `Laktasjon`,
                  CorpusRubrum_right = `CorpusRubrum_H`, 
                  CorpusRubrum_left = `CorpusRubrum_V`,
                  CorpusAlbicans_right = `CorpusAlbicans_H`, 
                  CorpusAlbicans_left = `CorpusAlbicans_V`,
                  CorpusLuteum_right = `TotaltCorporaLutea_H`, 
                  CorpusLuteum_left = `TotaltCorporaLutea_V`,
                  PlacentalScars = `TotaltAntArr`,
                  Fetuses_right = `AntallFoster_H`, Fetuses_left = `AntallFoster_V`,
                  Comments_Rep1 = `Merknad1c`, Comments_Rep2 = `Merknader2`, 
                  Comments_Rep3 = `Merknad3a`, Comments_Rep4 = `Merknader4`, 
                  Comments_Rep5 = `Merknader5`, Comments_Rep6 = `Merknader6`, 
                  Comments_Rep7 = `GenerelleMerknader7`,
                  MunicipalityNo = `Kommunenr`,
                  County = `Fylkesnavn`
    )
  
  
  ## Check for (and remove) any "not-wolverines"
  otherSpp_count <- length(which(data_carcass$Species != "Jerv"))
  
  if(otherSpp_count > 0){
    message(paste0("Removed ", otherSpp_count, " entries from carcass data that were not wolverines."))
    
    data_carcass <- data_carcass %>%
      dplyr::filter(Species != "Jerv")
  }
  
  ## Summarise reproductive information for left & right sides
  data_carcass <- data_carcass %>%
    dplyr::mutate(CorpusRubrum = CorpusRubrum_left + CorpusRubrum_right,
                  CorpusAlbicans = CorpusAlbicans_left + CorpusAlbicans_right,
                  CorpusLuteum = CorpusLuteum_left + CorpusLuteum_right,
                  FetusNo = Fetuses_left + Fetuses_right) %>%
    dplyr::select(-CorpusRubrum_left, -CorpusRubrum_right,
                  -CorpusAlbicans_left, -CorpusAlbicans_right,
                  -CorpusLuteum_left, -CorpusLuteum_right,
                  -Fetuses_left, -Fetuses_right)
  
  ## Extract information on fetuses from comments
  extraInfo <- data_carcass[, c("ID", paste0("Comments_Rep", 1:7))] %>%
    dplyr::filter(!(all(is.na(c(Comments_Rep1, Comments_Rep2, Comments_Rep3, 
                                Comments_Rep4, Comments_Rep5, Comments_Rep6,
                                Comments_Rep7))))) %>%
    
    # For every comments column, extract string before "foster"
    dplyr::mutate(FetusInfo1_string = forstringr::str_extract_part(string = Comments_Rep1, pattern = "foster", before = TRUE),
                  FetusInfo2_string = forstringr::str_extract_part(string = Comments_Rep2, pattern = "foster", before = TRUE),
                  FetusInfo3_string = forstringr::str_extract_part(string = Comments_Rep3, pattern = "foster", before = TRUE),
                  FetusInfo4_string = forstringr::str_extract_part(string = Comments_Rep4, pattern = "foster", before = TRUE),
                  FetusInfo5_string = forstringr::str_extract_part(string = Comments_Rep5, pattern = "foster", before = TRUE),
                  FetusInfo6_string = forstringr::str_extract_part(string = Comments_Rep6, pattern = "foster", before = TRUE),
                  FetusInfo7_string = forstringr::str_extract_part(string = Comments_Rep7, pattern = "foster", before = TRUE)) %>%
    
    # For every comments column, extract last number from string before "foster"
    dplyr::mutate(FetusNo_fromComment1 = as.numeric(stringr::str_extract(FetusInfo1_string, stringr::regex("(\\d+)(?!.*\\d)"))),
                  FetusNo_fromComment2 = as.numeric(stringr::str_extract(FetusInfo2_string, stringr::regex("(\\d+)(?!.*\\d)"))),
                  FetusNo_fromComment3 = as.numeric(stringr::str_extract(FetusInfo3_string, stringr::regex("(\\d+)(?!.*\\d)"))),
                  FetusNo_fromComment4 = as.numeric(stringr::str_extract(FetusInfo4_string, stringr::regex("(\\d+)(?!.*\\d)"))),
                  FetusNo_fromComment5 = as.numeric(stringr::str_extract(FetusInfo5_string, stringr::regex("(\\d+)(?!.*\\d)"))),
                  FetusNo_fromComment6 = as.numeric(stringr::str_extract(FetusInfo6_string, stringr::regex("(\\d+)(?!.*\\d)"))),
                  FetusNo_fromComment7 = as.numeric(stringr::str_extract(FetusInfo7_string, stringr::regex("(\\d+)(?!.*\\d)"))))
  
  # Extract maximum number of fetuses reported in comments & drop intermediate columns
  extraInfo_sum <- extraInfo %>%
    dplyr::mutate(FetusNo_fromComments = pmax(FetusNo_fromComment1, FetusNo_fromComment2, FetusNo_fromComment3,
                                              FetusNo_fromComment4, FetusNo_fromComment5, FetusNo_fromComment6,
                                              FetusNo_fromComment7, na.rm = TRUE)) %>%
    dplyr::select(ID, FetusNo_fromComments) %>%
    dplyr::filter(!is.na(FetusNo_fromComments))
  
  ## Merge and consolidate information on number of fetuses from comments
  data_carcass <- data_carcass %>%
    dplyr::left_join(extraInfo_sum, by = "ID") %>%
    dplyr::mutate(FetusNo_consolidated = dplyr::case_when(
      !is.na(FetusNo) ~ FetusNo,
      !is.na(FetusNo_fromComments) ~ FetusNo_fromComments,
      TRUE ~ NA
    ))
  
  # Check for inconsistencies between information sources for fetus numbers
  FetusNo_conflicts_count <- nrow(subset(data_carcass, FetusNo != FetusNo_fromComments))
  message(paste0("There was conflicting information on fetus numbers from the columns `AntallFoster_H/V` and from comments (`Merknader`) for ", FetusNo_conflicts_count, " entries. Currently, information from `AntallFoster_H/V` is given priority in these cases."))
  
  ## Recode column content & drop comments
  data_carcass <- data_carcass %>%
    
    # Code sex and death cause
    dplyr::mutate(Sex = dplyr::case_when(Sex == 1 ~ "male",
                                         Sex == 2 ~ "female",
                                         is.na(Sex) ~ NA),
                  DeathCause = dplyr::case_when(DeathCause == 0 ~ "unknown",
                                                DeathCause == 1 ~ "harvest",
                                                DeathCause == 3 ~ "drowned",
                                                DeathCause == 4 ~ "poaching",
                                                DeathCause == 7 ~ "natural",
                                                DeathCause %in% c(8, 9, 10) ~ "other",
                                                TRUE ~ NA)
                  ) %>%

    # Drop comments columns
    dplyr::select(-Comments_Rep1, -Comments_Rep2, -Comments_Rep3,
                  -Comments_Rep4, -Comments_Rep5, -Comments_Rep6,
                  -Comments_Rep7)
  
  ## Retrun data
  return(data_carcass)
}