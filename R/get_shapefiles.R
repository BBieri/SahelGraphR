#' Download (Sub)National Shapefiles
#'
#' Function to download (sub)national shapefiles for the four countries in the
#' Sahel ASP RCT.
#'
#' @param country_code One of c("BFA", "NER", "MRT", "SEN"). ISO country code.
#' @param level One of c(0, 1, 2, 3). Administrative level.
#'
#' @return An `{sf}` object ready to be worked with.
#' @export
#'
#' @examples
#' (SEN <- get_shapefiles(country_code = "SEN", level = 2) |>
#'   dplyr::rename(ADM2_NAME = ADM2_FR) |>
#'   dplyr::select(ADM2_NAME, geometry))
#' (BFA <- get_shapefiles(country_code = "BFA", level = 2) |>
#'   dplyr::rename(ADM2_NAME = adm2_name1) |>
#'   dplyr::select(ADM2_NAME, geometry))
#' (NER <- get_shapefiles(country_code = "NER", level = 2) |>
#'   dplyr::rename(ADM2_NAME = adm_02) |>
#'   dplyr::select(ADM2_NAME, geometry))
#' (MRT <- get_shapefiles(country_code = "MRT", level = 2) |>
#'   dplyr::rename(ADM2_NAME = ADM2_EN) |>
#'   dplyr::select(ADM2_NAME, geometry))


get_shapefiles <-
  function(country_code = c("BFA", "NER", "MRT", "SEN"),
           level = 1) {
    # This ifelse structure could be extended to retrieve subnational indicators
    if (country_code == "BFA") {
      link <-
        "https://data.humdata.org/dataset/2940ed80-4b69-4b98-abaf-af79088852c5/resource/931f500c-1ae6-4812-b2e0-840cc0573672/download/bf_hno_admin_0_3.zip"
      if (level == 0) {
        message("National Level Selected")
        sf_path <- "tempdata/bf_hno_admin_0_3/bf_admin0_hno_2022.shp"
      } else if (level == 1) {
        message("Regional Level Selected")
        sf_path <- "tempdata/bf_hno_admin_0_3/bf_admin1_hno_2022.shp"
      } else if (level == 2) {
        message("Departement Level Selected")
        sf_path <- "tempdata/bf_hno_admin_0_3/bf_admin2_hno_2022.shp"
      } else if (level == 3) {
        message("Arrondissement Level Selected")
        sf_path <- "tempdata/bf_hno_admin_0_3/bf_admin3_hno_2022.shp"
      } else {
        stop("Wrong level specified. Needs to be between 0 and 3.")
      }
    } else if (country_code == "NER") {
      if (level == 0) {
        message("National Level Selected")
        link <- "https://data.humdata.org/dataset/c0e0998c-b45a-4aea-ac06-c1de1d94e596/resource/fc164d0b-0df4-45ca-a709-a540e68e62ee/download/ner_adm00_feb2018.zip"
        sf_path <- "tempdata/NER_adm00_feb2018.shp"
      } else if (level == 1) {
        message("Regional Level Selected")
        link <- "https://data.humdata.org/dataset/c0e0998c-b45a-4aea-ac06-c1de1d94e596/resource/94e06212-3f8f-4c74-a433-1b132d059ea8/download/ner_adm01_feb2018.zip"
        sf_path <- "tempdata/NER_adm01_feb2018.shp"
      } else if (level == 2) {
        message("Departement Level Selected")
        link <- "https://data.humdata.org/dataset/c0e0998c-b45a-4aea-ac06-c1de1d94e596/resource/4b2bfec2-2f26-4354-bfe0-801f161c23fb/download/ner_adm02_feb2018.zip"
        sf_path <- "tempdata/NER_adm02_feb2018.shp"
      } else if (level == 3) {
        message("Arrondissement Level Selected")
        link <- "https://data.humdata.org/dataset/c0e0998c-b45a-4aea-ac06-c1de1d94e596/resource/847c8435-38e2-4fdf-be6f-973966f77470/download/ner_adm03_feb2018.zip"
        sf_path <- "tempdata/NER_adm03_feb2018.shp"
      } else {
        stop("Wrong level specified. Needs to be between 0 and 3.")
      }
    } else if (country_code == "MRT") {
      link <-
        "https://data.humdata.org/dataset/8d49f50d-92a8-46d9-9462-f821a8058f6d/resource/dacb6ad2-13b6-4f14-b1e9-44b800d76e58/download/mrt_adm_gov_itos_20200801_shp.zip"
      if (level == 0) {
        message("National Level Selected")
        sf_path <- "tempdata/mrt_admbnda_adm0_gov_itos_20200801.shp"
      } else if (level == 1) {
        message("Regional Level Selected")
        sf_path <- "tempdata/mrt_admbnda_adm1_gov_20200801.shp"
      } else if (level == 2) {
        message("Departement Level Selected")
        sf_path <- "tempdata/mrt_admbnda_adm2_gov_20200801.shp"
      } else if (level == 3) {
        message("Arrondissement Level Selected")
        stop("Arrondissement level not availiable. Use departement level instead.")
      } else {
        stop("Wrong level specified. Needs to be between 0 and 3.")
      }
    } else if (country_code == "SEN") {
      link <-
        "https://data.humdata.org/dataset/bd9bc484-155d-41a3-87cf-064310a94492/resource/4ef61299-7edb-4529-aa09-76137c14962a/download/shapefiles.zip"
      if (level == 0) {
        message("National Level Selected")
        sf_path <-
          "tempdata/Shapefiles/sen_admbnda_adm0_1m_gov_ocha_20190426.shp"
      } else if (level == 1) {
        message("Regional Level Selected")
        sf_path <-
          "tempdata/Shapefiles/sen_admbnda_adm1_1m_gov_ocha_20190426.shp"
      } else if (level == 2) {
        message("Departement Level Selected")
        sf_path <-
          "tempdata/Shapefiles/sen_admbnda_adm2_1m_gov_ocha_20190426.shp"
      } else if (level == 3) {
        message("Arrondissement Level Selected")
        sf_path <-
          "tempdata/Shapefiles/sen_admbnda_adm3_1m_gov_ocha_20190426.shp"
      } else {
        stop("Wrong level specified. Needs to be between 0 and 3.")
      }
    } else {
      stop("Country not supported.")
    }
    # Download
    temp = tempfile()
    download.file(link, temp)
    unzip(temp, exdir = "tempdata") # Extract to tempdata folder
    # Read SF object
    out <- sf::read_sf(sf_path)
    # Clean up
    unlink("tempdata", recursive = T)
    return(out)
}
