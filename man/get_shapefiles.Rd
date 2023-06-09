% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_shapefiles.R
\name{get_shapefiles}
\alias{get_shapefiles}
\title{Download (Sub)National Shapefiles}
\usage{
get_shapefiles(country_code = c("BFA", "NER", "MRT", "SEN"), level = 1)
}
\arguments{
\item{country_code}{One of c("BFA", "NER", "MRT", "SEN"). ISO country code.}

\item{level}{One of c(0, 1, 2, 3). Administrative level.}
}
\value{
An \code{{sf}} object ready to be worked with.
}
\description{
Function to download (sub)national shapefiles for the four countries in the
Sahel ASP RCT.
}
\examples{
(SEN <- get_shapefiles(country_code = "SEN", level = 2) |>
  dplyr::rename(ADM2_NAME = ADM2_FR) |>
  dplyr::select(ADM2_NAME, geometry))
(BFA <- get_shapefiles(country_code = "BFA", level = 2) |>
  dplyr::rename(ADM2_NAME = adm2_name1) |>
  dplyr::select(ADM2_NAME, geometry))
(NER <- get_shapefiles(country_code = "NER", level = 2) |>
  dplyr::rename(ADM2_NAME = adm_02) |>
  dplyr::select(ADM2_NAME, geometry))
(MRT <- get_shapefiles(country_code = "MRT", level = 2) |>
  dplyr::rename(ADM2_NAME = ADM2_EN) |>
  dplyr::select(ADM2_NAME, geometry))
}
