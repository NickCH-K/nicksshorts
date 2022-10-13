#' State Information
#'
#' A dataset that links state (and Washington DC) names, FIPs codes, two-letter abbreviations (called "region" because this is what it is called in SafeGraph files that use it), and Census regions. Can be merged with \code{fips_to_names} using \code{state_fips} and \code{statename}.
#'
#' This also includes Canadian data on provinces.
#'
#' Note that this is a data set purely of Canadian provinces, US *states*, and DC. Some SafeGraph files contain information on \code{region} values of \code{GU} (Guam), \code{PR} (Puerto Rico), etc., but those will be lost if merging with \code{state_info}.
#'
#' @format A \code{data.table} with 51 rows and 4 variables:
#' \describe{
#'   \item{statename}{The full name of the state / province}
#'   \item{CensusRegion}{The broad Census regions}
#'   \item{region}{The state's two-digit abbreviation / the province's international alpha code}
#'   \item{state_fips}{State FIPS code / Canadian SGC code}
#'   \item{iso_country_code}{Indicator for US or Canada}
#' }
#' @source \url{US Census}
"state_info"

#' County Population
#'
#' A dataset with county-level information from the 2020-2021 census estimates. See \url{https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2021/CO-EST2021-ALLDATA.pdf} for variable definitions.
#'
#' @format A \code{data.table} with 3143 rows and 35 variables.
#' @source \url{US Census}
"county_pop"

#' State Population
#'
#' A dataset with state-level information from the 2020-2021 census estimates, including population. See \url{https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2021/CO-EST2021-ALLDATA.pdf} for variable definitions.
#'
#' @format A \code{data.table} with 51 rows and 33 variables
#' @source \url{US Census}
"state_pop"


#' State Hex Map
#'
#' A dataset with a hexgrid map of the United States
#'
#' @format A \code{data.table} with 357 rows and 7 variables:
#' \describe{
#'   \item{long, lat, order, hold, piece, group}{Vertex information}
#'   \item{id}{Two-letter state code}
#' }
"spdf_fortified"


#' Centroid Information for Hex Map
#'
#' A dataset with locations of the centers of the hex grid hexes
#'
#' @format A \code{data.table} with 51 rows and 3 variables:
#' \describe{
#'   \item{x, y}{Centroid information}
#'   \item{id}{Two-letter state code}
#' }
"centers"
