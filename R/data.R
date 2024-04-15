#' GBIF Australian Plant Data 
#'
#' A subset of plant data from the Global Biodiversity Information Facility
#' 
#'
#' @format `gbif_lite`
#' A tibble with 129 rows and 7 columns:
#' \describe{
#'   \item{species}{The name of the first or species of scientificname}
#'   \item{infraspecificepithet}{	The name of the lowest or terminal infraspecific epithet of the scientificname}
#'   \item{taxonrank}{The taxonomic rank of the most specific name}
#'   \item{decimalLongitude}{Longitude in decimal degrees}
#'   \item{decimalLatitude}{Latitude in decimal degrees}
#'   \item{scientificname}{Scientific Name}
#'   \item{verbatimscientificname}{Scientific name as it appeared in original record}
#' }
#' @source <https://www.gbif.org/>
"gbif_lite"