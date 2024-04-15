#' Strip taxonomic names of taxon rank abbreviations and qualifiers and special characters
#'
#' Given a vector of taxonomic names, this function removes subtaxa designations ("subsp.", "var.", "f.", and "ser"),
#' special characters (e.g., "-", ".", "(", ")", "?"), and extra whitespace. The resulting vector
#' of names is also converted to lowercase.
#'
#' @param taxon_names A character vector of taxonomic names to be stripped.
#'
#' @return A character vector of stripped taxonomic names, with subtaxa designations, special
#' characters, and extra whitespace removed, and all letters converted to lowercase.
#'
#'
#' @examples
#' strip_names(c("Abies lasiocarpa subsp. lasiocarpa",
#'               "Quercus kelloggii",
#'               "Pinus contorta var. latifolia"))
#'
#' @export
strip_names <- function(taxon_names) {
  taxon_names %>%
    stringr::str_replace_all("\\.", "") %>%
    stringr::str_replace_all("\\ \\)", "") %>%
    stringr::str_replace_all("\\(\\ ", "") %>%
    stringr::str_replace_all("[:punct:]", " ") %>%
    stringr::str_replace_all("\\u2215", " ") %>%
    stringr::str_replace_all("\\,", "") %>%
    stringr::str_replace_all("\\=", " ") %>%
    stringr::str_replace_all("  ", " ") %>%
    stringr::str_replace_all(" subsp ", " ") %>%
    stringr::str_replace_all(" var ", " ") %>%   
    stringr::str_replace_all(" ser ", " ") %>%
    stringr::str_replace_all(" f ", " ") %>%
    stringr::str_squish() %>%
    tolower()
}

#' Strip taxonomic names of taxon rank abbreviations and qualifiers, filler words and special characters
#'
#' Given a vector of taxonomic names, this function removes subtaxa designations ("subsp.", "var.", "f.", and "ser"),
#' additional filler words and characters (" x " for hybrid taxa, "sp."), 
#' special characters (e.g., "-", ".", "(", ")", "?"), and extra whitespace. The resulting vector
#' of names is also converted to lowercase.
#'
#' @param taxon_names A character vector of taxonomic names to be stripped.
#'
#' @return A character vector of stripped taxonomic names, with subtaxa designations, special
#' characters, additional filler words and extra whitespace removed, and all letters converted to lowercase.
#'
#'
#' @examples
#' strip_names_2(c("Abies lasiocarpa subsp. lasiocarpa",
#'               "Quercus kelloggii",
#'               "Pinus contorta var. latifolia",
#'               "Acacia sp.",
#'               "Lepidium sp. Tanguin Hill (K.R.Newbey 10501)"))
#'
#' @export
strip_names_2 <- function(taxon_names) {
  taxon_names %>%
    stringr::str_replace_all("\\.", "") %>%
    stringr::str_replace_all("[:punct:]", " ") %>%
    stringr::str_replace_all("\\u2215", " ") %>%
    stringr::str_replace_all(" subsp ", " ") %>%
    stringr::str_replace_all(" var ", " ") %>%   
    stringr::str_replace_all(" ser ", " ") %>%
    stringr::str_replace_all(" f ", " ") %>%
    stringr::str_replace_all(" species ", " ") %>%
    stringr::str_replace_all(" x ", " ") %>%
    stringr::str_replace_all(" sp ", " ") %>%
    stringr::str_replace_all(" sp1", " 1") %>%
    stringr::str_replace_all(" sp2", " 2") %>%
    stringr::str_replace_all("\\=", " ") %>%
    stringr::str_replace_all("  ", " ") %>%
    stringr::str_squish() %>%
    tolower()
}
