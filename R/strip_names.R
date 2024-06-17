#' @title Strip taxon names
#' 
#' @description
#' Strip taxonomic names of taxon rank abbreviations and qualifiers 
#' and special characters
#'
#' @details
#' Given a vector of taxonomic names, this function removes:
#' -  subtaxa designations ("subsp.", "var.", "f.", and "ser")
#' -  special characters (e.g., "-", ".", "(", ")", "?")
#' -  extra whitespace
#' 
#'  The resulting vector of names is also converted to lowercase.
#'
#' @param taxon_names A character vector of taxonomic names to be stripped.
#'
#' @return A character vector of stripped taxonomic names,
#'  with subtaxa designations, special characters, and extra whitespace
#'  removed, and all letters converted to lowercase.
#'
#'
#' @examples
#' strip_names(c("Abies lasiocarpa subsp. lasiocarpa",
#'               "Quercus kelloggii",
#'               "Pinus contorta var. latifolia"))
#'
#' @export
strip_names <- function(taxon_names) {
  
  f <- function(x, find, replace) {
    gsub(find, replace, x, perl = TRUE)
  }
  
  taxon_names %>%
    f("\\.", "") %>%
    f("\\ \\)", "") %>%
    f("\\(\\ ", "") %>%
    stringr::str_replace_all("[:punct:]", " ") %>%
    stringr::str_replace_all("\\u2215", " ") %>%
    f("\\,", "") %>%
    f("\\=", " ") %>%
    f("  ", " ") %>%
    f(" subsp ", " ") %>%
    f(" var ", " ") %>%   
    f(" ser ", " ") %>%
    f(" f ", " ") %>%
    stringr::str_squish() %>%
    stringr::str_to_lower()
}

#' @title Strip taxon names, extra
#' 
#' @description
#' Strip taxonomic names of `sp.` and hybrid symbols. This function assumes 
#' that a character function has already been run through `strip_names`.
#' 
#' @details
#' Given a vector of taxonomic names, this function removes additional filler
#' words (" x " for hybrid taxa, "sp.") not removed by the function 
#' `strip_names`
#'
#' @param taxon_names A character vector of taxonomic names to be stripped.
#'
#' @return A character vector of stripped taxonomic names, 
#'  with `sp.` and hybrid symbols removed.
#'
#'
#' @examples
#' strip_names_extra(c("Abies lasiocarpa subsp. lasiocarpa",
#'               "Quercus kelloggii",
#'               "Pinus contorta var. latifolia",
#'               "Acacia sp.",
#'               "Lepidium sp. Tanguin Hill (K.R.Newbey 10501)"))
#'
#' @export
strip_names_extra <- function(taxon_names) {
  
  f <- function(x, find, replace) {
    gsub(find, replace, x, perl = TRUE)
  }
  
  taxon_names %>%
    f(" species ", " ") %>%
    f(" x ", " ") %>%
    f(" sp ", " ") %>%
    f(" sp1", " 1") %>%
    f(" sp2", " 2") %>%
    stringr::str_squish() 
}
