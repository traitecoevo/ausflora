
#' Standardises taxon names by performing a series of text substitutions to remove common inconsistencies in taxonomic nomenclature.
#'
#' The function takes a character vector of taxon names as input and 
#' returns a character vector of taxon names using standardised taxonomic syntax as output. 
#' In particular it standardises taxon rank abbreviations and qualifiers (subsp., var., f.), as people use many variants of these terms. 
#' It also standardises or removes a few additional filler words used within taxon names (affinis becomes aff.; s.l. and s.s. are removed).
#'
#' @param taxon_names A character vector of taxon names that need to be standardised.
#'
#' @return A character vector of standardised taxon names.
#'
#'
#' @examples
#' standardise_names(c("Quercus suber",
#'                     "Eucalyptus sp.",
#'                     "Eucalyptus spp.",
#'                     "Agave americana var. marginata",
#'                     "Agave americana v marginata",
#'                     "Notelaea longifolia forma longifolia",
#'                     "Notelaea longifolia f longifolia"))
#' @export
standardise_names <- function(taxon_names) {
  f <- function(x, find, replace) {
    gsub(find, replace, x, perl = TRUE)
  }
  
  taxon_names %>%
    ## for hybrid markers
    stringi::stri_trans_general("Any-Latin; Latin-ASCII") %>%
    f("\\*", "x") %>%
  
    ## remove ".."
    stringr::str_replace("\\.\\.", "\\.") %>%

    ## Weird formatting
    f("[\\n\\t]", " ") %>%
    f("[\\n\\t]", " ") %>%
    
    ## Remove spaces before or after brackets
    f("\\ \\)", "\\)") %>%
    f("\\(\\ ", "\\(") %>%
    
    ## Capitalise first letter
    f("^([a-z])", "\\U\\1") %>%
    
    ## sp. not sp or spp
    f("\\ssp(\\s|$)",   " sp. ") %>%
    f("\\sspp.(\\s|$)", " sp. ") %>%
    f("\\sspp(\\s|$)",  " sp. ") %>%
    
    ## subsp. not ssp, ssp., subsp or sub sp.
    f("\\sssp(\\s|$)",     " subsp. ") %>%
    f("\\sssp.(\\s|$)",    " subsp. ") %>%
    f("\\ssubsp(\\s|$)",   " subsp. ") %>%
    f("\\ssub sp.(\\s|$)", " subsp. ") %>%
    
    ## var. not var or v or v.
    f("\\svar(\\s|$)",   " var. ") %>%
    f("\\sv(\\s|$|\\.)", " var. ") %>%
    
    ## aff. not affin, aff affn affinis
    f("\\saffin(\\s|$)",    " aff. ") %>%
    f("\\saff(\\s|$)",      " aff. ") %>%
    f("\\saffn(\\s|$|\\.)", " aff. ") %>%
    f("\\saffinis(\\s|$)",  " aff. ") %>%
    
    ## f. not forma or form or form. or f
    f("\\sforma(\\s|$)",       " f. ") %>%
    f("\\sform(\\s|$|\\.\\s)", " f. ") %>%
    f("\\sf(\\s|$)",           " f. ") %>%
    
    ## remove " ms" if present
    f("\\sms(\\s|$|\\.\\s)", " ") %>%
    
    ## remove " s.l" or " s.s." or "s s " or " s l " if present
    f("\\ssl(\\s|$)", " ") %>%
    f("\\ss\\.l\\.(\\s|$)", " ") %>%
    f("\\sss(\\s|$)", "") %>%
    f("\\ss\\.s\\.(\\s|$)", " ") %>%
    f("\\ss\\ss(\\s|$)", " ") %>%
    f("\\ss\\sl(\\s|$)", " ") %>%
    f("\\ss\\.\\ss(\\s|$|\\.\\s)", " ") %>%
    f("\\ss\\.\\sl(\\s|$|\\.\\s)", " ") %>%
    f("\\ss(\\.\\s|\\s)lat(\\s|$|\\.\\s)", " ") %>%
    f("\\ssensu\\slato(\\s|$|\\.\\s)", " ") %>%
    f("\\ssensu\\sstricto(\\s|$|\\.\\s)", " ") %>%
    f("(\\s|\\()s\\.lat\\.(\\s|\\))", "") %>%
    f("(\\s|\\()s\\.str\\.(\\s|\\))", "") %>%
    
    ## standarise "ser"
    f("\\sser(\\s|\\.\\s)", " ser. ") %>%

    ## clean white space
    stringr::str_squish()
}

#' Extract Genus
#' 
#' This function extracts the genus component of a scientific name. 
#' It identifies if the genus is/is not a hybrid. For a hybrid genus,
#' the first two words of the taxon name are extracted (e.g. "x Cynochloris"),
#' while for a non-hybrid genus just the first word is extracted (e.g. "Banksia").
#'
#' @param taxon_name 
#'
#' @return The genus for a scientific name.
#'
#' @examples
#' genus = extract_genus(stripped_name)
#' 
#' @keywords internal
#' @noRd

extract_genus <- function(taxon_name) {
  genus <- 
    ifelse(
      stringr::word(taxon_name, 1) %>% stringr::str_to_lower() == "x",
      paste(stringr::word(taxon_name, 1) %>% stringr::str_to_lower(), stringr::word(taxon_name, 2) %>% stringr::str_to_sentence()),
      stringr::word(taxon_name, 1) %>% stringr::str_to_sentence()
    )
  genus
}
