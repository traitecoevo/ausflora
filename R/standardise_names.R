
#' @title Standardise taxon names
#' 
#' @description
#' Standardises taxon names by performing a series of text substitutions to 
#' remove common inconsistencies in taxonomic nomenclature.
#' 
#' The function takes a character vector of taxon names as input and 
#' returns a character vector of taxon names using standardised taxonomic syntax
#' as output. 
#'
#' @details
#' -  It removes stray punctuation at the start and end of a character string.
#' -  It standardises unusual characters and symbols to ASCII equivalents.
#' -  It standardises taxon rank abbreviations and qualifiers (subsp., var., f.),
#'  as people use many variants of these terms. 
#' -  It standardises or removes a few additional filler words used within
#'  taxon names (affinis becomes aff.; s.l. and s.s. are removed).
#'
#' @param taxon_names A character vector of taxon names that need to be standardised.
#'
#' @return A character vector of standardised taxon names.
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
    ## remove ? throughout
    f("\\?", "") %>%

    ## remove all punct and symbols at start of string
    ## this combination should catch almost everything
    ## it is essential there are no stray characters at the start of strings
    ## for fuzzy-matching to work once the reference list is split by first-character
    stringr::str_replace("^[~!@#$%^&*()_+-=`;',./<>?:{}|]+", "") %>%
    stringr::str_replace("^[:punct:]+", "") %>%
    
    ## remove * at end of string
    f("\\*$", "") %>%

    ## replace hybrid x marker with standard x 
    ## for certain hybrid x's that aren't dealt with below
    f("\u00D7", "x") %>%

    ## hybrid markers and other non-standard characters used are replaced with 
    ## the standard equivalent (e.g. x, \)
    stringi::stri_trans_general("Any-Latin; Latin-ASCII") %>%

    ## add spaces between letters and /
    f("([a-zA-Z])/([a-zA-Z])", "\\1 / \\2") %>%
  
    ## remove ".."
    f("\\.\\.", "\\.") %>%

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
    f("\\saffinis(\\s)",  " aff. ") %>%
    
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
    f("\\sseries(\\s|\\.\\s)", " ser. ") %>%

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
#' extract_genus(c("Banksia integrifolia", "Acacia longifolia"))
#' 
#' @keywords internal
#' @noRd
extract_genus <- function(taxon_name) {
  
  taxon_name <- standardise_names(taxon_name)

  genus <- stringr::str_split_i(taxon_name, " |\\/", 1) %>% stringr::str_to_sentence()
  
  # Deal with names that being with x, 
  # e.g."x Taurodium x toveyanum" or "x Glossadenia tutelata"
  i <- !is.na(genus) & genus =="X"
  
  genus[i] <- 
    stringr::str_split_i(taxon_name[i], " |\\/", 2) %>% stringr::str_to_sentence() %>%  paste("x", .)
  
  genus
}


#' @title Standardise taxon ranks
#' 
#' @description
#' Standardise taxon ranks from Latin into English.
#'
#' @details
#' The function takes a character vector of Latin taxon ranks as input and 
#' returns a character vector of taxon ranks using standardised English terms.
#'
#' @param taxon_rank A character vector of Latin taxon ranks.
#'
#' @return A character vector of English taxon ranks.
#'
#' @examples
#' standardise_taxon_rank(c("regnum", "kingdom", "classis", "class"))
#' @export
standardise_taxon_rank <- function(taxon_rank) {
  f <- function(x, find, replace) {
    gsub(find, replace, x, fixed = TRUE)
  }
  
  taxon_rank %>%
  stringr::str_to_lower() %>%
  f("regnum", "kingdom") %>%
  f("classis", "class") %>%
  f("ordo", "order") %>%
  f("familia", "family") %>%
  f("varietas", "variety") %>%
  f("forma", "form") %>%
  f("sectio", "section")
}
