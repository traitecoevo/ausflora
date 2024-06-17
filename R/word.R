#' Extract words from a sentence. Intended as a faster 
#' replacement for stringr::word
#'
#' @param string A character vector
#' @param start,end Pair of integer vectors giving range of words (inclusive)
#'   to extract. The default value select the first word.
#' @param sep Separator between words. Defaults to single space.
#' @return A character vector with the same length as `string`/`start`/`end`.
#' @noRd
#' @examples
#' spp <- c("Banksia serrata", "Actinotus helanthii")
#' APCalign:::word(spp, 1)
#' APCalign:::word(spp, 2)
#' @noRd 
word <- function(string, start = 1L, end = start, sep = " ") {
  if(end == start) {
    stringr::str_split_i(string, " ", start)
  } else if(end == start+1) {
    w1 <- stringr::str_split_i(string, sep, start)
    w2 <- stringr::str_split_i(string, sep, start+1)
    
    out <- paste(w1, w2) 
    out[is.na(w2)] <- NA_character_
    
    return(out)
  } else if(end == start+2) {
    
    w1 <- stringr::str_split_i(string, sep, start)
    w2 <- stringr::str_split_i(string, sep, start+1)
    w3 <- stringr::str_split_i(string, sep, start+2)
    
    out <- paste(w1, w2, w3) 
    out[is.na(w2) | is.na(w3)] <- NA_character_
    
    return(out)
  } else {
    i <- seq(start, end)
    
    txt <- stringr::str_split(string, sep)
    out <- purrr::map(txt, ~paste(.x[i], collapse = sep))
    
    lngth <- purrr::map_int(txt, length)
    out[lngth < end] <- NA
    
    return(out)
  }
}
