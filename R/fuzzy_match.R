#' Fuzzy match taxonomic names
#' 
#' This function attempts to match input strings to a list of allowable
#'  taxonomic names.
#' It requires that the first letter (or digit) of each word is identical
#'  between the input and output strings to avoid mis-matches
#' 
#' @param txt The string of text requiring a match
#' @param accepted_list The list of accepted names attempting to match to
#' @param max_distance_abs The maximum allowable number of characters
#'  differing between the input string and the match
#' @param max_distance_rel The maximum proportional difference between the
#'  input string and the match
#' @param n_allowed The number of allowable matches returned. Defaults to 1
#' @param epithet_letters A string specifying if 1 or 2 letters remain fixed
#'  at the start of the species epithet.
#'
#' @return A text string that matches a recognised taxon name or scientific
#'  name
#' 
#' 
#' @examples
#' fuzzy_match("Baksia serrata", c("Banksia serrata", 
#'                                 "Banksia integrifolia"), 
#'                                  max_distance_abs = 1, 
#'                                  max_distance_rel = 1)
#' 
#' @noRd
fuzzy_match <- function(txt, accepted_list,
                        max_distance_abs,
                        max_distance_rel,
                        n_allowed = 1,
                        epithet_letters = 1
                        ) {

  if (!epithet_letters %in% c(1,2)) {
    stop("Epithet must be 1 or 2.")
    }
  ## identify number of words in the text to match
  words_in_text <- 1 + stringr::str_count(txt," ")
  
  ## extract first letter of first word
  txt_word1_start <- stringr::str_extract(txt, "[:alpha:]") %>%
                     stringr::str_to_lower()
  
  ## for text matches with 2 or more words,
  ## extract the first letter/digit of the second word
  if(words_in_text > 1 & epithet_letters == 2) 
    {if(nchar(word(txt,2)) == 1) {
      txt_word2_start <- stringr::str_extract(word(txt,2),
                                              "[:alpha:]|[:digit:]")
    } else {
      txt_word2_start <- stringr::str_extract(word(txt,2),
                                          "[:alpha:][:alpha:]|[:digit:]")     
    }
  }

  if(words_in_text > 1 & epithet_letters == 1) {
    txt_word2_start <- stringr::str_extract(word(txt,2), "[:alpha:]|[:digit:]")
  }
    
  ## for text matches with 3 or more words,
  ## extract the first letter/digit of the third word
  if(words_in_text > 2) {
    txt_word3_start <- stringr::str_extract(word(txt,3), "[:alpha:]|[:digit:]")
  }
  
  ## subset accepted list to taxa that begin with the same first letter to
  ## reduce the number of fuzzy matches that are made in the next step.
  ## has also wanted to do this for the second word, but then need to separate
  ## different lists of reference names - smaller time saving and not worth it.
  ## need to add `unique`, because for `APC-known`,
  ## sometimes duplicate canonical names each with a different taxonomic 
  ## status, and then you just want to retain the first one
  accepted_list <- accepted_list[(stringr::str_extract(accepted_list, "[:alpha:]") %>% 
                                    stringr::str_to_lower()) == 
                                    (txt_word1_start  %>% stringr::str_to_lower())] %>%
                    unique()

  ## identify the number of characters that must change for the text string to
  ## match each of the possible accepted names
  if (length(accepted_list) > 0) {
  distance_c <- stringdist::stringdist(txt, accepted_list, method = "dl")
  
  ## identify the minimum number of characters that must change for the text
  ## string to match a string in the list of accepted names
  min_dist_abs_c <-  min(distance_c)
  min_dist_per_c <-  min(distance_c) / stringr::str_length(txt)

  i <- which(distance_c==min_dist_abs_c)
  potential_matches <- accepted_list[i]
  
  ## Is there an acceptable fuzzy match? if not, break here
  if(!(
    ## Within allowable number of characters (absolute)
    min_dist_abs_c <= max_distance_abs &
    ## Within allowable number of characters (relative)
    min_dist_per_c <= max_distance_rel &
    ## Solution has up to n_allowed matches
    length(potential_matches) <= n_allowed
    ) ) { 
    return(NA)
  }
  
  } else {
    return(NA)
  }
  
  # function to check if a match is ok
  check_match <- function(potential_match) {
  
    ## identify number of words in the matched string
    words_in_match <- 1 + stringr::str_count(potential_match," ")
    
    ## identify the first letter of the first word in the matched string
    match_word1_start <- stringr::str_extract(potential_match, "[:alpha:]") %>% 
                          stringr::str_to_lower()
    
    ## identify the first letter of the second word in the matched string
    ## (if the matched string includes 2+ words)
    if(words_in_text > 1 & epithet_letters == 2) {
      x <- word(potential_match,2)
      if(nchar(x) == 1) {
        match_word2_start <- stringr::str_extract(x, "[:alpha:]|[:digit:]")
      } else {
        match_word2_start <- stringr::str_extract(x, "[:alpha:][:alpha:]|[:digit:]")
      }
    }
    
    if(words_in_text > 1 & epithet_letters == 1) {
        match_word2_start <- stringr::str_extract(word(potential_match,2), "[:alpha:]|[:digit:]")
    }

    ## identify the first letter of the third word in the matched string
    ## (if the matched string includes 3+ words)
    if(words_in_text > 2) {
      match_word3_start <- stringr::str_extract(word(potential_match,3), "[:alpha:]|[:digit:]")
    }
    
    ## keep match if the first letters of the first three words
    ## (or fewer if applicable) in the string to match are identical to the
    ## first letters of the first three words in the matched string

    if(words_in_text == 1) {
    ## next line is no longer being used,
    ## since only comparing to first-letter matches
      if (txt_word1_start == match_word1_start) {  
        return(TRUE)
      }
      
    } else if(words_in_text == 2) {
      if (
          txt_word1_start == match_word1_start &
          txt_word2_start == match_word2_start
      ) {
        return(TRUE)
      }
    } else if(words_in_text > 2) {
      if (words_in_match > 2) {
        if (
          txt_word1_start == match_word1_start & 
          txt_word2_start == match_word2_start &
          txt_word3_start == match_word3_start
        ) {
          return(TRUE)
        }
      } else if (
          txt_word1_start == match_word1_start &
          txt_word2_start == match_word2_start
        ) {
          return(TRUE)}
    }
    return(FALSE)
  }
  
  j <- purrr::map_lgl(potential_matches, check_match)
  
  if(!any(j)) return(NA)
  
  return(potential_matches[j])
}

