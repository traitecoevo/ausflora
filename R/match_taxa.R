#' Match taxonomic names to accepted names in list
#' 
#' This function attempts to match input strings to a list of allowable taxonomic names.
#' It cycles through more than 20 different string patterns, sequentially searching for additional match patterns.
#' It identifies string patterns in input names that suggest a name can only be aligned to a genus (hybrids that are not accepted names; graded species; taxa not identified to species).
#' It prioritises matches that do not require fuzzy matching (i.e. synonyms, orthographic variants) over those that do.
#' If prioritises matches to taxa in the APC over names in the APNI.
#' 
#' @param taxa The list of taxa requiring checking
#
#' @param resources The list(s) of accepted names to check against, loaded through the function `load_taxonomic_resources()`
#' @param fuzzy_abs_dist The number of characters allowed to be different for a fuzzy match.
#' @param fuzzy_rel_dist The proportion of characters allowed to be different for a fuzzy match. 
#' @param fuzzy_matches Fuzzy matches are turned on as a default. The relative and absolute distances allowed for fuzzy matches to species and infraspecific taxon names are defined by the parameters `fuzzy_abs_dist` and `fuzzy_rel_dist`
#' @param imprecise_fuzzy_matches Imprecise fuzzy matches are turned off as a default.
#' @param APNI_matches Name matches to the APNI (Australian Plant Names Index) are turned off as a default.
#' @param identifier A dataset, location or other identifier, which defaults to NA.
#'
#' @noRd
match_taxa <- function(
    taxa, 
    resources,
    fuzzy_abs_dist = 3, 
    fuzzy_rel_dist = 0.2, 
    fuzzy_matches = TRUE, 
    imprecise_fuzzy_matches = FALSE, 
    APNI_matches = TRUE, 
    identifier = NA_character_
) {
  
  update_na_with <- function(current, new) {
    ifelse(is.na(current), new, current)
  }
  
  
  ## A function that specifies particular fuzzy matching conditions (for the function fuzzy_match) when matching is being done at the genus level.
  if (fuzzy_matches == TRUE) {
    fuzzy_match_genera <- function(x, y) {
      purrr::map_chr(x, ~ fuzzy_match(.x, y, 2, 0.35, n_allowed = 1))
    }
  } else {  
    fuzzy_match_genera <- function(x, y) {
      purrr::map_chr(x, ~ fuzzy_match(.x, y, 0, 0.0, n_allowed = 1))
    }    
  }
  
  ## set default imprecise fuzzy matching parameters
  imprecise_fuzzy_abs_dist <- 5
  imprecise_fuzzy_rel_dist <- 0.25
  
  ## override all fuzzy matching parameters with absolute and relative distances of 0 if fuzzy matching is turned off
  if (fuzzy_matches == FALSE) {
    fuzzy_abs_dist <- 0
    fuzzy_rel_dist <- 0
    imprecise_fuzzy_abs_dist <- 0
    imprecise_fuzzy_rel_dist <- 0
  }
  
  ## remove APNI-listed genera from resources if APNI matches are turned off (the default)
  if (APNI_matches == TRUE) {
    resources$genera_all2 <- resources$genera_all
  } else {
    resources$genera_all2 <- resources$genera_all %>% filter(taxonomic_dataset != "APNI")
  }
  
  ## Repeatedly used identifier strings are created. 
  ## These identifier strings are added to the aligned names of taxa that do not match to an APC or APNI species or infra-specific level name.
  taxa$tocheck <- taxa$tocheck %>%
    dplyr::mutate(
      identifier_string = ifelse(is.na(identifier), NA_character_, paste0(" [", identifier, "]")),
      identifier_string2 = ifelse(is.na(identifier), NA_character_, paste0("; ", identifier)),
      aligned_name_tmp = NA_character_
    )
  
  ## In the tocheck dataframe, add columns with manipulated versions of the string to match
  ## Various stripped versions of the string to match, versions with 1, 2 and 3 words (genus, binomial, trinomial), and fuzzy-matched genera are propagated.
  taxa$tocheck <- taxa$tocheck %>%
    dplyr::mutate(
      cleaned_name = cleaned_name %>%
        update_na_with(standardise_names(original_name)),
      stripped_name = stripped_name %>%
        update_na_with(strip_names(cleaned_name)),
      stripped_name2 = stripped_name2 %>%
        update_na_with(strip_names_2(cleaned_name)),
      trinomial = stringr::word(stripped_name2, start = 1, end = 3),
      binomial = stringr::word(stripped_name2, start = 1, end = 2),
      genus = extract_genus(original_name),
      fuzzy_match_genus =
        fuzzy_match_genera(genus, resources$genera_accepted$genus),
      fuzzy_match_genus_synonym =
        fuzzy_match_genera(genus, resources$genera_synonym$genus),
      fuzzy_match_genus_APNI =
        fuzzy_match_genera(genus, resources$genera_APNI$genus)
    )
  
  ## Taxa that have been checked are moved from `taxa$tocheck` to `taxa$checked`
  ## These lines of code are repeated after each matching cycle to progressively move taxa from `tocheck` to `checked`
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # START MATCHES
  # match_01a: Scientific name matches
  # Taxon names that are an accepted scientific name, with authorship.
  
  i <-
    taxa$tocheck$original_name %in% resources$`APC list (accepted)`$scientific_name
  
  ii <-
    match(
      taxa$tocheck[i,]$original_name,
      resources$`APC list (accepted)`$scientific_name
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = resources$`APC list (accepted)`$taxon_rank[ii],
      aligned_name = resources$`APC list (accepted)`$canonical_name[ii],
      aligned_reason = paste0(
        "Exact match of taxon name to an APC-accepted scientific name (including authorship) (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_01a_accepted_scientific_name_with_authorship"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_01b: Scientific name matches
  # Taxon names that are an APC-known scientific name, with authorship.
  
  i <-
    taxa$tocheck$original_name %in% resources$`APC list (known names)`$scientific_name
  
  ii <-
    match(
      taxa$tocheck[i,]$original_name,
      resources$`APC list (known names)`$scientific_name
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = resources$`APC list (known names)`$taxon_rank[ii],
      aligned_name = resources$`APC list (known names)`$canonical_name[ii],
      aligned_reason = paste0(
        "Exact match of taxon name to an APC-known scientific name (including authorship) (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_01b_synonym_scientific_name_with_authorship"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_01c: APC-accepted canonical name
  # Taxon names that are exact matches to APC-accepted canonical names, once filler words and punctuation are removed.
  i <-
    taxa$tocheck$cleaned_name %in% resources$`APC list (accepted)`$canonical_name
  
  ii <-
    match(
      taxa$tocheck[i,]$cleaned_name,
      resources$`APC list (accepted)`$canonical_name
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = resources$`APC list (accepted)`$taxon_rank[ii],
      aligned_name = resources$`APC list (accepted)`$canonical_name[ii],
      aligned_reason = paste0(
        "Exact match of taxon name to an APC-accepted canonical name once punctuation and filler words are removed (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_01c_accepted_canonical_name"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_01d: APC-known canonical name
  # Taxon names that are exact matches to APC-known canonical names, once filler words and punctuation are removed.
  i <-
    taxa$tocheck$cleaned_name %in% resources$`APC list (known names)`$canonical_name
  
  ii <-
    match(
      taxa$tocheck[i,]$cleaned_name,
      resources$`APC list (known names)`$canonical_name
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = resources$`APC list (known names)`$taxon_rank[ii],
      aligned_name = resources$`APC list (known names)`$canonical_name[ii],
      aligned_reason = paste0(
        "Exact match of taxon name to an APC-known canonical name once punctuation and filler words are removed (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_01d_synonym_canonical_name"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_02a: Genus-level resolution
  # Exact matches of APC-accepted or APC-known genus for names where the final "word" is `sp` or `spp`
  # Aligned name includes identifier to indicate `genus sp.` refers to a specific species (or infra-specific taxon), associated with a specific dataset/location.
  
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, "[:space:]sp\\.$") &
    taxa$tocheck$genus %in% resources$genera_all2$genus &
    stringr::word(taxa$tocheck$cleaned_name, 2) %in% c("sp.")
  
  ii <-
    match(
      taxa$tocheck[i,]$genus,
      resources$genera_all2$genus
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = resources$genera_all2$taxonomic_dataset[ii],
      taxon_rank = "genus",
      aligned_name_tmp = paste0(resources$genera_all2$genus[ii], " sp."),
      aligned_name = ifelse(is.na(identifier_string),
                            aligned_name_tmp,
                            paste0(aligned_name_tmp, identifier_string)
      ),
      aligned_reason = paste0(
        "Exact match of taxon name ending with `sp.` to an ",
        taxonomic_dataset,
        " genus (",
        Sys.Date(),
        ")"
      ),
      checked = TRUE,
      known = TRUE,
      alignment_code = "match_02a_exact_genus_accepted_or_synonym"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_02b: Genus-level resolution
  # Fuzzy matches of APC accepted genera for names where the final "word" is `sp` or `spp` and 
  # there isn't an exact match to an APC accepted genus name
  # Aligned name includes identifier to indicate `genus sp.` refers to a specific species (or infra-specific taxon), associated with a specific dataset/location.
  
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, "[:space:]sp\\.$") &
    taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$genus &
    stringr::word(taxa$tocheck$cleaned_name, 2) %in% c("sp.")
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_genus,
      resources$genera_accepted$genus
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = resources$genera_accepted$taxonomic_dataset[ii],
      taxon_rank = "genus",      
      aligned_name_tmp = 
        paste0(resources$genera_accepted$genus[ii], " sp."),
      aligned_name = ifelse(is.na(identifier_string),
                            aligned_name_tmp,
                            paste0(aligned_name_tmp, identifier_string)
      ),
      aligned_reason = paste0(
        "Fuzzy match of taxon name ending with `sp.` to an APC-accepted genus (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_02b_fuzzy_genus_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_02c: Genus-level resolution
  # Fuzzy matches of APC synonymous genera for names where the final "word" is `sp` or `spp` and 
  # there isn't an exact match to an APC synonymous genus name.
  # Aligned name includes identifier to indicate `genus sp.` refers to a specific species (or infra-specific taxon), associated with a specific dataset/location.
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, "[:space:]sp\\.$") &
    taxa$tocheck$fuzzy_match_genus_synonym %in% resources$genera_synonym$genus &
    stringr::word(taxa$tocheck$cleaned_name, 2) %in% c("sp.")
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_genus_synonym,
      resources$genera_synonym$genus
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = resources$genera_synonym$taxonomic_dataset[ii],
      taxon_rank = "genus",
      aligned_name_tmp = paste0(resources$genera_synonym$genus[ii], " sp."),
      aligned_name = ifelse(is.na(identifier_string),
                            aligned_name_tmp,
                            paste0(aligned_name_tmp, identifier_string)
      ),
      aligned_reason = paste0(
        "Fuzzy match of taxon name ending with `sp.` to an APC-known genus (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_02c_fuzzy_genus_synonym"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_02d: Family-level resolution
  # Exact matches of APC-accepted family for names where the final "word" is `sp` or `spp`.
  # Aligned name includes identifier to indicate `family sp.` refers to a specific species (or infra-specific taxon), associated with a specific dataset/location.
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, "[:space:]sp\\.$") &
    taxa$tocheck$genus %in% resources$family_accepted$canonical_name &
    stringr::word(taxa$tocheck$cleaned_name, 2) %in% c("sp.")
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = "family",
      aligned_name = ifelse(is.na(identifier_string),
                            paste0(genus, " sp."),
                            paste0(genus, " sp.", identifier_string)),
      aligned_reason = paste0(
        "Exact match of taxon name ending with `sp.` to an APC-accepted family (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_02d_exact_family_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_03a: Intergrade taxon
  # Exact match to APC-accepted or APNI-listed genus for taxon names where a double hyphen indicates the plant is an intergrade.
  # For taxon names the fitting pattern, `genus species_A -- species_B` (intergrade) automatically align to genus,
  # since this is the highest taxon rank that can be attached to the plant name
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, "\\ -- |\\--") &
    taxa$tocheck$genus %in% resources$genera_all2$genus
  
  ii <-
    match(
      taxa$tocheck[i,]$genus,
      resources$genera_all2$genus
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = resources$genera_all2$taxonomic_dataset[ii],
      taxon_rank = "genus",
      aligned_name_tmp = paste0(resources$genera_all2$genus[ii], " sp. [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")
      ),
      aligned_reason = paste0(
        "Exact match to ",
        taxonomic_dataset,
        " genus. Taxon name includes '--' (double dash) indicating an intergrade between two taxa and taxon can only be aligned to genus-rank (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_03a_intergrade_accepted_or_synonym_genus"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_03b: Intergrade taxon, APC-accepted fuzzy
  # Fuzzy match to APC-accepted genus for taxon names where a double hyphen indicates the plant is an intergrade.
  # For taxon names fitting the pattern, `genus species_A -- species_B` (intergrade) automatically align to genus,
  # since this is the highest taxon rank that can be attached to the plant name.
  
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, "\\ -- |\\--") &
    taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$genus
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = "genus",
      aligned_name_tmp = paste0(fuzzy_match_genus, " sp. [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")
      ),
      aligned_reason = paste0(
        "Fuzzy match to APC-accepted genus. Taxon name includes '--' (double dash) indicating an intergrade between two taxa and taxon can only be aligned to genus-rank (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_03b_intergrade_fuzzy_accepted_genus"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_03c: Intergrade matches, APC-known fuzzy
  # Fuzzy match to APC-known genus for taxon names where a double hyphen indicates the plant is an intergrade.
  # For taxon names fitting the pattern, `genus species_A -- species_B` (intergrade) automatically align to genus,
  # since this is the highest taxon rank that can be attached to the plant name.
  
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, "\\ -- |\\--") &
    taxa$tocheck$fuzzy_match_genus_synonym %in% resources$genera_synonym$genus
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = "genus",
      aligned_name_tmp = paste0(fuzzy_match_genus_synonym, " sp. [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")
      ),
      aligned_reason = paste0(
        "Fuzzy match to APC-known genus. Taxon name includes '--' (double dash) indicating an intergrade between two taxa and taxon can only be aligned to genus-rank (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_03c_intergrade_fuzzy_synonym_genus"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_03d: Intergrade matches, APNI-listed fuzzy
  # Fuzzy match to APNI-listed genus for taxon names where a double hyphen indicates the plant is an intergrade.
  # For taxon names fitting the pattern, `genus species_A -- species_B` (intergrade) automatically align to genus,
  # since this is the highest taxon rank that can be attached to the plant name.
  if (APNI_matches == TRUE) {
    i <-
      stringr::str_detect(taxa$tocheck$cleaned_name, "\\ -- |\\--") &
      taxa$tocheck$fuzzy_match_genus_APNI %in% resources$genera_APNI$genus
    
    taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
      mutate( 
        taxonomic_dataset = "APNI",
        taxon_rank = "genus", 
        aligned_name_tmp = paste0(fuzzy_match_genus_APNI, " sp. [", cleaned_name),
        aligned_name = ifelse(is.na(identifier_string2),
                              paste0(aligned_name_tmp, "]"),
                              paste0(aligned_name_tmp, identifier_string2, "]")
        ),
        aligned_reason = paste0(
          "Fuzzy match to APNI-listed genus. Taxon name includes '--' (double dash) indicating an intergrade between two taxa and taxon can only be aligned to genus-rank (",
          Sys.Date(),
          ")"
        ),
        known = TRUE,
        checked = TRUE,
        alignment_code = "match_03d_intergrade_fuzzy_APNI_genus"
      )
    
    taxa <- redistribute(taxa)
    if (nrow(taxa$tocheck) == 0)
      return(taxa)
  }

  # match_03e: Intergrade with unknown genus
  # For taxon names fitting the pattern, `genus species_A -- species_B` (intergrade) automatically align to genus,
  # since this is the highest taxon rank that can be attached to the plant name.
  # Neither perfect nor fuzzy matches identify the genus.
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, "\\ -- |\\--") &
    !taxa$tocheck$fuzzy_match_genus %in% resources$genera_all2$genus
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = NA_character_,
      taxon_rank = "genus",
      aligned_name_tmp = paste0(stringr::word(cleaned_name,1), " sp. [", cleaned_name),
      aligned_name = NA,
      aligned_reason = paste0(
        "Taxon name includes '--' (double dash) indicating an intergrade between two taxa, but exact and fuzzy matches fail to align to a genus in the APC or APNI (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_03e_intergrade_unknown_genus"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_04a: Genus species_A / species_B
  # Exact match to APC-accepted or APNI-listed genus for taxon names where a slash ("/") indicates the author is uncertain of the proper taxon name
  # and can only identify the taxon to genus.
  # For taxon names fitting the pattern, `genus species_A / species_B` ("indecision") automatically align to genus,
  # since this is the highest taxon rank that can be attached to the plant name.
  
  i <-
    (
      stringr::str_detect(taxa$tocheck$cleaned_name, "[:alpha:]\\/") |
        stringr::str_detect(taxa$tocheck$cleaned_name, "\\s\\/")
    ) & 
    !stringr::str_detect(taxa$tocheck$cleaned_name, "[:digit:]") &
    !stringr::str_detect(taxa$tocheck$cleaned_name, "\\(") &
    !stringr::str_detect(taxa$tocheck$cleaned_name, "\\'") &
    taxa$tocheck$genus %in% resources$genera_all2$genus
  
  ii <-
    match(
      taxa$tocheck[i,]$genus,
      resources$genera_all2$genus
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = resources$genera_all2$taxonomic_dataset[ii],
      taxon_rank = "genus",
      aligned_name_tmp = paste0(resources$genera_all2$genus[ii], " sp. [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")
      ),
      aligned_reason = paste0(
        "Exact match to ",
        taxonomic_dataset,
        " genus. Taxon name includes '/' (slash) indicating an uncertain species identification but an accepted genus and taxon can only be aligned to genus-rank (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_04a_indecision_accepted_or_synonym_genus"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_04b: Genus species_A / species_B
  # Fuzzy match to APC-accepted genus for taxon names where a slash ("/") indicates the author is uncertain of the proper taxon name
  # and can only identify the taxon to genus.
  # For taxon names fitting the pattern, `genus species_A / species_B` ("indecision") automatically align to genus,
  # since this is the highest taxon rank that can be attached to the plant name.
  
  i <-
    (
      stringr::str_detect(taxa$tocheck$cleaned_name, "[:alpha:]\\/") |
        stringr::str_detect(taxa$tocheck$cleaned_name, "\\s\\/")
    ) &
    !stringr::str_detect(taxa$tocheck$cleaned_name, "[:digit:]") &
    !stringr::str_detect(taxa$tocheck$cleaned_name, "\\(") &
    !stringr::str_detect(taxa$tocheck$cleaned_name, "\\'") &
    taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$genus
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = "genus",     
      aligned_name_tmp = paste0(fuzzy_match_genus, " sp. [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")
      ),
      aligned_reason = paste0(
        "Fuzzy match to APC-accepted genus. Taxon name includes '/' (slash) indicating an uncertain species identification but an accepted genus and taxon can only be aligned to genus-rank (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_04b_indecision_fuzzy_accepted_genus"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_04c: Genus species_A / species_B
  # Fuzzy match to APC-known genus for taxon names where a slash ("/") indicates the author is uncertain of the proper taxon name
  # and can only identify the taxon to genus.
  # For taxon names fitting the pattern, `genus species_A / species_B` ("indecision") automatically align to genus,
  # since this is the highest taxon rank that can be attached to the plant name.
  
  i <-
    (
      stringr::str_detect(taxa$tocheck$cleaned_name, "[:alpha:]\\/") |
        stringr::str_detect(taxa$tocheck$cleaned_name, "\\s\\/")
    ) &
    !stringr::str_detect(taxa$tocheck$cleaned_name, "[:digit:]") &
    !stringr::str_detect(taxa$tocheck$cleaned_name, "\\(") &
    !stringr::str_detect(taxa$tocheck$cleaned_name, "\\'") &
    taxa$tocheck$fuzzy_match_genus_synonym %in% resources$genera_synonym$genus
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(  
      taxonomic_dataset = "APC",
      taxon_rank = "genus",
      aligned_name_tmp = paste0(fuzzy_match_genus_synonym, " sp. [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")
      ),
      aligned_reason = paste0(
        "Fuzzy match to APC-known genus. Taxon name includes '/' (slash) indicating an uncertain species identification but an accepted genus and taxon can only be aligned to genus-rank (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_04c_indecision_fuzzy_synonym_genus"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_04d: Genus species_A / species_B
  # Fuzzy match to APNI-listed genus for taxon names where a slash ("/") indicates the author is uncertain of the proper taxon name
  # and can only identify the taxon to genus.
  # For taxon names fitting the pattern, `genus species_A / species_B` ("indecision") automatically align to genus,
  # since this is the highest taxon rank that can be attached to the plant name.
  if (APNI_matches == TRUE) {
    i <-
      (
        stringr::str_detect(taxa$tocheck$cleaned_name, "[:alpha:]\\/") |
          stringr::str_detect(taxa$tocheck$cleaned_name, "\\s\\/")
      ) &
    !stringr::str_detect(taxa$tocheck$cleaned_name, "[:digit:]") &
    !stringr::str_detect(taxa$tocheck$cleaned_name, "\\(") &
    !stringr::str_detect(taxa$tocheck$cleaned_name, "\\'") &
      taxa$tocheck$fuzzy_match_genus_APNI %in% resources$genera_APNI$genus
    
    taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
      mutate(
        taxonomic_dataset = "APNI",
        taxon_rank = "genus",     
        aligned_name_tmp = paste0(fuzzy_match_genus_APNI, " sp. [", cleaned_name),
        aligned_name = ifelse(is.na(identifier_string2),
                              paste0(aligned_name_tmp, "]"),
                              paste0(aligned_name_tmp, identifier_string2, "]")
        ),
        aligned_reason = paste0(
          "Fuzzy match to APNI-listed genus. Taxon name includes '/' (slash) indicating an uncertain species identification but an accepted genus and taxon can only be aligned to genus-rank (",
          Sys.Date(),
          ")"
        ),
        known = TRUE,
        checked = TRUE,
        alignment_code = "match_04d_indecision_fuzzy_APNI_genus"
      )
    
    taxa <- redistribute(taxa)
    if (nrow(taxa$tocheck) == 0)
      return(taxa)
  }

  # match_04e: Genus species_A / species_B
  # For taxon names fitting the pattern, `genus species_A / species_B` ("indecision") automatically align to genus,
  # since this is the highest taxon rank that can be attached to the plant name.
  # A slash ("/") indicates the author is uncertain of the proper taxon name.
  # and can only identify the taxon to genus.
  # Neither perfect nor fuzzy matches identify the genus.
  
  i <-
    (
      stringr::str_detect(taxa$tocheck$cleaned_name, "[:alpha:]\\/") |
        stringr::str_detect(taxa$tocheck$cleaned_name, "\\s\\/")
    ) &
    !stringr::str_detect(taxa$tocheck$cleaned_name, "[:digit:]") &
    !stringr::str_detect(taxa$tocheck$cleaned_name, "\\(") &
    !stringr::str_detect(taxa$tocheck$cleaned_name, "\\'") &
    !taxa$tocheck$fuzzy_match_genus %in% resources$genera_all2$genus
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = NA_character_,
      taxon_rank = "genus",
      aligned_name_tmp = paste0(stringr::word(cleaned_name,1), " sp. [", cleaned_name),
      aligned_name = NA,
      aligned_reason = paste0(
        "Taxon name includes '/' (slash) indicating an uncertain species identification but an accepted genus and taxon can only be aligned to genus-rank. Exact and fuzzy matches fail to align to a genus in the APC or APNI (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_04e_indecision_unknown_genus"
    )
  
  # Note:  -- Finished with checking genus sp. above, now continue with full species
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_05a: fuzzy match to APC-accepted canonical name
  # Fuzzy match of taxon name to an APC-accepted canonical name, once filler words and punctuation are removed.
  for (i in 1:nrow(taxa$tocheck)) {    
    taxa$tocheck$fuzzy_match_cleaned_APC[i] <-
      fuzzy_match(
        txt = taxa$tocheck$stripped_name[i],
        accepted_list = resources$`APC list (accepted)`$stripped_canonical,
        max_distance_abs = fuzzy_abs_dist,
        max_distance_rel = fuzzy_rel_dist,
        n_allowed = 1
      )
  }
  
  i <-
    taxa$tocheck$fuzzy_match_cleaned_APC %in% resources$`APC list (accepted)`$stripped_canonical
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_cleaned_APC,
      resources$`APC list (accepted)`$stripped_canonical
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = resources$`APC list (accepted)`$taxon_rank[ii],
      aligned_name = resources$`APC list (accepted)`$canonical_name[ii],
      aligned_reason = paste0(
        "Fuzzy match of taxon name to an APC-accepted canonical name once punctuation and filler words are removed (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_05a_fuzzy_accepted_canonical_name"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_05b: fuzzy match to APC-known canonical name
  # Fuzzy match of taxon name to an APC-known canonical name, once filler words and punctuation are removed.
  for (i in 1:nrow(taxa$tocheck)) {    
    taxa$tocheck$fuzzy_match_cleaned_APC_synonym[i] <-
      fuzzy_match(
        txt = taxa$tocheck$stripped_name[i],
        accepted_list = resources$`APC list (known names)`$stripped_canonical,
        max_distance_abs = fuzzy_abs_dist,
        max_distance_rel = fuzzy_rel_dist,
        n_allowed = 1
      )
  }
  
  i <-
    taxa$tocheck$fuzzy_match_cleaned_APC_synonym %in% resources$`APC list (known names)`$stripped_canonical
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_cleaned_APC_synonym,
      resources$`APC list (known names)`$stripped_canonical
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = resources$`APC list (known names)`$taxon_rank[ii],
      aligned_name = resources$`APC list (known names)`$canonical_name[ii],
      aligned_reason = paste0(
        "Fuzzy match of taxon name to an APC-known canonical name once punctuation and filler words are removed (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_05b_fuzzy_synonym_canonical_name"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
    
  # match_05c: APNI-listed canonical name
  # Taxon names that are exact matches to APNI-listed canonical names, once filler words and punctuation are removed.
  if (APNI_matches == TRUE) {
    i <-
      taxa$tocheck$cleaned_name %in% resources$`APNI names`$canonical_name
    
    ii <-
      match(
        taxa$tocheck[i,]$cleaned_name,
        resources$`APNI names`$canonical_name
      )
    
    taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
      mutate(
        taxonomic_dataset = "APNI",
        taxon_rank = resources$`APNI names`$taxon_rank[ii],
        aligned_name = resources$`APNI names`$canonical_name[ii],
        aligned_reason = paste0(
          "Exact match of taxon name to an APNI-listed canonical name once punctuation and filler words are removed (",
          Sys.Date(),
          ")"
        ),
        known = TRUE,
        checked = TRUE,
        alignment_code = "match_05c_APNI_canonical_name"
      )
    
    taxa <- redistribute(taxa)
    if (nrow(taxa$tocheck) == 0)
      return(taxa)
  }

  # match_06a: `genus aff. species` and `genus cf. species`taxa
  # Exact match to APC-accepted or APC-known genus for names where "aff" indicates the taxon has an affinity to another taxon, but isn't the other taxon.
  # Similarly, "cf" indicates that a comparison should be made between the specific taxon and another taxon, but again, isn't the other taxon.
  # Taxon names fitting this pattern that are not APC-accepted, APC-known, or APNI-listed species are automatically aligned to genus,
  # since this is the highest taxon rank that can be attached to the plant name.
  # This alignment can only be made after exact matches of complete taxon names to APC/APNI + fuzzy matches to APC are complete,
  # because there are APC/APNI phrase names that include "sp. aff.".
  
  i <-
    (
      stringr::str_detect(taxa$tocheck$cleaned_name, "[Aa]ff[\\.\\s]") |
        stringr::str_detect(taxa$tocheck$cleaned_name, " affinis ")  |
        stringr::str_detect(taxa$tocheck$cleaned_name, " cf[\\.\\s]")
    ) &
    taxa$tocheck$genus %in% resources$genera_all2$genus
  
  ii <-
    match(
      taxa$tocheck[i,]$genus,
      resources$genera_all2$genus
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = resources$genera_all2$taxonomic_dataset[ii],
      taxon_rank = "genus",
      aligned_name_tmp = paste0(resources$genera_all2$genus[ii], " sp. [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")
      ),
      aligned_reason = paste0(
        "Exact match to ",
        taxonomic_dataset,
        " genus. Taxon name includes 'affinis' or 'aff' indicating an unknown taxon that bears an affinity to a different taxon in the same genus and taxon can only be aligned to genus-rank (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_06a_species_affinis_APC_exact"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_06b: `genus aff. species` taxa
  # Fuzzy match to APC-accepted genus for names where "aff" indicates the taxon has an affinity to another taxon, but isn't the other taxon.
  # Taxon names fitting this pattern that are not APC-accepted, APC-known, or APNI-listed species are automatically aligned to genus,
  # since this is the highest taxon rank that can be attached to the plant name.
  # This alignment can only be made after exact matches of complete taxon names to APC/APNI + fuzzy matches to APC are complete,
  # because there are APC/APNI phrase names that include "sp. aff.".
  i <-
    (
      stringr::str_detect(taxa$tocheck$cleaned_name, "[Aa]ff[\\.\\s]") |
        stringr::str_detect(taxa$tocheck$cleaned_name, " affinis ")
    ) &
    taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$genus
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = "genus",
      aligned_name_tmp = paste0(fuzzy_match_genus, " sp. [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")
      ),
      aligned_reason = paste0(
        "Fuzzy match to APC-accepted genus. Taxon name includes 'affinis' or 'aff' indicating an unknown taxon that bears an affinity to a different taxon in the same genus and taxon can only be aligned to genus-rank",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_06b_species_affinis_APC_accepted_fuzzy"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_06c: `genus aff. species` taxa
  # Fuzzy match to APC-known genus for names where "aff" indicates the taxon has an affinity to another taxon, but isn't the other taxon.
  # Taxon names fitting this pattern that are not APC-accepted, APC-known, or APNI-listed species are automatically aligned to genus,
  # since this is the highest taxon rank that can be attached to the plant name.
  # This alignment can only be made after exact matches of complete taxon names to APC/APNI + fuzzy matches to APC are complete,
  # because there are APC/APNI phrase names that include "sp. aff.".
  i <-
    (
      stringr::str_detect(taxa$tocheck$cleaned_name, "[Aa]ff[\\.\\s]") |
        stringr::str_detect(taxa$tocheck$cleaned_name, " affinis ")
    ) &
    taxa$tocheck$fuzzy_match_genus_synonym %in% resources$genera_synonym$genus
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = "genus",      
      aligned_name_tmp = paste0(fuzzy_match_genus_synonym, " sp. [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")
      ),
      aligned_reason = paste0(
        "Fuzzy match to APC-known genus. Taxon name includes 'affinis' or 'aff' indicating an unknown taxon that bears an affinity to a different taxon in the same genus and taxon can only be aligned to genus-rank",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_06c_species_affinis_APC_synonym_fuzzy"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_06d: `genus aff. species` taxa
  # Fuzzy match to APNI_listed genus for names where "aff" indicates the taxon has an affinity to another taxon, but isn't the other taxon.
  # Taxon names fitting this pattern that are not APC-accepted, APC-known, or APNI-listed species are automatically aligned to genus,
  # since this is the highest taxon rank that can be attached to the plant name.
  # This alignment can only be made after exact matches of complete taxon names to APC/APNI + fuzzy matches to APC are complete,
  # because there are APC/APNI phrase names that include "sp. aff.".
  if (APNI_matches == TRUE) {
    i <-
      (
        stringr::str_detect(taxa$tocheck$cleaned_name, "[Aa]ff[\\.\\s]") |
          stringr::str_detect(taxa$tocheck$cleaned_name, " affinis ")
      ) &
      taxa$tocheck$fuzzy_match_genus_APNI %in% resources$genera_APNI$genus
    
    taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
      mutate(
        taxonomic_dataset = "APNI",
        taxon_rank = "genus",
        aligned_name_tmp = paste0(fuzzy_match_genus_APNI, " sp. [", cleaned_name),
        aligned_name = ifelse(is.na(identifier_string2),
                              paste0(aligned_name_tmp, "]"),
                              paste0(aligned_name_tmp, identifier_string2, "]")
        ),
        aligned_reason = paste0(
          "Fuzzy match to APNI-listed genus. Taxon name includes 'affinis' or 'aff' indicating an unknown taxon that bears an affinity to a different taxon in the same genus and taxon can only be aligned to genus-rank",
          Sys.Date(),
          ")"
        ),
        known = TRUE,
        checked = TRUE,
        alignment_code = "match_06d_species_affinis_APNI_fuzzy"
      )
    
    taxa <- redistribute(taxa)
    if (nrow(taxa$tocheck) == 0)
      return(taxa)
  }

  # match_06e: `genus aff. species` taxa
  # Taxon names where "aff" indicates the taxon has an affinity to another taxon, but isn't the other taxon, 
  # when an exact or fuzzy genus-level match to APC & APNI genera cannot be made.
  # Taxon names fitting this pattern that are not APC-accepted, APC-known, or APNI-listed species are automatically aligned to genus,
  # since this is the highest taxon rank that can be attached to the plant name.
  # This alignment can only be made after exact matches of complete taxon names to APC/APNI + fuzzy matches to APC are complete,
  # because there are APC/APNI phrase names that include "sp. aff.".
  i <-
    (
      stringr::str_detect(taxa$tocheck$cleaned_name, "[Aa]ff[\\.\\s]") |
        stringr::str_detect(taxa$tocheck$cleaned_name, " affinis ")
    ) &
    !taxa$tocheck$fuzzy_match_genus %in% resources$genera_all2$genus
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = NA_character_,
      taxon_rank = "genus",
      aligned_name_tmp = paste0(stringr::word(cleaned_name,1), " sp. [", cleaned_name),
      aligned_name = NA,
      aligned_reason = paste0(
        "Taxon name includes 'affinis' or 'aff' indicating an unknown taxon that bears an affinity to a different taxon in the same genus and taxon can only be aligned to genus-rank. Exact and fuzzy matches fail to align to a genus in the APC or APNI ",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_06e_species_affinis_unknown_genus"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_07a: imprecise fuzzy match
  # Imprecise fuzzy match of taxon name to an APC-accepted canonical name, once filler words and punctuation are removed.
  # For imprecise fuzzy matches, the taxon name can differ from the `APC-accepted` names by 5 characters & up to 25% of the string length.
  # These matches require individual review and are turned off as a default.
  if (imprecise_fuzzy_matches == TRUE) {
    for (i in 1:nrow(taxa$tocheck)) {
      taxa$tocheck$fuzzy_match_cleaned_APC_imprecise[i] <-
        fuzzy_match(
          txt = taxa$tocheck$stripped_name[i],
          accepted_list = resources$`APC list (accepted)`$stripped_canonical,
          max_distance_abs = imprecise_fuzzy_abs_dist,
          max_distance_rel = imprecise_fuzzy_rel_dist,
          n_allowed = 1,
          epithet_letters = 2
        )
    }
    
    i <-
      taxa$tocheck$fuzzy_match_cleaned_APC_imprecise %in% resources$`APC list (accepted)`$stripped_canonical
    
    ii <-
      match(
        taxa$tocheck[i,]$fuzzy_match_cleaned_APC_imprecise,
        resources$`APC list (accepted)`$stripped_canonical
      )
    
    taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
      mutate(
        taxonomic_dataset = "APC",
        taxon_rank = resources$`APC list (accepted)`$taxon_rank[ii],
        aligned_name = resources$`APC list (accepted)`$canonical_name[ii],
        aligned_reason = paste0(
          "Imprecise fuzzy match of taxon name to an APC-accepted canonical name once punctuation and filler words are removed (",
          Sys.Date(),
          ")"
        ),
        known = TRUE,
        checked = TRUE,
        alignment_code = "match_07a_imprecise_fuzzy_accepted_canonical_name"
      )
    
    taxa <- redistribute(taxa)
    if (nrow(taxa$tocheck) == 0)
      return(taxa)
  }
  
  # match_07b: imprecise fuzzy match
  # Imprecise fuzzy match of taxon name to an APC-known canonical name, once filler words and punctuation are removed.
  # For imprecise fuzzy matches, the taxon name can differ from the `APC -known` names by 5 characters & up to 25% of the string length.
  # These matches require individual review and are turned off as a default.
  if (imprecise_fuzzy_matches == TRUE) {
    for (i in 1:nrow(taxa$tocheck)) {
      taxa$tocheck$fuzzy_match_cleaned_APC_synonym_imprecise[i] <-
        fuzzy_match(
          txt = taxa$tocheck$stripped_name[i],
          accepted_list = resources$`APC list (known names)`$stripped_canonical,
          max_distance_abs = imprecise_fuzzy_abs_dist,
          max_distance_rel = imprecise_fuzzy_rel_dist,
          n_allowed = 1,
          epithet_letters = 2
        )
    }
    
    i <-
      taxa$tocheck$fuzzy_match_cleaned_APC_synonym_imprecise %in% resources$`APC list (known names)`$stripped_canonical
    
    ii <-
      match(
        taxa$tocheck[i,]$fuzzy_match_cleaned_APC_synonym_imprecise,
        resources$`APC list (known names)`$stripped_canonical
      )
    
    taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
      mutate(
        taxonomic_dataset = "APC",
        taxon_rank = resources$`APC list (known names)`$taxon_rank[ii],
        aligned_name = resources$`APC list (known names)`$canonical_name[ii],
        aligned_reason = paste0(
          "Imprecise fuzzy match of taxon name to an APC-known canonical name once punctuation and filler words are removed (",
          Sys.Date(),
          ")"
        ),
        known = TRUE,
        checked = TRUE,
        alignment_code = "match_07b_imprecise_fuzzy_synonym_canonical_name"
      )
    
    taxa <- redistribute(taxa)
    if (nrow(taxa$tocheck) == 0)
      return(taxa)
  }
  
  # match_08a: hybrid taxa
  # Exact match to APC-accepted, APC-known, or APNI-listed genus for names where " x " indicates taxon is a hybrid.
  # Taxon names fitting this pattern that are not APC-accepted, APC-known, or APNI-listed species are automatically aligned to genus,
  # since this is the highest taxon rank that can be attached to the plant name.
  # This alignment can only be made after exact matches of complete taxon names to APC/APNI + fuzzy matches to APC are complete,
  # because there are hybrid taxa listed in both APC & APNI.
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, " [xX] ") &
    taxa$tocheck$genus %in% resources$genera_all2$genus
  
  ii <-
    match(
      taxa$tocheck[i,]$genus,
      resources$genera_all2$genus
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = resources$genera_all2$taxonomic_dataset[ii],
      taxon_rank = "genus",
      aligned_name_tmp = paste0(resources$genera_all2$genus[ii], " x [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")
      ),
      aligned_reason = paste0(
        "Exact match to ",
        taxonomic_dataset,
        " genus. Taxon name includes ' x ' indicating a hybrid taxon and taxon can only be aligned to genus-rank (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_08a_hybrid_taxon_exact"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_08b: hybrid taxa
  # Fuzzy match to APC-accepted genus for names where " x " indicates taxon is a hybrid.
  # Taxon names fitting this pattern that are not APC-accepted, APC-known, or APNI-listed species are automatically aligned to genus,
  # since this is the highest taxon rank that can be attached to the plant name.
  # This alignment can only be made after exact matches of complete taxon names to APC/APNI + fuzzy matches to APC are complete,
  # because there are hybrid taxa listed in both APC & APNI.
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, " [xX] ") &
    taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$genus
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = "genus",
      aligned_name_tmp = paste0(fuzzy_match_genus, " x [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")
      ),
      aligned_reason = paste0(
        "Fuzzy match to APC-accepted genus. Taxon name includes ' x ' indicating a hybrid taxon and taxon can only be aligned to genus-rank (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_08b_hybrid_taxon_accepted_fuzzy"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_08c: hybrid taxa
  # Fuzzy match to APC-known genus for names where " x " indicates taxon is a hybrid.
  # Taxon names fitting this pattern that are not APC-accepted, APC-known, or APNI-listed species are automatically aligned to genus,
  # since this is the highest taxon rank that can be attached to the plant name.
  # This alignment can only be made after exact matches of complete taxon names to APC/APNI + fuzzy matches to APC are complete,
  # because there are hybrid taxa listed in both APC & APNI.
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, " [xX] ") &
    taxa$tocheck$fuzzy_match_genus_synonym %in% resources$genera_synonym$genus
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = "genus",
      aligned_name_tmp = paste0(fuzzy_match_genus_synonym, " x [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")
      ),
      aligned_reason = paste0(
        "Fuzzy match to APC-known genus. Taxon name includes ' x ' indicating a hybrid taxon and taxon can only be aligned to genus-rank (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_08c_hybrid_taxon_synonym_fuzzy"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_08d: hybrid taxa
  # Fuzzy match to APNI-listed genus for names where " x " indicates taxon is a hybrid.
  # Taxon names fitting this pattern that are not APC-accepted, APC-known, or APNI-listed species are automatically aligned to genus,
  # since this is the highest taxon rank that can be attached to the plant name.
  # This alignment can only be made after exact matches of complete taxon names to APC/APNI + fuzzy matches to APC are complete,
  # because there are hybrid taxa listed in both APC & APNI.
  if (APNI_matches == TRUE) {
    i <-
      stringr::str_detect(taxa$tocheck$cleaned_name, " [xX] ") &
      taxa$tocheck$fuzzy_match_genus_APNI %in% resources$genera_APNI$genus
    
    taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
      mutate(
        taxonomic_dataset = "APNI",
        taxon_rank = "genus",
        aligned_name_tmp = paste0(fuzzy_match_genus_APNI, " x [", cleaned_name),
        aligned_name = ifelse(is.na(identifier_string2),
                              paste0(aligned_name_tmp, "]"),
                              paste0(aligned_name_tmp, identifier_string2, "]")
        ),
        aligned_reason = paste0(
          "Fuzzy match to APNI-listed genus. Taxon name includes ' x ' indicating a hybrid taxon and taxon can only be aligned to genus-rank (",
          Sys.Date(),
          ")"
        ),
        known = TRUE,
        checked = TRUE,
        alignment_code = "match_08d_hybrid_taxon_APNI_fuzzy"
      )
    
    taxa <- redistribute(taxa)
    if (nrow(taxa$tocheck) == 0)
      return(taxa)
  }
  
  # match_08e: hybrid taxa
  # Taxon names where " x " indicates taxon is a hybrid, but an exact or fuzzy genus-level match to APC & APNI genera cannot be made.
  # Taxon names fitting this pattern that are not APC-accepted, APC-known, or APNI-listed species are automatically aligned to genus,
  # since this is the highest taxon rank that can be attached to the plant name.
  # This alignment can only be made after exact matches of complete taxon names to APC/APNI + fuzzy matches to APC are complete,
  # because there are hybrid taxa listed in both APC & APNI.
  i <-
    stringr::str_detect(taxa$tocheck$cleaned_name, " [xX] ") &
    !taxa$tocheck$fuzzy_match_genus %in% resources$genera_all2$genus
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = NA_character_,
      taxon_rank = "genus",
      aligned_name_tmp = paste0(stringr::word(cleaned_name,1), " x [", cleaned_name),
      aligned_name = NA,
      aligned_reason = paste0(
        "Taxon name includes ' x ' indicating a hybrid taxon and taxon can only be aligned to genus-rank. Exact and fuzzy matches fail to align to a genus in the APC or APNI (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_08e_hybrid_taxon_unknown"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_09a: exact trinomial matches, APC
  # Exact match of first three words of taxon name ("trinomial") to APC-accepted canonical name.
  # The purpose of matching only the first three words only to APC-accepted names is that
  # sometimes the submitted taxon name is a valid trinomial + notes and 
  # such names will only be aligned by matches considering only the first three words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names.
  i <-
    taxa$tocheck$trinomial %in% resources$`APC list (accepted)`$trinomial
  
  ii <-
    match(
      taxa$tocheck[i,]$trinomial,
      resources$`APC list (accepted)`$trinomial
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = resources$`APC list (accepted)`$taxon_rank[ii],
      aligned_name = resources$`APC list (accepted)`$canonical_name[ii],
      aligned_reason = paste0(
        "Exact match of the first three words of the taxon name to an APC-accepted canonical name (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_09a_trinomial_exact_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_09b: exact trinomial matches, APC
  # Exact match of first three words of taxon name ("trinomial") to APC-known canonical name.
  # The purpose of matching only the first three words only to APC-known names is that
  # sometimes the submitted taxon name is a valid trinomial + notes and 
  # such names will only be aligned by matches considering only the first three words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names.
  i <-
    taxa$tocheck$trinomial %in% resources$`APC list (known names)`$trinomial
  
  ii <-
    match(
      taxa$tocheck[i,]$trinomial,
      resources$`APC list (known names)`$trinomial
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = resources$`APC list (known names)`$taxon_rank[ii],
      aligned_name = resources$`APC list (known names)`$canonical_name[ii],
      aligned_reason = paste0(
        "Exact match of the first three words of the taxon name to an APC-known canonical name (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_09b_trinomial_exact_synonym"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_09c: fuzzy trinomial matches, APC
  # Fuzzy match of first three words of taxon name ("trinomial") to APC-accepted canonical name.
  # The purpose of matching only the first three words only to APC-accepted names is that
  # sometimes the submitted taxon name is a valid trinomial + notes and 
  # such names will only be aligned by matches considering only the first three words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names
  for (i in 1:nrow(taxa$tocheck)) {
    if (!is.na(taxa$tocheck$trinomial[i])) {
      taxa$tocheck$fuzzy_match_trinomial[i] <-
        fuzzy_match(
          txt = taxa$tocheck$trinomial[i],
          accepted_list = resources$`APC list (accepted)`$trinomial,
          max_distance_abs = fuzzy_abs_dist,
          max_distance_rel = fuzzy_rel_dist,
          n_allowed = 1
        )
    }
  }
  
  i <-
    taxa$tocheck$fuzzy_match_trinomial %in% resources$`APC list (accepted)`$trinomial
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_trinomial,
      resources$`APC list (accepted)`$trinomial
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = resources$`APC list (accepted)`$taxon_rank[ii],
      aligned_name = resources$`APC list (accepted)`$canonical_name[ii],
      aligned_reason = paste0(
        "Fuzzy match of the first three words of the taxon name to an APC-accepted canonical name (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_09c_trinomial_fuzzy_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_09d: fuzzy trinomial matches, APC
  # Fuzzy match of first three words of taxon name ("trinomial") to APC-known canonical name.
  # The purpose of matching only the first three words only to APC-known names is that
  # sometimes the submitted taxon name is a valid trinomial + notes and 
  # such names will only be aligned by matches considering only the first three words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names
  for (i in 1:nrow(taxa$tocheck)) {
    if (!is.na(taxa$tocheck$trinomial[i])) {
      taxa$tocheck$fuzzy_match_trinomial_synonym[i] <-
        fuzzy_match(
          txt = taxa$tocheck$trinomial[i],
          accepted_list = resources$`APC list (known names)`$trinomial,
          max_distance_abs = fuzzy_abs_dist,
          max_distance_rel = fuzzy_rel_dist,
          n_allowed = 1
        )
    }
  }
  
  i <-
    taxa$tocheck$fuzzy_match_trinomial_synonym %in% resources$`APC list (known names)`$trinomial
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_trinomial_synonym,
      resources$`APC list (known names)`$trinomial
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = resources$`APC list (known names)`$taxon_rank[ii],
      aligned_name = resources$`APC list (known names)`$canonical_name[ii],
      aligned_reason = paste0(
        "Fuzzy match of the first three words of the taxon name to an APC-known canonical name (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_09d_trinomial_fuzzy_synonym"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_10a: exact binomial matches, APC
  # Exact match of first two words of taxon name ("binomial") to APC-accepted canonical name.
  # The purpose of matching only the first two words only to APC-accepted names is that
  # sometimes the submitted taxon name is a valid binomial + notes 
  # or a valid binomial + invalid infraspecific epithet.
  # Such names will only be aligned by matches considering only the first two words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names.
  
  i <-
    taxa$tocheck$binomial %in% resources$`APC list (accepted)`$binomial
  
  ii <-
    match(
      taxa$tocheck[i,]$binomial,
      resources$`APC list (accepted)`$binomial
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = resources$`APC list (accepted)`$taxon_rank[ii],
      aligned_name = resources$`APC list (accepted)`$canonical_name[ii],
      aligned_reason = paste0(
        "Exact match of the first two words of the taxon name to an APC-accepted canonical name (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_10a_binomial_exact_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_10b: exact binomial matches, APC
  # Exact match of first two words of taxon name ("binomial") to APC-known canonical name.
  # The purpose of matching only the first two words only to APC-known names is that
  # sometimes the submitted taxon name is a valid binomial + notes 
  # or a valid binomial + invalid infraspecific epithet.
  # Such names will only be aligned by matches considering only the first two words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names.
  i <-
    taxa$tocheck$binomial %in% resources$`APC list (known names)`$binomial
  
  ii <-
    match(
      taxa$tocheck[i,]$binomial,
      resources$`APC list (known names)`$binomial
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = resources$`APC list (known names)`$taxon_rank[ii],
      aligned_name = resources$`APC list (known names)`$canonical_name[ii],
      aligned_reason = paste0(
        "Exact match of the first two words of the taxon name to an APC-known canonical name (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_10b_binomial_exact_synonym"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_10c: fuzzy binomial matches, APC
  # Fuzzy match of first two words of taxon name ("binomial") to APC-accepted canonical name.
  # The purpose of matching only the first two words only to APC-accepted names is that
  # sometimes the submitted taxon name is a valid binomial + notes 
  # or a valid binomial + invalid infraspecific epithet.
  # Such names will only be aligned by matches considering only the first two words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names.
  for (i in 1:nrow(taxa$tocheck)) {
    if (!is.na(taxa$tocheck$binomial[i]) &
        is.na(taxa$tocheck$fuzzy_match_binomial[i])) {
      taxa$tocheck$fuzzy_match_binomial[i] <-
        fuzzy_match(
          txt = taxa$tocheck$binomial[i],
          accepted_list = resources$`APC list (accepted)`$binomial,
          max_distance_abs = fuzzy_abs_dist,
          max_distance_rel = fuzzy_rel_dist,
          n_allowed = 1,
          epithet_letters = 2
        )
    }
  }
  
  i <-
    taxa$tocheck$fuzzy_match_binomial %in% resources$`APC list (accepted)`$binomial
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_binomial,
      resources$`APC list (accepted)`$binomial
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = resources$`APC list (accepted)`$taxon_rank[ii],
      aligned_name = resources$`APC list (accepted)`$canonical_name[ii],
      aligned_reason = paste0(
        "Fuzzy match of the first two words of the taxon name to an APC-accepted canonical name (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_10c_binomial_fuzzy_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_10d: fuzzy binomial matches, APC
  # Fuzzy match of first two words of taxon name ("binomial") to APC-known canonical name.
  # The purpose of matching only the first two words only to APC-known names is that
  # sometimes the submitted taxon name is a valid binomial + notes 
  # or a valid binomial + invalid infraspecific epithet.
  # Such names will only be aligned by matches considering only the first two words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names.
  for (i in 1:nrow(taxa$tocheck)) {
    if (!is.na(taxa$tocheck$binomial[i]) &
        is.na(taxa$tocheck$fuzzy_match_binomial_APC_synonym[i])) {
      taxa$tocheck$fuzzy_match_binomial_APC_synonym[i] <-
        fuzzy_match(
          txt = taxa$tocheck$binomial[i],
          accepted_list = resources$`APC list (known names)`$binomial,
          max_distance_abs = fuzzy_abs_dist,
          max_distance_rel = fuzzy_rel_dist,
          n_allowed = 1,
          epithet_letters = 2
        )
    }
  }
  
  i <-
    taxa$tocheck$fuzzy_match_binomial_APC_synonym %in% resources$`APC list (known names)`$binomial
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_binomial_APC_synonym,
      resources$`APC list (known names)`$binomial
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = resources$`APC list (known names)`$taxon_rank[ii],
      aligned_name = resources$`APC list (known names)`$canonical_name[ii],
      aligned_reason = paste0(
        "Fuzzy match of the first two words of the taxon name to an APC-known canonical name (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_10d_binomial_fuzzy_synonym"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_11a: fuzzy match to APNI-listed canonical name
  # Fuzzy match of taxon name to an APNI-listed canonical name, once filler words and punctuation are removed.
  # Fuzzy matches to APNI names occur toward the end of the alignment function, 
  # because names exclusively in the APNI are often misspellings of APC accepted/known taxa and
  # many different string searches need to be completed on the APC-accepted and APC-known taxa (e.g. trinomial, binomial, less precise matches) first
  # to avoid incorrectly aligning an APC accepted/known taxa to an APNI name.
  # This is especially true to accurately align phrase names.
  if (APNI_matches == TRUE) {
    for (i in 1:nrow(taxa$tocheck)) {
      taxa$tocheck$fuzzy_match_cleaned_APNI[i] <-
        fuzzy_match(
          txt = taxa$tocheck$stripped_name[i],
          accepted_list = resources$`APNI names`$stripped_canonical,
          max_distance_abs = fuzzy_abs_dist,
          max_distance_rel = fuzzy_rel_dist,
          n_allowed = 1,
          epithet_letters = 2
        )
    }
    
    i <-
      taxa$tocheck$fuzzy_match_cleaned_APNI %in% resources$`APNI names`$stripped_canonical
    
    ii <-
      match(
        taxa$tocheck[i,]$fuzzy_match_cleaned_APNI,
        resources$`APNI names`$stripped_canonical
      )
    
    taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
      mutate(
        taxonomic_dataset = "APNI",
        taxon_rank = resources$`APNI names`$taxon_rank[ii],
        aligned_name = resources$`APNI names`$canonical_name[ii],
        aligned_reason = paste0(
          "Fuzzy match of taxon name to an APNI-listed canonical name once punctuation and filler words are removed (",
          Sys.Date(),
          ")"
        ),
        known = TRUE,
        checked = TRUE,
        alignment_code = "match_11a_fuzzy_APNI_canonical"
      )
    
    taxa <- redistribute(taxa)
    if (nrow(taxa$tocheck) == 0)
      return(taxa)
  }
  
  # match_11b: imprecise fuzzy APNI match
  # Imprecise fuzzy match of taxon name to an APNI-listed canonical name, once filler words and punctuation are removed.
  # For imprecise fuzzy matches, the taxon name can differ from the `APNI-listed` names by 5 characters & up to 25% of the string length.
  # These matches require individual review and are turned off as a default.
  if (APNI_matches == TRUE & imprecise_fuzzy_matches == TRUE) {
    
    for (i in 1:nrow(taxa$tocheck)) {
      taxa$tocheck$fuzzy_match_cleaned_APNI_imprecise[i] <-
        fuzzy_match(
          txt = taxa$tocheck$cleaned_name[i],
          accepted_list = resources$`APNI names`$canonical_name,
          max_distance_abs = imprecise_fuzzy_abs_dist,
          max_distance_rel = imprecise_fuzzy_rel_dist,
          n_allowed = 1,
          epithet_letters = 2
        )
    }
    
    i <-
      taxa$tocheck$fuzzy_match_cleaned_APNI_imprecise %in% resources$`APNI names`$canonical_name
    
    ii <-
      match(
        taxa$tocheck[i,]$fuzzy_match_cleaned_APNI_imprecise,
        resources$`APNI names`$canonical_name
      )
    
    taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
      mutate(
        taxonomic_dataset = "APNI",
        taxon_rank = resources$`APNI names`$taxon_rank[ii],
        aligned_name = resources$`APNI names`$canonical_name[ii],
        aligned_reason = paste0(
          "Imprecise fuzzy match of taxon name to an APNI-listed canonical name once punctuation and filler words are removed (",
          Sys.Date(),
          ")"
        ),
        known = TRUE,
        checked = TRUE,
        alignment_code = "match_11b_imprecise_fuzzy_APNI_canonical_name"
      )
    
    taxa <- redistribute(taxa)
    if (nrow(taxa$tocheck) == 0)
      return(taxa)
  }
  
  # match_11c: exact trinomial matches, APNI
  # Exact match of first three words of taxon name ("trinomial") to APNI-listed canonical name.
  # The purpose of matching only the first three words only to APNI-listed names is that
  # sometimes the submitted taxon name is a valid trinomial + notes and 
  # such names will only be aligned by matches considering only the first three words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names.
  if (APNI_matches == TRUE) {
    i <-
      taxa$tocheck$trinomial %in% resources$`APNI names`$trinomial
    
    ii <-
      match(
        taxa$tocheck[i,]$trinomial,
        resources$`APNI names`$trinomial
      )
    
    taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
      mutate(
        taxonomic_dataset = "APNI",
        taxon_rank = resources$`APNI names`$taxon_rank[ii],
        aligned_name = resources$`APNI names`$canonical_name[ii],
        aligned_reason = paste0(
          "Exact match of the first three words of the taxon name to an APNI-listed canonical name (",
          Sys.Date(),
          ")"
        ),
        known = TRUE,
        checked = TRUE,
        alignment_code = "match_11c_trinomial_exact_APNI"
      )
    
    taxa <- redistribute(taxa)
    if (nrow(taxa$tocheck) == 0)
      return(taxa)
  }
  
  # match_11d: exact binomial matches, APNI
  # Exact match of first two words of taxon name ("binomial") to APNI-listed canonical name.
  # The purpose of matching only the first two words only to APNI-listed names is that
  # sometimes the submitted taxon name is a valid binomial + notes 
  # or a valid binomial + invalid infraspecific epithet.
  # Such names will only be aligned by matches considering only the first two words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names.
  if (APNI_matches == TRUE) {
    i <-
      taxa$tocheck$binomial %in% resources$`APNI names`$binomial
    
    ii <-
      match(
        taxa$tocheck[i,]$binomial,
        resources$`APNI names`$binomial
      )
    
    taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
      mutate(
        taxonomic_dataset = "APNI",
        taxon_rank = resources$`APNI names`$taxon_rank[ii],
        aligned_name = resources$`APNI names`$canonical_name[ii],
        aligned_reason = paste0(
          "Exact match of the first two words of the taxon name to an APNI-listed canonical name (",
          Sys.Date(),
          ")"
        ),
        known = TRUE,
        checked = TRUE,
        alignment_code = "match_11d_binomial_exact_APNI"
      )
    
    taxa <- redistribute(taxa)
    if (nrow(taxa$tocheck) == 0)
      return(taxa)
  }
  
  # match_12a: genus-level alignment
  # Toward the end of the alignment function, see if first word of unmatched taxa is an APC-accepted genus.
  # The 'taxon name' is then reformatted  as `genus sp.` with the original name in square brackets.
  i <-
    taxa$tocheck$genus %in% resources$genera_accepted$genus
  
  ii <-
    match(
      taxa$tocheck[i,]$genus,
      resources$genera_accepted$genus
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = "genus",
      aligned_name_tmp = paste0(resources$genera_accepted$genus[ii], " sp. [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")
      ),
      aligned_reason = paste0(
        "Exact match of the first word of the taxon name to an APC-accepted genus (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_12a_genus_exact_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_12b: genus-level alignment
  # Toward the end of the alignment function, see if first word of unmatched taxa is an APC-known genus.
  # The 'taxon name' is then reformatted  as `genus sp.` with the original name in square brackets.
  i <-
    taxa$tocheck$genus %in% resources$genera_synonym$genus
  
  ii <-
    match(
      taxa$tocheck[i,]$genus,
      resources$genera_synonym$genus
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = "genus",
      aligned_name_tmp = paste0(resources$genera_synonym$genus[ii], " sp. [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")
      ),
      aligned_reason = paste0(
        "Exact match of the first word of the taxon name to an APC-known genus (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_12b_genus_exact_synonym"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_12c: genus-level alignment
  # Toward the end of the alignment function, see if first word of unmatched taxa is an APNI-listed genus.
  # The 'taxon name' is then reformatted  as `genus sp.` with the original name in square brackets.
  if (APNI_matches == TRUE) {
    i <-
      (taxa$tocheck$genus %in% resources$genera_APNI$genus)
    
    ii <-
      match(
        taxa$tocheck[i,]$genus,
        resources$genera_APNI$genus
      )
    
    taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
      mutate(
        taxonomic_dataset = "APNI",
        taxon_rank = "genus",
        aligned_name_tmp = paste0(resources$genera_APNI$genus[ii], " sp. [", cleaned_name),
        aligned_name = ifelse(is.na(identifier_string2),
                              paste0(aligned_name_tmp, "]"),
                              paste0(aligned_name_tmp, identifier_string2, "]")
        ),
        aligned_reason = paste0(
          "Exact match of the first word of the taxon name to an APNI-listed genus (",
          Sys.Date(),
          ")"
        ),
        known = TRUE,
        checked = TRUE,
        alignment_code = "match_12c_genus_exact_APNI"
      )
    
    taxa <- redistribute(taxa)
    if (nrow(taxa$tocheck) == 0)
      return(taxa)
  }
  
  # match_12d: family-level alignment
  # Toward the end of the alignment function, see if first word of unmatched taxa is an APC-accepted family.
  # The 'taxon name' is then reformatted  as `family sp.` with the original name in square brackets.
  
  i <-
    stringr::str_detect(stringr::word(taxa$tocheck$cleaned_name, 1), "aceae$") &
    taxa$tocheck$genus %in% resources$family_accepted$canonical_name
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = "family",
      aligned_name_tmp = paste0(genus, " sp. [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")
      ),
      aligned_reason = paste0(
        "Exact match of the first word of the taxon name to an APC-accepted family (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_12d_family_exact_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_12e: genus-level fuzzy alignment
  # The final alignment step is to see if a fuzzy match can be made for the first word of unmatched taxa to an APC-accepted genus .
  # The 'taxon name' is then reformatted  as `genus sp.` with the original name in square brackets.
  
  i <-
    taxa$tocheck$fuzzy_match_genus %in% resources$genera_accepted$genus
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = "genus",
      aligned_name_tmp = paste0(fuzzy_match_genus, " sp. [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")
      ),
      aligned_reason = paste0(
        "Fuzzy match of the first word of the taxon name to an APC-accepted genus (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_12e_genus_fuzzy_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_12f: genus-level fuzzy alignment
  # The final alignment step is to see if a fuzzy match can be made for the first word of unmatched taxa to an APC-known genus .
  # The 'taxon name' is then reformatted  as `genus sp.` with the original name in square brackets.
  
  i <-
    taxa$tocheck$fuzzy_match_genus_synonym %in% resources$genera_synonym$genus
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    mutate(
      taxonomic_dataset = "APC",
      taxon_rank = "genus",
      aligned_name_tmp = paste0(fuzzy_match_genus_synonym, " sp. [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")
      ),
      aligned_reason = paste0(
        "Fuzzy match of the first word of the taxon name to an APC-known genus (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_12f_genus_fuzzy_synonym"
    )
  
  taxa <- redistribute(taxa)
  
  if (nrow(taxa$tocheck) == 0)
    return(taxa)

  taxa$tocheck <- taxa$tocheck %>% dplyr::select(-identifier_string, -identifier_string2, -aligned_name_tmp)
  taxa$checked <- taxa$checked %>% dplyr::select(-identifier_string, -identifier_string2, -aligned_name_tmp)

  return(taxa)
}
