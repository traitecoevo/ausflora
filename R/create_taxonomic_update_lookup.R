#' @title Create a table with the best-possible scientific name match for 
#' Australian plant names
#'
#' @description
#' This function takes a list of Australian plant names that need to be 
#' reconciled with current taxonomy and generates a lookup table of the 
#' best-possible scientific name match for each input name.
#' 
#' Usage case: This is APCalign’s core function, merging together the alignment 
#' and updating of taxonomy.
#' 
#' @details
#' - It uses first the function `align_taxa`, then the function `update_taxonomy` 
#' to achieve the output. The aligned name is plant name that has been aligned 
#' to a taxon name in the APC or APNI by the align_taxa function.
#' 
#' Notes:
#' 
#' - If you will be running the function APCalign::create_taxonomic_update_lookup 
#' many times, it is best to load the taxonomic resources separately using 
#' `resources <- load_taxonomic_resources()`, then add the argument 
#' resources = resources
#' - The name Banksia cerrata does not align as the fuzzy matching algorithm 
#' does not allow the first letter of the genus and species epithet to change.
#' - The argument taxonomic_splits allows you to choose the outcome for updating 
#' the names of taxa with ambiguous taxonomic histories; this applies to 
#' scientific names that were once attached to a more broadly circumscribed 
#' taxon concept, that was then split into several more narrowly circumscribed 
#' taxon concepts, one of which retains the original name. There are three 
#' options: most_likely_species returns the name that is retained, with 
#' alternative names documented in square brackets; return_all adds additional 
#' rows to the output, one for each possible taxon concept; 
#' collapse_to_higher_taxon returns the genus with possible names in square 
#' brackets.
#' - The argument identifier allows you to add a fix text string to all genus- 
#' and family- level names, such as identifier = "Royal NP" would return 
#' `Acacia sp. \[Royal NP]`.
#'
#' @family taxonomic alignment functions
#'
#' @param taxa A list of Australian plant species that needs to be reconciled
#'  with current taxonomy.
#' @param stable_or_current_data either "stable" for a consistent version,
#'  or "current" for the leading edge version.
#' @param version The version number of the dataset to use.
#' @param taxonomic_splits How to handle one_to_many taxonomic matches.
#'  Default is "return_all".  The other options are "collapse_to_higher_taxon"
#'  and "most_likely_species". most_likely_species defaults to the original_name
#'  if that name is accepted by the APC; this will be right for certain species
#'  subsets, but make errors in other cases, use with caution.
#' @param full logical for whether the full lookup table is returned or
#'  just key columns 
#' @param fuzzy_abs_dist The number of characters allowed to be different for
#'  a fuzzy match.
#' @param fuzzy_rel_dist The proportion of characters allowed to be different
#'  for a fuzzy match. 
#' @param fuzzy_matches Fuzzy matches are turned on as a default. The relative
#'  and absolute distances allowed for fuzzy matches to species and
#'  infraspecific taxon names are defined by the parameters `fuzzy_abs_dist`
#'  and `fuzzy_rel_dist`.
#' @param resources These are the taxonomic resources used for cleaning, this
#'  will default to loading them from a local place on your computer. If this is
#'  to be called repeatedly, it's much faster to load the resources using
#'  \code{\link{load_taxonomic_resources}} separately and pass the data in.
#' @param APNI_matches Name matches to the APNI (Australian Plant Names Index)
#'  are turned off as a default. 
#' @param imprecise_fuzzy_matches Imprecise fuzzy matches uses the fuzzy
#'  matching function with lenient levels set (absolute distance of
#'  5 characters; relative distance = 0.25). 
#'  It offers a way to get a wider range of possible names, possibly
#'  corresponding to very distant spelling mistakes. 
#'  This is FALSE as default and all outputs should be checked as it often
#'  makes erroneous matches.
#' @param identifier A dataset, location or other identifier,
#'  which defaults to NA.
#' @param quiet Logical to indicate whether to display messages while
#'  aligning taxa.
#' @param output file path to save the output. If this file already exists,
#'  this function will check if it's a subset of the species passed in and try
#'  to add to this file. This can be useful for large and growing projects. 
#' @return A lookup table containing the accepted and suggested names for each
#'  original name input, and additional taxonomic information such as taxon
#'  rank, taxonomic status, taxon IDs and genera. 
#' - original_name: the original plant name.
#' - aligned_name: the input plant name that has been aligned to a taxon name in
#'   the APC or APNI by the align_taxa function.
#' - accepted_name: the APC-accepted plant name, when available.
#' - suggested_name: the suggested plant name to use. Identical to the
#'   accepted_name, when an accepted_name exists;
#'   otherwise the the suggested_name is the aligned_name.
#' - genus: the genus of the accepted (or suggested) name; 
#'  only APC-accepted genus names are filled in.
#' - family: the family of the accepted (or suggested) name;
#'   only APC-accepted family names are filled in.
#' - taxon_rank: the taxonomic rank of the suggested (and accepted) name.
#' - taxonomic_dataset: the source of the suggested (and accepted) names
#'   (APC or APNI).
#' - taxonomic_status: the taxonomic status of the suggested (and accepted) name.
#' - taxonomic_status_aligned: the taxonomic status of the aligned name,
#'   before any taxonomic updates have been applied.
#' - aligned_reason: the explanation of a specific taxon name alignment
#'   (from an original name to an aligned name).
#' - update_reason: the explanation of a specific taxon name update
#'   (from an aligned name to an accepted or suggested name).
#' - subclass: the subclass of the accepted name.
#' - taxon_distribution: the distribution of the accepted name;
#'   only filled in if an APC accepted_name is available.
#' - scientific_name_authorship: the authorship information for the accepted
#'   (or synonymous) name; available for both APC and APNI names.
#' - taxon_ID: the unique taxon concept identifier for the accepted_name;
#'   only filled in if an APC accepted_name is available.
#' - taxon_ID_genus: an identifier for the genus;
#'   only filled in if an APC-accepted genus name is available.
#' - scientific_name_ID: an identifier for the nomenclatural (not taxonomic)
#'   details of a scientific name; available for both APC and APNI names.
#' - row_number: the row number of a specific original_name in the input.
#' - number_of_collapsed_taxa: when taxonomic_splits == "collapse_to_higher_taxon",
#'   the number of possible taxon names that have been collapsed.
#' 
#' @export
#'
#' @seealso \code{\link{load_taxonomic_resources}}
#' @examples
#' \donttest{
#' resources <- load_taxonomic_resources()
#' 
#' # example 1
#' create_taxonomic_update_lookup(c("Eucalyptus regnans",
#'                                  "Acacia melanoxylon",
#'                                  "Banksia integrifolia",
#'                                  "Not a species"),
#'                                  resources = resources)
#'                                  
#' # example 2
#' input <- c("Banksia serrata", "Banksia serrate", "Banksia cerrata", 
#' "Banksea serrata", "Banksia serrrrata", "Dryandra")
#' 
#' create_taxonomic_update_lookup(
#'     taxa = input,
#'     identifier = "APCalign test",
#'     full = TRUE,
#'     resources = resources
#'   )
#'
#' # example 3
#' taxon_list <-
#'   readr::read_csv(
#'   system.file("extdata", "test_taxa.csv", package = "APCalign"),
#'   show_col_types = FALSE)
#' 
#' create_taxonomic_update_lookup(
#'     taxa = taxon_list$original_name,
#'     identifier = taxon_list$notes,
#'     full = TRUE,
#'     resources = resources
#'   )
#' }
#' 
create_taxonomic_update_lookup <- function(taxa,
                                           stable_or_current_data = "stable",
                                           version = default_version(),
                                           taxonomic_splits = "most_likely_species",
                                           full = FALSE,
                                           fuzzy_abs_dist = 3, 
                                           fuzzy_rel_dist = 0.2, 
                                           fuzzy_matches = TRUE, 
                                           APNI_matches = TRUE, 
                                           imprecise_fuzzy_matches = FALSE, 
                                           identifier = NA_character_,
                                           resources = load_taxonomic_resources(),
                                           quiet = FALSE,
                                           output = NULL) {

  validate_taxonomic_splits_input(taxonomic_splits)

  aligned_data <- 
    align_taxa(taxa, resources = resources, 
               APNI_matches = APNI_matches, 
               identifier = identifier, 
               fuzzy_abs_dist = fuzzy_abs_dist, 
               fuzzy_rel_dist = fuzzy_rel_dist, 
               fuzzy_matches = fuzzy_matches, 
               imprecise_fuzzy_matches = imprecise_fuzzy_matches,
               quiet = quiet,
               output=output)

  updated_data <- 
    update_taxonomy(aligned_data, 
      taxonomic_splits = taxonomic_splits,
      resources = resources,
      quiet = quiet,
      output = output)
  
  if (!full) {
    updated_data <-
      updated_data %>%
        dplyr::select(
          dplyr::any_of(c(
            "original_name", "aligned_name", "accepted_name", "suggested_name",
            "genus", "taxon_rank", "taxonomic_dataset", "taxonomic_status",
            "scientific_name", "aligned_reason", "update_reason", 
            "alternative_possible_names", "possible_names_collapsed",
            "number_of_collapsed_taxa"
          ))
        )        
  }

  # todo - should we add file caching here? Or is it enough to have in component functions
  return(updated_data)
}

#' @noRd
validate_taxonomic_splits_input <- function(taxonomic_splits) {
  valid_inputs <-
    c("return_all",
      "collapse_to_higher_taxon",
      "most_likely_species")
  if (!taxonomic_splits %in% valid_inputs)
    stop(
      paste(
        "Invalid input:",
        taxonomic_splits,
        ". Valid inputs are 'return_all', 'collapse_to_higher_taxon', or
         'most_likely_species'."
      )
    )
}
