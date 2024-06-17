#' @title Update to currently accepted APC name and add APC/APNI name metadata 
#' 
#' @description
#' For a list of taxon names aligned to the APC, update the name to an accepted
#' taxon concept per the APC and add scientific name and taxon concept metadata 
#' to names aligned to either the APC or APNI. 
#'
#' @details
#' - This function uses the APC to update the taxonomy of names aligned to a
#' taxon concept listed in the APC to the currently accepted name for the taxon 
#' concept. 
#' - The aligned_data data frame that is input must contain 5 columns, 
#' `original_name`, `aligned_name`, `taxon_rank`, `taxonomic_dataset`, and 
#' `aligned_reason`. (These are the columns output by the function `align_taxa`.) 
#' - The aligned name is a plant name that has been aligned to a taxon name in 
#' the APC or APNI by the align_taxa function.
#' 
#' Notes:
#' - As the input for this function is a table with 5 columns (output by 
#' align_taxa), this function will only be used when you explicitly want to 
#' separate the alignment and updating components of APCalign. This function is 
#' the second half of create_taxonomic_update_lookup.
#'
#' @family taxonomic alignment functions
#'
#' @param aligned_data A tibble of plant names to update. This table must 
#' include 5 columns, original_name, aligned_name, taxon_rank, 
#' taxonomic_dataset, and aligned_reason.
#' These columns are created by the function `align_taxa`. 
#' The columns `original_name` and `aligned_name` must be in the format of the 
#' scientific name, with genus and species, 
#' and may contain additional qualifiers such as subspecies or varieties. The 
#' names are case insensitive.
#' @param taxonomic_splits Variable that determines what protocol to use to 
#' update taxon names that are ambiguous due to taxonomic splits. 
#' The three options are:
#'    - `most_likely_species`, which returns the species name in use before the 
#'    split; alternative names are returned in a separate column
#'   - `return_all`, which returns all possible names
#'   - `collapse_to_higher_taxon`, which declares that an ambiguous name cannot 
#'   be aligned to an accepted species/infraspecific name and the name is 
#'   demoted to genus rank
#' @param quiet Logical to indicate whether to display messages while updating 
#' taxa.
#' @param output (optional) Name of the file where results are saved. The 
#' default is NULL and no file is created. If specified, the output will be 
#' saved in a CSV file with the given name.
#' @param resources the taxonomic resources required to make the summary 
#' statistics.  Loading this can be slow, so call load_taxonomic_resources 
#' separately to greatly speed this function up and pass the resources in.
#'
#'
#' @return A tibble with updated taxonomy for the specified plant names. The 
#' tibble contains the following columns:
#' - original_name: the original plant name.
#' - aligned_name: the input plant name that has been aligned to a taxon name 
#' in the APC or APNI by the align_taxa function.
#' - accepted_name: the APC-accepted plant name, when available.
#' - suggested_name: the suggested plant name to use. Identical to the 
#' accepted_name, when an accepted_name exists; otherwise the the suggested_name
#'  is the aligned_name.
#' - genus: the genus of the accepted (or suggested) name; only APC-accepted 
#' genus names are filled in.
#' - family: the family of the accepted (or suggested) name; only APC-accepted 
#' family names are filled in.
#' - taxon_rank: the taxonomic rank of the suggested (and accepted) name.
#' - taxonomic_dataset: the source of the suggested (and accepted) names (APC or
#'  APNI).
#' - taxonomic_status: the taxonomic status of the suggested (and accepted) name.
#' - taxonomic_status_aligned: the taxonomic status of the aligned name, before 
#' any taxonomic updates have been applied.
#' - aligned_reason: the explanation of a specific taxon name alignment (from an 
#' original name to an aligned name).
#' - update_reason: the explanation of a specific taxon name update (from an 
#' aligned name to an accepted or suggested name).
#' - subclass: the subclass of the accepted name.
#' - taxon_distribution: the distribution of the accepted name; only filled in 
#' if an APC accepted_name is available.
#' - scientific_name_authorship: the authorship information for the accepted 
#' (or synonymous) name; available for both APC and APNI names.
#' - taxon_ID: the unique taxon concept identifier for the accepted_name; only 
#' filled in if an APC accepted_name is available.
#' - taxon_ID_genus: an identifier for the genus; only filled in if an 
#' APC-accepted genus name is available.
#' - scientific_name_ID: an identifier for the nomenclatural (not taxonomic) 
#' details of a scientific name; available for both APC and APNI names.
#' - row_number: the row number of a specific original_name in the input.
#' - number_of_collapsed_taxa: when taxonomic_splits == "collapse_to_higher_taxon", 
#' the number of possible taxon names that have been collapsed.
#'   
#'
#' @seealso load_taxonomic_resources
#'
#' @export
#'
#' @examples
#' # Update taxonomy for two plant names and print the result
#' \donttest{
#' resources <- load_taxonomic_resources()
#' 
#' update_taxonomy(
#'  dplyr::tibble(
#'    original_name = c("Dryandra preissii", "Banksia acuminata"),
#'    aligned_name = c("Dryandra preissii", "Banksia acuminata"),
#'    taxon_rank = c("species", "species"),
#'    taxonomic_dataset = c("APC", "APC"),
#'    aligned_reason = c(NA_character_,
#'    NA_character_)
#'  ),
#'  resources = resources
#' )
#' }

update_taxonomy <- function(aligned_data,
                            taxonomic_splits = "most_likely_species",
                            quiet = TRUE,
                            output = NULL,
                            resources = load_taxonomic_resources()) {
    
  aligned_data <- 
    aligned_data %>%
    dplyr::select(original_name, aligned_name, taxon_rank, taxonomic_dataset, aligned_reason) %>%
    dplyr::mutate(
      genus = extract_genus(aligned_name),
      row_number = dplyr::row_number()
    )

  ## split tibble of aligned names based on 
  ## - the taxonomic dataset (APC, APNI), 
  ## - the taxon rank (family, genus, species/infraspecific) assigned during taxon alignments
  taxa_out <-
    split(aligned_data,
      paste(
        # Taxonomic reference
        aligned_data$taxonomic_dataset,
        # Taxon rank
        ifelse(species_and_infraspecific(aligned_data$taxon_rank), "species_and_infraspecific_taxa", aligned_data$taxon_rank)
      )
    )

  ## taxa whose aligned_names are taxon_rank = genus and taxonomic_dataset = APC
  taxa_out[["APC genus"]] <- taxa_out[["APC genus"]] %>%
    update_taxonomy_APC_genus(resources)
    
  ## taxa whose aligned_names are taxon_rank = genus and taxonomic_dataset = APNI
  ## these are genera that are not recorded in the APC and therefore there are no identifiers for these taxa
  ## the only information that can be added is, when available, the family name
  taxa_out[["APNI genus"]] <- taxa_out[["APNI genus"]] %>%
    update_taxonomy_APNI_genus(resources)
    
  ## taxa whose aligned_names are taxon_rank = family and taxonomic_dataset = APC
  ## these are taxa for which the `genus` and `accepted_name` fields will be NA's and all identifiers will be blank
  taxa_out[["APC family"]] <- taxa_out[["APC family"]] %>%
    update_taxonomy_APC_family(resources)
    
  ## taxa whose aligned_names are taxon_rank = species/infraspecific and taxonomic_dataset = APC
  ## these are the subset of names in `taxa_out` that *should* have an APC-accepted name
  taxa_out[["APC species_and_infraspecific_taxa"]] <- taxa_out[["APC species_and_infraspecific_taxa"]] %>%
    update_taxonomy_APC_species_and_infraspecific_taxa(resources, taxonomic_splits)
  
  ## taxa whose aligned_names are taxon_rank = species/infraspecific and taxonomic_dataset = APNI
  taxa_out[["APNI species_and_infraspecific_taxa"]] <- taxa_out[["APNI species_and_infraspecific_taxa"]] %>%
    update_taxonomy_APNI_species_and_infraspecific_taxa(resources)
  
  ## create a blank tibble with all columns, for taxon lists where some columns aren't created in any of the individual tibbles
  taxa_blank <-
      dplyr::tibble(
        original_name = character(0L),
        aligned_name = character(0L),
        accepted_name = character(0L),
        suggested_name = character(0L),
        genus = character(0L),
        family = character(0L),
        taxon_rank = character(0L),
        taxonomic_dataset = character(0L),
        taxonomic_status = character(0L),
        taxonomic_status_aligned = character(0L),
        aligned_reason = character(0L),
        update_reason = character(0L),
        subclass = character(0L),
        taxon_distribution = character(0L),
        scientific_name = character(0L),
        taxon_ID = character(0L),
        taxon_ID_genus = character(0L),
        scientific_name_ID = character(0L),
        canonical_name = character(0L),
        row_number = numeric(0L),
        number_of_collapsed_taxa = numeric(0L)
      )

  ## bind taxa_out back together.
  ## there are some tibbles that can be created when taxa_out is split that will not have been manipulated, namely those without alignments and that
  ## therefore are NA for either `taxonomic_dataset` or `taxon_rank`; the mutate steps below are primarily to ensure the final `suggested_name`
  ## `taxon_rank`, `taxonomic_status`, `taxonomic_dataset`, `genus` are properly filled in for those rows of data
  taxa_out <-
    dplyr::bind_rows(taxa_out) %>%
    dplyr::bind_rows(taxa_blank) %>%
    dplyr::mutate(
      suggested_name = ifelse(is.na(suggested_name), aligned_name, suggested_name),
      update_reason = ifelse(taxonomic_status_aligned == "accepted", "aligned name accepted by APC", update_reason),
      taxonomic_status = ifelse(is.na(taxonomic_status), "unknown", taxonomic_status),
      taxonomic_dataset = ifelse(stringr::str_detect(taxonomic_dataset, "APC"), "APC", taxonomic_dataset),
      ## `genus` was the first word of the `aligned_name` in the input table; now needs to be set to NA for unknown taxa
      genus = ifelse(taxonomic_status == "unknown", NA_character_, genus),
      taxon_rank = ifelse(taxonomic_status == "unknown", NA_character_, taxon_rank),
      # the next line makes everything inconsistent. If we want low, should do on loading APC
      taxon_rank = stringr::str_to_lower(taxon_rank),
      canonical_name = suggested_name,
      taxonomic_status_aligned = ifelse(is.na(taxonomic_status_aligned), NA_character_, taxonomic_status_aligned)
    ) %>%
    dplyr::select(
      dplyr::any_of(c(
      "original_name",
      "aligned_name",
      "accepted_name",
      "suggested_name",
      "genus",
      "family",
      "taxon_rank",
      "taxonomic_dataset",
      "taxonomic_status",
      "taxonomic_status_aligned",
      "aligned_reason",
      "update_reason",
      "subclass",
      "taxon_distribution",
      "scientific_name",
      "taxon_ID",
      "taxon_ID_genus",
      "scientific_name_ID",
      "canonical_name",
      "taxonomic_status_aligned",
      "row_number",
      "number_of_collapsed_taxa"
    )))      
  
  # Assemble output in the order of the input `aligned_names`
  # sort so that has same order as input
  taxa_out <- taxa_out  %>% dplyr::arrange(row_number)

  if (!is.null(output)) {
    taxa_out$checked<-TRUE
    taxa_out$known<-!is.na(taxa_out$accepted_name)
    readr::write_csv(taxa_out, output)
    if(!quiet)
      message("  - output saved in file: ", output)
  }
  
  taxa_out
}

# Logical based on rank of a taxon
species_and_infraspecific <- function(taxon_rank) {
  taxon_rank %in% c("species", "form", "variety", "subspecies", "series")
}

# preferred order of taxonomic updates
relevel_taxonomic_status_preferred_order <- function(taxonomic_status) {
  
  preferred_order <-
    c(
      "accepted",
      "taxonomic synonym",
      "basionym",
      "nomenclatural synonym",
      "isonym",
      "orthographic variant",
      "common name",
      "doubtful taxonomic synonym",
      "replaced synonym",
      "doubtful pro parte taxonomic synonym",
      "pro parte nomenclatural synonym",
      "pro parte taxonomic synonym",
      "pro parte misapplied",
      "misapplied",
      "unplaced", 
      "excluded",
      "doubtful misapplied",
      "doubtful pro parte misapplied",
      "included"
    )
  
  factor(taxonomic_status, levels =
    subset(
      preferred_order, 
      preferred_order %in% taxonomic_status
      )
  )
}
        

# Function to update names of taxa whose aligned_names are 
# taxon_rank = genus and taxonomic_dataset = APC
update_taxonomy_APC_genus <- function(data, resources) {

  if(is.null(data)) return(NULL)
  
  data %>% 
  # merge in columns from APC, at the genus-level
  dplyr::left_join(
    by = "genus",
    resources$genera_all %>%
      dplyr::filter(stringr::str_detect(taxonomic_dataset, "APC")) %>%
      dplyr::arrange(canonical_name, taxonomic_status) %>%
      dplyr::distinct(canonical_name, .keep_all = TRUE) %>%
      dplyr::mutate(
        genus = canonical_name,
        taxonomic_dataset_genus = taxonomic_dataset
      ) %>%
      dplyr::select(
        genus,
        taxonomic_dataset_genus,
        accepted_name_usage_ID,
        taxonomic_status
      )
  ) %>%
    dplyr::mutate(my_order = relevel_taxonomic_status_preferred_order(taxonomic_status)) %>%
    dplyr::arrange(aligned_name, my_order) %>%
    dplyr::mutate(
      # if required, update the genus name in the `aligned_name` to the currently APC-accepted genus
      genus_accepted = resources$genera_all$canonical_name[match(accepted_name_usage_ID, resources$genera_all$taxon_ID)],
      # add variables specifying the taxonomic_status and taxon_ID of the genus, since these will be APC-accepted taxon names, even though the
      # `accepted_name` and `taxon_ID` at the species-level will be blank
      taxonomic_status_genus = resources$genera_all$taxonomic_status[match(accepted_name_usage_ID, resources$genera_all$taxon_ID)],
      taxonomic_status = ifelse(is.na(accepted_name_usage_ID), as.character(my_order), paste("genus", taxonomic_status_genus)),
      taxon_ID_genus = resources$genera_all$taxon_ID[match(accepted_name_usage_ID, resources$genera_all$accepted_name_usage_ID)],
      # genus names in `aligned_name` that are not APC-accepted need to be updated to their current name in `suggested_name`
      aligned_minus_genus = ifelse(is.na(genus_accepted), NA, stringr::str_replace(aligned_name, extract_genus(aligned_name), "")),
      # if there is an APC-accepted genus, replace whatever the initial genus was with the accepted genus, otherwise the suggested name is the aligned name
      suggested_name = ifelse(taxonomic_status == "genus accepted", paste0(genus_accepted, aligned_minus_genus), aligned_name),
      # indicate taxonomic_status of the genus name in `aligned_name` and why it needed to be updated for the `suggested_name`
      genus_update_reason = as.character(my_order),
      genus = genus_accepted,
      taxonomic_dataset = "APC"
    ) %>%
    # add family alignments in
    dplyr::left_join(
      by = "genus",
      resources$APC %>%
        dplyr::filter(family %in% resources$family_accepted$family) %>%
        dplyr::select(
          genus,
          family
        ) %>%
        dplyr::distinct(genus, .keep_all = TRUE)
    ) %>%
    dplyr::select(-taxonomic_dataset_genus, -taxonomic_status_genus, -aligned_minus_genus, -my_order, -genus_accepted, -accepted_name_usage_ID)
}

# Function to update names of taxa whose aligned_names are
#  taxon_rank = genus and taxonomic_dataset = APNI
update_taxonomy_APNI_genus <- function(data, resources) {

  if(is.null(data)) return(NULL)

  data %>%
    dplyr::left_join(
      by = "genus",
      resources$APNI %>%
        dplyr::filter(family %in% resources$family_accepted$family) %>%
        dplyr::select(
          genus,
          family
        ) %>%
        dplyr::distinct(genus, .keep_all = TRUE)
    ) %>%
    # the `suggested_name` is set to the aligned_name and other columns are set to NA
    dplyr::mutate(
      accepted_name = NA_character_,
      suggested_name = aligned_name,
      taxonomic_status_genus = "genus unplaced by APC",
      taxonomic_status = "genus unplaced by APC",
    )
}

# Function to update names of taxa whose aligned_names are
# taxon_rank = family and taxonomic_dataset = APC
update_taxonomy_APC_family <- function(data, resources) {

  if(is.null(data)) return(NULL)

  families <- resources$family_accepted %>% 
                dplyr::bind_rows(resources$family_synonym) %>%
                dplyr::mutate(family = genus)

  data %>%
    dplyr::mutate(
      family = genus,
      genus = NA_character_,
      taxonomic_status_genus = NA_character_
    ) %>%  
    dplyr::left_join(
      by = "family",
      families %>%
        dplyr::arrange(canonical_name, taxonomic_status) %>%
        dplyr::distinct(canonical_name, .keep_all = TRUE) %>%
        dplyr::select(
          family,
          accepted_name_usage_ID,
          taxonomic_status
        )
    ) %>%
    dplyr::mutate(my_order = relevel_taxonomic_status_preferred_order(taxonomic_status)) %>%
    dplyr::arrange(aligned_name, my_order) %>%
    dplyr::mutate(
      # if required, update the family name in the `aligned_name` to the currently APC-accepted family
      family_accepted = families$canonical_name[match(accepted_name_usage_ID, families$taxon_ID)]
    ) %>%
    dplyr::mutate(
      accepted_name = NA_character_,
      # family names in `aligned_name` that are not APC-accepted need to be updated to their current name in `suggested_name`
      aligned_minus_genus = stringr::str_replace(aligned_name, family, ""),
      # if there is an APC-accepted genus, replace whatever the initial genus was with the accepted genus, otherwise the suggested name is the aligned name
      suggested_name = ifelse(my_order == "accepted", aligned_name, paste0(family_accepted, aligned_minus_genus)),
      taxonomic_status = "family accepted",
      taxonomic_dataset = "APC",
      family = family_accepted
    ) %>%
    dplyr::select(-accepted_name_usage_ID, -family_accepted, -my_order)
}

# Function to update names of taxa whose aligned_names are
# taxon_rank = species/infraspecific and taxonomic_dataset = APC
update_taxonomy_APC_species_and_infraspecific_taxa <- function(data, resources, taxonomic_splits) {

  if(is.null(data)) return(NULL)
  
  ## All canonical names which can link to multiple accepted name usages, because the taxon has been split
  split_taxa_canonical <- 
    resources$APC %>%
    dplyr::filter(pro_parte == TRUE) %>%
    dplyr::filter(species_and_infraspecific(taxon_rank)) %>%
    dplyr::distinct(canonical_name, .keep_all = TRUE) %>%
    dplyr::select(canonical_name, accepted_name_usage_ID)
    
  split_taxa_table <- 
    resources$APC %>%
    dplyr::filter(species_and_infraspecific(taxon_rank)) %>%
    dplyr::filter(canonical_name %in% split_taxa_canonical$canonical_name ) %>%  # ## see https://id.biodiversity.org.au/node/apni/2912962 ; somehow need both canonical in split and a few (few!) additional accepted_name_usage_id's to match
    #dplyr::filter(taxonomic_status != "misapplied" & taxonomic_status != "excluded") %>%
    #dplyr::filter(pro_parte == TRUE | taxonomic_status == "accepted") %>% # this is the filter that needs to be applied for what names gets joined in if "return_all" is true
    dplyr::mutate(
      alternative_accepted_name = resources$'APC list (accepted)'$canonical_name[match(accepted_name_usage_ID, resources$'APC list (accepted)'$accepted_name_usage_ID)],
      alternative_accepted_name = ifelse(is.na(alternative_accepted_name), resources$APC$canonical_name[match(accepted_name_usage_ID, resources$APC$accepted_name_usage_ID)], alternative_accepted_name),
      accepted_name_2 = alternative_accepted_name
    ) %>%
    dplyr::distinct(canonical_name, alternative_accepted_name, taxonomic_status, taxon_rank, .keep_all = TRUE) %>% ## there can be multiple rows for this same combination, due to different `instances` of a name; we only maintain 1 row
    dplyr::mutate(
      alternative_accepted_name = ifelse(alternative_accepted_name == canonical_name, NA, alternative_accepted_name),
      alternative_possible_names = NA_character_,
      suggested_collapsed_name = NA_character_,
      number_of_collapsed_taxa = 1,
      my_order = relevel_taxonomic_status_preferred_order(taxonomic_status)
      ) %>%
    dplyr::arrange(my_order) %>%
    dplyr::select(canonical_name, accepted_name_2, alternative_accepted_name, alternative_possible_names, taxonomic_status, accepted_name_usage_ID, 
                  taxon_rank, my_order, pro_parte, suggested_collapsed_name, number_of_collapsed_taxa) %>%
    dplyr::rename(taxonomic_status_aligned = taxonomic_status)
  
  if (taxonomic_splits == "most_likely_species") {
    split_taxa_table <-
      split_taxa_table %>%
      dplyr::mutate(
        alternative_accepted_name_tmp = ifelse(is.na(alternative_accepted_name), NA, paste0(alternative_accepted_name, " (", taxonomic_status_aligned, ")"))
      ) %>%
      dplyr::group_by(canonical_name) %>%
      dplyr::mutate(alternative_possible_names = 
                      alternative_accepted_name_tmp %>%
                      unique() %>% 
                      .[-1] %>%
                      paste0(collapse = " | ") %>%
                      dplyr::na_if("")
      ) %>%
      dplyr::arrange(my_order) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        alternative_possible_names = ifelse(taxonomic_status_aligned != "accepted" & canonical_name %in% resources$'APC list (accepted)'$canonical_name, NA, alternative_possible_names),
        alternative_possible_names = stringr::str_replace_all(alternative_possible_names, "\\ \\|\\ NA", ""),    
        suggested_collapsed_name = paste0(accepted_name_2, " [alternative possible names: ", alternative_possible_names, "]"),
      ) %>%
      dplyr::select(-alternative_accepted_name_tmp)
  } else {
    # while `misapplied` and `excluded` names are documented as `alternative_possible_names`, it is not appropriate for them to be returned as a `suggested_name`
    # this means that the number of names reported when taxonomic_splits = "most_likely_species" is different to the number of names reported for other values
    split_taxa_table <-
      split_taxa_table %>%
      dplyr::filter(taxonomic_status_aligned != "misapplied" & taxonomic_status_aligned != "excluded")
  } 
  
  ## with the option `collapse_to_higher_taxon` names whose connection to a taxon concept is ambiguous due to a taxonomic split are demoted to taxon_rank = genus
  ## and the suggested name is given as `genus sp.`
  ## the possible species/infraspecific taxon names are collapsed into a single column
  if (taxonomic_splits == "collapse_to_higher_taxon") {
  split_taxa_table <-
    split_taxa_table %>%
    dplyr::mutate(
      alternative_accepted_name = ifelse(is.na(alternative_accepted_name), accepted_name_2, alternative_accepted_name),
      alternative_accepted_name_tmp = ifelse(is.na(alternative_accepted_name), NA, paste0(alternative_accepted_name, " (", taxonomic_status_aligned, ")")),
      number_of_collapsed_taxa = 1
    ) %>%
    dplyr::group_by(canonical_name) %>%
    dplyr::mutate(
      number_of_collapsed_taxa = sum(number_of_collapsed_taxa),
      accepted_name_2 = paste(word(accepted_name_2, 1), "sp."),
      alternative_possible_names = 
                    alternative_accepted_name_tmp %>%
                    unique() %>%
                    paste0(collapse = " | ") %>%
                    dplyr::na_if("")
    ) %>%
    dplyr::arrange(my_order) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      alternative_possible_names = ifelse(taxonomic_status_aligned != "accepted" & canonical_name %in% resources$'APC list (accepted)'$canonical_name, NA, alternative_possible_names),
      alternative_possible_names = stringr::str_replace_all(alternative_possible_names, "\\ \\|\\ NA", ""),
      suggested_collapsed_name = paste(word(accepted_name_2, 1), "sp. [collapsed names:", alternative_possible_names, "]"),
      taxon_rank = ifelse(number_of_collapsed_taxa > 1 & species_and_infraspecific(taxon_rank), "genus", taxon_rank)
    ) %>%
    dplyr::select(-alternative_accepted_name_tmp, -alternative_possible_names)
  }
  
  data %>%
    ## First propagate extra entries for taxa that have been split, based on the aligned names
    ## `misapplied` names need to be filtered out, as these are names that should not be include in the output when `taxonomic_splits = "return_all"`
    ## for taxa where there is ambiguity due to a taxon split, there can be multiple `canonical_name` (per APC) matches to a single `aligned_name`
    ## however the APC-column `taxon_ID` will be unique for each row of data
    ## the `accepted_named_usage_ID` column will provide the link to the possibly-accepted name
    dplyr::left_join(
      by = "aligned_name",
      resources$APC %>%
        dplyr::filter(
          species_and_infraspecific(taxon_rank)
        ) %>%
        dplyr::mutate(
          my_order = relevel_taxonomic_status_preferred_order(taxonomic_status)
        ) %>%
        dplyr::arrange(my_order) %>%
        ## due to splits there can be duplicate `canonical_name` entries, which would causes row duplication; we retain the accepted name, 
        ## or the name with the "highest priority" taxonomic_status, per `my_order` 
        dplyr::distinct(canonical_name, .keep_all = TRUE) %>%
        dplyr::rename(
          aligned_name = canonical_name,
          taxonomic_status_aligned = taxonomic_status
        ) %>%
        dplyr::select(
          aligned_name,
          taxonomic_status_aligned,
          accepted_name_usage_ID
        )
    ) %>%
    ## Second, find accepted names for each name in the species (and infraspecific taxon) list (sometimes they are the same)
    ## `accepted_name_usage_ID` provides the code to update the names. A given accepted_name has a single `accepted_name_usage_ID`
    dplyr::left_join(
      by = "accepted_name_usage_ID",
      resources$APC %>%
        dplyr::filter(taxonomic_status == "accepted") %>%
        dplyr::mutate(
          ## the canonical_name for an APC-accepted species becomes the `accepted_name` in the output
          accepted_name = canonical_name
        ) %>%
        # add in the other columns we care about
        dplyr::select(
          accepted_name_usage_ID,
          accepted_name,
          taxonomic_status,
          scientific_name,
          scientific_name_ID,
          family,
          subclass,
          taxon_distribution
        )
    ) %>%
    ## Third, we join in the information on taxonomic splits, from the `split_taxa_table` created above.
    ## The columns in this table varies based on the value of the variable `taxonomic_splits`
    ## If `taxonomic_splits = "return_all"`, this step increases the number of rows in the table; for other parameter values the number of rows should stay the same
    dplyr::left_join(
      by = "aligned_name",
      split_taxa_table %>%
        dplyr::rename(aligned_name = canonical_name) %>%
        dplyr::select(aligned_name, accepted_name_2, alternative_accepted_name, suggested_collapsed_name, number_of_collapsed_taxa)
    ) %>%
    ## Fourth, there are a number of final manipulations, including taking all the different information sources merged in to determine the best possible `suggested_name`
    dplyr::mutate(
      ## the `accepted_name` coming from the `split_taxa_table` takes priority over the `accepted_name` simply generated from the `aligned_name`
      accepted_name = ifelse(!is.na(accepted_name_2), accepted_name_2, accepted_name),
      ## except this introduces some names where the `taxonomic_status` isn't actually `accepted`
      accepted_name = ifelse(taxonomic_status == "accepted", accepted_name, NA_character_),
      taxon_ID = accepted_name_usage_ID, 
      accepted_name_usage_ID = ifelse(taxonomic_status == "accepted", accepted_name_usage_ID, NA_character_),
      ## for APC-accepted species, the `suggested_name` is the `accepted_name`
      suggested_name = accepted_name,
      ## if `taxonomic_splits` == "collapse_to_higher_taxon", the suggested_name, becomes the genus-level name created in this table
      suggested_name = ifelse(!is.na(suggested_collapsed_name), suggested_collapsed_name, suggested_name),
      ## these are occasionally taxa where the `accepted_name_usage_ID` links to a taxon that is "known" by APC, but doesn't have taxonomic_status = "accepted"
      ## for these taxa, the suggested name is the `canonical_name` associated with the particular `accepted_name_usage_ID`
      suggested_name = ifelse(is.na(suggested_name) & !is.na(taxon_ID), resources$`APC list (known names)`$canonical_name[match(taxon_ID,resources$`APC list (known names)`$accepted_name_usage_ID)], suggested_name),
      ## if there are no "accepted names" (or similar), the aligned name becomes the suggested name
      suggested_name = ifelse(is.na(suggested_name), aligned_name, suggested_name),
      taxonomic_status = ifelse(is.na(accepted_name),  taxonomic_status_aligned, "accepted"),
      # for APC-accepted species, the `genus` is the first word of the `accepted_name`
      genus_accepted = extract_genus(suggested_name),
      taxon_ID_genus = resources$genera_all$taxon_ID[match(genus_accepted, resources$genera_all$canonical_name)],
      update_reason =  taxonomic_status_aligned,
      taxonomic_dataset = "APC",
      ## there are rare cases of names within the APC that do not align to an accepted name.
      ## For these taxa, the `suggested_name` is the `aligned_name` and the family name must be added
      genus = ifelse(is.na(genus_accepted), genus, genus_accepted),
      family = ifelse(is.na(family), resources$APC$family[match(word(suggested_name, 1), resources$APC$genus)], family),
      update_reason = ifelse(
          (number_of_collapsed_taxa > 1) & !is.na(number_of_collapsed_taxa),
          "collapsed to genus due to ambiguity",
          update_reason
        )
    ) %>%
    ## next line just in case duplication snuck in - there are rare cases where one of the left_joins duplicates a row 
    dplyr::distinct(row_number, original_name, aligned_name, accepted_name, .keep_all = TRUE) %>%
    dplyr::select(original_name, aligned_name, suggested_name, accepted_name, accepted_name_2, 
                  taxonomic_status, taxonomic_status_aligned, taxon_rank, number_of_collapsed_taxa, dplyr::everything())
}

# Function to update names of taxa whose aligned_names are
# taxon_rank = species/infraspecific and taxonomic_dataset = APNI
update_taxonomy_APNI_species_and_infraspecific_taxa <- function(data, resources) {
  
  if(is.null(data)) return(NULL)

  data %>%
    dplyr::left_join(
      by = "aligned_name",
      resources$APNI %>%
        dplyr::filter(species_and_infraspecific(taxon_rank)) %>%
        dplyr::distinct(canonical_name, .keep_all = TRUE) %>%
        dplyr::mutate(
          aligned_name = canonical_name
        ) %>%
        # APNI names do not have a taxon_ID; therefore the only identifier is `scientific_name_ID`
        dplyr::select(
          aligned_name,
          canonical_name,
          scientific_name,
          scientific_name_ID,
          taxonomic_status,
          family,
          ccAttributionIRI
        )
    ) %>%
    ## These taxa will not have an `accepted_name` and therefore the `suggested_name` is the `aligned_name`
    dplyr::mutate(
      canonical_name = ifelse(is.na(scientific_name_ID), NA, aligned_name),
      accepted_name = NA_character_,
      suggested_name = ifelse(
        species_and_infraspecific(taxon_rank),
        aligned_name,
        suggested_name
      ),
      genus = word(suggested_name, 1)
    ) %>%
    # when possible the genus of APNI names is matched to an APC-accepted genus and the appropriate genus-level taxon_ID is added
    dplyr::left_join(
      by = "genus",
      resources$genera_all %>%
        dplyr::mutate(my_order = relevel_taxonomic_status_preferred_order(taxonomic_status)) %>%
        dplyr::arrange(canonical_name, my_order) %>%
        dplyr::distinct(canonical_name, .keep_all = TRUE) %>%
        dplyr::mutate(
          genus = canonical_name,
          accepted_name_usage_ID_genus = accepted_name_usage_ID,
          taxonomic_status_genus = taxonomic_status,
          taxonomic_dataset_genus = taxonomic_dataset
        ) %>%
        dplyr::select(
          genus,
          accepted_name_usage_ID_genus,
          taxonomic_status_genus,
          taxonomic_dataset_genus
        )
    ) %>%
    ## final manipulations to ensure APNI names that align with APC-accepted genus
    ## have proper columns filled in (`genus` & `taxonomic_ID_genus` & `taxonomic_dataset_genus`),
    ## while APNI names that do not align with an APC-accepted genus have these columns set to NA
    dplyr::mutate(
      genus = ifelse(
        is.na(accepted_name_usage_ID_genus), 
        genus, 
        resources$genera_all$canonical_name[match(accepted_name_usage_ID_genus, resources$genera_all$taxon_ID)]),
      taxon_ID_genus = resources$genera_all$taxon_ID[match(genus, resources$genera_all$canonical_name)],
      taxonomic_dataset_genus = ifelse(stringr::str_detect(taxonomic_dataset_genus, "APC"), "APC", taxonomic_dataset_genus)
    ) %>%
    dplyr::select(-accepted_name_usage_ID_genus)
  
}