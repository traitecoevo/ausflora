#' For Australian states and territories, use data from the APC to calculate state-level diversity for native, introduced, and more complicated species origins
#'
#' This function calculates state-level diversity for native, introduced, and more complicated species origins 
#' based on the geographic data available in the APC.
#'
#' @family diversity methods
#' @param state A character string indicating the Australian state or territory to calculate the diversity for. Possible values are "NSW", "NT", "Qld", "WA", "ChI", "SA", "Vic", "Tas", "ACT", "NI", "LHI", "MI", "HI", "MDI", "CoI", "CSI", and "AR".
#' @param resources the taxonomic resources required to make the summary statistics.  loading this can be slow, so call load_taxonomic_resources separately to greatly speed this function up and pass the resources in.
#'
#' @return A tibble of diversity counts for the specified state or territory, including native, introduced, and more complicated species origins.
#' The tibble has three columns: "origin" indicating the origin of the species, "state" indicating the Australian state or territory, and "num_species" indicating the number of species for that origin and state.
#'
#' @seealso \code{\link{load_taxonomic_resources}}
#'
#' @export
#'
#' @examples
#'  \donttest{state_diversity_counts(state = "NSW")}
state_diversity_counts <- function(state,
                                   resources = load_taxonomic_resources()) {
  valid_inputs <- c(
    "NSW",
    "NT",
    "Qld",
    "WA",
    "ChI",
    "NSW",
    "SA",
    "Vic",
    "Tas",
    "ACT",
    "NI",
    "LHI",
    "MI",
    "HI",
    "MDI",
    "CoI",
    "CSI",
    "AR",
    "CaI"
  )
  if (!(state %in% valid_inputs)) {
    stop(paste(
      "Invalid str_input:",
      state,
      ". Expected one of:",
      paste(valid_inputs, collapse = ", ")
    ))
  }
  test <-
    create_species_state_origin_matrix(resources = resources)
  test2 <- test[test[[state]] != "not present", ]
  state_table <- table(test2[[state]])
  return(tibble(
    origin = names(state_table),
    state = state,
    num_species = state_table
  ))
}



#' @noRd
get_apc_genus_family_lookup <-
  function(resources = load_taxonomic_resources()) {
    apc_s <- filter(resources$APC,
                    taxon_rank == "species")
    tibble(genus = word(apc_s$scientific_name, 1, 1),
           family = apc_s$family) %>%
      distinct() -> lu
    return(lu)
  }


