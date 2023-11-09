
test_that("state_diversity() works", {
  if(is.null(resources)) skip("Server down.")
  nsw_species_counts <-
    state_diversity_counts(state = "NSW", resources = resources)
  expect_true(
    sum(nsw_species_counts$num_species) > 7000 &
      sum(nsw_species_counts$num_species) < 10000
  )
  expect_error(state_diversity_counts(state = "NOTASTATE", resources = resources))
  ss <- create_species_state_origin_matrix(resources = resources)
  sd <- readr::read_csv("benchmarks/state_diversity.csv", show_col_types = FALSE)
  ss_subset <- filter(ss, ss$species %in% sd$species)
  expect_equal(ss_subset, sd)
})


test_that("native_anywhere_in_australia() works", {
  if(is.null(resources)) skip("Server down.")
  expect_warning(native_check <-
    native_anywhere_in_australia(
      c(
        "Eucalyptus globulus",
        "Pinus radiata",
        "Brassica rapa",
        "banksis notaspecies"
      ),
      resources = resources
    ))
  # readr::write_csv(native_check,"tests/testthat/benchmarks/native_check.csv")
  previous_check <- readr::read_csv("benchmarks/native_check.csv", show_col_types = FALSE)
  expect_equal(native_check, previous_check)
  expect_warning(native_anywhere_in_australia(species = "NOTASPECIES", resources = resources))
})
