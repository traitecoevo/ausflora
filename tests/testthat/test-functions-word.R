test_that("Word", {
  
  taxa <-
    c(
      NA,
      "Banksia integrifolia",
      "Acacia longifolia",
      "Commersonia rosea",
      "Thelymitra pauciflora",
      "Justicia procumbens",
      "Hibbertia",
      "Rostellularia long leaves",
      "Hibbertia sericea var  silliafolius",
      "Hibbertia sp.",
      "x Cynochloris macivorii",
      "(Dockrillia pugioniformis x Dockrillia striolata) x Dockrillia pugioniformis"
    )
  
  expect_equal(APCalign:::word(taxa, 1), stringr::word(taxa, 1))
  expect_equal(APCalign:::word(taxa, 2), stringr::word(taxa, 2))
  expect_equal(APCalign:::word(taxa, 3), stringr::word(taxa, 3))
  expect_equal(APCalign:::word(taxa, 1,2), stringr::word(taxa, 1,2))
  expect_equal(APCalign:::word(taxa, 1,3), stringr::word(taxa, 1,3))
})
