# hack to enable faster load times for tests
if(exists("resources", envir = globalenv())) {
  resources <- get("resources", envir = globalenv())
} else {
  resources <- load_taxonomic_resources(stable_or_current_data = "stable", version = "0.0.2.9000")
}
