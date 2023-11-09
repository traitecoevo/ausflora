


# why isn't there a base R function that does this?
##' @noRd
split_vector <- function(v, N = parallel::detectCores() - 1) {
  # Check for edge cases
  if (N <= 0)
    stop("N should be greater than 0")
  if (length(v) == 0)
    return(vector("list", N))
  
  # Calculate chunk size
  chunk_size <- floor(length(v) / N)
  
  # Compute number of chunks that get an extra element
  extras <- length(v) %% N
  
  chunks <- vector("list", N)
  start_idx <- 1
  
  for (i in 1:N) {
    end_idx <- start_idx + chunk_size - 1
    # Assign extra elements to the first few chunks
    if (i <= extras) {
      end_idx <- end_idx + 1
    }
    chunks[[i]] <- v[start_idx:end_idx]
    start_idx <- end_idx + 1
  }
  
  return(chunks)
}

##' @noRd
multicore_tax_update <-
  function(data_vec,
           cores = parallel::detectCores() - 1,
           resources = resources,
           taxonomic_splits = "most_likely_species") {
    # Split the vector into a list of individual elements
    data_list <- split_vector(data_vec, N = cores)
    
    results <-
      parallel::mclapply(data_list, function(x)
        create_taxonomic_update_lookup(x, resources = resources, taxonomic_splits =
                                         taxonomic_splits), mc.cores = cores)
    
    # Bind all results together into a single dataframe
    result_df <- do.call(rbind, results)
    
    return(result_df)
  }
