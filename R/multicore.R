


# why isn't there a base R function that does this?
##' @noRd
split_vector <- function(v, N = parallel::detectCores() - 1) {
  # Check for valid number of chunks
  if (N <= 0)
    stop("N should be greater than 0")
  
  # Return empty list if vector is empty
  if (length(v) == 0)
    return(vector("list", N))
  
  # Calculate basic chunk size and number of extras
  chunk_size <- floor(length(v) / N)
  extras <- length(v) %% N
  
  # Create start and end indices for each chunk
  starts <- c(1, rep(NA, N-1))
  for(i in 2:N){
    starts[i] <- starts[i-1] + chunk_size + ifelse(i-1 <= extras, 1, 0)
  }
  ends <- starts + chunk_size + (1:N <= extras) - 1
  
  # Adjust last chunk to not exceed the length of v
  ends[N] <- min(ends[N], length(v))
  
  # Extract chunks
  chunks <- mapply(function(start, end) v[start:end], starts, ends, SIMPLIFY = FALSE)
  
  return(chunks)
}

##' Perform parallel taxonomic updates on a data vector.
##'
##' This function takes a vector of taxonomic data and processes it in parallel,
##' using multiple cores. It splits the input vector into chunks, one for each core,
##' and applies a taxonomic update lookup to each chunk. The results are then
##' combined into a single dataframe. This function is useful for efficiently
##' processing large datasets by utilizing multicore processing.
##'
##' @param data_vec A vector containing taxonomic data that needs updating.
##' @param cores Optional; the number of processor cores to use for parallel processing.
##'              Defaults to the number of detected cores minus one.
##' @param resources A list or environment containing data or functions required for
##'                  updating the taxonomic data. This could include databases, APIs,
##'                  or local files necessary for the function to execute.
##' @param taxonomic_splits Optional; a character string indicating the method or criteria
##'                         for splitting taxonomic data into more specific categories.
##'                         Defaults to "most_likely_species".
##'
##' @return A dataframe that consolidates the results of the taxonomic updates from each chunk.
##'         Each row in the dataframe corresponds to an updated record of the input vector.
##'
multicore_tax_update <-
  function(data_vec,
           cores = parallel::detectCores() - 1,
           name_resources = load_taxonomic_resources(),
           taxonomic_splits = "most_likely_species") {
    # Split the vector into a list of individual elements
    data_list <- split_vector(v=data_vec, N = cores)
    
    results <-
      parallel::mclapply(data_list, function(x)
        create_taxonomic_update_lookup(x, resources = name_resources, taxonomic_splits =
                                         taxonomic_splits), mc.cores = cores)
    
    # Bind all results together into a single dataframe
    result_df <- do.call(rbind, results)
    
    return(result_df)
  }
