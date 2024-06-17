 #' Download taxonomic resources for GitHub Release
#'
#' @param version_name character string of version name, follow semantic versioning
#' @param path to download parquets to upload
#' @keywords internal
#' @noRd

download_taxonomic_resources_for_release<- function(version_name = NULL, path = "ignore/"){

# TODO: Use gh package to release programmatically
#   body <- paste0('{"tag_name":"',version_name,'","target_commitish":"master","name":"',version_name,'","body":"Download of taxonomic resources from APC and APNI as of ',Sys.Date(),'","draft":true,"prerelease":false,"generate_release_notes":false}') 
# 
# # Creating release via GH API
# gh::gh("POST /repos/{owner}/{repo}/releases",
#    owner = "traitecoevo", repo = "APCalign",
#    charToRaw(body),
#    .send_headers = c(
#      Accept = "application/vnd.github.switcheroo-preview+json",
#      "Content-Type" = "application/json"
#    )
#    )

# Download APC
 APC <-
   readr::read_csv(
          "https://biodiversity.org.au/nsl/services/export/taxonCsv",
          col_types =
            readr::cols(
              .default = readr::col_character(),
              proParte = readr::col_logical(),
              taxonRankSortOrder = readr::col_double(),
              created = readr::col_datetime(format = ""),
              modified = readr::col_datetime(format = "")
            )
   )

 # Save APC as parquet 
arrow::write_parquet(APC, sink = paste0(path,"apc.parquet"))
# Save APC as tar.gz
readr::write_csv(APC, file = paste0(path,"apc.tar.gz"))
          
# Download APNI
 APNI <-
   readr::read_csv(
     "https://biodiversity.org.au/nsl/services/export/namesCsv",
     col_types =
       readr::cols(
         .default = readr::col_character(),
         autonym = readr::col_logical(),
         hybrid = readr::col_logical(),
         cultivar = readr::col_logical(),
         formula = readr::col_logical(),
         scientific = readr::col_logical(),
         nomInval = readr::col_logical(),
         nomIlleg = readr::col_logical(),
         namePublishedInYear = readr::col_double(),
         taxonRankSortOrder = readr::col_double(),
         created = readr::col_datetime(format = ""),
         modified = readr::col_datetime(format = "")
       )
   )

# Exclude names that are in APC from APNI
 APNI_cleaned <- APNI |> 
   dplyr::filter(!canonicalName %in% APC$canonicalName)
 
# Save APNI as parquet
arrow::write_parquet(APNI_cleaned, sink = paste0(path,"apni.parquet"))

# Save APNI as tar.gz
readr::write_csv(APNI_cleaned, file = paste0(path,"apni.tar.gz"))

}

