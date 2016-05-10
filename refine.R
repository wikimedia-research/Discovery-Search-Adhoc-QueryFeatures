if (file.exists(file.path(data_root, paste0("features-matrix_", dataset_to_use)))) {
  features_matrix <- readr::read_rds(file.path(data_root, paste0("features-matrix_", dataset_to_use)))
} else {
  # What we're interested in is turning a comma-separated list of features into a vector of 1s and 0s
  #   (or rather TRUEs and FALSEs) so that when we work with the queries' features, we would actually
  #   dealing with a boolean matrix.
  library(magrittr)
  library(progress)
  pb <- progress_bar$new(total = nrow(queries))
  features_matrix <- queries$features %>%
    # head(200) %>%
    strsplit(", ") %>%
    lapply(function(feats) {
      x <- rep(TRUE, length(feats))
      names(x) <- feats
      pb$tick()
      return(as.data.frame(t(x)))
    }) %>%
    do.call(dplyr::bind_rows, .) %>%
    lapply(function(column) {
      return(replace(column, is.na(column), FALSE))
    }) %>%
    dplyr::as_data_frame()
  rm(pb)
  # Quick check to see that everything went as it should have:
  if (sum(rowSums(features_matrix) != queries$n_feats) == 0) {
    readr::write_rds(features_matrix, file.path(data_root, paste0("features-matrix_", dataset_to_use)), "gz")
  } else {
    message("Row sums did not match number of features!")
  }
}

# dim(features_matrix) = 5,740,254 queries x 25 possible features
