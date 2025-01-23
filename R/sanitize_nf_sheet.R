sanitize_nf_sheet <- function(x, remove_na_cols = TRUE,
                              style = c('scri', 'nf-core' )) {
  # x: data.frame
  
  # 1. replace special character
  x <- x %>%
    dplyr::mutate(sample_id = str_replace_all(sample_id, "\\(", "_")) %>%  # Replace '(' with '_'
    dplyr::mutate(sample_id = str_replace_all(sample_id, "[^a-zA-Z0-9_-]", ""))  # Remove all other special characters
  
  # 2. delete columns without entries (NA)
  if (remove_na_cols) {
    x <- x %>%
      dplyr::select(where(~ !all(is.na(.))))
  }

  return(x)
}

x <- data.frame(sample_id = 'apple;11(orange)-kiwi_grapes',
                empty = NA)
y <- sanitize_nf_sheet(x)