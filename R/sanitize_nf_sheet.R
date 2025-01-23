sanitize_nf_sheet <- function(x, style = c('scri', 'nf-core' )) {
  # x: df or a sheet
  
  # check if there is special 
  x  <- x %>%
    dplyr::mutate(sample_id = case_when(
      str_detect(sample_id, '\\(|\\)') ~ str_replace_all(sample_id, '\\(|\\)', ''),
      str_detect(sample_id, ';') ~ str_replace(sample_id, ';', ''),
      .default = sample_id)
    )


}