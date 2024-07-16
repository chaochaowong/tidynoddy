tidy_sample_sheet <- function(master_sheet,
                              sheet = 1,
                              unique_id,
                              antibody,
                              cell_line,
                              single_end = FALSE,
                              spike_in = FALSE,
                              sample_id = NULL,
                              sample = NULL,
                              treatment = NULL,
                              extra_columns = NULL) {
  # yield sample sheet that only matters to NF sample sheet and downstream
  # analysis
  # about parameter
  # sheet (sheet's number of name, pass to readxl::read_xlsx(): default to first sheet
  #
  # if unique_id is given, then sample_id will be unique_id with
  # cell_line, antibody, treatment (if not NULL) and
  # extra_columns (if not NULL) appending to it
  #
  # sample: is basically the cell_line based variable; it is used to
  #.        group targets and controls

  assertFILEDOESNTEXIST <- stopifnot(file.exists(master_sheet))

  require(readxl)
  require(readr)

  ext <- tools::file_ext(master_sheet)
  if (ext == 'xlsx')
    df <- readxl::read_xlsx(master_sheet, sheet=sheet)
  if (ext == 'csv')
    df <- readr::read_csv(master_sheet)

  # sanity check: if not NULL, check if it exist
  column_names <- names(df)
  assert <- all(c(unique_id, sample_id, antibody, cell_line, treatment,
                  sample, extra_columns) %in% column_names)


}
