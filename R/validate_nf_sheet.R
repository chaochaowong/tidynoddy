#' validate_nf_sheet
#'
#' Validate the nextflow sample sheet for cutandrun pipeline to obtain
#' mandatory columns: sample_id, sample, single_end, target_or_control,
#' read1, and read2. In addition, validate the "sample" group to include at
#' least one target sample
#'
#' @param nf_sample_sheet A path to the nextflow cutandrun pipeline sample sheet
#'
#' @example
#' @importFrom readr read_csv
#' @export
#'

.check_file_exists <- function(filepath) {
  if (file.exists(filepath)) {
    message(paste("The file", filepath, "exists."))
    return(TRUE)
  } else {
    message(paste("The file", filepath, "does not exist."))
    return(FALSE)
  }
}

validate_nf_sheet <- function(nf_sample_sheet) {

  # check file existence
  sheet_exists <- .check_file_exists(nf_sample_sheet)
  stopifnot(sheet_exists)

  tb <- readr::read_csv(nf_sample_sheet)

  # check mandatory columns
  mandatory_columns <- c('sample_id', 'sample', 'single_end',
                         'target_or_control',
                         'read1', 'read2')

  tb <- read_csv(nf_sample_sheet)
  valid_columns <- (mandatory_columns %in% names(nf_sample_sheet))
  if (!valid_columns) {
    stop(paste("For a valid sample sheet for NextFlow cutandrun pipeline
               must have columns called:",
               paste(manatory_columns, collapse = ","), "."), call. = FALSE)
  }

  # check sample: each sample group must have a least one target
  num_of_targets <- tb %>%
    group_split(sample, .keep = TRUE) %>%
    setNames(group_keys(tb %>% group_by(sample))$sample) %>%
    map(function(x) {
      sum(x$target_or_control == 'target')
    })

  validate_sample <- unlist(num_of_targets) == 0
  if (any(validate_sample)) {
    which_sample <- paste(names(validate_sample)[validate_sample],
                          caollapse  = ',')
    stop(paste(which_sample, 'does not have targets.'), call.=FALSE)
  }

  # all pass
  message('cutandrun nextflow sample sheet validation: passed!')
  return(NULL)


}
