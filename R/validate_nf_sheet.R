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


validate_nf_sheet <- function(nf_sample_sheet) {

  is_vector_unique <- function(vec) {
    length(vec) == length(unique(vec))
  }

  is_file_exists <- function(filepath) {
    if (file.exists(filepath)) {
      message(paste("The file", filepath, "exists."))
      return(TRUE)
    } else {
      message(paste("The file", filepath, "does not exist."))
      return(FALSE)
    }
  }
  # 1. check file existence
  sheet_exists <- is_file_exists(nf_sample_sheet)
  stopifnot(sheet_exists)

  tb <- readr::read_csv(nf_sample_sheet)

  # 2. check mandatory columns
  mandatory_columns <- c('sample_id', 'sample', 'single_end',
                         'target_or_control',
                         'read1', 'read2')

  tb <- read_csv(nf_sample_sheet)
  assert <- (mandatory_columns %in% names(nf_sample_sheet))
  if (!assert) {
    stop(paste("For a valid sample sheet for NextFlow cutandrun pipeline
               must have columns called:",
               paste(manatory_columns, collapse = ","), "."), call. = FALSE)
  }

  # 3. unique sample_id
  assert <- is_vector_unique(tb$sample_id)
  if (!assert)
    stop('Vales in the sample_id column must be unique.')

  # 4. check sample: each sample group must have a least one target
  num_of_targets <- tb %>%
    group_split(sample, .keep = TRUE) %>%
    setNames(group_keys(tb %>% group_by(sample))$sample) %>%
    map(function(x) {
      sum(x$target_or_control == 'target')
    })

  invalidate_group <- unlist(num_of_targets) == 0
  if (any(invalidate_group)) {
    which_sample <- paste(names(invalidate_group)[invalidate_group],
                          caollapse  = ',')
    stop(paste(which_sample, 'does not have targets.'), call.=FALSE)
  }

  # all pass
  message('cutandrun nextflow sample sheet validation: passed!')
  return(NULL)


}
