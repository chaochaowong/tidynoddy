#' tidy_sample_sheet
#'
#' Process a sample sheet, tidying and converting it
#' to a sample sheet format specifically for CUT&RUN/CUT&Tag NextFlow
#' pipeline.
#'
#' @param master_sheet A path to the master sample sheet file; Must be in either xlsx or csv format.
#' @param sheet Sheet to read
#' @param unique_id Either \code{NULL} or a character string specifying
#'        the column name in the input sample sheet
#'        that contains the \emph{unique_id} information. Note that \code{unique_id} and \code{sample_id} cannot be both NULL.
#' @param sample_id Either \code{NULL} or a character string specifying
#'        the column name in the input sample sheet that contains
#'        the \emph{sample_id} information. If NULL,
#'        default to the concatenation of \code{unique_id}, \code{cell_line},
#'        and \code{antibody}. Note that \code{unique_id} and \code{sample_id} cannot be both NULL.
#' @param antibody A character string specifying the column name
#'        in the input sample sheet that contains the \emph{antibody} information.
#' @param cell_line A character string specifying column name in the
#'        input sample sheet that contains of the \emph{cell_line} information.
#' @param single_end FALSE if the FASTQ is paired-end or TRUE is the FASTQ is single-ended.
#' @param sample A character string specifying the column name in the
#'        input sample sheet that contains of the \emph{sample}
#'        information. If NULL, default to the value in the
#'        \code{cell_line} column.
#' @param treatment A character string specifying the column name
#'        in the input sample sheet that contains of the
#'        \emph{treatment} information.
#' @details The cutandrun NextFlow core pipeline requires a sample sheet
#'          with six mandated columns: sample_id, sample, single_end,
#'          target_or_control, read1, and read2. Note that \code{target_or_control}
#'          is defined by \code{antiboyd}: if IgG, then "control", else "target".
#'          The \code{read1} and \code{read2} columns can be prepared
#'          by \code{get-fastq_path()} and be joined with the resulted
#'          \code{data.frame} of this function. In addition, the \code{sample} column
#'          groups the target and control samples; it is usually
#'          the same as \code{cell_line}. The extra \code{cell_line}
#'          and \code{antibody} columns are for CUNT&RUN-seq downstream
#'          analysis. \code{unique_id} and \code{sample_id} cannot be
#'          both NULL. If is is NULL, default to concatenation of
#'          \code{unique_id}, \code{cell_line}, and \code{antibody}.
#' @return A data frame formatted specifically to cutandrun NextFlow
#'         core pipeline sample sheet
#' @examples
#' \dontrun{
#' master_sheet <- file.path('/Users/cwo11/Projects/tidynoddy/inst/extdata',
#'                           'JFSe8_Free_CnR_Template.xlsx')
#' df <- tidy_sample_sheet(master_sheet,
#'                         sheet = 1,
#'                         unique_id = 'samp_name',
#'                         single_end = FALSE,
#'                         cell_line = 'Cell line',
#'                         antibody = 'Condition',
#'                         treatment = NULL,
#'                         sample = NULL)
#' # do some sanity check:
#' df
#'
#' # the following is to make a complete Nextflow-specific sample sheet for cutandrun pipeline:
#' # 1. use get_fastq_path() and define sample_id or unique id
#' path <- "/archive/sarthy_j/FASTQs/240708_VH01189_307_AAFYY2YM5/Unaligned/Project_jsarthy"
#' fq_df <- get_fastq_path(path, pattern = "\\.fastq.gz$",
#'                         reads_pattern = "_R1|_R2",
#'                         read1_pattern = "_R1") \%>\%
#'   dplyr::mutate(unique_id = str_split(fq_sample_name, '-',
#'                                       simplify=TURE)[, 4])
#' # 2. inner join the results of get_fastq_path() and tidy_sample_sheet() by either 'sample_id' or 'unique_id'
#'
#' nf_df <- df %>%
#'   inner_join(fq_df, by = 'unique_id')
#' nf
#' }
#'
#' @export
tidy_sample_sheet <- function(master_sheet,
                              sheet = 1,
                              unique_id = NULL,
                              sample_id = NULL,
                              antibody,
                              cell_line,
                              single_end = FALSE,
                              sample = NULL,
                              treatment = NULL) {
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

  #
  assertFILEDOESNTEXIST <- stopifnot(file.exists(master_sheet))
  if (!is.null(single_end) && !is.logical(single_end)) {
    stop("Error: 'single_end' must be a logical value.")
  }

  if (is.null(unique_id) && is.null(sample_id)) {
    # stop the function and print an error message
    stop("Error: both unique_id and sample_id cannot be NULL.")
  }

  require(readxl)
  require(readr)

  # 1. extension validation
  ext <- tools::file_ext(master_sheet)
  if (!stringr::str_detect(ext, 'xlsx|xls|csv'))
    stop('The format of the sample sheet must by xlsx, xls, or csv.')


  if (ext == 'xlsx')
    df <- readxl::read_xlsx(master_sheet, sheet=sheet)
  if (ext == 'xls')
    df <- readxl::read_xls(master_sheet, sheet=sheet)
  if (ext == 'csv')
    df <- readr::read_csv(master_sheet)

  # 2. sanity check: if not NULL, check if it exist
  # make .check_input_columns
  column_names <- names(df)
  input_columns <- c(unique_id = unique_id, sample_id = sample_id,
                     antibody = antibody, cell_line = cell_line,
                     treatment = treatment,
                     sample = sample)
  assert <- input_columns %in% column_names

  if (!all(assert)) {
    stop('The sample sheet does not contain column(s) for ',
         names(input_columns)[which(!assert)], ' named ',
         input_columns[which(!assert)]) }


  # tidy the sample sheet (df)
  # 1. process "unique_id" and "sample_id"
  if (is.null(sample_id) && !is.null(unique_id)) {
    df <- df %>%
      tidyr::drop_na(one_of(unique_id)) %>%
      dplyr::rename(unique_id = one_of(unique_id)) %>%
      dplyr::mutate(unique_id = str_trim(unique_id)) %>%
      dplyr::mutate(unique_id = str_replace_all(unique_id, ' ', '-'))
  }

  if (is.null(unique_id) && !is.null(sample_id)) {
    df <- df %>%
      tidyr::drop_na(one_of(sample_id)) %>%
      dplyr::rename(sample_id = one_of(sample_id)) %>%
      dplyr::mutate(sample_id = str_trim(sample_id)) %>%
      dplyr::mutate(sample_id = str_replace_all(sample_id, ' ', '-'))
  }

  if (!is.null(unique_id) && !is.null(sample_id)) {
    df <- df %>%
      tidyr::drop_na(one_of(sample_id)) %>%
      dplyr::rename(sample_id = one_of(sample_id),
                    unique_id = one_of(unique_id)) %>%
      dplyr::mutate(unique_id = str_trim(unique_id),
                    sample_id = str_trim(sample_id)) %>%
      dplyr::mutate(unique_id = str_replace_all(unique_id, ' ', '-'),
                    sample_id = str_replace_all(sample_id, ' ', '-'))
  }

  # tast: make sure there is no other
  # column names 'cell_line' and 'antibody'

  # tidy up "cell_line" and "antibody"
  # trim and replace space by '-'
  df <- df %>%
    dplyr::rename(antibody  = one_of(antibody),
                  cell_line = one_of(cell_line)) %>%
    dplyr::mutate(cell_line = str_trim(cell_line),
                  antibody  = str_trim(antibody)) %>%
    dplyr::mutate(cell_line = str_replace_all(cell_line, ' ', '-'),
                  antibody  = str_replace_all(antibody, ' ', '-'))

  # tidy up "treatment": trim and replace space by '-'
  if (!is.null(treatment)) {
    df <- df %>%
      dplyr::rename(treatment = one_of(treatment)) %>%
      dplyr::mutate(treatment = str_trim(treatment)) %>%
      dplyr::mutate(treatment = str_replace_all(treatment, ' ', '-'))
  }

  # tidy up "sample_id"
  # if not exist: patch cell_line, treatment (if not null), antibody
  if (is.null(sample_id)) {
    df <- df %>%
      dplyr::mutate(sample_id =
                      paste0(unique_id, '_', cell_line))
    if (!is.null(treatment)) {
      df <- df %>%
        dplyr::mutate(sample_id = paste0(sample_id, '_', treatment))
    }

    if (!is.null(antibody)) {
      df <- df %>%
        dplyr::mutate(sample_id = paste0(sample_id, '_', antibody))
    }
  }

  # tidy up "sample"
  if (is.null(sample))
    df$sample <-df$cell_line

  if (!is.null(sample)) {
    df <- df %>%
      dplyr::rename(sample = one_of(sample)) %>%
      dplyr::mutate(sample = str_trim(sample)) %>%
      dplyr::mutate(sample = str_replace_all(sample, ' ', '-'))
  }

  # tidy up "target_or_control" and "single_end"
  df <- df %>%
    dplyr::mutate(single_end = single_end) %>%
    dplyr::mutate(target_or_control =
                    if_else(antibody == 'IgG', 'control', 'target')) %>%
    dplyr::mutate(target_or_control =
                    if_else(is.na(target_or_control), 'target',
                            target_or_control))

  return(df)
}
