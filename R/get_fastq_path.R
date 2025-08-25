#' get_fastq_path
#'
#' Construct a data.frame with three columns:sequence name, read 1 and
#' read 2 fastq file paths, given a fastq path for paired-end reads
#'
#' @param path A path to directory of FASTQ
#' @param ext_pattern A character string indicating the FASTQ extension pattern, e.g. '\\.fq\\.gz$' or '\\.fastq\\.gz'
#' @param reads_pattern A character string indicating the forward and reverse read files naming convention, e.g. '_R1|_R2' or '_1|_2'
#' @return A data.frame of three columns, \emph{sample_id}, \emph{fastq_1}, and \emph{fastq_2} 
#' @details The first column \code{sample_id} is set by trimming the paramters \code{ext_pattern} and \code{reads_pattern} from the FASTQ files. The second and third columns are the location of the forward (fastq_1) and reverse (fastq_2) FASTQ files.
#' @export
#' @importFrom tidyr pivot_wider
#' @examples
#' \dontrun{
#' # Novogene pattern
#' path <- "/archive/sarthy_j/FASTQs/GBe6_M07e_WSU_frz_CnT_06213024/usftp21.novogene.com/01.RawData"
#' df <- get_fastq_path(path, 
#'                      ext_pattern = "\\.fq\\.gz$",
#'                      reads_pattern = "_1|_2")
#'
#' # Regular pattern
#' path <- "/archive/sarthy_j/FASTQs/240708_VH01189_307_AAFYY2YM5/Unaligned/Project_jsarthy"
#' df <- get_fastq_path(path, 
#'                      ext_pattern = "\\.fastq\\.gz$",
#'                      reads_pattern = "_R1|_R2")
#' }
#'
get_fastq_path <- function(path, 
                           ext_pattern = "\\.fastq\\.gz$",
                           reads_pattern = "_R1|_R2") {
  # check if path exists
  if (!file.exists(path))
    stop(path, ' does not exist.')

  require(tidyr)

  # list files
  fq_df <- list.files(path,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE
  ) %>%
    tidyr::as_tibble() %>%
    dplyr::rename(fq_file = "value")

  # if number of row is zero
  if (nrow(fq_df) < 1) {
    stop(path, " does not have fastq (fq) or fastq.gz (fq.gz) files.")
  }

  trim_pattern <- paste0("(", reads_pattern, ").*")

  #
  # clean up if the file size is too small
  #
  fq_df <- fq_df %>%
    dplyr::filter(str_detect(fq_file, reads_pattern)) %>%
    dplyr::mutate(
      base_name = basename(fq_file),
      file_size = file.size(fq_file)
    ) %>% # only retain sample with reasonable size
    dplyr::filter(file_size > 1e5) 
  
  #
  # future cleaning
  #
  
  # Build patterns from the variables
  read_token_re <- paste0("(", reads_pattern, ")", ext_pattern)
  strip_re      <- paste0("(?:", reads_pattern, ")", ext_pattern) # remove final read token + ext
  
  
  fq_df %>%
    dplyr::mutate(
      read_token = str_match(base_name, read_token_re)[, 2],
      reads     = str_replace(read_token, "^_(?:R)?", "fastq_"),    # "R1"/"R2"
      sample_id  = str_replace(base_name, strip_re, "")        # e.g., "JR_1"
    ) %>%
    dplyr::select(fq_file, sample_id, reads) %>%
    tidyr::pivot_wider(names_from = reads, values_from = fq_file)
}
