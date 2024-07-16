#' get_fastq_path
#'
#' construct a data.frame with three columns containing sequence name, read 1 and read 2 fastq files
#'
#' @param path A path to directory of FASTQ
#' @param pattern A character string indicating the FASTQ extension pattern
#' @param reads_pattern A character string indicating the read 1 and 2 files naming convention
#' @param reads_pattern A character string indicating the read 1 and 2 files naming convention
#' @param read1_pattern A character string indicating the read 1 files naming convention
#' @return A data.frame of three columns,  of the sequence sample name, read 1 and read 2 FASTQ file paths
#' @details The first column \code{fq_sample_name} is set by trimming the paramters \code{pattern} and \code{reads_pattern} from the FASTQ files. The second and third columns are the location of the forward (read1) and reverse (read2) FASTQ files.
#' @export
#' @examples
#' # Novogene pattern
#' path <- "/archive/sarthy_j/FASTQs/GBe6_M07e_WSU_frz_CnT_06213024/usftp21.novogene.com/01.RawData"
#' df <- get_fastq_path(path,
#'   pattern = "\\.fq.gz$",
#'   reads_pattern = "_1|_2",
#'   read1_pattern = "_1"
#' )
#'
#' # Regular pattern
#' path <- "/archive/sarthy_j/FASTQs/240708_VH01189_307_AAFYY2YM5/Unaligned/Project_jsarthy"
#' df <- get_fastq_path(path,
#'   pattern = "\\.fastq.gz$",
#'   reads_pattern = "_R1|_R2",
#'   read1_pattern = "_R1"
#' )
#'
get_fastq_path <- function(path, pattern = "\\.fastq.gz$",
                           reads_pattern = "_R1|_R2",
                           read1_pattern = "_R1") {
  # this generate a data.frame with three colums corresponding to
  # the fastq names, read1 and read2
  library(dplyr)
  library(readxl)
  library(stringr)
  library(tidyr)

  fq_df <- list.files(path,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE
  ) %>%
    tidyr::as_tibble() %>%
    dplyr::rename(fq_file = "value")

  # if number of row is zero
  if (nrow(fq_df) < 1) {
    stop(path, "does not have fastq or fastq.gz files.")
  }

  trim_pattern <- paste0("(", reads_pattern, ").*")

  fq_df <- fq_df %>%
    dplyr::filter(str_detect(fq_file, reads_pattern)) %>%
    dplyr::mutate(
      base_name = basename(fq_file),
      file_size = file.size(fq_file)
    ) %>% # only retain sample with reasonable size
    dplyr::filter(file_size > 1e5) %>%
    dplyr::mutate(reads = if_else(
      str_detect(base_name, read1_pattern),
      "read1", "read2"
    )) %>%
    dplyr::mutate(
      fq_sample_name =
        str_replace(base_name, trim_pattern, "")
    ) %>%
    dplyr::select(-base_name, -file_size) %>%
    tidyr::spread(reads, fq_file)
}
