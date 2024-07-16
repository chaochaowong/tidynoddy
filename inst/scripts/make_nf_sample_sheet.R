make_nextflow_sample_sheet <- function(master_sheet, fq_dir, 
                                       sample_id, 
                                       sample=NULL,
                                       single_end = FALSE,
                                       antibody = NULL,
                                       cell_line = NULL,
                                       treatment = NULL,
                                       spike_in = NULL,
                                       file_name = './nf-sample-sheet.csv') {
  # Master_sheet 
  assertFILEDOESNTEXIST <- stopifnot(file.exists(master_sheet))
  # make nextflow sample sheet: sample_id, sample, single_end, 
  # target_or_control (determined by antibody), read1 and read2


  library(tidyverse)
  library(readxl)
  
  # sanity check
  ext <- tools::file_ext(master_sheet)
    if (ext == 'xlsx')
      df <- readxl::read_xlsx(master_sheet, sheet=1) 
    if (ext == 'csv') 
      df <- readr::read_csv(master_sheet)

  # sanity check: if not NULL, check if it exist
  column_names <- names(df)  
  assert <- all(c(sample_id, antibody, cell_line, treatment) %in% column_names)

  df <- df %>% 
      tidyr::drop_na(one_of(sample_id)) %>% 
      dplyr::rename(sample_id = one_of(sample_id),
                    antibody = one_of(antibody),
                    cell_line = one_of(cell_line),
                    treatment = one_of(treatment)) %>% 
      dplyr::mutate(single_end = single_end,
                    sample = ifelse(is.null(sample), sample_id, sample),
                    target_or_control = if_else(antibody == 'IgG', 'control', 'target')) %>%
      dplyr::select(sample_id, sample, single_end, target_or_control,
                    antibody, cell_line, treatment)                    

  #
  # data.frame for fq
  #
  fq_files <- list.files(fq_dir, pattern="fastq.gz|fastq",
                         recursive = TRUE, 
                         full.names = TRUE)

  if (length(fq_files) < 1)
    stop(fq_dir, 'does not have fastq or fastq.gz files.')                                

  # sample_id = Name_Sample_Ab
  fq_df <-  
    tibble(fq_file = fq_files) %>%
    dplyr::mutate(base_name = basename(fq_file),
                  file_size = file.size(fq_file)) %>%
    dplyr::filter(file_size > 1e5) %>%   
    dplyr::mutate(sample_id = str_split(base_name, "_S[0-9]", simplify=TRUE)[, 1]) %>%
    dplyr::mutate(Read = if_else(grepl("R1", base_name), "read1", "read2")) %>%
    dplyr::select(-base_name, -file_size) %>%
    tidyr::spread(Read, fq_file) %>% 
    tidyr::drop_na(read1, read2)
  # double check the size of the fq
  nf_df <- df %>%
    dplyr::inner_join(fq_df, by='sample_id') 

  message(paste0('Nextflow sample sheet is deposit to \n- CBF-GLIS manuscript: plot heatmap for JFS1740 and 1741 (CG_data) on SK_M07e_CBF2T3xxx (from GB_SK_MW_pilot_20240222)
    - Which peak sets are we suing for SK M07e CBF2T3?', file_name))

  write_csv(nf_df, file=file_name)

}

