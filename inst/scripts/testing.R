# testing samples
# install tidynoddy
# library(devtools)
# install_github('chaochaowong/tidynoddy')
library(tidynoddy)
library(dplyr)

#
# 1. define path to FASTQ files
#

# if on Cybertron
path <- '/archive/sarthy_j/FASTQs/GBe6_M07e_WSU-AML_fresh-v-frozen_novogene/usftp21.novogene.com/01.RawData'

# if on local MAC
dir_header <- '///Volumes'
path <- file.path(dir_header, 'Archive',
                  'sarthy_j/FASTQs',
                  'GBe6_M07e_WSU-AML_fresh-v-frozen_novogene',
                  'usftp21.novogene.com/01.RawData')

# sanity check
file.exists(path)

#
# 2. testing get_fastq_path()
#

# if novogene
df_GBe6 <- get_fastq_path(path, pattern='\\.fq.gz$',
                          reads_pattern='_1|_2',
                          read1_pattern='_1')
# Gabe's sample_id is self-explanatory such that we don't need a sample
# sheet to make the nextflow sample sheet
df_GBe6 <- df_GBe6 %>%
  dplyr::mutate(read1 = str_replace(read1, '///Volumes/Archive', '/active'),
                read2 = str_replace(read2, '///Volumes/Archive', '/active')) %>%
  dplyr::mutate(sample_id =
                  str_replace(fq_sample_name, '_CKDL.*', ''),
                cell_line =
                  str_split(fq_sample_name, '_', simplify=TRUE)[, 2],
                antibody  =
                  str_split(fq_sample_name, '_', simplify=TRUE)[, 4],
                .before='fq_sample_name') %>%
  dplyr::mutate(sample = cell_line,
                target_or_control = 'target',
                single_end = FALSE,
                .before = 'fq_sample_name')

# if regular: this df will need a sample sheet to make the NF sample sheet
path <-  '/archive/sarthy_j/FASTQs/240708_VH01189_307_AAFYY2YM5/Unaligned/Project_jsarthy'
df <- get_fastq_path(path, pattern='\\.fastq.gz$',
                     reads_pattern = '_R1|_R2',
                     read1_pattern = '_R1')


#
# 3. testing tidy_sample_sheet()
#

master_sheet <- file.path('/Users/cwo11/Projects/tidynoddy/inst/extdata',
                          'JFSe8_Free_CnR_Template.xlsx')
sheet = 1
unique_id = 'unique_id'
extra_columns <- c('Ab', 'Seq_name')
cell_line = 'Cell line'
antibody = 'Condition'
treatment = NULL
sample = NULL
spike_in = FALSE
single_end = FALSE
sample_id = NULL

tidy_sample_sheet(master_sheet,
                  sheet = 1,
                  unique_id = 'samp_name',
                  single_end = FALSE,
                  cell_line = 'Cell line',
                  antibody = 'Condition',
                  treatment = NULL,
                  sample = NULL)

# GBe6

