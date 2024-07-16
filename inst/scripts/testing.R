# testing samples
library(makeNFSampleSheet)

#
# 1. define path to FASTQ files
#

# if on Cybertron
path <- '/archive/sarthy_j/FASTQs/GBe6_M07e_WSU_frz_CnT_06213024/usftp21.novogene.com/01.RawData'

# if on local MAC
dir_header <- '///Volumes'
path <- file.path(dir_header, 'Archive',
                  'sarthy_j/FASTQs',
                  'GBe6_M07e_WSU_frz_CnT_06213024',
                  'usftp21.novogene.com/01.RawData')

# sanity check
file.exists(path)

#
# 2. testing get_fastq_path()
#
df <- get_fastq_path(path, pattern='\\.fq.gz$',
                     reads_pattern='_1|_2',
                     read1_pattern='_1')

path <-  '/archive/sarthy_j/FASTQs/240708_VH01189_307_AAFYY2YM5/Unaligned/Project_jsarthy'
df <- get_fastq_path(path, pattern='\\.fastq.gz$',
                     reads_pattern = '_R1|_R2',
                     read1_pattern = '_R1')


#
# 3. testing tidy_sample_sheet()
#

master_sheet <- file.path('/Users/cwo11/Projects/makeNFSampleSheet/inst/extdata',
                          'JFSe8_Free_CnR_Template.xlsx')
sheet = 1
extra_columns <- c('Ab', 'Seq_name')
cell_line = 'Cell line'
antibody = 'Condition'
treatment = NULL
sample = NULL
spike_in = FALSE
single_end = FALSE

tidy_sample_sheet(master_sheet,
                  sheet = 1,
                  unique_id = 'unique_id',
                  single_end = FALSE,
                  cell_line = 'Cell line',
                  antibody = 'Condition',
                  treatment = NULL,
                  sample = NULL,
                  extra_columns = NULL,
                  spike_in = FALSE)

