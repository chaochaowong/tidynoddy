# texting tools
path <- '/archive/sarthy_j/FASTQs/GBe6_M07e_WSU_frz_CnT_06213024/usftp21.novogene.com/01.RawData'
df <- get_fastq_path(path, pattern='\\.fq.gz$', 
                     reads_pattern='_1|_2',
                     read1_pattern='_1')

path <-  '/archive/sarthy_j/FASTQs/240708_VH01189_307_AAFYY2YM5/Unaligned/Project_jsarthy'
df <- get_fastq_path(path, pattern='\\.fastq.gz$',
                     reads_pattern = '_R1|_R2',
                     read1_pattern = '_R1')                    
# conform sample sheet to nextflow sample sheet format to include

sample_sheet <- '/active/sarthy_j/Sarthy/Sarthy_Lab/CUTRUN_Records/JFSe8_CBF-Cord_WSU_M07e_JIH5_K562_CnR_06252024/data/JFSe8_Free_CnR_Template.xlsx'                     
