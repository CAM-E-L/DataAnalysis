########################################
# Script to get synonyms of drawn concepts
#> Remarks:
########################################

# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


if(!exists(x = "CAMfiles") && !exists(x = "CAMdrawn")){
  cat('\n
      please run script "aa_start_load_draw" before running this script
      \n')
}


########################################
# load internal data set
########################################
### synonyms data set(s)
setwd("data/internal data sets")
dir()
syn_English <- xlsx::read.xlsx2(file = "syn_dat_English.xlsx", sheetIndex = 1)
setwd("../..")


########################################
# get synonyms
########################################
SynonymList_out <- SynonymList(vectorWords = CAMfiles[[1]]$text, syn_dat = syn_English)
### percentage found
SynonymList_out[[2]]
### synonyms found
SynonymList_out[[1]]
