########################################
# Script to save CAMs as .json and .png (using igraph and Cairo package)
#> Remarks: you can upload your .json files of your CAM to
#> https://camgalaxy.github.io/?ShowResearcherButtons=true&fullScreen=false
########################################

# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


if(!exists(x = "CAMfiles") && !exists(x = "CAMdrawn")){
  cat('\n
      please run script "aa_start_load_draw" before running this script
      \n')
}

########################################
# load rater data
########################################
setwd("data/rater")
dir()

files <- list()
for(i in 1:length(dir())){
  # print(dir()[i])
  files[[i]] <- xlsx::read.xlsx2(file = dir()[i], sheetIndex = 1)
}


########################################
# compute inter-rater coefficients
########################################
cohensKappas <- computeCohensKappa(files = files,
                                   numberRaters = length(files))
cohensKappas



cohensKappasMaximized <- computeCohensKappaMaximized(files =files,
                                                     numberRaters = length(files))
cohensKappasMaximized
