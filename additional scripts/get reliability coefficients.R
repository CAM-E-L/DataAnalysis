# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


if(!exists(x = "raw_CAM")){
  cat('\n
      please run script "01_load_everything" before running this script and load your dataset
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
