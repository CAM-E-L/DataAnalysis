########################################
# Script to compute network indicators
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
# compute network indicators
########################################
networkIndicators <- compute_indicatorsCAM(drawn_CAM = CAMdrawn,
                                micro_degree = NULL,
                                micro_valence = NULL,
                                micro_centr_clo = NULL,
                                micro_transitivity = NULL,
                                largestClique = FALSE)
dim(networkIndicators)
# head(networkIndicators)
networkIndicators$reciprocity_macro <- NULL

########################################
# get descriptives
########################################
getDescriptives(dataset = networkIndicators, nameAPAtable = NULL)

psych::cor.plot(r =networkIndicators[, unlist(lapply(networkIndicators, is.numeric))], xlas = 2)
