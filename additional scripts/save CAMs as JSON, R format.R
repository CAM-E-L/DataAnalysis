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
# save CAMs as JSON, R format
########################################
setwd("output/saved CAMs as JSON, R format")
if(length(list.files()) >= 1){
  file.remove(list.files())
  cat('\n!
      all former files have been deleted')
}


if(all(CAMfiles[[3]]$participantCAM.x == "noID")){
  CAMfiles[[3]]$participantCAM.x <- CAMfiles[[3]]$CAM.x
}



ids_CAMs <- unique(CAMfiles[[3]]$participantCAM.x); length(ids_CAMs)

for(i in 1:length(ids_CAMs)){
  save_graphic(filename = paste0(ids_CAMs[i]))
  CAM_igraph <- CAMdrawn[[c(1:length(CAMdrawn))[
    names(CAMdrawn) == paste0(unique(CAMfiles[[3]]$participantCAM.x)[i])]]]
  plot(CAM_igraph, edge.arrow.size = .7,
       layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = -0.1,
       vertex.size = 10, vertex.label.cex = .9)
  dev.off()
}

for(i in 1:length(raw_CAM)){
  if(!is_empty(raw_CAM[[i]]$nodes)){
    if(nrow(raw_CAM[[i]]$nodes) > 5){
      write(toJSON(raw_CAM[[i]], encoding = "UTF-8"),
            paste0(raw_CAM[[i]]$idCAM, ".json"))
    }
  }
}


setwd("../..")
