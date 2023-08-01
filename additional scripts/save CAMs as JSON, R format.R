# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


if(!exists(x = "raw_CAM")){
  cat('\n
      please run script "01_load_everything" before running this script and load your dataset
      \n')
}


########################################
# create CAM files, draw CAMs
########################################
### create CAM single files (nodes, connectors, merged)
CAMfiles <- create_CAMfiles(datCAM = raw_CAM, reDeleted = TRUE)

### draw CAMs
CAMdrawn <- draw_CAM(dat_merged = CAMfiles[[3]],
                     dat_nodes = CAMfiles[[1]],ids_CAMs = "all",
                     plot_CAM = FALSE,
                     useCoordinates = TRUE,
                     relvertexsize = 3,
                     reledgesize = 1)

########################################
# save CAMs as JSON, R format
########################################
setwd("saved CAMs as JSON, R format")
if(length(list.files()) >= 1){
  cat('\n!
      all former files have been deleted')
  file.remove(list.files())
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

dev.off()

setwd("..")




### upload your json files to:
# https://camgalaxy.github.io/?ShowResearcherButtons=true&fullScreen=false
