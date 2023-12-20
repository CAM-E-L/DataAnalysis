########################################
# Script to aggregate CAMs
#> Remarks: run the script multiple times till you happy with the generated layout of your aggregated CAM
########################################

# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


if(!exists(x = "CAMfiles") && !exists(x = "CAMdrawn")){
  cat('\n
      please run script "aa_start_load_draw" before running this script
      \n')
}


########################################
# create aggregated CAM
########################################
### aggregated CAM
if(all(CAMfiles[[1]]$participantCAM == "noID")){
  sel_ids <- unique(CAMfiles[[1]]$CAM)
}else{
  sel_ids <- unique(CAMfiles[[1]]$participantCAM)
}

if(length(sel_ids) > 6){
  sel_ids <- sample(x = sel_ids, size = 6, replace = FALSE)
}


CAMaggregated <- aggregate_CAMs(dat_merged = CAMfiles[[3]], dat_nodes = CAMfiles[[1]],
                                ids_CAMs = sel_ids)

# plot(CAMaggregated[[2]], vertex.size=diag(CAMaggregated[[1]]) / max(diag(CAMaggregated[[1]]))*20, edge.arrow.size=0.01)
# plot(CAMaggregated[[2]], vertex.size=(abs(V(CAMaggregated[[2]])$value)+1)*5, edge.arrow.size=0.01)


g = CAMaggregated[[2]]
g2 = simplify(CAMaggregated[[2]])
# plot(g2, edge.arrow.size=0.01,
#      vertex.size=diag(CAMaggregated[[1]]) / max(diag(CAMaggregated[[1]]))*20)

E(g2)$weight = sapply(E(g2), function(e) {
  length(all_shortest_paths(g, from=ends(g2, e)[1], to=ends(g2, e)[2])$res) } )
E(g2)$weight = E(g2)$weight / 2

V(g2)$color[V(g2)$value <= .5 & V(g2)$value >= -.5] <- "yellow"

V(g2)$shape <- NA
V(g2)$shape <- ifelse(test = V(g2)$color == "yellow", yes = "square", no = "circle")



plot(g2, edge.arrow.size = .5,
     layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = -0.1,
     vertex.size = 5, vertex.label.cex = .9)



########################################
# create word list
########################################
selectedIDs <- sample(x = names(CAMdrawn), size = 3, replace = FALSE)


if(all(selectedIDs %in% CAMfiles[[1]]$CAM)){
  sel_nodes <- CAMfiles[[1]][CAMfiles[[1]]$CAM %in% selectedIDs, ]
  sel_merged <- CAMfiles[[3]][CAMfiles[[3]]$CAM.x %in% selectedIDs, ]
}else{
  sel_nodes <- CAMfiles[[1]][CAMfiles[[1]]$participantCAM %in% selectedIDs, ]
  sel_merged <- CAMfiles[[3]][CAMfiles[[3]]$participantCAM.x %in% selectedIDs, ]
}


CAMwordlist <- create_wordlist(
  dat_nodes = sel_nodes,
  dat_merged = sel_merged,
  useSummarized = FALSE,
  order = "frequency",
  splitByValence = FALSE,
  comments = FALSE,
  raterSubsetWords = NULL,
  rater = FALSE
)



CAMwordlist
