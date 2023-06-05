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


########################################
# create aggregated CAM
########################################
### aggregated CAM
sel_ids <- unique(CAMfiles[[1]]$participantCAM)
CAMaggregated <- aggregate_CAMs(dat_merged = CAMfiles[[3]], dat_nodes = CAMfiles[[1]],
                                ids_CAMs = sel_ids)

plot(CAMaggregated[[2]], vertex.size=diag(CAMaggregated[[1]]) / max(diag(CAMaggregated[[1]]))*20, edge.arrow.size=0.01)
plot(CAMaggregated[[2]], vertex.size=(abs(V(CAMaggregated[[2]])$value)+1)*5, edge.arrow.size=0.01)


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

