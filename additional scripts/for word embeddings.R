
########################### for word embeddings
# variables:
#> "personID", "wave", "group", "wordID", "word", "valence", "firstOrder", "comment"

# i = tmp_Indicators$participantCAM[1]


tmp_index <- 1
out_mat <- NULL
for(i in tmp_Indicators$participantCAM){

  tmp_mat <- matrix(data = NA, nrow = length(V(CAMdrawn[[i]])$name), ncol = 8)

  # personID
  tmp_mat[,1] <- i
  # wave
  tmp_mat[,2] <- str_split(string = i, pattern = "_", simplify = TRUE)[2]
  # group
  if(!is.na(str_split(string = i, pattern = "_", simplify = TRUE)[3])){
    tmp_mat[,3] <- str_split(string = i, pattern = "_", simplify = TRUE)[3]
  }else{
    tmp_mat[,3] <- "experimentalGroup" # wave
  }


  tmp_mat[,4] <- V(graph = CAMdrawn[[i]])$name

  # word
  tmp_mat[,5] <- V(graph = CAMdrawn[[i]])$label
  # valence
  tmp_mat[,6] <- V(graph = CAMdrawn[[i]])$value


  # firstOrder
  for(l in 1:length(V(graph = CAMdrawn[[i]])$label)){
    tmp_graph <- make_ego_graph(graph = CAMdrawn[[i]],
                                order = 1,
                                V(graph = CAMdrawn[[i]])$name[l])[[1]]
    tmp_nodes_neighborhood <- V(graph = tmp_graph)$label



    tmp_nodes_neighborhood <- tmp_nodes_neighborhood[
      tmp_nodes_neighborhood != V(graph = CAMdrawn[[i]])$label[l]]

    tmp_mat[l,7] <- paste0(tmp_nodes_neighborhood, collapse = " ## ")
  }



  # comment
  tmp_nodes <- CAMfiles[[1]][CAMfiles[[1]]$participantCAM == i, ]
  tmp_nodes <- tmp_nodes[match(V(graph = CAMdrawn[[i]])$label, tmp_nodes$text),] # reorder
  tmp_mat[,8] <- tmp_nodes$comment

  if(tmp_index == 1){
    out_mat <- tmp_mat
  }else{
    out_mat <- rbind(out_mat, tmp_mat)
  }

  tmp_index = tmp_index + 1

}
out_mat <- as.data.frame(out_mat)
colnames(out_mat) <- c("personID", "wave", "group", "wordID", "word", "valence", "firstOrder", "comment")
out_mat[out_mat == ""] <- NA
out_mat$valence <- as.numeric(out_mat$valence)
xlsx::write.xlsx2(x = out_mat, file = "wordEmbeddings.xlsx")
saveRDS(out_mat, file = "wordEmbeddings.rds")

#####################################################
