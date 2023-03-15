# ==============================================================================
# R-Code - CAM
# date of creation: March 2023
# authors: Julius Fenn
# function:
# ==============================================================================

############################################################################
# create_CAMfiles()
#
############################################################################
### args
# i <- 3
# singleCAM = drawn_CAM[[i]]
# singleCAMid =  names(drawn_CAM)[i]
# removeConnection = c("Eigener Pkw", "Öffentliche Verkehrsmittel")
# removeNode = NULL
# plot = TRUE
# verbose = FALSE


sliceCAM <- function(singleCAM = NULL, singleCAMid = NULL,
                     removeConnection = NULL, removeNode = NULL, plot = FALSE, verbose = FALSE){
  
  
  ## check of given arguments
  if(!is.null(removeConnection) & length(removeConnection) != 2){
    cat("Error removeConnection must be a vector with the length of two, removeConnection:",
        removeConnection, "\n")
    return(NULL)
  }
  
  if(!is.null(removeNode) & length(removeNode) != 1){
    cat("Error removeNode must be a vector with the length of one, removeNode:",
        removeNode, "\n")
    return(NULL)
  }
  
  
  tmp_id <- cbind(V(singleCAM)$label, V(singleCAM)$name)
  ### remove connection
  #> if both name exists
  if(!is.null(removeConnection)){
    if(sum(tmp_id[,1] %in% removeConnection) == 2){
      #> if connected
      if(are.connected(graph = singleCAM, v1 = tmp_id[,2][tmp_id[,1] == removeConnection[1]],
                       v2 = tmp_id[,2][tmp_id[,1] == removeConnection[2]])){
        if(verbose){
          cat("CAM with ID", singleCAMid, "was sliced", "\n")
        }
        
        singleCAM <- singleCAM - edge(paste0(tmp_id[,2][tmp_id[,1] == removeConnection[1]], "|",
                                             tmp_id[,2][tmp_id[,1] == removeConnection[2]]))
      }else{
        if(verbose){
          cat("CAM with ID", singleCAMid, "was NOT sliced (no connection)", "\n")
        }
      }
    }else{
      if(sum(tmp_id[,1] %in% removeConnection) < 2){
        if(verbose){
          cat("In CAM with ID", singleCAMid, 'both or one of the arguments of "removeConnection" were not found',
              "\n")
        }
        # return(NULL)
      }else{
        if(verbose){
          cat("In CAM with ID", singleCAMid, 'arguments of "removeConnection" were found more than 2 times',
              "\n")
        }
        # return(NULL)
      }
    }
  }
  
  
  ### remove connection
  if(!is.null(removeNode)){
    if(sum(tmp_id[,1] %in% removeNode) == 1){
      if(verbose){
        cat("concept in CAM with ID", singleCAMid, "was deleted", "\n")
      }
      
      singleCAM <- delete_vertices(graph = singleCAM, v =  V(singleCAM)$name[V(singleCAM)$label == removeNode])
      
    }else{
      if(sum(tmp_id[,1] %in% removeNode) == 0){
        if(verbose){
          cat("In CAM with ID", singleCAMid, 'the argument of "removeNode" was not found',
              "\n")
        }
        # return(NULL)
      }else{
        if(verbose){
          cat("In CAM with ID", singleCAMid, 'the argument of "removeNode" were found more than 1 times',
              "\n")
        }
        # return(NULL)
      }
    }
  }
  
  
  if(plot){
    plot.igraph(singleCAM,
                edge.arrow.size = .2,
                layout=layout_nicely, vertex.frame.color="black", asp = .5,
                margin = 0, vertex.label.cex = .7, main = singleCAMid)
  }
  
  return(singleCAM)
}
