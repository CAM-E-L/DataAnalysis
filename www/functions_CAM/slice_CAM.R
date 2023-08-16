# ==============================================================================
# R-Code - CAM
# date of creation: March 2023
# authors: Julius Fenn
# function:
# ==============================================================================

############################################################################
# sliceCAM()
#
############################################################################
### args
# i <- 3
# singleCAM = CAMdrawn[[i]]
# singleCAMid =  names(CAMdrawn)[i]
# removeConnection = c("Öffentliche Verkehrsmittel", "Eigener Pkw")
# removeNode = NULL
# plot = TRUE
# verbose = TRUE
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

      ## get direction of cutting
      if(are.connected(graph = singleCAM, v1 = tmp_id[,2][tmp_id[,1] == removeConnection[1]],
                       v2 = tmp_id[,2][tmp_id[,1] == removeConnection[2]])){
        cutDirection <- paste0(tmp_id[,2][tmp_id[,1] == removeConnection[1]], "|",
                               tmp_id[,2][tmp_id[,1] == removeConnection[2]])
      }

      if(are.connected(graph = singleCAM, v1 = tmp_id[,2][tmp_id[,1] == removeConnection[2]],
                       v2 = tmp_id[,2][tmp_id[,1] == removeConnection[1]])){
        cutDirection <- paste0(tmp_id[,2][tmp_id[,1] == removeConnection[2]], "|",
                               tmp_id[,2][tmp_id[,1] == removeConnection[1]])
      }

      #> if connected
      if(are.connected(graph = singleCAM, v1 = tmp_id[,2][tmp_id[,1] == removeConnection[1]],
                       v2 = tmp_id[,2][tmp_id[,1] == removeConnection[2]]) ||
         are.connected(graph = singleCAM, v1 = tmp_id[,2][tmp_id[,1] == removeConnection[2]],
                       v2 = tmp_id[,2][tmp_id[,1] == removeConnection[1]])){
        if(verbose){
          cat("CAM with ID", singleCAMid, "was sliced", "\n")
        }

        singleCAM <- singleCAM - edge(cutDirection)
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



############################################################################
# sliceAllCAMs_combined()
#
############################################################################
### args
# CAMfilesList = CAMfiles
# drawnCAMs = CAMdrawn
# connectionToRemove = NULL
# nodeToRemove = "Covid-19"
# centralConceptsSubgraphs = c("negative aspects", "positive aspects")
# useSummarized = FALSE
# plot = TRUE
sliceAllCAMs_combined <- function(CAMfilesList = NULL,
                                  drawnCAMs = NULL,
                                  connectionToRemove = NULL,
                                  nodeToRemove = NULL,
                                  centralConceptsSubgraphs = NULL,
                                  plot = FALSE){

  vec_separableCAMs <- c(); h=1
  for(i in 1:length(drawnCAMs)){
    tmp_CAM <- sliceCAM(singleCAM = drawnCAMs[[i]], singleCAMid = names(drawnCAMs)[i],
                        removeConnection = connectionToRemove,
                        removeNode = nodeToRemove,
                        plot = FALSE, verbose = FALSE)

    tmp_id <- cbind(V(tmp_CAM)$label, V(tmp_CAM)$name)

    tmp_com <- components(graph = tmp_CAM)

    ## if 2 components
    if(tmp_com$no == 2){
      tmp_C1 <- tmp_id[,1][tmp_id[,2] %in% names(tmp_com$membership[tmp_com$membership == 1])]
      tmp_C2 <- tmp_id[,1][tmp_id[,2] %in% names(tmp_com$membership[tmp_com$membership == 2])]

      ## if every components includes predefined nodes
      if(sum(tmp_C1 %in% centralConceptsSubgraphs) == 1 &&
         sum(tmp_C2 %in% centralConceptsSubgraphs) == 1){
        # print(i)
        vec_separableCAMs[h] <- i
        h = h + 1

        if(plot){
          plot(drawnCAMs[[i]], edge.arrow.size = .3,
               layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = -0.1,
               vertex.size = 10, vertex.label.cex = .9)
          plot(tmp_CAM, edge.arrow.size = .3,
               layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = -0.1,
               vertex.size = 10, vertex.label.cex = .9)
        }


        tmp_diff <- difference(drawnCAMs[[i]], tmp_CAM, byname = "auto")
        tmp_diff_dat <- igraph::as_data_frame(x = tmp_diff)

        ## remove concepts
        id_out_node <- V(drawnCAMs[[i]])$name[!V(drawnCAMs[[i]])$name %in% V(tmp_CAM)$name]
        if(!identical(id_out_node, character(0))){
          # print("remove node")
          # print(CAMfilesList[[1]]$text[CAMfilesList[[1]]$id %in% id_out_node])
          CAMfilesList[[1]] <- CAMfilesList[[1]][!CAMfilesList[[1]]$id %in% id_out_node, ]
        }

        ## remove connections
        if(nrow(tmp_diff_dat) >= 1){
          for(i in 1:nrow(tmp_diff_dat)){
            CAMfilesList[[2]] <- CAMfilesList[[2]][!(CAMfilesList[[2]]$motherID %in% tmp_diff_dat$from[i] &
                                                       CAMfilesList[[2]]$daughterID %in% tmp_diff_dat$to[i]), ]
            CAMfilesList[[3]] <- CAMfilesList[[3]][!(CAMfilesList[[3]]$id %in% tmp_diff_dat$from[i] &
                                                       CAMfilesList[[3]]$idending %in% tmp_diff_dat$to[i]), ]
          }
        }


      }else{
        ## remove all CAMs where separation was not possible
        if(sum(CAMfilesList[[1]]$CAM %in% names(drawnCAMs)[i]) == 0){
          ## use participant ID
          CAMfilesList[[1]] <- CAMfilesList[[1]][!CAMfilesList[[1]]$participantCAM %in% names(drawnCAMs)[i], ]
          CAMfilesList[[2]] <- CAMfilesList[[2]][!CAMfilesList[[2]]$participantCAM %in% names(drawnCAMs)[i], ]
          CAMfilesList[[3]] <- CAMfilesList[[3]][!CAMfilesList[[3]]$participantCAM.x %in% names(drawnCAMs)[i], ]
        }else{
          ## else CAM ID
          CAMfilesList[[1]] <- CAMfilesList[[1]][!CAMfilesList[[1]]$CAM %in% names(drawnCAMs)[i], ]
          CAMfilesList[[2]] <- CAMfilesList[[2]][!CAMfilesList[[2]]$CAM %in% names(drawnCAMs)[i], ]
          CAMfilesList[[3]] <- CAMfilesList[[3]][!CAMfilesList[[3]]$CAM.x %in% names(drawnCAMs)[i], ]
        }
      }
    }else{
      ## remove all CAMs where separation was not possible
      if(sum(CAMfilesList[[1]]$CAM %in% names(drawnCAMs)[i]) == 0){
        ## use participant ID
        CAMfilesList[[1]] <- CAMfilesList[[1]][!CAMfilesList[[1]]$participantCAM %in% names(drawnCAMs)[i], ]
        CAMfilesList[[2]] <- CAMfilesList[[2]][!CAMfilesList[[2]]$participantCAM %in% names(drawnCAMs)[i], ]
        CAMfilesList[[3]] <- CAMfilesList[[3]][!CAMfilesList[[3]]$participantCAM.x %in% names(drawnCAMs)[i], ]
      }else{
        ## else CAM ID
        CAMfilesList[[1]] <- CAMfilesList[[1]][!CAMfilesList[[1]]$CAM %in% names(drawnCAMs)[i], ]
        CAMfilesList[[2]] <- CAMfilesList[[2]][!CAMfilesList[[2]]$CAM %in% names(drawnCAMs)[i], ]
        CAMfilesList[[3]] <- CAMfilesList[[3]][!CAMfilesList[[3]]$CAM.x %in% names(drawnCAMs)[i], ]
      }
    }
  }


  return(CAMfilesList)
}


############################################################################
# sliceAllCAMs_seperated()
#
############################################################################
### args
sliceAllCAMs_seperated <- function(slicedCAMs = NULL,
                                   centralConceptsSubgraphs = NULL,
                                   plot = FALSE){

  drawnCAMs <- draw_CAM(dat_merged = slicedCAMs[[3]],
                        dat_nodes = slicedCAMs[[1]], ids_CAMs = "all",
                        plot_CAM = FALSE,
                        useCoordinates = TRUE,
                        relvertexsize = 3,
                        reledgesize = 1)

  ## create list for output
  naming_centralConceptsSubgraphs <-
    paste0(
      str_replace_all(
        string = centralConceptsSubgraphs,
        pattern = " ",
        repl = ""
      )
    )

  names_slicedCAMs_combined <- paste0(rep(x = naming_centralConceptsSubgraphs, each = 3),
                                      c("_nodes", "_connectors", "_merged"))
  slicedCAMs_combined <- vector("list", length(names_slicedCAMs_combined))
  names(slicedCAMs_combined) <- names_slicedCAMs_combined

  #> fill list
  # first central concept
  slicedCAMs_combined[[1]] <- slicedCAMs[[1]]
  slicedCAMs_combined[[2]] <- slicedCAMs[[2]]
  slicedCAMs_combined[[3]] <- slicedCAMs[[3]]
  # second central concept
  slicedCAMs_combined[[4]] <- slicedCAMs[[1]]
  slicedCAMs_combined[[5]] <- slicedCAMs[[2]]
  slicedCAMs_combined[[6]] <- slicedCAMs[[3]]

  for(i in 1:length(drawnCAMs)){
    tmp_com <- components(graph = drawnCAMs[[i]])


    tmp_id <- cbind(V(drawnCAMs[[i]])$label, V(drawnCAMs[[i]])$name)

    tmp_C1 <- tmp_id[tmp_id[,2] %in% names(tmp_com$membership[tmp_com$membership == 1]), ]
    tmp_C2 <- tmp_id[tmp_id[,2] %in% names(tmp_com$membership[tmp_com$membership == 2]), ]


    if(centralConceptsSubgraphs[1] %in% tmp_C1[,1]){
      g_c1 <- delete_vertices(drawnCAMs[[i]], tmp_C2[,2])
      g_c2 <- delete_vertices(drawnCAMs[[i]], tmp_C1[,2])
    }else{
      g_c2 <- delete_vertices(drawnCAMs[[i]], tmp_C2[,2])
      g_c1 <- delete_vertices(drawnCAMs[[i]], tmp_C1[,2])

      backup_tmp_C2 <- tmp_C2
      tmp_C2 <- tmp_C1
      tmp_C1 <- backup_tmp_C2
    }

    if(plot){
      plot(g_c1, edge.arrow.size = .3,
           layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = -0.1,
           vertex.size = 10, vertex.label.cex = .9, main = centralConceptsSubgraphs[1])
      plot(g_c2, edge.arrow.size = .3,
           layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = -0.1,
           vertex.size = 10, vertex.label.cex = .9, main = centralConceptsSubgraphs[2])
    }

    ## difference graphs c1
    tmp_diff_c1 <- difference(drawnCAMs[[i]], g_c1, byname = "auto")
    tmp_diff_dat_c1 <- igraph::as_data_frame(x = tmp_diff_c1)
    ## difference graphs c2
    tmp_diff_c2 <- difference(drawnCAMs[[i]], g_c2, byname = "auto")
    tmp_diff_dat_c2 <- igraph::as_data_frame(x = tmp_diff_c2)


    ## remove concepts
    if(!(all(tmp_C1[,2] == V(drawnCAMs[[i]])$name[V(drawnCAMs[[i]])$name %in% V(g_c1)$name]) &
         all(tmp_C2[,2] == V(drawnCAMs[[i]])$name[V(drawnCAMs[[i]])$name %in% V(g_c2)$name]))){
      stop("Possible Error in Data Set? -> IDs not matching")
    }

    names(slicedCAMs_combined)


    ## remove concepts
    # first central concept
    slicedCAMs_combined[[1]] <- slicedCAMs_combined[[1]][!slicedCAMs_combined[[1]]$id %in% tmp_C2[,2], ]
    # second central concept
    slicedCAMs_combined[[4]] <- slicedCAMs_combined[[4]][!slicedCAMs_combined[[4]]$id %in% tmp_C1[,2], ]


    for(i in 1:nrow(tmp_diff_dat_c1)){
      slicedCAMs_combined[[2]] <- slicedCAMs_combined[[2]][!(slicedCAMs_combined[[2]]$motherID %in% tmp_diff_dat_c1$from[i] &
                                                               slicedCAMs_combined[[2]]$daughterID %in% tmp_diff_dat_c1$to[i]), ]
      slicedCAMs_combined[[3]] <- slicedCAMs_combined[[3]][!(slicedCAMs_combined[[3]]$id %in% tmp_diff_dat_c1$from[i] &
                                                               slicedCAMs_combined[[3]]$idending %in% tmp_diff_dat_c1$to[i]), ]
    }

    for(i in 1:nrow(tmp_diff_dat_c2)){
      slicedCAMs_combined[[5]] <- slicedCAMs_combined[[5]][!(slicedCAMs_combined[[5]]$motherID %in% tmp_diff_dat_c2$from[i] &
                                                               slicedCAMs_combined[[5]]$daughterID %in% tmp_diff_dat_c2$to[i]), ]
      slicedCAMs_combined[[6]] <- slicedCAMs_combined[[6]][!(slicedCAMs_combined[[6]]$id %in% tmp_diff_dat_c2$from[i] &
                                                               slicedCAMs_combined[[6]]$idending %in% tmp_diff_dat_c2$to[i]), ]
    }
  }

  return(slicedCAMs_combined)
}
