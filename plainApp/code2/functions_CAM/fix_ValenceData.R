############################################################################
##### fix_ValenceData()

############################################################################
### args:
# dat_nodes = CAMfiles[[1]]; dim(CAMfiles[[1]])
# dat_connectors = CAMfiles[[2]]; dim(CAMfiles[[2]])
# dat_merged = CAMfiles[[3]]; dim(CAMfiles[[3]])
# verbose = TRUE
# i <- 82 # Claras MA data set
fix_ValenceData <- function(dat_nodes, dat_connectors, dat_merged, verbose = FALSE){
  
  
  vec_ghostNodes <- c()
  vec_unconnectedNetworks <- c()
  vec_multipleConnections <- c()
  h=1; k=1; z=1
  
  
  for(i in 1:length(unique(dat_nodes$CAM))){
    
    
    tmp_nodes <- dat_nodes %>%
      filter(CAM == unique(dat_nodes$CAM)[i])
    tmp_connectors <- dat_connectors %>%
      filter(CAM == unique(dat_nodes$CAM)[i])
    tmp_merged <- dat_merged %>%
      filter(CAM.x == unique(dat_nodes$CAM)[i])
    
    
    
    
    ################
    # > check: unconnected networks:
    ################
    g_own <- igraph::graph.data.frame(as.data.frame(
      tmp_merged[,c("id", "idending")]))
    clu <- igraph::components(g_own)
    # plot(g_own)
    
    if(clu$no > 1){
      if(sum(clu$csize == max(clu$csize)) > 1){
        tmp_cluster <- sample(x = which(clu$csize == max(clu$csize)), size = 1)
      }else{
        tmp_cluster <- which(clu$csize == max(clu$csize))
      }
      
      tmp_cluster_IDs <- names(clu$membership)[clu$membership == tmp_cluster]
      
      
      ### verbose:
      if(verbose){
        cat("\nfollowing CAM sets containing ",
            clu$no,
            " numbers of clusters, for CAM", unique(dat_nodes$CAM)[i], "\n")
      }
      
      
      ## keep only fully connteced components
      tmp_nodes <- tmp_nodes[tmp_nodes$id %in% tmp_cluster_IDs,]
      
      tmp_connectors <- tmp_connectors[tmp_connectors$daughterID %in% tmp_cluster_IDs |
                                         tmp_connectors$motherID %in% tmp_cluster_IDs,]
      
      tmp_merged <- tmp_merged[tmp_merged$id %in% tmp_cluster_IDs,]
      
      
      ### add CAM id
      # print( unique(tmp_nodes$CAM))
      # print( tmp_nodes$CAM)
      
      
      
      vec_unconnectedNetworks[z] <- unique(tmp_nodes$CAM)
      z=z+1
      
    }
    
    ################
    # > check: multiple connections
    ################
    # check if function is necessary
    if(sum(table(tmp_merged$id, tmp_merged$idending) > 1) != 0){
      
      ### verbose:
      if(verbose){
        cat("\nin CAM", unique(dat_nodes$CAM)[i], sum(table(tmp_merged$id, tmp_merged$idending) > 1) ,
            "rows need to be removed, because arrow(s) is (are) multiple drawn (x-times)", "\n")
      }
      
      
      ## set up data frame
      tmp <- table(tmp_merged$id, tmp_merged$idending) > 1
      # initialize parameters for loop
      g = 1
      tmp_colnames <- c(); tmp_rownames <- c()
      # if any arrows x-times drawn in CAM X
      if(sum(tmp) > 0){
        # ids rows, cols
        for(r in 1:nrow(tmp)){
          for(c in 1:ncol(tmp)){
            if(tmp[r,c]){
              tmp_rownames[g] <- rownames(tmp)[r]
              tmp_colnames[g] <- colnames(tmp)[c]
              g = g + 1
            }
          }
        }
      }
      
      # > if 1 arrow is drawn twice
      if(length(tmp_colnames) == 1){
        tmp_out_decRows <- sum((tmp_merged$id == tmp_rownames & tmp_merged$idending == tmp_colnames))
        ## delete only the older connetions
        tmp <- tmp_merged[tmp_merged$id == tmp_rownames & tmp_merged$idending == tmp_colnames, ]
        tmp <- tmp[order(tmp$date, decreasing = TRUE),] # decreasing (1 = newest)
        tmp_merged <- tmp_merged[!(tmp_merged$text == tmp$text[1] &
                                     tmp_merged$id.y %in% tmp$id.y[2:length(tmp$id.y)]), ]
        
        if(verbose){
          cat("number of dec. rows (1 connector multiple drawn): ", tmp_out_decRows)
        }
      }
      
      # > if multiple arrows are drawn
      if(length(tmp_colnames) > 1){
        for(j in 1:length(tmp_colnames)){
          tmp <- tmp_merged[tmp_merged$id == tmp_rownames[j] & tmp_merged$idending == tmp_colnames[j], ]
          tmp <- tmp[order(tmp$date, decreasing = TRUE),] # decreasing (1 = newest)
          
          if(verbose){
            cat("\n dec. rows from", nrow(tmp_merged),"-> ")
          }
          
          tmp_merged <- tmp_merged[!(tmp_merged$text == tmp$text[1] &
                                       tmp_merged$id.y %in% tmp$id.y[2:length(tmp$id.y)]), ]
          
          # tmp_merged <- tmp_merged[!(tmp_merged$text == tmp$text[1] &
          #                        tmp_merged$id.y %in% tmp$id.y[2:length(tmp$id.y)]), ]
          if(verbose){
            cat(nrow(tmp_merged))
          }
        }
      }
      
      
      ### remove connections not included in tmp_merged anymore
      tmp_connectors <- tmp_connectors[tmp_connectors$daughterID %in% tmp_merged$id |
                                         tmp_connectors$motherID %in% tmp_merged$id,]
      
      
      ### add CAM id
      vec_multipleConnections[k] <- unique(tmp_nodes$CAM)
      k=k+1
    }
    
    
    
    ################
    # > check: ghost node (isolated vertex)
    ################
    if(sum(!tmp_nodes$id %in% tmp_connectors$daughterID &
           !tmp_nodes$id %in% tmp_connectors$motherID) > 0){
      
      ### verbose:
      if(verbose){
        cat("\nfollowing CAM sets containing ",
            sum(!tmp_nodes$id %in% tmp_connectors$daughterID &
                  !tmp_nodes$id %in% tmp_connectors$motherID),
            " isolated vertex / vertices with the CAM-ID:", unique(dat_nodes$CAM)[i], "\n")
        cat(" > no edges to nodes:",
            paste0(tmp_nodes$text[!tmp_nodes$id %in% tmp_connectors$daughterID &
                                    !tmp_nodes$id %in% tmp_connectors$motherID], collapse = " // "),"\n\n")
      }
      
      ### remove ghost nodes
      tmp_nodes <- tmp_nodes[tmp_nodes$id %in% tmp_connectors$daughterID |
                               tmp_nodes$id %in% tmp_connectors$motherID, ]
      
      ### add CAM id
      vec_ghostNodes[h] <- unique(tmp_nodes$CAM)
      h=h+1
      
    }
    
    ####
    
    
    ## save datsets
    if(i == 1){
      out_dat_nodes <- tmp_nodes
      out_dat_connectors <- tmp_connectors
      out_dat_merged <- tmp_merged
      
    }else{
      out_dat_nodes <- rbind(out_dat_nodes, tmp_nodes)
      out_dat_connectors <- rbind(out_dat_connectors, tmp_connectors)
      out_dat_merged <- rbind(out_dat_merged, tmp_merged)
      
    }
    
  }
  
  
  
  return(list(out_dat_nodes, out_dat_connectors, out_dat_merged,
              vec_unconnectedNetworks,
              vec_multipleConnections,
              vec_ghostNodes))
}
