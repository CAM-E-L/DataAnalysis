# ==============================================================================
# R-Code - CAM
# date of creation: December 2021
# authors: Julius Fenn
# function: create_CAMfiles()
# ==============================================================================

############################################################################
# create_CAMfiles()
#
############################################################################
# datCAM = raw_CAM
# reDeleted = TRUE
# verbose=TRUE
create_CAMfiles <- function(datCAM = raw_CAM, reDeleted = TRUE, verbose=FALSE){
  ### create block dataset
  for(i in 1:length(datCAM)){
    if(length(datCAM[[i]]$nodes) == 0){
      
      cat("following CAM containing zero nodes: ",
          datCAM[[i]]$creator, "\n")
    }
    if(length(datCAM[[i]]$nodes) > 0){
      if(!is.null(datCAM[[i]]$creator)){
        tmp_participantCAM = rep(x = datCAM[[i]]$creator, times = nrow(datCAM[[i]]$nodes))
      }else{
        tmp_participantCAM = rep(x = "NO ID PROVIDED", times = nrow(datCAM[[i]]$nodes))
      }
      tmp <- data.frame(CAM = rep(x = datCAM[[i]]$idCAM, times = nrow(datCAM[[i]]$nodes)),
                        participantCAM = tmp_participantCAM,
                        id = datCAM[[i]]$nodes$id,
                        text = datCAM[[i]]$nodes$text,
                        value = datCAM[[i]]$nodes$value,
                        comment = datCAM[[i]]$nodes$comment,
                        date = lubridate::as_datetime(datCAM[[i]]$nodes$date / 1000),
                        x_pos = datCAM[[i]]$nodes$position$x,
                        y_pos = datCAM[[i]]$nodes$position$y,
                        predefinedConcept = ifelse(test = datCAM[[i]]$nodes$isDraggable == FALSE | 
                                                     datCAM[[i]]$nodes$isDeletable == FALSE |
                                                     datCAM[[i]]$nodes$isTextChangeable == FALSE, yes = TRUE, no = FALSE),
                        isDraggable = as.numeric(datCAM[[i]]$nodes$isDraggable),
                        isDeletable = as.numeric(datCAM[[i]]$nodes$isDeletable),
                        isTextChangeable = as.numeric(datCAM[[i]]$nodes$isTextChangeable),
                        isActive = datCAM[[i]]$nodes$isActive)
      if(i == 1){
        dat_nodes <- tmp
      }else{
        dat_nodes <- rbind(dat_nodes, tmp)
      }
    }
  }
  
  
  
  ### create connectors dataset
  for(i in 1:length(datCAM)){
    if(length(datCAM[[i]]$connectors) > 0){
      if(!is.null(datCAM[[i]]$creator)){
        tmp_participantCAM = rep(x = datCAM[[i]]$creator, times = nrow(datCAM[[i]]$connectors))
      }else{
        tmp_participantCAM = rep(x = "NO ID PROVIDED", times = nrow(datCAM[[i]]$connectors))
      }
      tmp <- data.frame(CAM = rep(x = datCAM[[i]]$idCAM, times = nrow(datCAM[[i]]$connectors)),
                        participantCAM = tmp_participantCAM,
                        id = datCAM[[i]]$connectors$id,
                        date = lubridate::as_datetime(datCAM[[i]]$connectors$date / 1000),
                        daughterID = datCAM[[i]]$connectors$target,
                        motherID = datCAM[[i]]$connectors$source,
                        intensity  = datCAM[[i]]$connectors$intensity ,
                        agreement  = as.numeric(datCAM[[i]]$connectors$agreement) ,
                        isBidirectional = as.numeric(datCAM[[i]]$connectors$isBidirectional),
                        isDeletable = as.numeric(datCAM[[i]]$connectors$isDeletable),
                        isActive = datCAM[[i]]$connectors$isActive)
      if(i == 1){
        dat_connectors <- tmp
      }else{
        dat_connectors <- rbind(dat_connectors, tmp)
      }
    }
  }
  
  
  ## if true then remove non active nodes
  if(reDeleted){
    cat("Nodes and connectors, which were deleted by participants were removed.", "\n",
        "# deleted nodes: ", sum(!dat_nodes$isActive), "\n",
        "# deleted connectors: ", sum(!dat_connectors$isActive))
    dat_nodes <- dat_nodes[dat_nodes$isActive,]
    dat_connectors <- dat_connectors[dat_connectors$isActive,]
  }
  
  
  ### merge data sets
  for(i in 1:length(unique(dat_nodes$CAM))){
    
    tmp_nodes <- dat_nodes %>%
      filter(CAM == unique(dat_nodes$CAM)[i])
    tmp_connectors <- dat_connectors %>%
      filter(CAM == unique(dat_nodes$CAM)[i])
    
    ## > check: isolated vertex
    if(verbose){
      if(sum(!tmp_nodes$id %in% tmp_connectors$daughterID &
             !tmp_nodes$id %in% tmp_connectors$motherID) > 0){
        # print(i)
        cat("following CAM sets containing ",
            sum(!tmp_nodes$id %in% tmp_connectors$daughterID &
                  !tmp_nodes$id %in% tmp_connectors$motherID),
            " isolated vertex / vertices with the CAM-ID:", unique(dat_nodes$CAM)[i], "\n")
        cat(" > no edges to nodes:",
            paste0(tmp_nodes$text[!tmp_nodes$id %in% tmp_connectors$daughterID &
                                    !tmp_nodes$id %in% tmp_connectors$motherID], collapse = " // "),"\n\n")
      }
    }
    
    
    ## merge
    tmp_dat_ending <- dplyr::left_join(x = tmp_nodes,
                                       y = tmp_connectors, by = c("id" = "motherID"))
    tmp_dat_starting <- dplyr::left_join(x = tmp_nodes,
                                         y = tmp_connectors, by = c("id" = "daughterID"))
    
    ## remove missing
    tmp_dat_ending <- tmp_dat_ending[!is.na(tmp_dat_ending$CAM.y),]
    tmp_dat_starting <- tmp_dat_starting[!is.na(tmp_dat_starting$CAM.y),]
    
    ## keep only bidirectional
    # > uni directional is: ending_block -> starting_block
    tmp_dat_starting <- tmp_dat_starting[tmp_dat_starting$isBidirectional == 1,]
    
    
    
    
    # colnames(tmp_dat_ending)[colnames(tmp_dat_ending) != colnames(tmp_dat_starting)]
    # colnames(tmp_dat_starting)[colnames(tmp_dat_starting) != colnames(tmp_dat_ending)]
    colnames(tmp_dat_ending)[colnames(tmp_dat_ending) == "daughterID"]  <- "idending"
    colnames(tmp_dat_starting)[colnames(tmp_dat_starting) == "motherID"]  <- "idending"
    
    
    tmp_dat_merged <- rbind(tmp_dat_ending,tmp_dat_starting) # final data set
    
    
    if(i == 1){
      tmp_dat_out <- tmp_dat_merged
    }else{
      tmp_dat_out <- rbind(tmp_dat_out, tmp_dat_merged)
    }
  }
  
  return(list(dat_nodes, dat_connectors, tmp_dat_out))
}
