# ==============================================================================
# R-Code - CAM
# date of creation: January 2022
# authors: Julius Fenn
# function: create_CAMfiles()
# ==============================================================================

############################################################################
# create_ValenceFiles()
#
############################################################################
#
create_ValenceFiles <- function(datBlocks = blocks, datLinks = links, verbose=FALSE){
  ### create block dataset
  for(i in 1:length(datBlocks)){
    tmp <- datBlocks[[i]]$shape
    tmp_num <- as.numeric(ifelse(test = tmp == "ambivalent", yes = 10, no =
                                   ifelse(test = tmp == "neutral", yes = 0, no =
                                            ifelse(test = tmp == "negative weak", yes = -1, no =
                                                     ifelse(test = tmp == "negative", yes = -2, no =
                                                              ifelse(test = tmp == "negative strong", yes = -3, no =
                                                                       ifelse(test = tmp == "positive weak", yes = 1, no =
                                                                                ifelse(test = tmp == "positive", yes = 2, no =
                                                                                         ifelse(test = tmp == "positive strong", yes = 3, no = "ERROR")))))))))

    tmp <- data.frame(CAM = datBlocks[[i]]$CAM,
                      participantCAM = datBlocks[[i]]$participantCAM,
                      id = datBlocks[[i]]$id,
                      text = datBlocks[[i]]$title,
                      value = tmp_num,
                      comment = datBlocks[[i]]$comment,
                      date = datBlocks[[i]]$timestamp,
                      x_pos = datBlocks[[i]]$x_pos,
                      y_pos = datBlocks[[i]]$y_pos,
                      isDraggable = NA, # not existing in Valence
                      isDeletable = NA,
                      isTextChangeable = NA,
                      isActive = NA)


    if(i == 1){
      dat_nodes <- tmp
    }else{
      ## rename identical CAM IDs
      if(any(tmp$CAM %in% dat_nodes$CAM)){
        tmp$CAM<- paste0(tmp$CAM, "r")
        cat("\nthe following subject:", unique(tmp$participantCAM), "has identical CAM IDs - renamed\n")
      }
      dat_nodes <- rbind(dat_nodes, tmp)
    }
  }

  ## data preperation
  # comment
  dat_nodes$comment <- ifelse(test = dat_nodes$comment == "", yes = NA, no = dat_nodes$comment)


  ### create connectors dataset
  for(i in 1:length(datLinks)){
    tmp <- data.frame(CAM = datLinks[[i]]$CAM,
                      participantCAM = datLinks[[i]]$participantCAM,
                      id = datLinks[[i]]$id,
                      daughterID = datLinks[[i]]$starting_block, # ?!
                      motherID = datLinks[[i]]$ending_block,
                      intensity  = datLinks[[i]]$line_style,
                      agreement  = datLinks[[i]]$line_style,
                      isBidirectional = datLinks[[i]]$arrow_type,
                      isDeletable = NA,
                      isActive = NA)
    if(i == 1){
      dat_connectors <- tmp
    }else{
      ## rename identical CAM IDs
      if(any(tmp$CAM %in% dat_connectors$CAM)){
        tmp$CAM<- paste0(tmp$CAM, "r")
        cat("\nthe following subject:", unique(tmp$participantCAM), "has identical CAM IDs - renamed\n")
      }
      dat_connectors <- rbind(dat_connectors, tmp)
    }
  }

  ## data preperation
  # isBidirectional
  dat_connectors$isBidirectional <- ifelse(test = dat_connectors$isBidirectional == "uni", yes = 1, no = 0)
  # intensity
  dat_connectors$intensity[str_detect(string = dat_connectors$intensity, pattern = "-Weak")] <- 1
  dat_connectors$intensity[str_detect(string = dat_connectors$intensity, pattern = "-Weak|-Strong", negate = TRUE)] <- 2
  dat_connectors$intensity[str_detect(string = dat_connectors$intensity, pattern = "-Strong")] <- 3
  dat_connectors$intensity <- as.numeric(dat_connectors$intensity) * 3 # change eventually !
  # agreement
  dat_connectors$agreement[str_detect(string = dat_connectors$agreement, pattern = "Solid")] <- 1
  dat_connectors$agreement[str_detect(string = dat_connectors$agreement, pattern = "Dashed")] <- 0
  dat_connectors$agreement <- as.numeric(dat_connectors$agreement)


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

    colnames(tmp_dat_ending)[17] <- "idending"
    colnames(tmp_dat_starting)[17] <- "idending"


    tmp_dat_merged <- rbind(tmp_dat_ending,tmp_dat_starting) # final data set


    if(i == 1){
      tmp_dat_out <- tmp_dat_merged
    }else{
      tmp_dat_out <- rbind(tmp_dat_out, tmp_dat_merged)
    }
  }

  return(list(dat_nodes, dat_connectors, tmp_dat_out))
}

