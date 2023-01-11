# ==============================================================================
# R-Code - CAM
# date of creation: December 2021
# authors: Julius Fenn
# function: compute_indicatorsCAM()
# ==============================================================================

############################################################################
# compute_indicatorsCAM()
#
############################################################################
# drawn_CAM = CAMdrawn
# micro_degree = NULL
# micro_valence = NULL
# micro_centr_clo = NULL
# micro_degree = c("technological implant", "medical benefits")
# micro_valence = c("technological implant", "medical benefits")
# micro_centr_clo = c("technological implant", "medical benefits")
compute_indicatorsCAM <- function(drawn_CAM = NULL,
                                  micro_degree = NULL,
                                  micro_valence = NULL,
                                  micro_centr_clo = NULL,
                                  largestClique = FALSE){
  # check drawn_CAM
  if(!(typeof(drawn_CAM) == "list" & class(drawn_CAM[[1]]) == "igraph")){
    cat("Your specified drawn_CAM argument is of type:", typeof(drawn_CAM), "\n")
    cat("and / or the first list entry is of class:",  class(drawn_CAM[[1]]), "\n")
    stop("> specify a list with igraph classes")
  }
  #
  # second check -> useless micro indicators?

  ### loop over single igraph objects within list

  # tmp_mat <- matrix(data = NA, nrow = length(drawn_CAM), ncol = 46)

  out_netind <- data.frame(CAM_ID =  names(drawn_CAM))
  nrow(out_netind)


  ################
  # unique ID
  ################
  out_netind["participantCAM"]  <- NA


  ################
  # structural coefficients: MACRO hole network:
  ################
  out_netind["mean_valence_macro"]  <- NA
  out_netind["mean_valence_normed_macro"]  <- NA

  out_netind["density_macro"]  <- NA
  out_netind["transitivity_macro"]  <- NA
  out_netind["centr_degree_macro"]  <- NA
  out_netind["centr_clo_macro"]  <- NA
  out_netind["centr_betw_macro"]  <- NA
  out_netind["centr_eigen_macro"]  <- NA

  out_netind["meanDistance_directed_macro"]  <- NA
  out_netind["meanDistance_undirected_macro"]  <- NA
  out_netind["diameter_weighted_undirected_macro"]  <- NA
  out_netind["diameter_unweighted_undirected_macro"]  <- NA
  out_netind["diameter_unweighted_directed_macro"]  <- NA

  out_netind["num_nodes_macro"]  <- NA
  out_netind["num_nodes_pos_macro"]  <- NA
  out_netind["num_nodes_neg_macro"]  <- NA
  out_netind["num_nodes_neut_macro"]  <- NA
  out_netind["num_nodes_ambi_macro"]  <- NA

  out_netind["num_edges_macro"]  <- NA
  out_netind["num_edges_solid_macro"]  <- NA
  out_netind["num_edges_dashed_macro"]  <- NA
  out_netind["num_edges_invaliddashed_macro"]  <- NA
  out_netind["meanWeightEdges_macro"]  <- NA

  out_netind["reciprocity_macro"]  <- NA
  out_netind["assortativity_valence_macro"]  <- NA
  out_netind["assortativityDegree_macro"]  <- NA


  ################
  # structural coefficients: MEZZO part of network:
  ################
  if(largestClique){
    out_netind["largest_clique_length_mezzo"]  <- NA
    out_netind["largest_clique_nums_mezzo"]  <- NA
    out_netind["largest_clique_names_mezzo"]  <- NA
  }

  ################
  # structural coefficients: MICRO single nodes of network:
  ################
  # >> generated automatically if label lists are not empty

  ### degree total of single vertices
  if(!is.null(micro_degree)){
    tmp_name <- paste0("degreetot_micro_", str_replace_all(string=micro_degree, pattern=" ", repl=""))
    for(m in 1:length(micro_degree)){
      out_netind[tmp_name[m]]  <- NA
    }
  }

  ### degree total of single vertices
  if(!is.null(micro_valence)){
    tmp_name <- paste0("valence_micro_", str_replace_all(string=micro_valence, pattern=" ", repl=""))
    for(m in 1:length(micro_valence)){
      out_netind[tmp_name[m]]  <- NA
    }
  }

  ### centrality measures closeness of single vertices (if 1 == max)
  if(!is.null(micro_centr_clo)){
    tmp_name <- paste0("centr_clo_micro_", str_replace_all(string=micro_centr_clo, pattern=" ", repl=""))
    for(m in 1:length(micro_centr_clo)){
      out_netind[tmp_name[m]]  <- NA
    }
  }


  for(i in 1:length(drawn_CAM)){


    V(drawn_CAM[[i]])$value <- ifelse(test = V(drawn_CAM[[i]])$value == 10, yes = 0, no = V(drawn_CAM[[i]])$value)

    # cat("processing:", names(out_g)[i], "\n")

    ################
    # unique ID
    ################
    out_netind[i, "participantCAM"] <- unique(V(drawn_CAM[[i]])$participantCAM)


    ################
    # structural coefficients: MACRO hole network:
    ################
    ### mean valence not normed (red = -1 - -3, ambivalent = 0, green = 1 - 3)
    out_netind[i, "mean_valence_macro"]  <- mean(V(drawn_CAM[[i]])$value)
    ### mean valence normed (red = -1, ambivalent = 0, green = 1
    out_netind[i, "mean_valence_normed_macro"]  <- mean(ifelse(test = V(drawn_CAM[[i]])$color == "green", yes = 1, no =
                                                                 ifelse(test = V(drawn_CAM[[i]])$color == "red", yes = -1, no = 0)))

    ### measure density / centrality
    ### density
    out_netind[i, "density_macro"] <- igraph::graph.density(graph = drawn_CAM[[i]])

    ### transitivity (ratio of triangles)
    # type = "global": undirected, not weighted
    out_netind[i, "transitivity_macro"] <- transitivity(graph = drawn_CAM[[i]], weights = NA, type = "global")

    ### centrality measures degree, closeness, betweenness, eigenvector
    out_netind[i, "centr_degree_macro"]  <- igraph::centr_degree(graph = drawn_CAM[[i]], mode = "all", loops = TRUE, normalized = TRUE)$centralization
    out_netind[i, "centr_clo_macro"]  <- igraph::centr_clo(graph = drawn_CAM[[i]], mode="all", normalized=TRUE)$centralization
    out_netind[i, "centr_betw_macro"]  <- igraph::centr_betw(graph = drawn_CAM[[i]], directed = FALSE, normalized = TRUE)$centralization
    out_netind[i, "centr_eigen_macro"]  <- igraph::centr_eigen(graph = drawn_CAM[[i]], directed = FALSE, normalized = TRUE, scale = TRUE)$centralization
    ###

    ### mean distance
    # directed
    out_netind[i, "meanDistance_directed_macro"]  <- igraph::mean_distance(graph = drawn_CAM[[i]], directed = TRUE)
    # undirected
    out_netind[i, "meanDistance_undirected_macro"]  <- igraph::mean_distance(graph = drawn_CAM[[i]], directed = FALSE)
    ### diameter
    out_netind[i, "diameter_weighted_undirected_macro"]  <- diameter(graph = drawn_CAM[[i]], directed = FALSE) # weights are considered
    out_netind[i, "diameter_unweighted_undirected_macro"]  <- diameter(graph = drawn_CAM[[i]], directed = FALSE, weights = NA) # no weights are considered
    out_netind[i, "diameter_unweighted_directed_macro"]  <- diameter(graph = drawn_CAM[[i]], directed = TRUE, weights = NA) # no weights are considered, but direction


    ### num nodes + nodes type  / edges
    ## nodes:
    out_netind[i, "num_nodes_macro"]  <- gorder(graph = drawn_CAM[[i]]) # nodes
    # type:
    out_netind[i, "num_nodes_pos_macro"]  <- sum(V(drawn_CAM[[i]])$color == "green") # positive
    out_netind[i, "num_nodes_neg_macro"]  <- sum(V(drawn_CAM[[i]])$color == "red") # negative
    out_netind[i, "num_nodes_neut_macro"]  <- sum(V(drawn_CAM[[i]])$color == "yellow") # neutral
    out_netind[i, "num_nodes_ambi_macro"]  <- sum(V(drawn_CAM[[i]])$color == "purple") # ambivalent

    ### edges:
    out_netind[i, "num_edges_macro"]  <- gsize(graph = drawn_CAM[[i]])
    out_netind[i, "num_edges_solid_macro"]  <- sum(str_detect(string = E(drawn_CAM[[i]])$lty, pattern = "1")) # solid
    out_netind[i, "num_edges_dashed_macro"]  <- sum(str_detect(string = E(drawn_CAM[[i]])$lty, pattern = "2")) # dashed
    ## invalid dashed edges
    tmp_dat_edges <- as_data_frame(drawn_CAM[[i]])
    tmp_dat_vertex <- data.frame(name = V(graph = drawn_CAM[[i]])$name,
                                 label = V(graph = drawn_CAM[[i]])$label,
                                 color = V(graph = drawn_CAM[[i]])$color,
                                 valenceposneg = V(graph = drawn_CAM[[i]])$value)
    tmp_merge <- tmp_dat_edges %>%
      left_join(tmp_dat_vertex[, c(1,3)], by = c("from" = "name"))
    tmp_merge <- tmp_merge %>%
      left_join(tmp_dat_vertex[, c(1,3)], by = c("to" = "name"))
    ## invalid if green - - - green, red - - - red OR green --- red
    # dashed lines
    tmp_merge_valencesdashed <- tmp_merge[str_detect(string = tmp_merge$lty , pattern = "2"),
                                          c("color.y", "color")]
    # solid lines
    tmp_merge_valencessolid <- tmp_merge[str_detect(string = tmp_merge$lty , pattern = "1"),
                                         c("color.y", "color")]


    out_netind[i, "num_edges_invaliddashed_macro"] <- sum(tmp_merge_valencesdashed$color.y == "green" & tmp_merge_valencesdashed$color == "green" |
                                                            tmp_merge_valencesdashed$color.y == "red" & tmp_merge_valencesdashed$color == "red") +
      sum(tmp_merge_valencessolid$color.y == "green" & tmp_merge_valencessolid$color == "red")
    ##
    ### edges weight (1-3) - no difference neg. / pos.
    out_netind[i, "meanWeightEdges_macro"]  <- mean(E(drawn_CAM[[i]])$weight)

    ### mixed: reciprocity, assortativity
    out_netind[i, "reciprocity_macro"]  <- igraph::reciprocity(drawn_CAM[[i]])
    out_netind[i, "assortativity_valence_macro"]  <-  igraph::assortativity(graph = drawn_CAM[[i]], types1 = ifelse(test = V(drawn_CAM[[i]])$color == "green", yes = 3, no =
                                                                                                                      ifelse(test = V(drawn_CAM[[i]])$color == "red", yes = 2, no = 1)), directed = FALSE)
    out_netind[i, "assortativityDegree_macro"]  <- igraph::assortativity.degree(graph = drawn_CAM[[i]], directed = FALSE)

    ################
    # structural coefficients: MEZZO part of network:
    ################
    ### largest clique and names
    if(largestClique){
      out_netind[i, "largest_clique_length_mezzo"] <- length(largest_cliques(graph = as.undirected(drawn_CAM[[i]]))[[1]])
      out_netind[i, "largest_clique_nums_mezzo"]  <- length(largest_cliques(graph = as.undirected(drawn_CAM[[i]])))

      tmp <- cbind(names(V(graph = drawn_CAM[[i]])), V(graph = drawn_CAM[[i]])$label)
      vec_names_clique <- list()
      for(c in 1:length(largest_cliques(graph = as.undirected(drawn_CAM[[i]])))){
        vec_names_clique[[c]] <- tmp[,2][tmp[,1] %in% names(largest_cliques(graph = as.undirected(drawn_CAM[[i]]))[[c]])]
      }
      out_netind[i, "largest_clique_names_mezzo"] <- paste0(unlist(vec_names_clique), collapse = ", ")
    }


    ################
    # structural coefficients: MICRO single nodes of network:
    ################
    ### degree total of single vertices
    if(!is.null(micro_degree)){
      tmp_name <- paste0("degreetot_micro_", str_replace_all(string=micro_degree, pattern=" ", repl=""))
      for(m in 1:length(micro_degree)){
        ### degree total
        degree_tmp <- igraph::degree(drawn_CAM[[i]], mode="all", loops = TRUE)
        tmp <- cbind(V(drawn_CAM[[i]])$label, as.numeric(degree_tmp))

        if(any(str_detect(string = tmp[,1], pattern = paste0("^", micro_degree[m], "$")))){
          out_netind[i, tmp_name[m]] <- as.numeric(tmp[,2][str_detect(string = tmp[,1], pattern = paste0("^", micro_degree[m], "$"))])
        }
      }
    }

    ### valence single vertices
    if(!is.null(micro_valence)){
      tmp_name <- paste0("valence_micro_", str_replace_all(string=micro_valence, pattern=" ", repl=""))
      for(m in 1:length(micro_valence)){
        ### valence
        # valence_tmp <- ifelse(test = V(drawn_CAM[[i]])$color == "green", yes = 1, no =
        #                         ifelse(test = V(drawn_CAM[[i]])$color == "red", yes = -1, no = 0)) * V(drawn_CAM[[i]])$valence
        tmp <- cbind(V(drawn_CAM[[i]])$label, V(drawn_CAM[[i]])$value)

        if(any(str_detect(string = tmp[,1], pattern = paste0("^", micro_valence[m], "$")))){
          out_netind[i, tmp_name[m]] <- as.numeric(tmp[,2][str_detect(string = tmp[,1], pattern = paste0("^", micro_valence[m], "$"))])
        }
      }
    }



    ### centrality measures closeness of single vertices (if 1 == max)
    if(!is.null(micro_centr_clo)){
      tmp_name <- paste0("centr_clo_micro_", str_replace_all(string=micro_centr_clo, pattern=" ", repl=""))
      for(m in 1:length(micro_centr_clo)){
        ### centrality measures closeness
        centr_tmp <- igraph::centr_clo(drawn_CAM[[i]], mode="all", normalized=TRUE)
        tmp <- cbind(V(drawn_CAM[[i]])$label, centr_tmp$res / max(centr_tmp$res))

        if(any(str_detect(string = tmp[,1], pattern = paste0("^", micro_centr_clo[m], "$")))){
          out_netind[i, tmp_name[m]] <- as.numeric(tmp[,2][str_detect(string = tmp[,1], pattern = paste0("^", micro_centr_clo[m], "$"))])
        }
      }
    }
  }


  return(out_netind)
}
