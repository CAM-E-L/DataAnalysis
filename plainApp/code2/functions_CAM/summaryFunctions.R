# ==============================================================================
# R-Code - CAM
# date of creation: December 2021
# authors: Julius Fenn
# function: draw_CAM()
# ==============================================================================

############################################################################
##### des_sigCorr()
#
############################################################################
################################ !!! in function: significant correlations
# using loops to scan data for significant relationships (or mean differences)
# > which variables are significantly correlation to "mean_valence_macro", ...?

# indicatos_CAM = CAMindicators
# vars = vars = c("mean_valence_macro", "assortativity_valence_macro")
des_sigCorr <- function(indicatos_CAM = NULL, vars = NULL, printOut = FALSE){
  # check vars
  if(is.null(vars)){
    cat("Your specified vars argument is null.\n")
    stop("> please specify a variable / multiple variables")
  }
  if(!all(vars %in% colnames(indicatos_CAM))){
    cat("Your specified vars argument is:", vars, "\n")
    stop("> which is no variable in your dataset")
  }


  vec_names <- indicatos_CAM %>%
    select_if(Negate(is.character)) %>%
    colnames()

  tmp_vars <- c()
  tmp_vars2 <- c()
  tmp_estimate <- c()
  tmp_pvalue <- c()
  h=1
  for(v in 1:length(vars)){
    if(printOut){
      cat("significant correlations for variable:", vars[v], "\n")
    }

    for(i in 1:length(vec_names)){
      tmp_test <- cor.test(indicatos_CAM[, vars[v]], indicatos_CAM[, vec_names[i]])
      if(tmp_test$p.value < .05){
        if(printOut){
          cat("   <-->", vec_names[i]," r:", tmp_test$estimate,"\n")
        }
        tmp_vars[h] <- vars[v]
        tmp_vars2[h] <- vec_names[i]
        tmp_estimate[h] <- round(x = tmp_test$estimate, digits = 2)
        tmp_pvalue[h] <- round(x = tmp_test$p.value, digits = 2)
        h=h+1
      }
    }
    cat("\n\n")
  }
 return(data.frame(setvars = tmp_vars,
                    indvar = tmp_vars2,
                    cor = tmp_estimate,
                    pvalue = tmp_pvalue))
}


############################################################################
##### des_summaryStats()
#
############################################################################
################################ !!! in function: summary statistics
des_summaryStats <- function(indicatos_CAM = NULL, createHTML = TRUE){
  vec_names <- indicatos_CAM %>%
    select_if(Negate(is.character)) %>%
    colnames()

  mat <- matrix(NA, nrow = length(vec_names), ncol = 5)
  for(i in 1:length(vec_names)){
    tmp_mean <- round(x = mean(indicatos_CAM[, vec_names[i]], na.rm = TRUE), digits = 2)
    tmp_sd <- round(x = sd(indicatos_CAM[, vec_names[i]], na.rm = TRUE), digits = 2)
    tmp_min <- round(x = min(indicatos_CAM[, vec_names[i]], na.rm = TRUE), digits = 2)
    tmp_max <- round(x = max(indicatos_CAM[, vec_names[i]], na.rm = TRUE), digits = 2)

    mat[i,] <- c(vec_names[i], tmp_mean, tmp_sd, tmp_min, tmp_max)
  }

  colnames(mat) <- c("networkindicators", "mean", "sd", "min", "max")
  if(createHTML){
    stargazer::stargazer(mat, summary = FALSE,
                         out = "summaryStats.html", type = "html")
  }


  mat <- as.data.frame(mat)
  mat[2:5] <- sapply(mat[2:5],as.numeric)
  return(mat)
}


############################################################################
##### des_centralTerms()
#
############################################################################
################################ !!! in function: most central terms
# using centrality measures closeness
des_centralTerms <- function(drawn_CAM = NULL, createHTML = TRUE, type = NULL){

  # check drawn_CAM
  if(!(typeof(drawn_CAM) == "list" & class(drawn_CAM[[1]]) == "igraph")){
    cat("Your specified drawn_CAM argument is of type:", typeof(drawn_CAM), "\n")
    cat("and / or the first list entry is of class:",  class(drawn_CAM[[1]]), "\n")
    stop("> specify a list with igraph classes")
  }

  # check drawn_CAM
  if((typeof(type) != "character" |
      !all(type %in% c("closeness", "betweenness", "eigenvector", "degree")))){
    cat("Your specified type argument is of type:", typeof(type), "\n")
    cat("and you have written:", type, "\n")
    stop("> specify a character argument with length of 1 containing one of the following words:
         closeness AND / OR betweenness AND / OR eigenvector AND / OR degree")
  }

  mat <- matrix(NA, nrow = length(drawn_CAM), ncol = length(type) + 1)
  for(i in 1:length(drawn_CAM)){
    mat[i,1] <- names(drawn_CAM)[i]

    for(l in 1:length(type)){
      if(type[l]  == "closeness"){
        centr_tmp <- igraph::centr_clo(graph = drawn_CAM[[i]], mode = "all", normalized=TRUE) # undirected graph
        mat[i,l+1] <- paste0(V(drawn_CAM[[i]])$label[centr_tmp$res %in% max(centr_tmp$res)], collapse = ", ")
      }

      if(type[l]  == "degree"){
        centr_tmp <- igraph::centr_degree(graph = drawn_CAM[[i]], mode = "all", normalized=TRUE) # undirected graph
        mat[i,l+1] <- paste0(V(drawn_CAM[[i]])$label[centr_tmp$res %in% max(centr_tmp$res)], collapse = ", ")
      }

      if(type[l] == "betweenness"){
        centr_tmp <- igraph::centr_betw(graph = drawn_CAM[[i]], directed = FALSE, normalized = TRUE) # undirected graph
        mat[i,l+1] <- paste0(V(drawn_CAM[[i]])$label[centr_tmp$res %in% max(centr_tmp$res)], collapse = ", ")

      }

      if(type[l] == "eigenvector"){
        centr_tmp <- igraph::centr_eigen(graph = drawn_CAM[[i]], directed = FALSE, normalized = TRUE) # undirected graph
        mat[i,l+1] <- paste0(V(drawn_CAM[[i]])$label[centr_tmp$vector %in% max(centr_tmp$vector)], collapse = ", ")

      }
    }
  }
  colnames(mat) <- c("CAMs", type)

  if(createHTML){
    stargazer::stargazer(mat, summary = FALSE,
                         out = "centralTerms.html", type = "html")

  }


  mat <- as.data.frame(mat)
  return(mat)
}



############################################################################
##### des_extractComments()
#
############################################################################
### args:
# dat_nodes = CAMfiles[[1]]
des_extractComments <- function(dat_nodes = CAMfiles[[1]]){

  comments_dat_nodes <- dat_nodes %>%
    select(text, comment, CAM) %>%
    filter(!is.na(comment)) %>%
    filter(str_detect(string = comment, pattern = "[:alnum:]")) %>%
    arrange(text)


  return(comments_dat_nodes)
}




#
# tmp <- letters[1]
# for(i in 2:ceiling(x = length(letters)/3)){
#   print(tmp)
#   if(i %% 3 == 0){
#     tmp <- paste0(paste0(c(tmp, letters[i]), collapse = ", "), "\n")
#   }else{
#     tmp <- paste0(c(tmp, letters[i]), collapse = ", ")
#   }
# }
#
#
#
# v$mainPasteIDs <- tmp
# print(v$mainPasteIDs)


