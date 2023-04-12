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
    print("Your specified vars argument is null.")
    stop("> please specify a variable / multiple variables")
  }
  if(!all(vars %in% colnames(indicatos_CAM))){
    print("Your specified vars argument is:")
    print(vars)
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
##### getDescriptives()
#
############################################################################
################################ !!! in function: summary statistics
getDescriptives <- function(dataset = CAMindicators,
                            nameAPAtable = NULL){


  vec_names <- dataset %>%
    select_if(Negate(is.character)) %>%
    colnames()


  x <- dataset[, vec_names]


  ## table
  tmp_descriptives <- sapply(x, function(x) c(
    "Mean"= mean(x,na.rm=TRUE),
    "SD" = sd(x,na.rm=TRUE),
    "Median" = median(x,na.rm=TRUE),
    "CoeffofVariation" = sd(x)/mean(x,na.rm=TRUE),
    "Minimum" = min(x,na.rm=TRUE),
    "Maximun" = max(x,na.rm=TRUE),
    "Lower Quantile" = as.numeric(quantile(x,0,na.rm=TRUE)),
    "Upper Quantile" = as.numeric(quantile(x,1,na.rm=TRUE)),
    "Skewness" = moments::skewness(x = x,na.rm=TRUE),
    "Kurtosis(-3)" = moments::kurtosis(x = x,na.rm=TRUE) -3,
    "KS-Test" = ks.test(x = x, y = "pnorm", mean(x,na.rm=TRUE), sd(x,na.rm=TRUE))$p.value
  )
  )
  tmp_descriptives <- round(x = tmp_descriptives, digits = 2)

  # print(t(tmp_descriptives))


  ## round digits
  out_tmp <- round(x = t(tmp_descriptives), digits = 2)

  ## stargazer
  if(!is.null(nameAPAtable)){
    #notNeeded <- capture.output(stargazer(out_tmp, type = "html", summary = FALSE,
    #                                      out = paste0(nameAPAtable, ".html")))
     notNeeded <- capture.output(stargazer(out_tmp, type = "html", summary = FALSE,
                                          out = nameAPAtable,))
  }

    notNeeded <- capture.output(stargazer(out_tmp, type = "html", summary = FALSE))
  # return(out_tmp)
  return(notNeeded)
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


