###################################
# vectorWords = CAMfiles[[1]]$text
# syn_dat = syn_English
SynonymList <- function(vectorWords = NULL, syn_dat = NULL){

  singleWords <- vectorWords
  singleWords <- tolower(x = singleWords)
  singleWords <- unique(singleWords)

  ## check if single words are in the dictionary "qdapDictionaries::key.syn"
  singleWords_found <- singleWords[singleWords %in% syn_dat$searched]
  outPercent <- round(x = length(singleWords_found) / length(singleWords) * 100, digits = 2)

  if (identical(singleWords_found, character(0))) {
    return(NULL)
  }

  ### create synonyms
  singleWords_found_list <- list()

  for(w in singleWords_found){
    tmp_snys <- syn_dat$synonyms[str_detect(string = syn_dat$searched, pattern = paste0("^", w, "$"))]
    singleWords_found_list[[w]] <- c(unlist(str_split(string = tmp_snys, pattern = "\\+\\+")), w)
  }

  #print("singleWords_found_list")
  #print(singleWords_found_list)

  ### get overlapping words
  mat_synonym <- matrix(data = NA, nrow = length(singleWords_found), ncol = length(singleWords))
  for(i in 1:length(singleWords_found)){
    mat_synonym[i,] <- singleWords %in% singleWords_found_list[[singleWords_found[i]]]
  }

  colnames(x = mat_synonym) <- singleWords

  out_mat_synonym <- mat_synonym[rowSums(x = mat_synonym) >= 2, ]
  out_mat_synonym <- unique(x = out_mat_synonym)
  # print("DIM out_mat_synonym")
  # print(dim(out_mat_synonym))
  if (nrow(out_mat_synonym) == 0) {
    return(NULL)
  }

  ### reduce number of overlapping words (which in itself are overlapping)
  list_out_mat_synonym <- list()
  for(i in 1:nrow(x = out_mat_synonym)){
    list_out_mat_synonym[[i]] <-
      colnames(x = out_mat_synonym)[out_mat_synonym[i,]]
  }

  return(list(list_out_mat_synonym, outPercent))
}


###################################
SummarizedSynonymList <- function(listSynonyms = NULL){
if(length(listSynonyms) == 1) {
    return(listSynonyms)
} else {
  vec_remove <- NULL
  vec_keep <- NULL
  # print(listSynonyms[[1]])
  for(j in 2:length(x = listSynonyms)){
    if(any(listSynonyms[[1]] %in% listSynonyms[[j]])){
      vec_remove <- c(vec_remove, 1, j)
      # print(listSynonyms[[j]])
      vec_keep <- c(vec_keep, listSynonyms[[1]], listSynonyms[[j]])
    }
  }
  # print(vec_remove)
  if(!is.null(x = vec_remove)){
    for(v in sort(unique(vec_remove), decreasing = TRUE)){
      listSynonyms[[v]] <- NULL
    }

    vec_keep <- sort(unique(x = vec_keep))
    listSynonyms[[length(listSynonyms)+1]] <- vec_keep
  }else{
    listSynonyms[[length(listSynonyms)+1]] <- listSynonyms[[1]]
    listSynonyms[[1]] <- NULL
  }

  return(listSynonyms)
}
}