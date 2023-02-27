



library(qdap)
singleWords <- c("skjdkfdsj", "the", "cat", "job", "environment", "read", "teach", "aback")
singleWords_found <- singleWords[singleWords %in% qdapDictionaries::key.syn$x]
singleWords_found
outPercent <- round(x = length(singleWords_found) / length(singleWords) * 100, digits = 2)
outPercent


qdap::syn(terms = w, return.list = FALSE)

### create synonyms
# syn::syns()
for(w in singleWords_found){
cat("\nfor word -->", w, "<-- following synonyms were found:\n")
 print( qdap::syn(terms = w, return.list = FALSE))
}

?qdapDictionaries::key.syn
qdapDictionaries::key.syn[1,]




library(qdap)
synonyms(c("the", "cat", "job", "environment", "read", "teach"))
syn::syn(word = "help")


qdap::syn(terms = "flag")

library(syn)
syn::syn(word = "help")


data(key.syn)
key.syn$x
dim(key.syn)


# singleWords <- c("the", "cat", "job", "environment", "read", "teach", "climate", "demonstrate")



##################




################################# ! 1 word terms
head(CAMfiles[[1]]$text)
tmpOneWords <- str_split(string = unique(CAMfiles[[1]]$text), pattern = " ", simplify = TRUE)
sum(rowSums(x = tmpOneWords != "") == 1)
round(x = sum(rowSums(x = tmpOneWords != "") == 1) / nrow(tmp), digits = 2) * 100




word(string = CAMfiles[[1]]$text[1], start = 1L, end = 1, sep = fixed(" "))







###################################
RawSynonymList <- function(vectorWords = NULL){

  singleWords <- vectorWords
  singleWords <- tolower(x = singleWords)
  singleWords <- unique(singleWords)

  ## check if single words are in the dictionary
  # data(key.syn) # ???
  singleWords_found <- singleWords[singleWords %in% qdapDictionaries::key.syn$x]
  outPercent <- round(x = length(singleWords_found) / length(singleWords) * 100, digits = 2)


  ### create synonyms
  # syn::syns()
  singleWords_found_list <- list()
  for(w in singleWords_found){
    singleWords_found_list[[w]] <- c(qdap::syn(terms = w, return.list = FALSE), w)
  }


  ### get overlapping words
  mat_synonym <- matrix(data = NA, nrow = length(singleWords_found), ncol = length(singleWords))
  for(i in 1:length(singleWords_found)){
    mat_synonym[i,] <- singleWords %in% singleWords_found_list[[singleWords_found[i]]]
  }


  colnames(x = mat_synonym) <- singleWords

  out_mat_synonym <- mat_synonym[rowSums(x = mat_synonym) >= 2, ]
  out_mat_synonym <- unique(x = out_mat_synonym)
  # dim(out_mat_synonym)


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


raw_SynonymList <- RawSynonymList(vectorWords = CAMfiles[[1]]$text)
raw_SynonymList
reduced_SynonymList <- SummarizedSynonymList(listSynonyms = raw_SynonymList[[1]])
for(i in 1:length(reduced_SynonymList)){
  reduced_SynonymList <- SummarizedSynonymList(listSynonyms = reduced_SynonymList)
}


reduced_SynonymList


tmp_text <- CAMfiles[[1]]$text
tmp_text <- tolower(x = tmp_text)

tmp_text[tmp_text %in% reduced_SynonymList[[1]]]
CAMfiles[[1]][tmp_text %in% reduced_SynonymList[[1]], ]


CAMfiles[[1]][tmp_text %in% reduced_SynonymList[[2]], ]


CAMfiles[[1]][tmp_text %in% reduced_SynonymList[[3]], ]




reduced_SynonymList



labels_out <- reduced_SynonymList[[4]]
CAMfiles[[1]][tmp_text %in% labels_out, ]


length(raw_SynonymList[[1]])
length(reduced_SynonymList)






labels_out <- reduced_SynonymList[[1]]



