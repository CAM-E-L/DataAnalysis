# ==============================================================================
# R-Code - CAM
# date of creation: December 2021
# authors: Julius Fenn
# function: draw_CAM()
# ==============================================================================


############################################################################
##### testIfJson()
# test is a proper JSON file was uploaded as protocol
############################################################################
testIfJson <- function (file) {
  result <-
    suppressMessages(try(jsonlite::fromJSON(file), silent = TRUE)
    )
  # print(result)
  if (class(result) != "try-error") {
    return(TRUE)
  } else{
    return(FALSE)
  }
}


############################################################################
##### overwriteTextNodes()
# if summarize terms module was used overwrite raw node dataset
############################################################################
# protocolDat = protocol
# nodesDat = CAMfiles[[1]]
overwriteTextNodes <- function(protocolDat, nodesDat) {

  ##############################################
  ## merge summarzing functions approximate matchting, search terms, ... AND adjust encoding ##
  list_summarizeTerms <- c(protocolDat$approximateMatching,
  protocolDat$searchTerms,
  protocolDat$findSynonyms,
  protocolDat$modelwordVec)

  ## right encoding
  for(i in 1:length(list_summarizeTerms)){
    Encoding(x = list_summarizeTerms[[i]]$wordsFound) <- "latin1"
    Encoding(x = list_summarizeTerms[[i]]$superordinateWord) <- "latin1"
  }
  ##############################################


  ##############################################
  ## remove suffix
  tmp_text_summarized <-
    str_remove(string = nodesDat$text_summarized, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")

  ## list to collect already used words
  list_usedWords <- list()

  ## get vector of time
  vec_time <- c()
  for (i in 1:length(list_summarizeTerms)) {
    vec_time[i] <- unlist(list_summarizeTerms[[i]]$time)
  }
  vec_time_sorted <- sort(vec_time)
  ##############################################

  # cbind(vec_time, vec_time_sorted)


  cat("\n")



  # for (t in 1:length(vec_time)) {
    # print(t)
   #  tmp_first_index <- which(vec_time_sorted[t] == vec_time)
    # print(tmp_first_index)
   #  }


  r = 1; timeBefore = NULL
  for (t in 1:length(vec_time)) {
    ##############################################
    ## get current first element ##
    # tmp_first_index <- which(vec_time %in% vec_time[t])
    # tmp_first_index <- which(vec_time[t] == vec_time)
    tmp_first_index <- which(vec_time_sorted[t] == vec_time)


    ## if people summarized words too fast multiple indices are the smallest
    if(t >= 2){ # r counter is not increased too much
      if(r >= 3 & vec_time[t] != vec_time[t-1]){
        r = 1
      }
    }
    if(length(tmp_first_index) > 1){
      tmp_current <- list_summarizeTerms[[tmp_first_index[r]]]
    }else{
      tmp_current <- list_summarizeTerms[[tmp_first_index]]
    }

    ####
    ## print to console which operation is running:
    if(any(names(tmp_current) == "stringDistance")){
      tmp_print <- "approximate matching"
    }else  if(any(names(tmp_current) == "regularExpression")){
      tmp_print <- "search terms"
    }else  if(any(names(tmp_current) == "noneSearchArgumentSynonyms")){
      tmp_print <- "synonyms"
    }else  if(any(names(tmp_current) == "noneSearchArgumentWordVec")){
      tmp_print <- "word2vec"
    }

    cat("time",
        vec_time[t],
        "at index",
        tmp_first_index[r],
        "for",
        tmp_print,
        "\n")
    ####

    ## reset r counter
    if(length(tmp_first_index) > 1){
      r = r + 1
    }else{
      r = 1
    }
    ##############################################

    ##############################################
    ## overwrite words in data set ##
    tmp_current_words <- unlist(tmp_current$wordsFound)

    for (w in tmp_current_words) {
      tmp_w <-
        str_remove(string = w, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")

      if (str_detect(string = w, pattern = "_positive$")) {
        # print("_positive")
        nodesDat$text_summarized[tmp_text_summarized == tmp_w &
                                   nodesDat$value > 0 & nodesDat$value < 10] <-
          paste0(unlist(tmp_current$superordinateWord), "_positive")

      } else if (str_detect(string = w, pattern = "_negative$")) {
        # print("_negative")
        nodesDat$text_summarized[tmp_text_summarized == tmp_w &
                                   nodesDat$value < 0] <-
          paste0(unlist(tmp_current$superordinateWord), "_negative")

      } else if (str_detect(string = w, pattern = "_neutral$")) {
        # print("_neutral")
        nodesDat$text_summarized[tmp_text_summarized == tmp_w &
                                   nodesDat$value == 0] <-
          paste0(unlist(tmp_current$superordinateWord), "_neutral")

      } else if (str_detect(string = w, pattern = "_ambivalent$")) {
        # print("_ambivalent")
        nodesDat$text_summarized[tmp_text_summarized == tmp_w &
                                   nodesDat$value == 10] <-
          paste0(unlist(tmp_current$superordinateWord), "_ambivalent")
      }
    }
    ##############################################



    ##############################################
    ## create list of already used words ##
    tmp_positive <- str_remove_all(string = str_subset(string = tmp_current_words, pattern = "_positive$"),
                                   pattern =  "_positive$")
    tmp_negative <- str_remove_all(string = str_subset(string = tmp_current_words, pattern = "_negative$"),
                                   pattern =  "_negative$")
    tmp_neutral <- str_remove_all(string = str_subset(string = tmp_current_words, pattern = "_neutral$"),
                                  pattern =  "_neutral$")
    tmp_ambivalent <- str_remove_all(string = str_subset(string = tmp_current_words, pattern = "_ambivalent$"),
                                     pattern =  "_ambivalent$")

    if(length(tmp_positive) > 0){
      list_usedWords[["positive"]] <-
        append(list_usedWords[["positive"]],
               paste0(
                 tmp_current$superordinateWord,
                 " (",
                 paste0(tmp_positive, collapse = " // "),
                 ")"
               ))
    }
    if(length(tmp_negative) > 0){
      list_usedWords[["negative"]] <-
        append(list_usedWords[["negative"]],
               paste0(
                 tmp_current$superordinateWord,
                 " (",
                 paste0(tmp_negative, collapse = " // "),
                 ")"
               ))
    }
    if(length(tmp_neutral) > 0){
      list_usedWords[["neutral"]] <-
        append(list_usedWords[["neutral"]],
               paste0(
                 tmp_current$superordinateWord,
                 " (",
                 paste0(tmp_neutral, collapse = " // "),
                 ")"
               ))
    }
    if(length(tmp_ambivalent) > 0){
      list_usedWords[["ambivalent"]] <-
        append(list_usedWords[["ambivalent"]],
               paste0(
                 tmp_current$superordinateWord,
                 " (",
                 paste0(tmp_ambivalent, collapse = " // "),
                 ")"
               ))
    }
    ##############################################
  }

  return(list(nodesDat, list_usedWords))
}
