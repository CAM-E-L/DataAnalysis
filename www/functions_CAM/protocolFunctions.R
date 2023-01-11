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
# nodesDat = nodes_raw
overwriteTextNodes <- function(protocolDat, nodesDat) {

  ##
  list_usedWords <- list()
  ## check approximate and search term was used
  if(length(protocolDat$approximateMatching) > 0 && length(protocolDat$searchTerms) > 0){
    list_summarizeTerms <-
      c(protocolDat$approximateMatching, protocolDat$searchTerms)
  }else if(length(protocolDat$approximateMatching) > 0){
    list_summarizeTerms <- protocolDat$approximateMatching
  }else if(length(protocolDat$searchTerms) > 0){
    list_summarizeTerms <- protocolDat$searchTerms
  }


  vec_time <- c()
  for (i in 1:length(list_summarizeTerms)) {
    vec_time[i] <- unlist(list_summarizeTerms[[i]]$time)
  }

  for (t in vec_time) {
    ## current first element
    tmp_first_index <- which(vec_time %in% min(vec_time))


    tmp_current <- list_summarizeTerms[[tmp_first_index]]

    tmp_print <-
      ifelse(
        test = any(names(tmp_current) == "stringDistance"),
        yes = "search terms",
        no = "approximate matching"
      )
    cat("time",
        t,
        "at index",
        tmp_first_index,
        "for",
        tmp_print ,
        "\n")


    tmp_current_words <- unlist(tmp_current$wordsFound)



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
                 tmp_current$supordinateWord,
                 " (",
                 paste0(tmp_positive, collapse = " // "),
                 ")"
               ))
    }
    if(length(tmp_negative) > 0){
      list_usedWords[["negative"]] <-
        append(list_usedWords[["negative"]],
               paste0(
                 tmp_current$supordinateWord,
                 " (",
                 paste0(tmp_negative, collapse = " // "),
                 ")"
               ))
    }
    if(length(tmp_neutral) > 0){
      list_usedWords[["neutral"]] <-
        append(list_usedWords[["neutral"]],
               paste0(
                 tmp_current$supordinateWord,
                 " (",
                 paste0(tmp_neutral, collapse = " // "),
                 ")"
               ))
    }
    if(length(tmp_ambivalent) > 0){
      list_usedWords[["ambivalent"]] <-
        append(list_usedWords[["ambivalent"]],
               paste0(
                 tmp_current$supordinateWord,
                 " (",
                 paste0(tmp_ambivalent, collapse = " // "),
                 ")"
               ))
    }



    for (w in tmp_current_words) {
      # print(w)

      tmp_w <-
        str_remove(string = w, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
      tmp_text_summarized <-
        str_remove(string = nodesDat$text_summarized, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
      if (str_detect(string = w, pattern = "_positive$")) {
        # print("_positive")
        nodesDat$text_summarized[tmp_text_summarized == tmp_w &
                                   nodesDat$value > 0 & nodesDat$value < 10] <-
          paste0(unlist(tmp_current$supordinateWord), "_positive")

      } else if (str_detect(string = w, pattern = "_negative$")) {
        # print("_negative")
        nodesDat$text_summarized[tmp_text_summarized == tmp_w &
                                   nodesDat$value < 0] <-
          paste0(unlist(tmp_current$supordinateWord), "_negative")

      } else if (str_detect(string = w, pattern = "_neutral$")) {
        # print("_neutral")
        nodesDat$text_summarized[tmp_text_summarized == tmp_w &
                                   nodesDat$value == 0] <-
          paste0(unlist(tmp_current$supordinateWord), "_neutral")

      } else if (str_detect(string = w, pattern = "_ambivalent$")) {
        # print("_ambivalent")
        nodesDat$text_summarized[tmp_text_summarized == tmp_w &
                                   nodesDat$value == 10] <-
          paste0(unlist(tmp_current$supordinateWord), "_ambivalent")


      }

    }

    ## remove elements which have been processed
    vec_time <- vec_time[-tmp_first_index]
    list_summarizeTerms <- list_summarizeTerms[-tmp_first_index]
  }

  return(list(nodesDat, list_usedWords))
}
