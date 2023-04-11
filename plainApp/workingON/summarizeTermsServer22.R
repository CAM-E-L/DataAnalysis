output$alreadyUsedWords <- renderDataTable({
  # https://stackoverflow.com/questions/27153979/converting-nested-list-unequal-length-to-data-frame
  indx <- sapply(module_rv$usedWordsAM, length)
  outdat <- as.data.frame(do.call(rbind, lapply(
    module_rv$usedWordsAM, `length<-`,
    max(indx)
  )))
  t(outdat)
})


############################################
############################################
############################################
############################################
overwriteData_getProtocols <- function(protocolCounter = NULL, protocolDetailedOut = NULL,
                                       list_usedWords = NULL,
                                       dataSummarized = NULL,

                                       maxStringDistance = NULL,
                                       label_superordinate = NULL,
                                       label_Pos = NULL,  label_Neg = NULL, label_Neut = NULL, label_Ambi = NULL){


  ##############################################
  ## create temporary vectors / data sets ##
  # avoid adding multiple suffix
  text_summarized_NoSuffix <- str_remove_all(
    string = dataSummarized[[1]]$text_summarized,
    pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
  )
  ##############################################

  ##############################################
  ## set up detailed protocol ##
  # set up protocol
  # positive valence
  tmp_value_protocol_positive <-
    dataSummarized[[1]]$value[text_summarized_NoSuffix %in% input[[label_Pos]] &
                                dataSummarized[[1]]$value > 0 &
                                dataSummarized[[1]]$value < 10]
  # negative valence
  tmp_value_protocol_negative <-
    dataSummarized[[1]]$value[text_summarized_NoSuffix %in% input[[label_Neg]] &
                                dataSummarized[[1]]$value < 0]
  # neutral valence
  tmp_value_protocol_neutral <-
    dataSummarized[[1]]$value[text_summarized_NoSuffix %in% input[[label_Neut]] &
                                dataSummarized[[1]]$value == 0]
  # ambivalent valence
  tmp_value_protocol_ambivalent <-
    dataSummarized[[1]]$value[text_summarized_NoSuffix %in% input[[label_Ambi]] &
                                dataSummarized[[1]]$value == 10]




  tmp_protocol <- data.frame(
    "time" = as.character(as.POSIXct(Sys.time())),
    "searchRound" = protocolCounter,
    "stringDistance" = maxStringDistance,
    "superordinate" = input[[label_superordinate]],
    "subordinate_positive" = paste0(input[[label_Pos]], collapse = " // "),
    "N_positive" = length(tmp_value_protocol_positive),
    "mean_positive" = round(
      x = mean(tmp_value_protocol_positive),
      digits = 2
    ),
    "sd_positive" = round(
      x = sd(tmp_value_protocol_positive),
      digits = 2
    ),
    "subordinate_negative" = paste0(input[[label_Neg]], collapse = " // "),
    "N_negative" = length(tmp_value_protocol_negative),
    "mean_negative" = round(
      x = mean(tmp_value_protocol_negative),
      digits = 2
    ),
    "sd_negative" = round(
      x = sd(tmp_value_protocol_negative),
      digits = 2
    ),
    "subordinate_neutral" = paste0(input[[label_Neut]], collapse = " // "),
    "N_neutral" = length(tmp_value_protocol_neutral),
    "subordinate_ambivalent" = paste0(input[[label_Ambi]], collapse = " // "),
    "N_ambivalent" = length(tmp_value_protocol_ambivalent)
  )



  # save protocol
  if (protocolCounter == 1) {
    protocolDetailedOut <- tmp_protocol
  } else {
    #  tmp <- tmp_protocol
    protocolDetailedOut <-
      rbind(protocolDetailedOut, tmp_protocol)
  }

  protocolCounter <- protocolCounter + 1
  ##############################################



  ##############################################
  ## overwrite summarized words AND set JSON protocol AND get list of already summarized words ##
  tmpWordsSummarized <- c()

  ####### positive valence
  if (!identical(input[[label_Pos]], character(0))) {
    ## remove suffix
    matches_positive_NoSuffix <- str_remove_all(
      string = input[[label_Pos]],
      pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
    )

    ## overwrite summarized words
    dataSummarized[[1]]$text_summarized[text_summarized_NoSuffix %in% matches_positive_NoSuffix &
                                          dataSummarized[[1]]$value > 0 &
                                          dataSummarized[[1]]$value < 10] <-
      paste0(input[[label_superordinate]], "_positive")

    ## vector summarized words for protocolJsonOut
    tmpWordsSummarized <- paste0(matches_positive_NoSuffix, "_positive")

    ## list of already used words
    list_usedWords[["positive"]] <-
      append(list_usedWords[["positive"]],
             paste0(
               input[[label_superordinate]],
               " (",
               paste0(matches_positive_NoSuffix, collapse = " // "),
               ")"
             ))
  }


  #######  negative valence
  if (!identical(input[[label_Neg]], character(0))) {
    ## remove suffix
    matches_negative_NoSuffix <- str_remove_all(
      string = input[[label_Neg]],
      pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
    )

    ## overwrite summarized words
    dataSummarized[[1]]$text_summarized[text_summarized_NoSuffix %in% matches_negative_NoSuffix &
                                          dataSummarized[[1]]$value < 0] <-
      paste0(input[[label_superordinate]], "_negative")

    ## vector summarized words for protocolJsonOut
    if (length(tmpWordsSummarized) == 0) {
      tmpWordsSummarized <- paste0(matches_negative_NoSuffix, "_negative")
    } else{
      tmpWordsSummarized <- c(tmpWordsSummarized, paste0(matches_negative_NoSuffix, "_negative"))
    }

    ## list of already used words
    list_usedWords[["negative"]] <-
      append(list_usedWords[["negative"]],
             paste0(
               input[[label_superordinate]],
               " (",
               paste0(matches_negative_NoSuffix, collapse = " // "),
               ")"
             ))
  }


  ####### neutral valence
  if (!identical(input[[label_Neut]], character(0))) {
    ## remove suffix
    matches_neutral_NoSuffix <- str_remove_all(
      string = input[[label_Neut]],
      pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
    )

    ## overwrite summarized words
    dataSummarized[[1]]$text_summarized[text_summarized_NoSuffix %in% matches_neutral_NoSuffix &
                                          dataSummarized[[1]]$value == 0] <-
      paste0(input[[label_superordinate]], "_neutral")


    ## vector summarized words for protocolJsonOut
    if (length(tmpWordsSummarized) == 0) {
      tmpWordsSummarized <- paste0(matches_neutral_NoSuffix, "_neutral")
    } else{
      tmpWordsSummarized <- c(tmpWordsSummarized, paste0(matches_neutral_NoSuffix, "_neutral"))
    }

    ## list of already used words
    list_usedWords[["neutral"]] <-
      append(list_usedWords[["neutral"]],
             paste0(
               input[[label_superordinate]],
               " (",
               paste0(matches_neutral_NoSuffix, collapse = " // "),
               ")"
             ))
  }


  ####### amvivalent valence
  if (!identical(input[[label_Ambi]], character(0))) {
    ## remove suffix
    matches_ambivalent_NoSuffix <- str_remove_all(
      string = input[[label_Ambi]],
      pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
    )

    ## overwrite summarized words
    dataSummarized[[1]]$text_summarized[text_summarized_NoSuffix %in% matches_ambivalent_NoSuffix &
                                          dataSummarized[[1]]$value == 10] <-
      paste0(input[[label_superordinate]], "_ambivalent")

    ## vector summarized words for protocolJsonOut
    if (length(tmpWordsSummarized) == 0) {
      tmpWordsSummarized <- paste0(matches_ambivalent_NoSuffix, "_ambivalent")
    } else{
      tmpWordsSummarized <-
        c(tmpWordsSummarized, paste0(matches_ambivalent_NoSuffix, "_ambivalent"))
    }

    ## list of already used words
    list_usedWords[["ambivalent"]] <-
      append(list_usedWords[["ambivalent"]],
             paste0(
               input[[label_superordinate]],
               " (",
               paste0(matches_ambivalent_NoSuffix, collapse = " // "),
               ")"
             ))
  }
  ##############################################


  ##############################################
  ## set JSON protocol ##
  protocolJsonOut = vector(mode = "list", length = 4)
  protocolJsonOut[[1]] = as.character(as.POSIXct(Sys.time()))
  protocolJsonOut[[2]] = tmpWordsSummarized
  protocolJsonOut[[3]] = input[[label_superordinate]]
  protocolJsonOut[[4]] = maxStringDistance
  names(protocolJsonOut) <-
    c("time",
      "wordsFound",
      "supordinateWord",
      "stringDistance")



  return(list(summarizedData = dataSummarized,
              counterProtocol = protocolCounter,
              detailedProtocol = protocolDetailedOut,
              jsonProtocol = protocolJsonOut,
              usedWordsList = list_usedWords))
}
