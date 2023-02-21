## set protocol and overwrite data:
observeEvent(input$a_synonymsClickSummarize, {
  if (!isTRUE(synonym_rv$skip)) {
    ## avoid adding multiple suffix
    text_summarized_NoSuffix <- str_remove_all(
      string = module_rv$df[[1]]$text_summarized,
      pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
    )
    # positive valence
    tmp_value_protocol_positive <-
      module_rv$df[[1]]$value[text_summarized_NoSuffix %in% input$matches_positive_synonyms &
                                module_rv$df[[1]]$value > 0 &
                                module_rv$df[[1]]$value < 10]
    # negative valence
    tmp_value_protocol_negative <-
      module_rv$df[[1]]$value[text_summarized_NoSuffix %in% input$matches_negative_synonyms &
                                module_rv$df[[1]]$value < 0]
    # neutral valence
    tmp_value_protocol_neutral <-
      module_rv$df[[1]]$value[text_summarized_NoSuffix %in% input$matches_neutral_synonyms &
                                module_rv$df[[1]]$value == 0]
    # ambivalent valence
    tmp_value_protocol_ambivalent <-
      module_rv$df[[1]]$value[text_summarized_NoSuffix %in% input$matches_ambivalent_synonyms &
                                module_rv$df[[1]]$value == 10]
    # tmp_value_protocol_ambivalent <- ifelse(test = tmp_value_protocol_ambivalent == 10, yes = 0, no = tmp_value_protocol_ambivalent)


    tmp_protocol <- data.frame(
      "time" = as.character(as.POSIXct(Sys.time())),
      "searchRound" = synonym_rv$protocolCounter,
      "Synonyms" = paste0( reducedSynonymList()[[synonym_rv$counterAmbi]], collapse = " // "), # ???
      "superordinate" = input$a_synonymsSupordinateWord,
      "subordinate_positive" = paste0(input$matches_positive_synonyms, collapse = " // "),
      "N_positive" = length(tmp_value_protocol_positive),
      "mean_positive" = round(
        x = mean(tmp_value_protocol_positive),
        digits = 2
      ),
      "sd_positive" = round(
        x = sd(tmp_value_protocol_positive),
        digits = 2
      ),
      "subordinate_negative" = paste0(input$matches_negative_synonyms, collapse = " // "),
      "N_negative" = length(tmp_value_protocol_negative),
      "mean_negative" = round(
        x = mean(tmp_value_protocol_negative),
        digits = 2
      ),
      "sd_negative" = round(
        x = sd(tmp_value_protocol_negative),
        digits = 2
      ),
      "subordinate_neutral" = paste0(input$matches_neutral_synonyms, collapse = " // "),
      "N_neutral" = length(tmp_value_protocol_neutral),
      "subordinate_ambivalent" = paste0(input$matches_ambivalent_synonyms, collapse = " // "),
      "N_ambivalent" = length(tmp_value_protocol_ambivalent)
    )


    # print("tmp_protocol")
    # print(tmp_protocol)
    if (synonym_rv$protocolCounter == 1) {
      synonym_rv$protocol <- tmp_protocol
    } else {
      #  tmp <- tmp_protocol
      synonym_rv$protocol <-
        rbind(synonym_rv$protocol, tmp_protocol)
    }

    #print("synonym_rv$protocol:")
    #print(synonym_rv$protocol)
    synonym_rv$protocolCounter <- synonym_rv$protocolCounter + 1



    ## save changed text in
    tmpWordsSummarized <- c()
    if (!identical(input$matches_positive_synonyms, character(0))) {
      ## remove suffix
      matches_positive_NoSuffix <- str_remove_all(
        string = input$matches_positive_synonyms,
        pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
      )

      module_rv$df[[1]]$text_summarized[text_summarized_NoSuffix %in% matches_positive_NoSuffix &
                                          module_rv$df[[1]]$value > 0 &
                                          module_rv$df[[1]]$value < 10] <-
        paste0(input$a_synonymsSupordinateWord, "_positive")

      tmpWordsSummarized <- paste0(matches_positive_NoSuffix, "_positive")
      # which words have already been used?
      # module_rv$usedWordsAM[["positive"]] <- append(module_rv$usedWordsAM[["positive"]] , input$a_synonymsSupordinateWord)
      module_rv$usedWordsAM[["positive"]] <-
        append(module_rv$usedWordsAM[["positive"]],
               paste0(
                 input$a_synonymsSupordinateWord,
                 " (",
                 paste0(matches_positive_NoSuffix, collapse = " // "),
                 ")"
               ))


    }
    if (!identical(input$matches_negative_synonyms, character(0))) {
      ## remove suffix
      matches_negative_NoSuffix <- str_remove_all(
        string = input$matches_negative_synonyms,
        pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
      )
      module_rv$df[[1]]$text_summarized[text_summarized_NoSuffix %in% matches_negative_NoSuffix &
                                          module_rv$df[[1]]$value < 0] <-
        paste0(input$a_synonymsSupordinateWord, "_negative")

      if (length(tmpWordsSummarized) == 0) {
        tmpWordsSummarized <- paste0(matches_negative_NoSuffix, "_negative")
      } else{
        tmpWordsSummarized <- c(tmpWordsSummarized, paste0(matches_negative_NoSuffix, "_negative"))
      }

      # which words have already been used?
      module_rv$usedWordsAM[["negative"]] <-
        append(module_rv$usedWordsAM[["negative"]],
               paste0(
                 input$a_synonymsSupordinateWord,
                 " (",
                 paste0(matches_negative_NoSuffix, collapse = " // "),
                 ")"
               ))
    }
    if (!identical(input$matches_neutral_synonyms, character(0))) {
      ## remove suffix
      matches_neutral_NoSuffix <- str_remove_all(
        string = input$matches_neutral_synonyms,
        pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
      )
      module_rv$df[[1]]$text_summarized[text_summarized_NoSuffix %in% matches_neutral_NoSuffix &
                                          module_rv$df[[1]]$value == 0] <-
        paste0(input$a_synonymsSupordinateWord, "_neutral")

      if (length(tmpWordsSummarized) == 0) {
        tmpWordsSummarized <- paste0(matches_neutral_NoSuffix, "_neutral")
      } else{
        tmpWordsSummarized <- c(tmpWordsSummarized, paste0(matches_neutral_NoSuffix, "_neutral"))
      }
      # which words have already been used?
      module_rv$usedWordsAM[["neutral"]] <-
        append(module_rv$usedWordsAM[["neutral"]],
               paste0(
                 input$a_synonymsSupordinateWord,
                 " (",
                 paste0(matches_neutral_NoSuffix, collapse = " // "),
                 ")"
               ))
    }
    if (!identical(input$matches_ambivalent_synonyms, character(0))) {
      ## remove suffix
      matches_ambivalent_NoSuffix <- str_remove_all(
        string = input$matches_ambivalent_synonyms,
        pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
      )
      module_rv$df[[1]]$text_summarized[text_summarized_NoSuffix %in% matches_ambivalent_NoSuffix &
                                          module_rv$df[[1]]$value == 10] <-
        paste0(input$a_synonymsSupordinateWord, "_ambivalent")

      if (length(tmpWordsSummarized) == 0) {
        tmpWordsSummarized <- paste0(matches_ambivalent_NoSuffix, "_ambivalent")
      } else{
        tmpWordsSummarized <-
          c(tmpWordsSummarized, paste0(matches_ambivalent_NoSuffix, "_ambivalent"))
      }
      # which words have already been used?
      module_rv$usedWordsAM[["ambivalent"]] <-
        append(module_rv$usedWordsAM[["ambivalent"]],
               paste0(
                 input$a_synonymsSupordinateWord,
                 " (",
                 paste0(matches_ambivalent_NoSuffix, collapse = " // "),
                 ")"
               ))
    }

    #print(module_rv$df[[1]])
    #print("tmpWordsSummarized:")
    #print(tmpWordsSummarized)

  #   tmp_list = vector(mode = "list", length = 4)
  #   tmp_list[[1]] = as.character(as.POSIXct(Sys.time()))
  #   tmp_list[[2]] = tmpWordsSummarized
  #   tmp_list[[3]] = input$a_synonymsSupordinateWord
  #   tmp_list[[4]] = input$maxStringDis
  #   names(tmp_list) <-
  #     c("time",
  #       "wordsFound",
  #       "supordinateWord",
  #       "stringDistance")
  #   globals$protocol$approximateMatching[[length(globals$protocol$approximateMatching) + 1]] <-
  #     tmp_list
  #
  #   #> change condition
  #   globals$condition <-
  #     c(globals$condition, "approximateMatching")

  }
})
