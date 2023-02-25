
summarizeTermsServer <-
  function(id, dataCAM, drawnCAM, parent, globals) {
    moduleServer(id, function(input, output, session) {
      ns <- NS(id)

      ## reactive values
      outUI <- reactiveValues(elements = NULL)


      ################
      # default text + set output
      ################
      ## default text
      outUI$elements <- tagList(tags$div(
        h1("Summarize terms module"),
        tags$br(),
        tags$br(),
        HTML(
          'To start the module please click on one of the the module option on the sidebar panel. The options for
                     this module are the following:'
        ),
        tags$ul(
          tags$li(
            HTML(
              '<b>Approximate matching:</b> By using approximate string matching you can compute the string distances between all your unique concepts
              in the dataset (using optimal string alignment) to find words, which have been written slightly differently.'
            )
          ),
          tags$li(
            HTML(
              '<b>Searching terms:</b> Use regular expression to search your CAM data set for similar terms.'
            )
          ),
                    tags$li(
            HTML(
              '<b>Search for Synonyms:</b> Summarize concepts by searching for synonyms.'
            )
          ),
                    tags$li(
            HTML(
              '<b>Apply word2vec model:</b> The word2vec is a neural network, which learnd word associations from a large corpus of text.
              Based on these word associations get groups of similar terms (e.g. "happiness" is similar to "joy").'
            )
          ),
          tags$li(
            HTML(
              '<b>Information:</b> Further information regarding this module.'
            )
          )
        )
      ))

      ## set output
      output$ST_out <- renderUI({
        outUI$elements
      })


            ################################
      # global module options
      ################################
            #> Server
            ## local reactive values
      ST_rv <-
        reactiveValues(
         counter = 0L,
         skip = FALSE,
          protocolCounter = 1L,
          # protocol = NULL,
          # usedWords = list()
        )



      ## local functions
    ###############################
  getSuperordinateWord <- function(label_superordinate = NULL,
                                 label_Pos = NULL,  label_Neg = NULL, label_Neut = NULL, label_Ambi = NULL){
  if (!identical(input[[label_Pos]], character(0))) {
    updateTextInput(
      session,
      label_superordinate,
      value = str_remove_all(
        string = input[[label_Pos]][1],
        pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
      )
    )
  } else if (!identical(input[[label_Neg]], character(0))) {
    updateTextInput(
      session,
      label_superordinate,
      value = str_remove_all(
        string = input[[label_Neg]][1],
        pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
      )
    )
  } else if (!identical(input[[label_Neut]], character(0))) {
    updateTextInput(
      session,
      label_superordinate,
      value = str_remove_all(
        string = input[[label_Neut]][1],
        pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
      )
    )
  } else if (!identical(input[[label_Ambi]], character(0))) {
    updateTextInput(
      session,
      label_superordinate,
      value = str_remove_all(
        string = input[[label_Ambi]][1],
        pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
      )
    )
  }
}
###############################

###############################
overwriteData_getProtocols <- function(protocolCounter = NULL, protocolDetailedOut = NULL,
                                       list_usedWords = NULL,
                                       dataSummarized = NULL,
                                       
                                       maxStringDistance = NULL,
                                       label_superordinate = NULL,
                                       label_Pos = NULL,  label_Neg = NULL, label_Neut = NULL, label_Ambi = NULL){
  
  

if(all(c(identical(input[[label_Pos]], character(0)), 
identical(input[[label_Neg]], character(0)), 
identical(input[[label_Neut]], character(0)), 
identical(input[[label_Ambi]], character(0))))){
  print("No further words found, at the end of search round.")
  return(NULL)
}

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
      "superordinateWord",
      "stringDistance")
  ##############################################

  outList <- list(summarizedData = dataSummarized,
                  counterProtocol = protocolCounter,
                  detailedProtocol = protocolDetailedOut,
                  jsonProtocol = protocolJsonOut,
                  usedWordsList = list_usedWords)
  return(outList)
}
###############################


## local resetting
observeEvent(c(
  input$ST_approxMatch,
  input$ST_searchTerms,
  input$ST_synonyms,
  input$ST_wordVec
), {
  req(drawnCAM())
  ## reset global counter
  ST_rv$counter <- 0
  print("ST_rv$counter")
  print(ST_rv$counter)

  # print("globals$protocol$currentCAMs")
      # print(globals$protocol$currentCAMs)

    print("globals$protocol$deletedCAMs")
    print(globals$protocol$deletedCAMs)

})

            ################################
      # single module options
      ################################
        ###### approximate matching ######
                #> UI
observeEvent(input$ST_approxMatch, {
  ## change UI
  outUI$elements <- tagList(
    tags$h2("Summarize by approximate matching"),
    tags$br(),
    tags$div(
      HTML(
        "By using approximate string matching you can compute the string distances between all your unique concepts
              in the dataset (using optimal string aligment) to find words, which have been written slightly differently."
      ),
      style = "font-size:14px"
    ),
    tags$br(),
    tags$h3("Your Settings:"),
    tags$div(
      HTML(
        "Please select the maximum string distance (recommended: less than five) and
                click on button to start the approximate string matching. Please click only once and wait few seconds:"
      ),
      style = "font-size:14px"
    ),
    div(
      style = "margin: 0 auto; width: 100%; text-align:left;",
      div(
        style = "display: inline-block; vertical-align: top;",
        numericInput(
          ns("maxStringDis"),
          NULL,
          value = 2,
          width = "70px",
          min = 1,
          max = 6
        )
      ),
      actionButton(
        ns("clickAproxStriMatch"),
        "start approximate string matching",
        style = "display: inline-block;"
      ),
    ),
    tags$p(
      "Your uploaded CAM dataset contains ",
      tags$b(textOutput(ns(
        "Nodes_unique"
      ), inline = TRUE), " unique concepts"),
      " , whereby on your choosen setting ",
      tags$b(
        textOutput(ns("Nodes_matches"), inline = TRUE),
        " cases, where you can apply approximate string matching, were found."
      )),
    tags$br(),
    tags$p(
      "Word for which matches were found: ",
      tags$b(textOutput(
        ns("Nodes_matchesSinWor"), inline = TRUE
      )),
      HTML("&nbsp;&nbsp;"),
      " > ",
      tags$b(textOutput(
        ns("Nodes_matchesRounds"), inline = TRUE
      )),
      " more words to summarize"
    ),
    tags$br(),
    tags$div(
      HTML("Keep the words you want to summarize in the right lists:"),
      style = "font-size:18px"
    ),
    tags$div(
      HTML(
        '<i>The first word is automatically selected. If it contains a spelling error, you can correct it manually.
                                Important: all words will be saved in the following format: "Word_positive" for words with positive valence,
                                "Word_negative" for words with negative valence and so on.
                                 If you do not want to summarize a specific word please move it to the most left column.
                                 If you do not want to summarize any words just click on "skip":</i>'
      ),
      style = "font-size:14px"
    ),
    htmlOutput(ns("bucketlist")),
    fluidRow(
      column(
        width = 6,
        offset = 5,
        textInput(
          ns("supordinateWord_appMatch"),
          "Superordinate word:",
          placeholder = "all words, which are not in the most left column",
          width = "300px"
        ),
        actionButton(ns("clickSummarize"), "summarize"),
        actionButton(ns("clickSkip"), "skip")
      )
    ),
    tags$br(),
    tags$p(
      "Your CAM dataset contains ",
      tags$b(textOutput(ns(
        "Nodes_unique_after"
      ), inline = TRUE)),
      " unique concepts AFTER summarzing (ignoring valence)."
    ),
    tags$div(
      HTML(
        'The following table shows you which words you have already summarized. You can use the search functionalities of the table:'
      ),
      style = "font-size:14px"
    ),
    dataTableOutput(ns("alreadyUsedWords")),
  )
})




#> Server
###############################
### create data set for approximate matching
optimalMatchSim <-
  eventReactive(c(
    input$clickAproxStriMatch,
      input$ST_approxMatch), { ## initialize by clicking on sidebar
        req(drawnCAM())


      message("The value of input$clickAproxStriMatch is ",
              input$clickAproxStriMatch)



      ## reset counter:
      ST_rv$counter <- 0L # ???
      #ST_rv$counterPos <- 0L
      #ST_rv$counterNeg <- 0L
      #ST_rv$counterNeutral <- 0L
      #ST_rv$counterAmbi <- 0L

      nodes_text <- unique(globals$dataCAMsummarized[[1]]$text_summarized)
      nodes_text <-
        unique(stringr::str_trim(nodes_text, side = "both"))
      h <- 1

      ## data dummy if no additional matches were found
      dat_out <- data.frame(num = 1, word = "no word found")
      for (i in 1:length(nodes_text)) {
        tmp_allother <- nodes_text[nodes_text != nodes_text[i]]
        tmp_optimalmatching <-
          stringdist::stringdist(nodes_text[i], tmp_allother, method = "osa")
        if (any(tmp_optimalmatching <= input$maxStringDis)) {
          tmp_match <-
            sort(c(nodes_text[i], tmp_allother[tmp_optimalmatching <= input$maxStringDis]))
          if (h == 1) {
            dat_out <-
              data.frame(num = rep(h, times = length(tmp_match)),
                         word = tmp_match)
            h <- h + 1
          } else {
            if (!all(tmp_match %in% dat_out$word)) {
              dat_out <- rbind(dat_out,
                               data.frame(
                                 num = rep(h, times = length(tmp_match)),
                                 word = tmp_match
                               ))
              h <- h + 1
            }
          }
        }
      }

print("dat_out")
print(head(dat_out))
      dat_out
    })
###############################

###############################
### show number of
# > unique concepts
output$Nodes_unique <- renderText({
  length(unique(globals$dataCAMsummarized[[1]]$text))
})
# > found matches
output$Nodes_matches <- renderText({
  max(optimalMatchSim()$num)
})
# > word to match:
output$Nodes_matchesSinWor <- renderText({
  str_remove_all(string =  optimalMatchSim()$word[optimalMatchSim()$num == ST_rv$counter][1],
                 pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
})

# > rounds to click:
output$Nodes_matchesRounds <- renderText({
  max(optimalMatchSim()$num) - ST_rv$counter + 1
})

## implement skip functionality
observeEvent(input$clickSkip, {
  ST_rv$skip <- TRUE
  })
  
## number of nodes after summarizing words
output$Nodes_unique_after <- renderText({
   tmp_text_summarized <- str_remove_all(string = globals$dataCAMsummarized[[1]]$text_summarized,
   pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
   length(unique(tmp_text_summarized))
   })
###############################

###############################
### create labels for bucket list AND render bucketlist AND update superordinate word
    ## create labels for bucket list ##
# > words with positive valence:
labels_positive_appMatch <-
  eventReactive(c(
    input$clickAproxStriMatch,
    input$clickSummarize,
    input$clickSkip
  ),
  {
    req(optimalMatchSim())

  tmp_labels_out <- getlabels(typeLabels = "positive",
  getInput = optimalMatchSim(), 
  counter = ST_rv$counter, skipCond = ST_rv$skip,
  dataSummarized = globals$dataCAMsummarized)

  ST_rv$counter <- tmp_labels_out[[2]] # update counter
  ST_rv$skip <- tmp_labels_out[[3]] # update skip condition

tmp_labels_out[[1]]
  })

  # > words with negative valence:
labels_negative_appMatch <-
  eventReactive(c(
    input$clickAproxStriMatch,
    input$clickSummarize,
    input$clickSkip
  ),
  {
    req(optimalMatchSim())

  tmp_labels_out <- getlabels(typeLabels = "negative",
  getInput = optimalMatchSim(), 
  counter = ST_rv$counter-1, skipCond = ST_rv$skip,
  dataSummarized = globals$dataCAMsummarized)

  ST_rv$counter <- tmp_labels_out[[2]] # update counter
  ST_rv$skip <- tmp_labels_out[[3]] # update skip condition

tmp_labels_out[[1]]
  })

  # > words with neutral valence:
  labels_neutral_appMatch <-
  eventReactive(c(
    input$clickAproxStriMatch,
    input$clickSummarize,
    input$clickSkip
  ),
  {
    req(optimalMatchSim())

  tmp_labels_out <- getlabels(typeLabels = "neutral",
  getInput = optimalMatchSim(), 
  counter = ST_rv$counter-1, skipCond = ST_rv$skip,
  dataSummarized = globals$dataCAMsummarized)

  ST_rv$counter <- tmp_labels_out[[2]] # update counter
  ST_rv$skip <- tmp_labels_out[[3]] # update skip condition

tmp_labels_out[[1]]
  })

  # > words with ambivalent valence:
  labels_ambivalent_appMatch <-
  eventReactive(c(
    input$clickAproxStriMatch,
    input$clickSummarize,
    input$clickSkip
  ),
  {
    req(optimalMatchSim())

  tmp_labels_out <- getlabels(typeLabels = "ambivalent",
  getInput = optimalMatchSim(), 
  counter = ST_rv$counter-1, skipCond = ST_rv$skip,
  dataSummarized = globals$dataCAMsummarized)

  ST_rv$counter <- tmp_labels_out[[2]] # update counter
  ST_rv$skip <- tmp_labels_out[[3]] # update skip condition

tmp_labels_out[[1]]
  })

        ## render bucket list ##
      output$bucketlist <- renderUI({
        bucket_list(
          header = NULL,
          group_name = ns("bucket_list_group2"),
          orientation = "horizontal",
          add_rank_list(
            text = "move here to not summarize single words",
            labels = NULL,
            # labels from row selection
            input_id = ns("matches_suggestion_appMatch"),
            options = sortable_options(multiDrag = TRUE)
          ),
          add_rank_list(
            text = "suggested similar words with POSITIVE valence:",
            labels = labels_positive_appMatch(),
            input_id = ns("matches_positive_appMatch"),
            options = sortable_options(multiDrag = TRUE)
          ),
          add_rank_list(
            text = "suggested similar words with NEGATIVE valence:",
            labels = labels_negative_appMatch(),
            input_id = ns("matches_negative_appMatch"),
            options = sortable_options(multiDrag = TRUE)
          ),
          add_rank_list(
            text = "suggested similar words with NEUTRAL valence:",
            labels = labels_neutral_appMatch(),
            input_id = ns("matches_neutral_appMatch"),
            options = sortable_options(multiDrag = TRUE)
          ),
          add_rank_list(
            text = "suggested similar words with AMBIVALENT valence:",
            labels = labels_ambivalent_appMatch(),
            input_id = ns("matches_ambivalent_appMatch"),
            options = sortable_options(multiDrag = TRUE)
          )
        )
      })

## update superordinate word ##
            observe({
                req(drawnCAM())
getSuperordinateWord(label_superordinate = "supordinateWord_appMatch", 
label_Pos = "matches_positive_appMatch", 
label_Neg = "matches_negative_appMatch", 
label_Neut = "matches_neutral_appMatch", 
label_Ambi = "matches_ambivalent_appMatch")


      })
      observeEvent(input$ST_approxMatch, { # start when clicked on sidebar panel
        req(drawnCAM())
 getSuperordinateWord(label_superordinate = "supordinateWord_appMatch", 
label_Pos = "matches_positive_appMatch", 
label_Neg = "matches_negative_appMatch", 
label_Neut = "matches_neutral_appMatch", 
label_Ambi = "matches_ambivalent_appMatch")
      })

###############################

###############################
### overwrite summarized words AND set JSON protocol // get detailed protocol AND list of used words
## set protocol and overwrite data:
observeEvent(input$clickSummarize, {
        req(drawnCAM())

print("summarize - skip")
print(ST_rv$skip)
  if (!isTRUE(ST_rv$skip)) {
    tmp_overwriteData_getProtocols <- overwriteData_getProtocols(protocolCounter = ST_rv$protocolCounter,
                               protocolDetailedOut = globals$detailedProtocolAM, # !!!
                               list_usedWords = globals$usedWords,
                               dataSummarized = globals$dataCAMsummarized,

                               maxStringDistance = input$maxStringDis,
                               label_superordinate = "supordinateWord_appMatch", 
                               label_Pos = "matches_positive_appMatch", 
                               label_Neg = "matches_negative_appMatch", 
                               label_Neut = "matches_neutral_appMatch", 
                               label_Ambi = "matches_ambivalent_appMatch")

    # print("names(tmp_overwriteData_getProtocols)")
    # print(names(tmp_overwriteData_getProtocols))

    if(!is.null(tmp_overwriteData_getProtocols)){
    ## overwrite global data
    globals$dataCAMsummarized <- tmp_overwriteData_getProtocols$summarizedData
    ## overwrite protocol counter (detailed)
    ST_rv$protocolCounter <- tmp_overwriteData_getProtocols$counterProtocol
     ## overwrite detailed protocol
    globals$detailedProtocolAM <- tmp_overwriteData_getProtocols$detailedProtocol
     ## overwrite already used words
    globals$usedWords <- tmp_overwriteData_getProtocols$usedWordsList

     ## append JSON protocol
    globals$protocol$approximateMatching[[length(globals$protocol$approximateMatching) + 1]] <- 
    tmp_overwriteData_getProtocols$jsonProtocol
    #> change condition
    globals$condition <- c(globals$condition, "approximateMatching")

    # print("length(unique(globals$dataCAMsummarized[[1]]$text_summarized))")
    # print(length(unique(globals$dataCAMsummarized[[1]]$text_summarized)))
    }
  }
})


output$alreadyUsedWords <- renderDataTable({
  # https://stackoverflow.com/questions/27153979/converting-nested-list-unequal-length-to-data-frame
  indx <- sapply(globals$usedWords, length)
  outdat <- as.data.frame(do.call(rbind, lapply(
    globals$usedWords, `length<-`,
    max(indx)
  )))
  t(outdat)
})


###############################
###############################
###############################


        ###### searching terms ######
                #> UI
      observeEvent(input$ST_searchTerms, {
        ## change UI
        outUI$elements <-
          tagList(tags$h2("Summarize by searching terms"),
                  tags$div(
                    HTML(
                      "By using so called regular expression you can search your CAM concepts for semantically identical or similar terms
              (high likeness of their meaning). For more details see Information."
                    ),
              style = "font-size:14px"
                  ),
              tags$br(),
              div(
                style = "margin: 0 auto; width: 100%; text-align:left;",
                div(
                  style = "display: inline-block;",
                  textInput(ns("regularExp"), "Regular expression to search:", placeholder = "write a regexp",  width = "250px",)
                ),
                actionButton(
                  ns("regularExpSearch"),
                  "search",
                  style = "display: inline-block; width: 140px; height: 35px;"
                ),
              ),
              tags$p(
                "Your uploaded CAM dataset contains ",
                tags$b(textOutput(ns("Nodes_uniqueSearch"), inline = TRUE), " unique concepts")
              ),
              tags$br(),
              tags$div(
                HTML("Keep the words you want to summarize in the right lists:"),
                style = "font-size:18px"
              ),
              tags$div(
                HTML(
                  '<i>The first word is automatically selected. If it contains a spelling error, you can correct it manually.
                                Important: all words will be saved in the following format: Word_positive for words with positive valence,
                                Word_negative for words with negative valence and so on.
                                 If you do not want to summarize a specific word please move it to the most left column.
                                 If you do not want to summarize any words just do not click on "summarize" and start a new search:</i>'
                ),
                style = "font-size:14px"
              ),
              htmlOutput(ns("bucketlistSearch")),
              fluidRow(
                column(width = 6, offset = 5,
                       textInput(ns("supordinateWordSearch"), "Superordinate word:",
                                 placeholder = "all words, which are not in the most left column"),
                       actionButton(ns("clickSummarizeSearch"), "summarize")

                )
              ),
              tags$br(),


              tags$p(
                "Your CAM dataset contains ",
                tags$b(textOutput(ns("Nodes_unique_afterSearch"), inline = TRUE)),
                " unique concepts AFTER summarzing."),
              tags$div(
                HTML(
                  'The following table shows you which words you have already summarized. You can use the search functionalities of the table:'
                ),
                style = "font-size:14px"
              ),
              dataTableOutput(ns("usedwWordsSearch")),

          )
      })

      #> Server
output$usedwWordsSearch <- renderDataTable({
  # https://stackoverflow.com/questions/27153979/converting-nested-list-unequal-length-to-data-frame
  indx <- sapply(globals$usedWords, length)
  outdat <- as.data.frame(do.call(rbind, lapply(
    globals$usedWords, `length<-`,
    max(indx)
  )))
  t(outdat)
})






###############################
###############################
###############################
        ###### synonyms ######
                #> UI
      observeEvent(input$ST_synonyms, {
        ## change UI
        outUI$elements <- tagList(
          tags$h2("(a) Summarize concepts by searching for synonyms"),
          tags$br(),
          tags$div(
            HTML(
              'Here you can summarize concepts by searching for synonyms, whereby for words written in
              <ul>
                <li>English the <a href="https://dictionary.reverso.net/english-synonyms/" target="_blank">Reverso Online Dictionary</a></li>
                <li><i>other languages will be implemented</i></li>
              </ul>
              <br>
               are applied:'
            ),
            style = "font-size:14px"
          ),
          tags$br(),
          tags$h3("Your settings:"),
          tags$div(
            HTML(
              "Please select the language of your CAM dataset and click
                click on the button to start summarizing your concepts by searching for synonyms.
                Please click only once and wait few seconds:"
            ),
            style = "font-size:14px"
          ),
          div(
            style = "margin: 0 auto; width: 100%; text-align:left;",
            div(
              style = "display: inline-block; vertical-align: top;",
              selectInput(ns("a_synonymsLanugage"), NULL, c("English",
                                                            "no other implemented"), selected = "English", width = "200px")
            ),
            actionButton(
              ns("a_synonymsStart"),
              "search for synonyms",
              style = "display: inline-block;"
            ),
          ),
          tags$h3("Synonyms found:"),
          tags$p(
            "By searching for synonyms ",
            tags$b(textOutput(ns(
              "a_synonymsPercentageFound"
            ), inline = TRUE)),
            " percent of your unique concepts have been found in the synonym database."
          ),
          tags$p(
            "Your uploaded CAM dataset contains ",
            tags$b(textOutput(ns(
              "Nodes_unique"
            ), inline = TRUE), " unique concepts"),
            " , whereby on your current set of unique concepts (! only single words considered) ",
            tags$b(
              textOutput(ns("a_groupsSynonyms"), inline = TRUE),
              " groups of synonyms"
            ),
            " have been found."
          ),
          tags$br(),
          tags$p(
            HTML("&nbsp;&nbsp;"),
            " > ",
            tags$b(textOutput(
              ns("a_groupsSynonymsRound"), inline = TRUE
            )),
            " more groups of synonyms to look at."
          ),
          tags$br(),
          tags$div(
            HTML("Keep the words you want to summarize in the right lists:"),
            style = "font-size:18px"
          ),
          tags$div(
            HTML(
              '<i>The first word is automatically selected as a possible superordinate word. If it contains a spelling error, you can
                correct it manually.
                <br>
                Important: all words will be saved in the following format: Word_positive for words with positive valence,
                 Word_negative for words with negative valence and so on.
                 If you do not want to summarize a specific word please move it to the most left column.
                 If you do not want to summarize any words of the found synonyms just click on "skip":</i>'
            ),
            style = "font-size:14px"
          ),
          htmlOutput(ns("a_synonymsBucketlist")),
          fluidRow(
            column(
              width = 6,
              offset = 5,
              textInput(
                ns("a_synonymsSupordinateWord"),
                "Superordinate word:",
                placeholder = "all words, which are not in the most left column"
              ),
              actionButton(ns("a_synonymsClickSummarize"), "summarize"),
              actionButton(ns("a_synonymsClickSkip"), "skip")
            )
          ),
          tags$br(),
          tags$p(
            "Your CAM dataset contains ",
            tags$b(textOutput(ns(
              "Nodes_unique_afterSynonyms"
            ), inline = TRUE)),
            " unique concepts AFTER summarzing."
          ),
          tags$div(
            HTML(
              'The following table shows you which words you have already summarized. You can use the search functionalities of the table:'
            ),
            style = "font-size:14px"
          ),
          dataTableOutput(ns("alreadyUsedWordsSynonyms")),
        )
      })

      #> Server
output$alreadyUsedWordsSynonyms <- renderDataTable({
  # https://stackoverflow.com/questions/27153979/converting-nested-list-unequal-length-to-data-frame
  indx <- sapply(globals$usedWords, length)
  outdat <- as.data.frame(do.call(rbind, lapply(
    globals$usedWords, `length<-`,
    max(indx)
  )))
  t(outdat)
})




        ###### word2vec ######
                #> UI
 observeEvent(input$ST_wordVec, {
        ## change UI
        outUI$elements <- tagList(
          tags$h2("(b) Summarize concepts by applying a word2vec model"),
          tags$br(),
          tags$div(
            HTML(
              "Remark: To use this functionality you need to compute the pairwise similarity between concepts (written with a single word) using
              the provided Python code on GitHub. It is not possible to implement this procedure online, because trained word2vec models are
              relatively large (> 500mb) and it is technically difficult to implement Python Code within a Shiny Application written in R.
              <br>
              <br>
              To run the word2vec model, please do the following three steps: (1) download your summarized words as a text file using the button below
              and (2) download run the Python script (see GitHub link) and (3) upload the so computed pairwise similarities to the Shiny App."
            ),
            style = "font-size:14px"
          ),
          tags$br(),
          tags$h3("(1) Download summarized words:"),
          tags$h3("(2) Download and run Python script:"),
          tags$div(
            HTML(
              "...:"
            ),
            style = "font-size:14px"
          ),
          tags$h3("(3) Upload the computed pairwise similarities:"),
          tags$div(
            HTML(
              "...:"
            ),
            style = "font-size:14px"
          ),
          tags$h4("Pairwise similarities (correlation matrix):"),
          tags$h4("Suggested similar groups of concepts by computing a hierarchical clustering:"),
        )
      })

      #> Server




      ############
      # information summarize terms
      ############
      #> UI
      observeEvent(input$ST_information, {
        ## change UI
        outUI$elements <-
          tagList(tags$h2("Module Specific Information"),
                  tags$div(
                    HTML('The options for this module are the following:'),
        tags$ul(
          tags$li(
            HTML(
              '<b>Approximate matching:</b> By using approximate string matching you can compute the string distances between all your unique concepts
              in the dataset (using optimal string alignment) to find words, which have been written slightly differently.'
            )
          ),
          tags$li(
            HTML(
              '<b>Searching terms:</b> Use regular expression to search your CAM data set for similar terms.'
            )
          ),
                    tags$li(
            HTML(
              '<b>Search for Synonyms:</b> Summarize concepts by searching for synonyms.'
            )
          ),
                    tags$li(
            HTML(
              '<b>Apply word2vec model:</b> The word2vec is a neural network, which learnd word associations from a large corpus of text.
              Based on these word associations get groups of similar terms (e.g. "happiness" is similar to "joy").'
            )
          )
        )
                  ))
      })


    })
  }

