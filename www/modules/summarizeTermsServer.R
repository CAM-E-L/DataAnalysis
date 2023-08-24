
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
          'To start the module please click on one of the module options on the sidebar panel. The options for
                     this module are the following:'
        ),
        tags$ul(
          tags$li(
            HTML(
              '<b>Approximate matching:</b> By using approximate string matching you can compute the string distances between all your unique concepts
              in the dataset (using optimal string alignment) to find words which have been written slightly differently (e.g., singular or plural, or spelling errors).'
            )
          ),
          tags$li(
            HTML(
              '<b>Searching terms:</b> By using search terms you apply so called regular expressions, which is a concise language to describe patterns of text. Applying the stringr package, for example, using the expression “[[:digit:]]”, all drawn concepts including any digits can be identified.'
            )
          ),


          
          tags$li(
            HTML(
              '<b>Search for Synonyms:</b> By using search for synonyms all synonyms for single-worded concepts are automatically searched (<i>currently only English implemented</i>).'
            )
          ),
          tags$li(
            HTML(
              '<b>Apply word2vec model:</b> By applying a word2vec Model it is possible to compute the cosine similarity between drawn 
                  concepts pairwise to identify groups of drawn concepts with similar meaning. For example, cosine similarity between 
                  the words “responsibility” and “accountability” would be .70, whereby cosine similarity is ranging from -1 
                  (opposite vectors) to 1 (proportional vectors).'
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
          protocolCounter_appMatch = 1L, # for detailed protocols different protocol counter
          protocolCounter_Search = 1L,
          protocolCounter_Synonyms = 1L,
          protocolCounter_word2vec = 1L
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
                                       
                                       searchArgument = NULL,
                                       searchType = NULL,
                                       label_superordinate = NULL,
                                       label_Pos = NULL,  label_Neg = NULL, label_Neut = NULL, label_Ambi = NULL){
  
  
  if(all(c(identical(input[[label_Pos]], character(0)),
           identical(input[[label_Neg]], character(0)),
           identical(input[[label_Neut]], character(0)),
           identical(input[[label_Ambi]], character(0)))) ||
           all(c(is.null(input[[label_Pos]]),
       is.null(input[[label_Neg]]),
       is.null(input[[label_Neut]]),
       is.null(input[[label_Ambi]])))){
    print("No further words found, empty input lists (end of search, clicked too often summarize).")
    return(NULL)
  }

  tmp_lengthInput <- c(input[[label_Pos]], input[[label_Neg]], input[[label_Neut]], input[[label_Ambi]])
  if(searchType == "approximate" || searchType == "synonyms" || searchType == "word2vec"){
    if(length(tmp_lengthInput) == 1){
      print("Only one word found (clicked too often summarize for non-search summarize functions).")
      return(NULL)
    }
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
    "PLACEHOLDER" = searchArgument,
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
  
  # rename depending on type of search
  if(searchType == "approximate"){
    colnames(tmp_protocol)[3] <- "stringDistance"
  }else if(searchType == "search"){
    colnames(tmp_protocol)[3] <- "regularExpression"
  }else if(searchType == "synonyms"){
    colnames(tmp_protocol)[3] <- "noneSearchArgument"
  }else if(searchType == "word2vec"){
    colnames(tmp_protocol)[3] <- "noneSearchArgument"
  }
  
  
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
  protocolJsonOut[[4]] = searchArgument
  
  
  
  # rename depending on type of search
  if(searchType == "approximate"){
    names(protocolJsonOut) <-
      c("time",
        "wordsFound",
        "superordinateWord",
        "stringDistance")
  }else if(searchType == "search"){
    names(protocolJsonOut) <-
      c("time",
        "wordsFound",
        "superordinateWord",
        "regularExpression")
  }else if(searchType == "synonyms"){
    names(protocolJsonOut) <-
      c("time",
        "wordsFound",
        "superordinateWord",
        "noneSearchArgumentSynonyms")
  }else if(searchType == "word2vec"){
    names(protocolJsonOut) <-
      c("time",
        "wordsFound",
        "superordinateWord",
        "noneSearchArgumentWordVec")
  }
  
  
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

        ## reset skip functionality
        ST_rv$skip <- FALSE

        # print("globals$protocol$currentCAMs")
        # print(globals$protocol$currentCAMs)

        # print("globals$protocol$deletedCAMs")
        # print(globals$protocol$deletedCAMs)

      })

      ################################
      # single module options
      ################################


      ###############################
      ###############################
      ###############################

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
              in the dataset (using optimal string alignment) to find words, which have been written slightly differently."
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
                                 If you do not want to summarize a specific word please move it to the leftmost column.
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
            ST_rv$counter <- 0L

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
      ## START global for summarize screens
      # > unique concepts
      output$Nodes_unique <- renderText({
        length(unique(globals$dataCAMsummarized[[1]]$text))
      })

      # > number of nodes after summarizing words
      output$Nodes_unique_after <- renderText({
        tmp_text_summarized <- str_remove_all(string = globals$dataCAMsummarized[[1]]$text_summarized,
                                              pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
        length(unique(tmp_text_summarized))
      })
      ## END global for summarize screens

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
      ###############################

      ###############################
      ### create labels for bucket list AND render bucketlist AND update superordinate word
      ## create labels for bucket list ##
      # > words with positive valence:
      labels_positive_appMatch <-
        eventReactive(c(
          input$clickAproxStriMatch,
          input$clickSummarize,
          input$clickSkip,
        input$ST_approxMatch ## avoid interference with search function
        ),
        {
          req(optimalMatchSim())

          tmp_labels_out <- getlabels_appMatch(typeLabels = "positive",
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
          input$clickSkip,
        input$ST_approxMatch
        ),
        {
          req(optimalMatchSim())

          tmp_labels_out <- getlabels_appMatch(typeLabels = "negative",
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
          input$clickSkip,
        input$ST_approxMatch
        ),
        {
          req(optimalMatchSim())

          tmp_labels_out <- getlabels_appMatch(typeLabels = "neutral",
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
          input$clickSkip,
        input$ST_approxMatch
        ),
        {
          req(optimalMatchSim())

          tmp_labels_out <- getlabels_appMatch(typeLabels = "ambivalent",
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
          tmp_overwriteData_getProtocols <- overwriteData_getProtocols(protocolCounter = ST_rv$protocolCounter_appMatch,
                                                                       protocolDetailedOut = globals$detailedProtocolAM, # !!!
                                                                       list_usedWords = globals$usedWords,
                                                                       dataSummarized = globals$dataCAMsummarized,

                                                                       searchArgument = input$maxStringDis,
                                                                       searchType = "approximate",
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
            ST_rv$protocolCounter_appMatch <- tmp_overwriteData_getProtocols$counterProtocol
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

      ## START global for summarize screens
      ## list of used words
      output$alreadyUsedWords <- renderDataTable({
        # https://stackoverflow.com/questions/27153979/converting-nested-list-unequal-length-to-data-frame
        indx <- sapply(globals$usedWords, length)
        outdat <- as.data.frame(do.call(rbind, lapply(
          globals$usedWords, `length<-`,
          max(indx)
        )))
        t(outdat)
      })
      ## END global for summarize screens







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
                    HTML('By using so called regular expression you can search your CAM concepts for semantically identical or 
                      similar terms. Important regular expressions:
                <ul>
                <li>write "^" and / or "$" to match the beginning / end of your concept (e.g. ^inexp -> inexpensive but not expensive)</li>
                <li>write "|" to search for multiple words at once (e.g. cost|cheap -> low cost, cheap, ...)</li>
              </ul>
              For more details see Information.'
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
                tags$b(textOutput(ns("Nodes_unique"), inline = TRUE), " unique concepts")
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
                tags$b(textOutput(ns("Nodes_unique_after"), inline = TRUE)),
                " unique concepts AFTER summarzing (ignoring valence)."),
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
      ### create data set for search terms
      regularExpOut <-eventReactive(c(
        input$regularExpSearch,
        input$ST_searchTerms # start when clicked on sidebar panel
      ), {
        req(drawnCAM())

        message("The value of input$regularExpSearch is ", input$regularExpSearch)

        ## reset counter:
        ST_rv$counter <- 0L

        # print(input$regularExp)
        if(nchar(x = input$regularExp) > 0){

          words_out <- tryCatch(
            # expression
            unique(globals$dataCAMsummarized[[1]]$text_summarized[
            str_detect(string = str_remove_all(string = globals$dataCAMsummarized[[1]]$text_summarized,
                                               pattern = "_positive$|_negative$|_neutral$|_ambivalent$"),
                       pattern = input$regularExp, negate = FALSE)]),
            warning = function(cond) {
              cond <- gsub("Error in stri_detect_regex(string, pattern, negate = negate, opts_regex = opts(pattern)): ", "",
                           cond$message)
              showModal(
                modalDialog(
                  title = "Invalid search term",
                  paste0(
                    "The search term you have entered does not appear to be a valid regular expression: ",
                    cond,
                    " - Please check your syntax."
                  ),
                  easyClose = TRUE,
                  footer = tagList(modalButton("Ok"))
                )
              )
              return(NULL)
            },
            error = function(cond) {
              cond <- gsub("Error in stri_detect_regex(string, pattern, negate = negate, opts_regex = opts(pattern)): ", "",
                           cond$message)
              showModal(
                modalDialog(
                  title = "Invalid search term",
                  paste0(
                    "The search term you have entered does not appear to be a valid regular expression: ",
                    cond,
                    " - Please check your syntax."
                  ),
                  easyClose = TRUE,
                  footer = tagList(modalButton("Ok"))
                )
              )
              return(NULL)
            }
          )

        } else{
          words_out <- NULL
        }


        print("words_out")
        print(head(words_out))
        words_out
      })
      ###############################

      ###############################
      ### show number of
      # > see global
      ###############################

      ###############################
      ### create labels for bucket list AND render bucketlist AND update superordinate word
      ## create labels for bucket list ##
      # > words with positive valence:
      labels_positiveSearch <- eventReactive(c(input$clickSummarizeSearch, input$regularExpSearch), {
        req(regularExpOut())

        getlabels_Search(typeLabels = "positive",
                         getInput = regularExpOut(),
                         dataSummarized = globals$dataCAMsummarized)
      })

      # > words with negative valence:
      labels_negativeSearch <- eventReactive(c(input$clickSummarizeSearch, input$regularExpSearch), {
        req(regularExpOut())

        getlabels_Search(typeLabels = "negative",
                         getInput = regularExpOut(),
                         dataSummarized = globals$dataCAMsummarized)
      })

      # > words with neutral valence:
      labels_neutralSearch <- eventReactive(c(input$clickSummarizeSearch, input$regularExpSearch), {
        req(regularExpOut())

        getlabels_Search(typeLabels = "neutral",
                         getInput = regularExpOut(),
                         dataSummarized = globals$dataCAMsummarized)
      })

      # > words with ambivalent valence:
      labels_ambivalentSearch <- eventReactive(c(input$clickSummarizeSearch, input$regularExpSearch), {
        req(regularExpOut())

        getlabels_Search(typeLabels = "ambivalent",
                         getInput = regularExpOut(),
                         dataSummarized = globals$dataCAMsummarized)
      })


      ## render bucket list ##
      output$bucketlistSearch <- renderUI({
        bucket_list(
          header = NULL,
          group_name = ns("bucket_list_groupSearch"),
          orientation = "horizontal",
          add_rank_list(
            text = "move here to not summarize single words",
            labels = NULL, # labels from row selection
            input_id = ns("matches_suggestionSearch"),
            options = sortable_options(multiDrag = TRUE)
          ),
          add_rank_list(
            text = "found words with POSITIVE valence:",
            labels = labels_positiveSearch(),
            input_id = ns("matches_positiveSearch"),
            options = sortable_options(multiDrag = TRUE)
          ),
          add_rank_list(
            text = "found words with NEGATIVE valence:",
            labels = labels_negativeSearch(),
            input_id = ns("matches_negativeSearch"),
            options = sortable_options(multiDrag = TRUE)
          ),
          add_rank_list(
            text = "found words with NEUTRAL valence:",
            labels = labels_neutralSearch(),
            input_id = ns("matches_neutralSearch"),
            options = sortable_options(multiDrag = TRUE)
          ),
          add_rank_list(
            text = "found words with AMBIVALENT valence:",
            labels = labels_ambivalentSearch(),
            input_id = ns("matches_ambivalentSearch"),
            options = sortable_options(multiDrag = TRUE)
          )
        )
      })


      ## update superordinate word ##
      observe({
        req(drawnCAM())
        getSuperordinateWord(label_superordinate = "supordinateWordSearch",
                             label_Pos = "matches_positiveSearch",
                             label_Neg = "matches_negativeSearch",
                             label_Neut = "matches_neutralSearch",
                             label_Ambi = "matches_ambivalentSearch")
      })
      ###############################



      ###############################
      ### overwrite summarized words AND set JSON protocol // get detailed protocol AND list of used words
      ## set protocol and overwrite data:
      observeEvent(input$clickSummarizeSearch, {
        req(drawnCAM())

        if(nchar(x = input$regularExp) > 0){
          tmp_overwriteData_getProtocols <- overwriteData_getProtocols(protocolCounter = ST_rv$protocolCounter_Search,
                                                                       protocolDetailedOut = globals$detailedProtocolST, # !!!
                                                                       list_usedWords = globals$usedWords,
                                                                       dataSummarized = globals$dataCAMsummarized,

                                                                       searchArgument = input$regularExp,
                                                                       searchType = "search",
                                                                       label_superordinate = "supordinateWordSearch",
                                                                       label_Pos = "matches_positiveSearch",
                                                                       label_Neg = "matches_negativeSearch",
                                                                       label_Neut = "matches_neutralSearch",
                                                                       label_Ambi = "matches_ambivalentSearch")

          # print("names(tmp_overwriteData_getProtocols)")
          # print(names(tmp_overwriteData_getProtocols))
          tmp_vecFindings <- c(input$matches_positiveSearch,
          input$matches_negativeSearch,
          input$matches_neutralSearch,
          input$matches_ambivalentSearch,
          input$supordinateWordSearch)
          
          print("tmp_vecFindings")
          print(tmp_vecFindings)
          print("length(unique(tmp_vecFindings))")
          print(length(unique(tmp_vecFindings)))

          if(length(unique(tmp_vecFindings)) == 1){
                        showModal(
                          modalDialog(
                            title = "You cannot summarize identical words",
                            paste0(
                              "Please change your search term or the name of the superordinate word to summarize your words by searching."
                            ),
                            easyClose = TRUE,
                            footer = tagList(modalButton("Ok"))
                          )
                        )
          tmp_overwriteData_getProtocols <- NULL             
          }

          if(!is.null(tmp_overwriteData_getProtocols)){

            ## overwrite global data
            globals$dataCAMsummarized <- tmp_overwriteData_getProtocols$summarizedData
            ## overwrite protocol counter (detailed)
            ST_rv$protocolCounter_Search <- tmp_overwriteData_getProtocols$counterProtocol
            ## overwrite detailed protocol
            globals$detailedProtocolST <- tmp_overwriteData_getProtocols$detailedProtocol
            ## overwrite already used words
            globals$usedWords <- tmp_overwriteData_getProtocols$usedWordsList

            ## append JSON protocol
            globals$protocol$searchTerms[[length(globals$protocol$searchTerms) + 1]] <-
              tmp_overwriteData_getProtocols$jsonProtocol
            #> change condition
            globals$condition <- c(globals$condition, "searchTerms")

            # print("length(unique(globals$dataCAMsummarized[[1]]$text_summarized))")
            # print(length(unique(globals$dataCAMsummarized[[1]]$text_summarized)))
          }
        }
      })


      ## list of used words
      # > see global








      ###############################
      ###############################
      ###############################

      ###### synonyms ######
      #> UI
      observeEvent(input$ST_synonyms, {
        ## change UI
        outUI$elements <- tagList(
          tags$h2("Summarize concepts by searching for synonyms"),
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
              selectInput(ns("synonymsLanugage"), NULL, c("English",
                                                            "no other implemented"), selected = "English", width = "200px")
            ),
            actionButton(
              ns("synonymsStart"),
              "search for synonyms",
              style = "display: inline-block;"
            ),
          ),
          tags$h3("Synonyms found:"),
          tags$p(
            "By searching for synonyms ",
            tags$b(textOutput(ns(
              "synonymsPercentageFound"
            ), inline = TRUE)),
            " percent of unique concepts have been found in the synonym database."
          ),
                              tags$div(
            HTML(
              "<i> Remark: Only unique concepts, which contains only one word and only word characters (A-Z, a-z) can be considered; 
            if needed use the other summarize functions to summarize your concepts further.</i>"
            ),
            style = "font-size:10px"
          ),
          tags$p(
            "Your uploaded CAM dataset contains ",
            tags$b(textOutput(ns(
              "Nodes_unique"
            ), inline = TRUE), " unique concepts"),
            " , whereby on your current set of unique concepts (! only single words considered) ",
            tags$b(
              textOutput(ns("synonymsGroups"), inline = TRUE),
              " groups of synonyms"
            ),
            " have been found."
          ),
          tags$p(
            HTML("&nbsp;&nbsp;"),
            " > ",
            tags$b(textOutput(
              ns("synonymsGroupsRounds"), inline = TRUE
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
          htmlOutput(ns("synonymsBucketlist")),
          fluidRow(
            column(
              width = 6,
              offset = 5,
              textInput(
                ns("synonymsSupordinateWord"),
                "Superordinate word:",
                placeholder = "all words, which are not in the most left column"
              ),
              actionButton(ns("synonymsClickSummarize"), "summarize"),
              actionButton(ns("synonymsClickSkip"), "skip")
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
## create data set for approximate matching
reducedSynonymList <-
  eventReactive(c(
    input$synonymsStart,
    input$ST_synonyms), {  ## initialize by clicking on sidebar
    req(drawnCAM())

    message("The choosen language is ",
            input$synonymsLanugage)

    # remove all suffix
    tmp_text <- str_remove_all(string = globals$dataCAMsummarized[[1]]$text_summarized, 
    pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
    ## get raw synonym list
    raw_SynonymList <- SynonymList(vectorWords = tmp_text, syn_dat = globals$dat_synonym$syn_English) # !!! IF language
    

    # print("raw_SynonymList")
    # print(raw_SynonymList)

    if(!is.null(raw_SynonymList)){
    ## out perentage of matches
    output$synonymsPercentageFound <- renderText({
      raw_SynonymList[[2]]
      })

    ## reset counter:
    ST_rv$counter <- 0L
    
    ## get reduced synonym list
    reduced_SynonymList <- SummarizedSynonymList(listSynonyms = raw_SynonymList[[1]])
    for(i in 1:length(reduced_SynonymList)){
      reduced_SynonymList <- SummarizedSynonymList(listSynonyms = reduced_SynonymList)
    }
    reduced_SynonymList
    }else{
      NULL
    }
  })

###############################

###############################
### show number of
# > see global

        #> number of groups of synonyms
      output$synonymsGroups <- renderText({
   req(reducedSynonymList())
  length(reducedSynonymList())
      })


    #> number of rounds to click:
     output$synonymsGroupsRounds <- renderText({
         req(reducedSynonymList())
        length(reducedSynonymList())  - ST_rv$counter + 1
      })

      ## implement skip functionality
      observeEvent(input$synonymsClickSkip, {
        ST_rv$skip <- TRUE
      })



###############################

###############################
### create labels for bucket list AND render bucketlist AND update superordinate word
## create labels for bucket list ##
# > words with positive valence:
labels_positive_Synonyms <-
  eventReactive(c(
    input$synonymsStart,
    input$synonymsClickSummarize,
    input$synonymsClickSkip,
        input$ST_synonyms ## avoid interference with search function
  ),
  {
    req(reducedSynonymList())

    tmp_labels_out <- getlabels_Synonyms(typeLabels = "positive",
                                         getInput = reducedSynonymList(),
                                         counter = ST_rv$counter, skipCond = ST_rv$skip,
                                         dataSummarized = globals$dataCAMsummarized)
    
    ST_rv$counter <- tmp_labels_out[[2]] # update counter
    ST_rv$skip <- tmp_labels_out[[3]] # update skip condition
    
    tmp_labels_out[[1]]
  })


# > words with negative valence:
labels_negative_Synonyms <-
  eventReactive(c(
    input$synonymsStart,
    input$synonymsClickSummarize,
    input$synonymsClickSkip,
        input$ST_synonyms
  ),
  {
    req(reducedSynonymList())
    
    tmp_labels_out <- getlabels_Synonyms(typeLabels = "negative",
                                         getInput = reducedSynonymList(),
                                         counter = ST_rv$counter-1, skipCond = ST_rv$skip,
                                         dataSummarized = globals$dataCAMsummarized)
    
    ST_rv$counter <- tmp_labels_out[[2]] # update counter
    ST_rv$skip <- tmp_labels_out[[3]] # update skip condition
    
    tmp_labels_out[[1]]
  })


  # > words with neutral valence:
labels_neutral_Synonyms <-
  eventReactive(c(
    input$synonymsStart,
    input$synonymsClickSummarize,
    input$synonymsClickSkip,
        input$ST_synonyms
  ),
  {
    req(reducedSynonymList())
    
    tmp_labels_out <- getlabels_Synonyms(typeLabels = "neutral",
                                         getInput = reducedSynonymList(),
                                         counter = ST_rv$counter-1, skipCond = ST_rv$skip,
                                         dataSummarized = globals$dataCAMsummarized)
    
    ST_rv$counter <- tmp_labels_out[[2]] # update counter
    ST_rv$skip <- tmp_labels_out[[3]] # update skip condition
    
    tmp_labels_out[[1]]
  })


# > words with ambivalent valence:
labels_ambivalent_Synonyms <-
  eventReactive(c(
    input$synonymsStart,
    input$synonymsClickSummarize,
    input$synonymsClickSkip,
        input$ST_synonyms
  ),
  {
    req(reducedSynonymList())
    
    tmp_labels_out <- getlabels_Synonyms(typeLabels = "ambivalent",
                                         getInput = reducedSynonymList(),
                                         counter = ST_rv$counter-1, skipCond = ST_rv$skip,
                                         dataSummarized = globals$dataCAMsummarized)
    
    ST_rv$counter <- tmp_labels_out[[2]] # update counter
    ST_rv$skip <- tmp_labels_out[[3]] # update skip condition
    
    tmp_labels_out[[1]]
  })


## render bucket list ##
      ## Render bucket list
      output$synonymsBucketlist <- renderUI({
        bucket_list(
          header = NULL,
          group_name = ns("bucket_list_groupSynonym"),
          orientation = "horizontal",
          add_rank_list(
            text = "move here to not summarize single words",
            labels = NULL,
            # labels from row selection
            input_id = ns("matches_suggestion_synonyms"),
            options = sortable_options(multiDrag = TRUE)
          ),
          add_rank_list(
            text = "suggested synonymous words with POSITIVE valence:",
            labels = labels_positive_Synonyms(),
            input_id = ns("matches_positive_synonyms"),
            options = sortable_options(multiDrag = TRUE)
          ),
          add_rank_list(
            text = "suggested synonymous words with NEGATIVE valence:",
            labels = labels_negative_Synonyms(),
            input_id = ns("matches_negative_synonyms"),
            options = sortable_options(multiDrag = TRUE)
          ),
          add_rank_list(
            text = "suggested synonymous words with NEUTRAL valence:",
            labels = labels_neutral_Synonyms(),
            input_id = ns("matches_neutral_synonyms"),
            options = sortable_options(multiDrag = TRUE)
          ),
          add_rank_list(
            text = "suggested synonymous words with AMBIVALENT valence:",
            labels = labels_ambivalent_Synonyms(),
            input_id = ns("matches_ambivalent_synonyms"),
            options = sortable_options(multiDrag = TRUE)
          )
        )
      })



## update superordinate word ##
observe({
  req(drawnCAM())
  getSuperordinateWord(label_superordinate = "synonymsSupordinateWord",
                       label_Pos = "matches_positive_synonyms",
                       label_Neg = "matches_negative_synonyms",
                       label_Neut = "matches_neutral_synonyms",
                       label_Ambi = "matches_ambivalent_synonyms")
})

observeEvent(input$ST_synonyms, { # start when clicked on sidebar panel
  req(drawnCAM())
  getSuperordinateWord(label_superordinate = "synonymsSupordinateWord",
                       label_Pos = "matches_positive_synonyms",
                       label_Neg = "matches_negative_synonyms",
                       label_Neut = "matches_neutral_synonyms",
                       label_Ambi = "matches_ambivalent_synonyms")
})

###############################



###############################
### overwrite summarized words AND set JSON protocol // get detailed protocol AND list of used words
## set protocol and overwrite data:
observeEvent(input$synonymsClickSummarize, {
  req(drawnCAM())
  
  print("summarize - skip")
  print(ST_rv$skip)

  if (!isTRUE(ST_rv$skip)) {
    tmp_overwriteData_getProtocols <- overwriteData_getProtocols(protocolCounter = ST_rv$protocolCounter_Synonyms,
                                                                 protocolDetailedOut = globals$detailedProtocolSynonyms, # !!!
                                                                 list_usedWords = globals$usedWords,
                                                                 dataSummarized = globals$dataCAMsummarized,
                                                                 
                                                                 searchArgument = "none",
                                                                 searchType = "synonyms",
                                                          label_superordinate = "synonymsSupordinateWord",
                       label_Pos = "matches_positive_synonyms",
                       label_Neg = "matches_negative_synonyms",
                       label_Neut = "matches_neutral_synonyms",
                       label_Ambi = "matches_ambivalent_synonyms")
    
    # print("names(tmp_overwriteData_getProtocols)")
    # print(names(tmp_overwriteData_getProtocols))


    if(!is.null(tmp_overwriteData_getProtocols)){
      ## overwrite global data
      globals$dataCAMsummarized <- tmp_overwriteData_getProtocols$summarizedData
      ## overwrite protocol counter (detailed)
      ST_rv$protocolCounter_Synonyms <- tmp_overwriteData_getProtocols$counterProtocol
      ## overwrite detailed protocol
      globals$detailedProtocolSynonyms <- tmp_overwriteData_getProtocols$detailedProtocol
      ## overwrite already used words
      globals$usedWords <- tmp_overwriteData_getProtocols$usedWordsList
      
      ## append JSON protocol
      globals$protocol$findSynonyms[[length(globals$protocol$findSynonyms) + 1]] <-
        tmp_overwriteData_getProtocols$jsonProtocol
      #> change condition
      globals$condition <- c(globals$condition, "findSynonyms")
      
      # print("length(unique(globals$dataCAMsummarized[[1]]$text_summarized))")
      # print(length(unique(globals$dataCAMsummarized[[1]]$text_summarized)))


      # print("globals$detailedProtocolSynonyms")
      # print(globals$detailedProtocolSynonyms)

      # print("globals$protocol$findSynonyms")
      # print(globals$protocol$findSynonyms)
    }
  }
})


## list of used words
# > see global








      ###############################
      ###############################
      ###############################

      ###### word2vec ######
      #> UI
      observeEvent(input$ST_wordVec, {
        ## change UI
        outUI$elements <- tagList(
          tags$h2("Summarize concepts by applying a word2vec model"),
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
          tags$div(HTML("To download the list of your summarized words, please click on the following button:"), style="font-size:14px"),
          downloadButton(ns("download_summarizedWords"), label = "download summarized words"),
          tags$p(
            "With your current data set you can search for ",
            tags$b(textOutput(ns(
              "word2vecPercentageFound"
            ), inline = TRUE)),
            " percent of your unique concepts for superordinate group of words using the word2vec model."
          ),
                    tags$div(
            HTML(
              "<i> Remark: Only unique concepts, which contains only one word and only word characters (A-Z, a-z) can be considered; 
            if needed use the other summarize functions to summarize your concepts further.</i>"
            ),
            style = "font-size:10px"
          ),
          tags$h3("(2) Download and run Python script:"),
          tags$div(
            HTML(
              'Please download the Python code from GitHub and run the Python script (see for more details readme page on GitHub): 
              <a href="https://github.com/Camel-app/DataAnalysis/tree/main/Python_word2vec" target="_blank">GitHub Python Code</a>'
            ),
            style = "font-size:14px"
          ),
          tags$h3("(3) Upload the computed pairwise similarities:"),
          tags$div(
            HTML(
              'After running the Python Script you should have created a .txt file called "distanceMatrix", please upload this file:'
            ),
            style = "font-size:14px"
          ),
                  fileInput(
          ns("uploadDistanceMatrix"),
          NULL,
          accept = c(".txt"),
          placeholder = "upload your file distanceMatrix",
          multiple = FALSE
        ),
              tags$p(
            "Of your provided concepts ",
            tags$b(textOutput(ns(
              "word2vecPercentageFound_distanceMatrix"
            ), inline = TRUE)),
            " percent matches were found for the word2vec model (the rest are non-words)."
          ),
     
                  fluidRow(
          column(7,plotOutput(ns("plotDendrogram"), width="100%")),  
          column(3,verbatimTextOutput(ns("numberGroups")))
        ),
             tags$div(HTML("Please cut the dendrogram by specifying the cutting height (horizontal red line):"), style="font-size:14px"),
       div(
            style = "margin: 0 auto; width: 100%; text-align:left;",
            div(
              style = "display: inline-block; vertical-align: top;",
 numericInput(ns("cuttingDendrogram"), label = NULL, value = .5, min = .2, max = 3, step = .1, width = "150px"),
            ),
            actionButton(
              ns("word2vecStart"),
              "search for similar words",
              style = "display: inline-block;"
            ),
          ),  
          tags$h4("Suggested similar groups of concepts by using hierarchical clustering:"),
              tags$p(
      "Your uploaded CAM dataset contains ",
      tags$b(textOutput(ns(
        "Nodes_unique"
      ), inline = TRUE), " unique concepts"),
      " , whereby on your specified cutting height ",
      tags$b(
        textOutput(ns("word2vecGroups"), inline = TRUE),
        " groups of similar words"
      ),
      " , identified by word2vec, have been found."
    ),
    tags$p(
      HTML("&nbsp;&nbsp;"),
      " > ",
      tags$b(textOutput(
        ns("word2vecGroupsRounds"), inline = TRUE
      )),
      " more groups of similar words to look at."
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
                 If you do not want to summarize any words of the found by word2vec just click on "skip":</i>'
      ),
      style = "font-size:14px"
    ),
    htmlOutput(ns("word2vecBucketlist")),
    fluidRow(
      column(
        width = 6,
        offset = 5,
        textInput(
          ns("word2vecSupordinateWord"),
          "Superordinate word:",
          placeholder = "all words, which are not in the most left column"
        ),
        actionButton(ns("word2vecClickSummarize"), "summarize"),
        actionButton(ns("word2vecClickSkip"), "skip")
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
    dataTableOutput(ns("alreadyUsedWords"))
        )
      })

      #> Server
      ###############################
      ### (1) Download summarized words




outword2vecWords <-
  eventReactive(c(
    input$ST_wordVec
  ),
  {
                   tmp_text_summarized <- str_remove_all(string = globals$dataCAMsummarized[[1]]$text_summarized,
                                              pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
                  ## keep only one-worded words
                  tmpOneWords <- str_split(string = tmp_text_summarized, pattern = " ", simplify = TRUE)
                  tmp_text_summarized <- tmp_text_summarized[rowSums(x = tmpOneWords != "") == 1]
                  ## clean
                  tmp_text_summarized <- tolower(tmp_text_summarized)
                  tmp_text_summarized <- sort(unique(tmp_text_summarized))
                  tmp_text_summarized <- tmp_text_summarized[str_detect(string = tmp_text_summarized, pattern = regex("\\W+"), negate = TRUE)]
                  tmp_text_summarized <- tmp_text_summarized[nchar(tmp_text_summarized) >= 3]
tmp_text_summarized
  })



            output$word2vecPercentageFound <- renderText({
              req(outword2vecWords())
                 tmp_text_summarized <- str_remove_all(string = globals$dataCAMsummarized[[1]]$text_summarized,
                                              pattern = "_positive$|_negative$|_neutral$|_ambivalent$")

              
              round(x = length(outword2vecWords()) /  length(unique(tmp_text_summarized)), digits = 2) * 100
              })

        ## download suitable summarized words:
        output$download_summarizedWords <- downloadHandler(
            filename = function() {
                paste0("summarizedWords", ".txt")
            },
            content = function(file) {
req(outword2vecWords())

              write.table(x = outword2vecWords(), 
              file = file, row.names = FALSE, col.names = FALSE)
              }
        )
      ###############################
    
      ###############################
## (3) Upload the computed pairwise similarities
        distanceMatrix <- reactive({
          distanceMatrix <- read.table(file = input$uploadDistanceMatrix$datapath)
          distanceMatrix
    })




    observeEvent(input$uploadDistanceMatrix, {
      if (!is.null(distanceMatrix())) {
        ## get percentage of words found:
        output$word2vecPercentageFound_distanceMatrix <- renderText({
          round(x = length(distanceMatrix()) /  length(outword2vecWords()), digits = 2) * 100
          })

        cosine_sim <- 1-as.dist(distanceMatrix()) # create similarity  using euclidean distance
        cluster_solution <- hclust(cosine_sim, method = "ward.D2") # Ward's method

        minHeight <- round(min(cluster_solution$height), digits = 2) + 0.01

        print("minHeight !!!")
        print(minHeight)

        ## get dendogram
        output$plotDendrogram <- renderPlot({
          plot(cluster_solution)
          abline(h = input$cuttingDendrogram, col = "red", lty = 2)
          rect.hclust(cluster_solution, h=input$cuttingDendrogram, border = "tomato")
        })


        ## get number of groups found
        output$numberGroups <- renderPrint({
          groups<-cutree(cluster_solution, h=input$cuttingDendrogram)
          groupsOut <- names(table(groups))[table(groups) >= 2]

          print("total number of groups >= 2:")
          print(length(groupsOut))
          })
      }
    })


      ## get groups out final
           groupsOutFinal <- eventReactive(c(input$uploadDistanceMatrix,
           input$cuttingDendrogram,
           input$word2vecStart), {
           if (!is.null(distanceMatrix())) {
               cosine_sim <- 1-as.dist(distanceMatrix()) # create similarity  using euclidean distance
        cluster_solution <- hclust(cosine_sim, method = "ward.D2") # Ward's method

                       groups<-cutree(cluster_solution, h=input$cuttingDendrogram)
groupsOut <- names(table(groups))[table(groups) >= 2]
 list_word2vec <- list()
for(i in 1:length(groupsOut)){
  list_word2vec[[i]] <- names(groups)[groups == groupsOut[i]]
}

list_word2vec
           }
        })


###############################
### show number of
# > see global

#> number of groups of word2vec
output$word2vecGroups <- renderText({
  if(is.null(input$uploadDistanceMatrix)){
    NULL
  }else{
  length(groupsOutFinal())
  }
})

#> number of rounds to click:
output$word2vecGroupsRounds <- renderText({
    if(is.null(input$uploadDistanceMatrix)){
    NULL
  }else{
  length(groupsOutFinal())  - ST_rv$counter + 1
  }
})

## implement skip functionality
observeEvent(input$word2vecClickSkip, {
  ST_rv$skip <- TRUE
})


## implement skip functionality
observeEvent(input$word2vecStart, {
  # reset counter
  ST_rv$counter <- 0
})
###############################


###############################
### create labels for bucket list AND render bucketlist AND update superordinate word
## create labels for bucket list ##
# > words with positive valence:
labels_positive_word2vec <-
  eventReactive(c(

    input$word2vecClickSummarize,
    input$word2vecClickSkip,
               input$word2vecStart
  ),
  {
  if(is.null(input$uploadDistanceMatrix)){
    NULL
  }else{
    tmp_labels_out <- getlabels_Synonyms(typeLabels = "positive",
                                         getInput = groupsOutFinal(),
                                         counter = ST_rv$counter, skipCond = ST_rv$skip,
                                         dataSummarized = globals$dataCAMsummarized)
    
    ST_rv$counter <- tmp_labels_out[[2]] # update counter
    ST_rv$skip <- tmp_labels_out[[3]] # update skip condition
    
    tmp_labels_out[[1]]
  }

  })

  # > words with negative valence:
labels_negative_word2vec <-
  eventReactive(c(

    input$word2vecClickSummarize,
    input$word2vecClickSkip,
               input$word2vecStart
  ),
  {
  if(is.null(input$uploadDistanceMatrix)){
    NULL
  }else{
    tmp_labels_out <- getlabels_Synonyms(typeLabels = "negative",
                                         getInput = groupsOutFinal(),
                                         counter = ST_rv$counter-1, skipCond = ST_rv$skip,
                                         dataSummarized = globals$dataCAMsummarized)
    
    ST_rv$counter <- tmp_labels_out[[2]] # update counter
    ST_rv$skip <- tmp_labels_out[[3]] # update skip condition
    
    tmp_labels_out[[1]]
  }

  })

  # > words with neutral valence:
labels_neutral_word2vec <-
  eventReactive(c(

    input$word2vecClickSummarize,
    input$word2vecClickSkip,
               input$word2vecStart
  ),
  {
  if(is.null(input$uploadDistanceMatrix)){
    NULL
  }else{
    tmp_labels_out <- getlabels_Synonyms(typeLabels = "neutral",
                                         getInput = groupsOutFinal(),
                                         counter = ST_rv$counter-1, skipCond = ST_rv$skip,
                                         dataSummarized = globals$dataCAMsummarized)
    
    ST_rv$counter <- tmp_labels_out[[2]] # update counter
    ST_rv$skip <- tmp_labels_out[[3]] # update skip condition
    
    tmp_labels_out[[1]]
  }

  })

  # > words with ambivalent valence:
labels_ambivalent_word2vec <-
  eventReactive(c(

    input$word2vecClickSummarize,
    input$word2vecClickSkip,
               input$word2vecStart
  ),
  {
  if(is.null(input$uploadDistanceMatrix)){
    NULL
  }else{
    tmp_labels_out <- getlabels_Synonyms(typeLabels = "ambivalent",
                                         getInput = groupsOutFinal(),
                                         counter = ST_rv$counter-1, skipCond = ST_rv$skip,
                                         dataSummarized = globals$dataCAMsummarized)
    
    ST_rv$counter <- tmp_labels_out[[2]] # update counter
    ST_rv$skip <- tmp_labels_out[[3]] # update skip condition
    
    tmp_labels_out[[1]]
  }

  })
      

        ## render bucket list ##
## Render bucket list
output$word2vecBucketlist <- renderUI({
  bucket_list(
    header = NULL,
    group_name = ns("bucket_list_groupword2vec"),
    orientation = "horizontal",
    add_rank_list(
      text = "move here to not summarize single words",
      labels = NULL,
      # labels from row selection
      input_id = ns("matches_suggestion_word2vec"),
      options = sortable_options(multiDrag = TRUE)
    ),
    add_rank_list(
      text = "suggested similar words with POSITIVE valence:",
      labels = labels_positive_word2vec(),
      input_id = ns("matches_positive_word2vec"),
      options = sortable_options(multiDrag = TRUE)
    ),
    add_rank_list(
      text = "suggested similar words with NEGATIVE valence:",
      labels = labels_negative_word2vec(),
      input_id = ns("matches_negative_word2vec"),
      options = sortable_options(multiDrag = TRUE)
    ),
    add_rank_list(
      text = "suggested similar words with NEUTRAL valence:",
      labels = labels_neutral_word2vec(),
      input_id = ns("matches_neutral_word2vec"),
      options = sortable_options(multiDrag = TRUE)
    ),
    add_rank_list(
      text = "suggested similar words with AMBIVALENT valence:",
      labels = labels_ambivalent_word2vec(),
      input_id = ns("matches_ambivalent_word2vec"),
      options = sortable_options(multiDrag = TRUE)
    )
  )
})


## update superordinate word ##
observe({
  req(drawnCAM())
  getSuperordinateWord(label_superordinate = "word2vecSupordinateWord",
                       label_Pos = "matches_positive_word2vec",
                       label_Neg = "matches_negative_word2vec",
                       label_Neut = "matches_neutral_word2vec",
                       label_Ambi = "matches_ambivalent_word2vec")
})
###############################



###############################
### overwrite summarized words AND set JSON protocol // get detailed protocol AND list of used words
## set protocol and overwrite data:
observeEvent(input$word2vecClickSummarize, {
  req(drawnCAM())
  
  print("summarize - skip")
  print(ST_rv$skip)
  if (!isTRUE(ST_rv$skip)) {
    tmp_overwriteData_getProtocols <- overwriteData_getProtocols(protocolCounter = ST_rv$protocolCounter_word2vec,
                                                                 protocolDetailedOut = globals$detailedProtocolword2vec, # !!!
                                                                 list_usedWords = globals$usedWords,
                                                                 dataSummarized = globals$dataCAMsummarized,
                                                                 
                                                                 searchArgument = "none",
                                                                 searchType = "word2vec",
                                                                 label_superordinate = "word2vecSupordinateWord",
                       label_Pos = "matches_positive_word2vec",
                       label_Neg = "matches_negative_word2vec",
                       label_Neut = "matches_neutral_word2vec",
                       label_Ambi = "matches_ambivalent_word2vec")
    
    # print("names(tmp_overwriteData_getProtocols)")
    # print(names(tmp_overwriteData_getProtocols))
    
    if(!is.null(tmp_overwriteData_getProtocols)){
      ## overwrite global data
      globals$dataCAMsummarized <- tmp_overwriteData_getProtocols$summarizedData
      ## overwrite protocol counter (detailed)
      ST_rv$protocolCounter_word2vec <- tmp_overwriteData_getProtocols$counterProtocol
      ## overwrite detailed protocol
      globals$detailedProtocolword2vec <- tmp_overwriteData_getProtocols$detailedProtocol
      ## overwrite already used words
      globals$usedWords <- tmp_overwriteData_getProtocols$usedWordsList
      
      ## append JSON protocol
      globals$protocol$modelwordVec[[length(globals$protocol$modelwordVec) + 1]] <-
        tmp_overwriteData_getProtocols$jsonProtocol
      #> change condition
      globals$condition <- c(globals$condition, "word2vec")
      
      print("length(unique(globals$dataCAMsummarized[[1]]$text_summarized))")
      print(length(unique(globals$dataCAMsummarized[[1]]$text_summarized)))
      
      
      print("globals$detailedProtocolword2vec")
      print(globals$detailedProtocolword2vec)
      
      print("globals$protocol$modelwordVec")
      print(globals$protocol$modelwordVec)
    }
  }
})
###############################

## list of used words
# > see global

















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
                          '<b>Approximate matching:</b> By using approximate string matching, string distances between all unique concepts in 
                          the dataset are computed (using optimal string alignment) to find words which have been written slightly differently. 
                          Internally the stringdist package applies the optimal string alignment distance to compute the distances between 
                          two strings. For example, using this method the distance between “dreams” and “dreasm” (spelling error) would 
                          be d = 1, because the adjacent character “s” would be transposed. Such a procedure can be used to correct for 
                          spelling errors and find words, which are written in singular / plural.'
                        )
                      ),
              tags$li(
                HTML(
                  '<b>Searching terms:</b> By using search terms you apply so called regular expressions, which is a concise language to 
                  describe patterns of text. Applying the stringr package, for example, using the expression “[[:digit:]]”, all drawn concepts 
                  including any digits can be identified. For possible regular expressions to use, please read the 
                  <a href="https://github.com/rstudio/cheatsheets/blob/main/strings.pdf" target="_blank">"cheatsheet" of the stringr package</a> and 
                  you can test combinations of regular expressions on the following webpage: 
                  <a href="https://regex101.com/" target="_blank">https://regex101.com/</a> '
                )
              ),
              tags$li(
                HTML(
                  '<b>Search for Synonyms:</b> By using search for synonyms all synonyms for single-worded concepts are automatically searched 
                  and internally the English synonym dictionary included in the 
                  <a href="https://cran.r-project.org/web/packages/qdap/" target="_blank">qdap</a> R package is applied. 
                  For example, the concepts “war” and “conflict” would be identified as synonyms.'
                )
              ),
              tags$li(
                HTML(
                  '<b>Apply word2vec model:</b> By applying a word2vec Model it is possible to compute the cosine similarity between drawn 
                  concepts pairwise to identify groups of drawn concepts with similar meaning. For example, cosine similarity between 
                  the words “responsibility” and “accountability” would be .70, whereby cosine similarity is ranging from -1 
                  (opposite vectors) to 1 (proportional vectors). The word vectors are "included" in a pre-trained language model 
                  from the Python library spaCy and there are currently language models for 25 languages, 
                  see: <a href="https://spacy.io/models" target="_blank">https://spacy.io/models</a>. 
                  A more detailed explenation how to apply the function can be found on our GitHub page: 
                  <a href="https://github.com/Camel-app/DataAnalysis/tree/main/Python_word2vec" target="_blank">https://github.com/Camel-app/DataAnalysis/tree/main/Python_word2vec</a>.'
                )
              )
                    )
                  ))
      })


    })
  }
