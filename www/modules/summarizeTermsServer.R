
summarizeTermsServer <-
  function(id, dataCAM, drawnCAM, parent, globals) {
    moduleServer(id, function(input, output, session) {
      ns <- NS(id)

      ## reactive values
      outUI <- reactiveValues(elements = NULL)
      module_rv <- reactiveValues(df = NULL, usedWordsAM = list(), checkUsedWordsOnce = TRUE)


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
          tags$li(HTML('<b>wordVec:</b> to be implemented.')),
          tags$li(
            HTML(
              '<b>not summarized:</b> Check which words you do not have summarized.'
            )
          ),
          tags$li(
            HTML(
              '<b>Reliability:</b> Download all your unique concepts as wordlists (EXCEL file) and let different raters summarize the concepts to compute scores for inter-rater-reliability.'
            )
          ),
          tags$li(
            HTML(
              '<b>Information:</b> Further information regarding the module.'
            )
          )
        )
      ))

      ## set output
      output$summarizeTermsOut <- renderUI({
        outUI$elements
      })

      ################################
      # single module options
      ################################
      observeEvent(c(
              input$approxMatch,
              input$searchTerms
        ), {
          req(dataCAM())
          req(drawnCAM())

          # print("clicked me")
          if(module_rv$checkUsedWordsOnce){
            module_rv$usedWordsAM <- globals$usedWords

            module_rv$checkUsedWordsOnce = FALSE
          }
        })




      ############
      # approximate matching
      ############
      #> UI
      observeEvent(input$approxMatch, {
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
                                Important: all words will be saved in the following format: Word_positive for words with positive valence,
                                Word_negative for words with negative valence and so on.
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
                  ns("supordinateWord"),
                  "Superordinate word:",
                  placeholder = "all words, which are not in the most left column"
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
              " unique concepts AFTER summarzing."
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
      am_rv <-
        reactiveValues(
          protocolCounter = 1L,
          counterPos = 0L,
          counterNeg = 0L,
          counterNeutral = 0L,
          counterAmbi = 0L,
          df = NULL,
          protocol = NULL,
          usedWords = list(),
          skip = FALSE
        )

      ## create data set for approximate matching
      optimalMatchSim <-
        eventReactive(c(
              input$clickAproxStriMatch,
              input$approxMatch # start when clicked on sidebar panel
        ), {
          req(dataCAM())
          req(drawnCAM())

          message("The value of input$clickAproxStriMatch is ",
                  input$clickAproxStriMatch)

          if (input$clickAproxStriMatch == 1) {
            am_rv$df <- dataCAM()
            ## remove deleted CAMs
            am_rv$df[[1]] <-
              am_rv$df[[1]][am_rv$df[[1]]$CAM %in% names(drawnCAM()), ]
            am_rv$df[[2]] <-
              am_rv$df[[2]][am_rv$df[[2]]$CAM %in% names(drawnCAM()), ]
            am_rv$df[[3]] <-
              am_rv$df[[3]][am_rv$df[[3]]$CAM.x %in% names(drawnCAM()), ]

            ## set values ob all ambivalent nodes to 0:
            # am_rv$df[[1]]$value <- ifelse(test = am_rv$df[[1]]$value == 10, yes = 0, no = am_rv$df[[1]]$value)



    if(length(globals$protocol$approximateMatching) > 0 || length(globals$protocol$searchTerms) > 0 ){
      am_rv$df[[1]]$text_summarized <- am_rv$df[[1]]$text_summarized
    }else{
       am_rv$df[[1]]$text_summarized <- am_rv$df[[1]]$text
    }

            if (is.null(module_rv$df)) {
              module_rv$df <- am_rv$df
            }
          }


          ## reset counter:
          am_rv$counterPos <- 0L
          am_rv$counterNeg <- 0L
          am_rv$counterNeutral <- 0L
          am_rv$counterAmbi <- 0L


          nodes_text <- unique(module_rv$df[[1]]$text_summarized)
          nodes_text <-
            unique(stringr::str_trim(nodes_text, side = "both"))
          h <- 1

          ## data dummy if no additional matches were found
          dat_out <- data.frame(num = 1, word = "no word found")
          for (i in 1:length(nodes_text)) {
            tmp_allother <- nodes_text[nodes_text[i] != nodes_text]
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

          # print(head(dat_out))

          dat_out
        })


      ### show number of
      # > unique concepts
      output$Nodes_unique <- renderText({
        length(unique(module_rv$df[[1]]$text))
      })
      # > found matches
      output$Nodes_matches <- renderText({
        max(optimalMatchSim()$num)
      })
      # > word to match:
      output$Nodes_matchesSinWor <- renderText({
        str_remove_all(string =  optimalMatchSim()$word[optimalMatchSim()$num == am_rv$counterPos][1], pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
        # optimalMatchSim()$word[optimalMatchSim()$num == am_rv$counterPos][1]
      })

      # > rounds to click:
      output$Nodes_matchesRounds <- renderText({
        max(optimalMatchSim()$num) - am_rv$counterPos + 1
      })


      ## implement skip functionality
      observeEvent(input$clickSkip, {
        am_rv$skip <- TRUE
      })

      ## number of nodes after summarizing words
      output$Nodes_unique_after <- renderText({
        tmp_text_summarized <- str_remove_all(string = module_rv$df[[1]]$text_summarized, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
        length(unique(tmp_text_summarized))
      })



      ### create labels for bucket list
      # > words with POSITIVE valence:
      labels_positive <-
        eventReactive(c(
          input$clickSummarize,
          input$clickSkip,
          input$clickAproxStriMatch
        ),
        {
          req(optimalMatchSim())
          am_rv$counterPos <- am_rv$counterPos + 1

          print("Counter labels_bucketlist positive:")
          print(am_rv$counterPos)

          ## avoid error
          if (am_rv$counterPos <= max(optimalMatchSim()$num)) {
            labels_out <-
              optimalMatchSim()[optimalMatchSim()$num == am_rv$counterPos, ]

            labels_list <- list()
            # print(table(dataCAM()[[1]]$value))
            for (i in 1:nrow(labels_out)) {
              tmp_dat <-
                module_rv$df[[1]][module_rv$df[[1]]$text_summarized == labels_out$word[i], ]
              tmp_value <-
                tmp_dat$value[tmp_dat$value > 0 &
                                tmp_dat$value < 10]

              # print(tmp_value)
              ## ignore zero value (if words are too similar)
              if (length(tmp_value) != 0) {
                ## compute N
                tmp_N <- length(tmp_value)
                ## compute mean
                tmp_mean <-
                  round(x = mean(tmp_value), digits = 2)
                ## compute SD
                tmp_SD <-
                  round(x = sd(tmp_value), digits = 2)

                ## remove X_positive, ...
                tmp_labelout <-
                  str_remove_all(string = labels_out$word[i], pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
                labels_list[[labels_out$word[i]]] <-
                  htmltools::tags$div(paste0(
                    tmp_labelout,
                    "   (N=",
                    tmp_N,
                    ", M=",
                    tmp_mean,
                    ", SD=",
                    tmp_SD,
                    ")"
                  ))
              }
            }

            ## reset skip condition
            am_rv$skip <- FALSE

            labels_list
          } else {
            print("maximum reached")
            am_rv$counterPos <- am_rv$counterPos - 1
            NULL
          }
        })

      # > words with NEGATIVE valence:
      labels_negative <-
        eventReactive(c(
          input$clickSummarize,
          input$clickSkip,
          input$clickAproxStriMatch
        ),
        {
          req(optimalMatchSim())
          am_rv$counterNeg <- am_rv$counterNeg + 1

          print("Counter labels_bucketlist negative:")
          print(am_rv$counterNeg)

          ## avoid error
          if (am_rv$counterNeg <= max(optimalMatchSim()$num)) {
            labels_out <-
              optimalMatchSim()[optimalMatchSim()$num == am_rv$counterNeg, ]

            labels_list <- list()
            for (i in 1:nrow(labels_out)) {
              tmp_dat <-
                module_rv$df[[1]][module_rv$df[[1]]$text_summarized == labels_out$word[i], ]
              tmp_value <- tmp_dat$value[tmp_dat$value < 0]
              ## ignore zero value (if words are to similar)
              if (length(tmp_value) != 0) {
                ## compute N
                tmp_N <- length(tmp_value)
                ## compute mean
                tmp_mean <-
                  round(x = mean(tmp_value), digits = 2)
                ## compute SD
                tmp_SD <-
                  round(x = sd(tmp_value), digits = 2)

                ## remove X_positive, ...
                tmp_labelout <-
                  str_remove_all(string = labels_out$word[i], pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
                labels_list[[labels_out$word[i]]] <-
                  htmltools::tags$div(paste0(
                    tmp_labelout,
                    "   (N=",
                    tmp_N,
                    ", M=",
                    tmp_mean,
                    ", SD=",
                    tmp_SD,
                    ")"
                  ))
              }
            }


            ## reset skip condition
            am_rv$skip <- FALSE

            labels_list
          } else {
            print("maximum reached")
            am_rv$counterNeg <- am_rv$counterNeg - 1
            NULL
          }
        })



      # > words with NEUTRAL valence:
      labels_neutral <-
        eventReactive(c(
          input$clickSummarize,
          input$clickSkip,
          input$clickAproxStriMatch
        ),
        {
          req(optimalMatchSim())
          am_rv$counterNeutral <- am_rv$counterNeutral + 1

          print("Counter labels_bucketlist neutral:")
          print(am_rv$counterNeutral)

          ## avoid error
          if (am_rv$counterNeutral <= max(optimalMatchSim()$num)) {
            labels_out <-
              optimalMatchSim()[optimalMatchSim()$num == am_rv$counterNeutral, ]

            labels_list <- list()
            for (i in 1:nrow(labels_out)) {
              tmp_dat <-
                module_rv$df[[1]][module_rv$df[[1]]$text_summarized == labels_out$word[i], ]
              tmp_value <- tmp_dat$value[tmp_dat$value == 0]
              ## ignore zero value (if words are to similar)
              if (length(tmp_value) != 0) {
                ## compute N
                tmp_N <- length(tmp_value)
                ## compute mean
                tmp_mean <-
                  round(x = mean(tmp_value), digits = 2)
                ## compute SD
                tmp_SD <-
                  round(x = sd(tmp_value), digits = 2)

                ## remove X_positive, ...
                tmp_labelout <-
                  str_remove_all(string = labels_out$word[i], pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
                labels_list[[labels_out$word[i]]] <-
                  htmltools::tags$div(paste0(
                    tmp_labelout,
                    "   (N=",
                    tmp_N,
                    ", M=",
                    tmp_mean,
                    ", SD=",
                    tmp_SD,
                    ")"
                  ))
              }
            }


            ## reset skip condition
            am_rv$skip <- FALSE

            labels_list
          } else {
            print("maximum reached")
            am_rv$counterNeutral <- am_rv$counterNeutral - 1
            NULL
          }
        })

      # > words with AMBIVALENT valence:
      labels_ambivalent <-
        eventReactive(c(
          input$clickSummarize,
          input$clickSkip,
          input$clickAproxStriMatch
        ),
        {
          req(optimalMatchSim())
          am_rv$counterAmbi <- am_rv$counterAmbi + 1

          print("Counter labels_bucketlist ambivalent:")
          print(am_rv$counterAmbi)

          ## avoid error
          if (am_rv$counterAmbi <= max(optimalMatchSim()$num)) {
            labels_out <-
              optimalMatchSim()[optimalMatchSim()$num == am_rv$counterAmbi, ]

            labels_list <- list()
            for (i in 1:nrow(labels_out)) {
              tmp_dat <-
                module_rv$df[[1]][module_rv$df[[1]]$text_summarized == labels_out$word[i], ]
              tmp_value <- tmp_dat$value[tmp_dat$value == 10]
              tmp_value <-
                rep(x = 0, times = length(tmp_value)) # set to 0
              ## ignore zero value (if words are to similar)
              if (length(tmp_value) != 0) {
                ## compute N
                tmp_N <- length(tmp_value)
                ## compute mean
                tmp_mean <-
                  round(x = mean(tmp_value), digits = 2)
                ## compute SD
                tmp_SD <-
                  round(x = sd(tmp_value), digits = 2)

                ## remove X_positive, ...
                tmp_labelout <-
                  str_remove_all(string = labels_out$word[i], pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
                labels_list[[labels_out$word[i]]] <-
                  htmltools::tags$div(paste0(
                    tmp_labelout,
                    "   (N=",
                    tmp_N,
                    ", M=",
                    tmp_mean,
                    ", SD=",
                    tmp_SD,
                    ")"
                  ))
              }
            }

            ## reset skip condition
            am_rv$skip <- FALSE

            labels_list
          } else {
            print("maximum reached")
            am_rv$counterAmbi <- am_rv$counterAmbi - 1
            NULL
          }
        })


      ## update text field value of "supordinateWord":
      getSupordinateWord <- function(){
                if (!identical(input$matches_positive, character(0))) {
          updateTextInput(
            session,
            "supordinateWord",
            value = str_remove_all(
              string = input$matches_positive[1],
              pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
            )
          )
        } else if (!identical(input$matches_negative, character(0))) {
          updateTextInput(
            session,
            "supordinateWord",
            value = str_remove_all(
              string = input$matches_negative[1],
              pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
            )
          )
        } else if (!identical(input$matches_neutral, character(0))) {
          updateTextInput(
            session,
            "supordinateWord",
            value = str_remove_all(
              string = input$matches_neutral[1],
              pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
            )
          )
        } else if (!identical(input$matches_ambivalent, character(0))) {
          updateTextInput(
            session,
            "supordinateWord",
            value = str_remove_all(
              string = input$matches_ambivalent[1],
              pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
            )
          )
        }
      }

    observe({
        getSupordinateWord()
      })
    observeEvent(input$approxMatch, { # start when clicked on sidebar panel
       getSupordinateWord()
      })



      ## Render bucket list
      output$bucketlist <- renderUI({
        bucket_list(
          header = NULL,
          group_name = ns("bucket_list_group2"),
          orientation = "horizontal",
          add_rank_list(
            text = "move here to not summarize single words",
            labels = NULL,
            # labels from row selection
            input_id = ns("matches_suggestion"),
            options = sortable_options(multiDrag = TRUE)
          ),
          add_rank_list(
            text = "suggested similar words with POSITIVE valence:",
            labels = labels_positive(),
            input_id = ns("matches_positive"),
            options = sortable_options(multiDrag = TRUE)
          ),
          add_rank_list(
            text = "suggested similar words with NEGATIVE valence:",
            labels = labels_negative(),
            input_id = ns("matches_negative"),
            options = sortable_options(multiDrag = TRUE)
          ),
          add_rank_list(
            text = "suggested similar words with NEUTRAL valence:",
            labels = labels_neutral(),
            input_id = ns("matches_neutral"),
            options = sortable_options(multiDrag = TRUE)
          ),
          add_rank_list(
            text = "suggested similar words with AMBIVALENT valence:",
            labels = labels_ambivalent(),
            input_id = ns("matches_ambivalent"),
            options = sortable_options(multiDrag = TRUE)
          )
        )
      })


      ## set protocol and overwrite data:
      observeEvent(input$clickSummarize, {
        if (!isTRUE(am_rv$skip)) {
            ## avoid adding multiple suffix
            text_summarized_NoSuffix <- str_remove_all(
              string = module_rv$df[[1]]$text_summarized,
              pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
            )
          # positive valence
          tmp_value_protocol_positive <-
            module_rv$df[[1]]$value[text_summarized_NoSuffix %in% input$matches_positive &
                                      module_rv$df[[1]]$value > 0 &
                                      module_rv$df[[1]]$value < 10]
          # negative valence
          tmp_value_protocol_negative <-
            module_rv$df[[1]]$value[text_summarized_NoSuffix %in% input$matches_negative &
                                      module_rv$df[[1]]$value < 0]
          # neutral valence
          tmp_value_protocol_neutral <-
            module_rv$df[[1]]$value[text_summarized_NoSuffix %in% input$matches_neutral &
                                      module_rv$df[[1]]$value == 0]
          # ambivalent valence
          tmp_value_protocol_ambivalent <-
            module_rv$df[[1]]$value[text_summarized_NoSuffix %in% input$matches_ambivalent &
                                      module_rv$df[[1]]$value == 10]
          # tmp_value_protocol_ambivalent <- ifelse(test = tmp_value_protocol_ambivalent == 10, yes = 0, no = tmp_value_protocol_ambivalent)


          tmp_protocol <- data.frame(
            "time" = as.character(as.POSIXct(Sys.time())),
            "searchRound" = am_rv$protocolCounter,
            "stringDistance" = input$maxStringDis,
            "superordinate" = input$supordinateWord,
            "subordinate_positive" = paste0(input$matches_positive, collapse = " // "),
            "N_positive" = length(tmp_value_protocol_positive),
            "mean_positive" = round(
              x = mean(tmp_value_protocol_positive),
              digits = 2
            ),
            "sd_positive" = round(
              x = sd(tmp_value_protocol_positive),
              digits = 2
            ),
            "subordinate_negative" = paste0(input$matches_negative, collapse = " // "),
            "N_negative" = length(tmp_value_protocol_negative),
            "mean_negative" = round(
              x = mean(tmp_value_protocol_negative),
              digits = 2
            ),
            "sd_negative" = round(
              x = sd(tmp_value_protocol_negative),
              digits = 2
            ),
            "subordinate_neutral" = paste0(input$matches_neutral, collapse = " // "),
            "N_neutral" = length(tmp_value_protocol_neutral),
            "subordinate_ambivalent" = paste0(input$matches_ambivalent, collapse = " // "),
            "N_ambivalent" = length(tmp_value_protocol_ambivalent)
          )

          if (am_rv$protocolCounter == 1) {
            am_rv$protocol <- tmp_protocol
          } else {
            #  tmp <- tmp_protocol
            am_rv$protocol <-
              rbind(am_rv$protocol, tmp_protocol)
          }

          #print("am_rv$protocol:")
          #print(am_rv$protocol)
          am_rv$protocolCounter <- am_rv$protocolCounter + 1



          ## save changed text in
          tmpWordsSummarized <- c()
          if (!identical(input$matches_positive, character(0))) {
           ## remove suffix
           matches_positive_NoSuffix <- str_remove_all(
              string = input$matches_positive,
              pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
            )

            module_rv$df[[1]]$text_summarized[text_summarized_NoSuffix %in% matches_positive_NoSuffix &
                                                module_rv$df[[1]]$value > 0 &
                                                module_rv$df[[1]]$value < 10] <-
              paste0(input$supordinateWord, "_positive")

            tmpWordsSummarized <- paste0(matches_positive_NoSuffix, "_positive")
            # which words have already been used?
            # module_rv$usedWordsAM[["positive"]] <- append(module_rv$usedWordsAM[["positive"]] , input$supordinateWord)
            module_rv$usedWordsAM[["positive"]] <-
              append(module_rv$usedWordsAM[["positive"]],
                     paste0(
                       input$supordinateWord,
                       " (",
                       paste0(matches_positive_NoSuffix, collapse = " // "),
                       ")"
                     ))


          }
          if (!identical(input$matches_negative, character(0))) {
            ## remove suffix
            matches_negative_NoSuffix <- str_remove_all(
              string = input$matches_negative,
              pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
            )
            module_rv$df[[1]]$text_summarized[text_summarized_NoSuffix %in% matches_negative_NoSuffix &
                                                module_rv$df[[1]]$value < 0] <-
              paste0(input$supordinateWord, "_negative")

            if (length(tmpWordsSummarized) == 0) {
              tmpWordsSummarized <- paste0(matches_negative_NoSuffix, "_negative")
            } else{
              tmpWordsSummarized <- c(tmpWordsSummarized, paste0(matches_negative_NoSuffix, "_negative"))
            }

            # which words have already been used?
            module_rv$usedWordsAM[["negative"]] <-
              append(module_rv$usedWordsAM[["negative"]],
                     paste0(
                       input$supordinateWord,
                       " (",
                       paste0(matches_negative_NoSuffix, collapse = " // "),
                       ")"
                     ))
          }
          if (!identical(input$matches_neutral, character(0))) {
            ## remove suffix
            matches_neutral_NoSuffix <- str_remove_all(
              string = input$matches_neutral,
              pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
            )
            module_rv$df[[1]]$text_summarized[text_summarized_NoSuffix %in% matches_neutral_NoSuffix &
                                                module_rv$df[[1]]$value == 0] <-
              paste0(input$supordinateWord, "_neutral")

            if (length(tmpWordsSummarized) == 0) {
              tmpWordsSummarized <- paste0(matches_neutral_NoSuffix, "_neutral")
            } else{
              tmpWordsSummarized <- c(tmpWordsSummarized, paste0(matches_neutral_NoSuffix, "_neutral"))
            }
            # which words have already been used?
            module_rv$usedWordsAM[["neutral"]] <-
              append(module_rv$usedWordsAM[["neutral"]],
                     paste0(
                       input$supordinateWord,
                       " (",
                       paste0(matches_neutral_NoSuffix, collapse = " // "),
                       ")"
                     ))
          }
          if (!identical(input$matches_ambivalent, character(0))) {
            ## remove suffix
            matches_ambivalent_NoSuffix <- str_remove_all(
              string = input$matches_ambivalent,
              pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
            )
            module_rv$df[[1]]$text_summarized[text_summarized_NoSuffix %in% matches_ambivalent_NoSuffix &
                                                module_rv$df[[1]]$value == 10] <-
              paste0(input$supordinateWord, "_ambivalent")

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
                       input$supordinateWord,
                       " (",
                       paste0(matches_ambivalent_NoSuffix, collapse = " // "),
                       ")"
                     ))
          }

          #print(module_rv$df[[1]])
          #print("tmpWordsSummarized:")
          #print(tmpWordsSummarized)

          tmp_list = vector(mode = "list", length = 4)
          tmp_list[[1]] = as.character(as.POSIXct(Sys.time()))
          tmp_list[[2]] = tmpWordsSummarized
          tmp_list[[3]] = input$supordinateWord
          tmp_list[[4]] = input$maxStringDis
          names(tmp_list) <-
            c("time",
              "wordsFound",
              "supordinateWord",
              "stringDistance")
          globals$protocol$approximateMatching[[length(globals$protocol$approximateMatching) + 1]] <-
            tmp_list

          #> change condition
          globals$condition <-
            c(globals$condition, "approximateMatching")
        }

      })


      output$alreadyUsedWords <- renderDataTable({
        # https://stackoverflow.com/questions/27153979/converting-nested-list-unequal-length-to-data-frame
        indx <- sapply(module_rv$usedWordsAM, length)
        outdat <- as.data.frame(do.call(rbind, lapply(
          module_rv$usedWordsAM, `length<-`,
          max(indx)
        )))
        t(outdat)
      })



      ############
      # searching terms
      ############

      #> UI
      observeEvent(input$searchTerms, {
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
        search_rv <-
        reactiveValues(
          df = NULL,
          counter = NULL,
          protocolCounter = 1L,
          protocol = NULL
        )
         ## create data set for approximate matching

        regularExpOut <-eventReactive(c(
              input$regularExpSearch,
              input$searchTerms # start when clicked on sidebar panel
        ), {
            req(dataCAM())
            req(drawnCAM())

          message("The value of input$regularExpSearch is ", input$regularExpSearch)

            if(input$regularExpSearch == 1){
                search_rv$df <- dataCAM()
                ## remove deleted CAMs
                search_rv$df[[1]] <- search_rv$df[[1]][search_rv$df[[1]]$CAM %in% names(drawnCAM()),]
                search_rv$df[[2]] <- search_rv$df[[2]][search_rv$df[[2]]$CAM %in% names(drawnCAM()),]
                search_rv$df[[3]] <- search_rv$df[[3]][search_rv$df[[3]]$CAM.x %in% names(drawnCAM()),]

              ## set values ob all ambivalent nodes to 0:
              # search_rv$df[[1]]$value <- ifelse(test = search_rv$df[[1]]$value == 10, yes = 0, no = search_rv$df[[1]]$value)



    if(length(globals$protocol$approximateMatching) > 0 || length(globals$protocol$searchTerms) > 0 ){
      search_rv$df[[1]]$text_summarized <- search_rv$df[[1]]$text_summarized
    }else{
       search_rv$df[[1]]$text_summarized <- search_rv$df[[1]]$text
    }


            if (is.null(module_rv$df)) {
              module_rv$df <- search_rv$df
            }
            }

            # print(input$regularExp)
            if(nchar(x = input$regularExp) > 0){
                ## remove X_positive, ...
                #module_rv$df[[1]]$text <- str_remove_all(string = module_rv$df[[1]]$text, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
                # words_out <- unique(str_subset(string = module_rv$df[[1]]$text, pattern = input$regularExp))
                words_out <- unique(module_rv$df[[1]]$text_summarized[str_detect(string = str_remove_all(string = module_rv$df[[1]]$text_summarized,
                pattern = "_positive$|_negative$|_neutral$|_ambivalent$"),
                pattern = input$regularExp, negate = FALSE)])
            }else{
                words_out <- NULL
            }


            search_rv$counter <- 0
            #print("words_out: ")
            #print(words_out)
            words_out
        })

      ### show number of
      # > unique concepts
        output$Nodes_uniqueSearch <- renderText({
          length(unique(module_rv$df[[1]]$text))
        })


        ## create labels for bucket lists
        # > words with POSITIVE valence:
        labels_positiveSearch <- eventReactive(c(input$clickSummarizeSearch, input$regularExpSearch), {
            req(regularExpOut())

            labels_out <- regularExpOut()
            labels_list <- list()
            for (i in 1:length(labels_out)) {
                tmp_value <-  module_rv$df[[1]]$value[module_rv$df[[1]]$text_summarized == labels_out[i]]
                tmp_value <- tmp_value[tmp_value > 0 & tmp_value < 10] # to positive


                ## ignore zero value (if words are to similar)
                if (length(tmp_value) != 0) {
                    ## compute N
                    tmp_N <- length(tmp_value)
                    ## compute mean
                    tmp_mean <- round(x = mean(tmp_value), digits = 2)
                    ## compute SD
                    tmp_SD <- round(x = sd(tmp_value), digits = 2)

               ## remove X_positive, ...
                    tmp_labelout <- str_remove_all(string = labels_out[i], pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
               labels_list[[labels_out[i]]] = htmltools::tags$div(
                        paste0(tmp_labelout, " (N=", tmp_N,", M=", tmp_mean, ", SD=", tmp_SD, ")")
                    )
                }
            }
            labels_list
        })

        # > words with NEGATIVE valence:
        labels_negativeSearch <- eventReactive(c(input$clickSummarizeSearch, input$regularExpSearch), {
            req(regularExpOut())

            labels_out <- regularExpOut()
            labels_list <- list()
            for (i in 1:length(labels_out)) {
                tmp_value <-  module_rv$df[[1]]$value[module_rv$df[[1]]$text_summarized == labels_out[i]]
                tmp_value <- tmp_value[tmp_value < 0] # to negative

                ## ignore zero value (if words are to similar)
                if (length(tmp_value) != 0) {
                    ## compute N
                    tmp_N <- length(tmp_value)
                    ## compute mean
                    tmp_mean <- round(x = mean(tmp_value), digits = 2)
                    ## compute SD
                    tmp_SD <- round(x = sd(tmp_value), digits = 2)

                 ## remove X_positive, ...
                    tmp_labelout <- str_remove_all(string = labels_out[i], pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
               labels_list[[labels_out[i]]] = htmltools::tags$div(
                        paste0(tmp_labelout, " (N=", tmp_N,", M=", tmp_mean, ", SD=", tmp_SD, ")")
                    )
                }
            }
            labels_list
        })


          # > words with NEUTRAL valence:
        labels_neutralSearch <- eventReactive(c(input$clickSummarizeSearch, input$regularExpSearch), {
            req(regularExpOut())

            labels_out <- regularExpOut()
            labels_list <- list()
            for (i in 1:length(labels_out)) {
                tmp_value <-  module_rv$df[[1]]$value[module_rv$df[[1]]$text_summarized == labels_out[i]]
                tmp_value <- tmp_value[tmp_value == 0] # to neutral

                ## ignore zero value (if words are to similar)
                if (length(tmp_value) != 0) {
                    ## compute N
                    tmp_N <- length(tmp_value)
                    ## compute mean
                    tmp_mean <- round(x = mean(tmp_value), digits = 2)
                    ## compute SD
                    tmp_SD <- round(x = sd(tmp_value), digits = 2)

               ## remove X_positive, ...
                    tmp_labelout <- str_remove_all(string = labels_out[i], pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
               labels_list[[labels_out[i]]] = htmltools::tags$div(
                        paste0(tmp_labelout, "   (N=", tmp_N," ,M=", tmp_mean, " ,SD=", tmp_SD, ")")
                    )
                }
            }
            labels_list
        })

          # > words with AMBIVALENT valence:
        labels_ambivalentSearch <- eventReactive(c(input$clickSummarizeSearch, input$regularExpSearch), {
            req(regularExpOut())

            labels_out <- regularExpOut()
            labels_list <- list()
            for (i in 1:length(labels_out)) {
                tmp_value <-  module_rv$df[[1]]$value[module_rv$df[[1]]$text_summarized == labels_out[i]]
                tmp_value <- tmp_value[tmp_value == 10] # to ambivalent
                tmp_value <- rep(x = 0, times = length(tmp_value)) # set to 0
                ## ignore zero value (if words are to similar)
                if (length(tmp_value) != 0) {
                    ## compute N
                    tmp_N <- length(tmp_value)
                    ## compute mean
                    tmp_mean <- round(x = mean(tmp_value), digits = 2)
                    ## compute SD
                    tmp_SD <- round(x = sd(tmp_value), digits = 2)

                 ## remove X_positive, ...
                    tmp_labelout <- str_remove_all(string = labels_out[i], pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
               labels_list[[labels_out[i]]] = htmltools::tags$div(
                        paste0(tmp_labelout, " (N=", tmp_N,", M=", tmp_mean, ", SD=", tmp_SD, ")")
                    )
                }
            }
            labels_list
        })


## Render bucket list
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



        observeEvent(input$clickSummarizeSearch, {

           ## avoid adding multiple suffix
            text_summarized_NoSuffix <- str_remove_all(
              string = module_rv$df[[1]]$text_summarized,
              pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
            )
 # positive valence
          tmp_value_protocol_positive <-
            module_rv$df[[1]]$value[text_summarized_NoSuffix %in% input$matches_positiveSearch &
                                      module_rv$df[[1]]$value > 0 &
                                      module_rv$df[[1]]$value < 10]
          # negative valence
          tmp_value_protocol_negative <-
            module_rv$df[[1]]$value[text_summarized_NoSuffix %in% input$matches_negativeSearch &
                                      module_rv$df[[1]]$value < 0]
          # neutral valence
          tmp_value_protocol_neutral <-
            module_rv$df[[1]]$value[text_summarized_NoSuffix %in% input$matches_neutralSearch &
                                      module_rv$df[[1]]$value == 0]
          # ambivalent valence
          tmp_value_protocol_ambivalent <-
            module_rv$df[[1]]$value[text_summarized_NoSuffix %in% input$matches_ambivalentSearch &
                                      module_rv$df[[1]]$value == 10]


          tmp_protocol <- data.frame(
            "time" = as.character(as.POSIXct(Sys.time())),
            "searchRound" = am_rv$protocolCounter,
            "stringDistance" = input$regularExp,
            "superordinate" = input$supordinateWordSearch,
            "subordinate_positive" = paste0(input$matches_positiveSearch, collapse = " // "),
            "N_positive" = length(tmp_value_protocol_positive),
            "mean_positive" = round(
              x = mean(tmp_value_protocol_positive),
              digits = 2
            ),
            "sd_positive" = round(
              x = sd(tmp_value_protocol_positive),
              digits = 2
            ),
            "subordinate_negative" = paste0(input$matches_negativeSearch, collapse = " // "),
            "N_negative" = length(tmp_value_protocol_negative),
            "mean_negative" = round(
              x = mean(tmp_value_protocol_negative),
              digits = 2
            ),
            "sd_negative" = round(
              x = sd(tmp_value_protocol_negative),
              digits = 2
            ),
            "subordinate_neutral" = paste0(input$matches_neutralSearch, collapse = " // "),
            "N_neutral" = length(tmp_value_protocol_neutral),
            "subordinate_ambivalent" = paste0(input$matches_ambivalentSearch, collapse = " // "),
            "N_ambivalent" = length(tmp_value_protocol_ambivalent)
          )


                if (search_rv$protocolCounter == 1) {
                    search_rv$protocol <- tmp_protocol
                } else {
                    search_rv$protocol <- rbind(search_rv$protocol, tmp_protocol)
                }

#print("head(search_rv$protocolCounter): ")
#print(head(search_rv$protocolCounter))
                search_rv$protocolCounter <- search_rv$protocolCounter + 1



              ## save changed text in
          tmpWordsSummarized <- c()
          if (!identical(input$matches_positiveSearch, character(0))) {
           ## remove suffix
           matches_positive_NoSuffix <- str_remove_all(
              string = input$matches_positiveSearch,
              pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
            )

            module_rv$df[[1]]$text_summarized[text_summarized_NoSuffix %in% matches_positive_NoSuffix &
                                                module_rv$df[[1]]$value > 0 &
                                                module_rv$df[[1]]$value < 10] <-
              paste0(input$supordinateWordSearch, "_positive")

            tmpWordsSummarized <- paste0(matches_positive_NoSuffix, "_positive")
            # which words have already been used?
            # module_rv$usedWordsAM[["positive"]] <- append(module_rv$usedWordsAM[["positive"]] , input$supordinateWord)
            module_rv$usedWordsAM[["positive"]] <-
              append(module_rv$usedWordsAM[["positive"]],
                     paste0(
                       input$supordinateWordSearch,
                       " (",
                       paste0(matches_positive_NoSuffix, collapse = " // "),
                       ")"
                     ))


          }
          if (!identical(input$matches_negativeSearch, character(0))) {
            ## remove suffix
            matches_negative_NoSuffix <- str_remove_all(
              string = input$matches_negativeSearch,
              pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
            )
            module_rv$df[[1]]$text_summarized[text_summarized_NoSuffix %in% matches_negative_NoSuffix &
                                                module_rv$df[[1]]$value < 0] <-
              paste0(input$supordinateWordSearch, "_negative")

            if (length(tmpWordsSummarized) == 0) {
              tmpWordsSummarized <- paste0(matches_negative_NoSuffix, "_negative")
            } else{
              tmpWordsSummarized <- c(tmpWordsSummarized, paste0(matches_negative_NoSuffix, "_negative"))
            }

            # which words have already been used?
            module_rv$usedWordsAM[["negative"]] <-
              append(module_rv$usedWordsAM[["negative"]],
                     paste0(
                       input$supordinateWordSearch,
                       " (",
                       paste0(matches_negative_NoSuffix, collapse = " // "),
                       ")"
                     ))
          }
          if (!identical(input$matches_neutralSearch, character(0))) {
            ## remove suffix
            matches_neutral_NoSuffix <- str_remove_all(
              string = input$matches_neutralSearch,
              pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
            )
            module_rv$df[[1]]$text_summarized[text_summarized_NoSuffix %in% matches_neutral_NoSuffix &
                                                module_rv$df[[1]]$value == 0] <-
              paste0(input$supordinateWordSearch, "_neutral")

            if (length(tmpWordsSummarized) == 0) {
              tmpWordsSummarized <- paste0(matches_neutral_NoSuffix, "_neutral")
            } else{
              tmpWordsSummarized <- c(tmpWordsSummarized, paste0(matches_neutral_NoSuffix, "_neutral"))
            }
            # which words have already been used?
            module_rv$usedWordsAM[["neutral"]] <-
              append(module_rv$usedWordsAM[["neutral"]],
                     paste0(
                       input$supordinateWordSearch,
                       " (",
                       paste0(matches_neutral_NoSuffix, collapse = " // "),
                       ")"
                     ))
          }
          if (!identical(input$matches_ambivalentSearch, character(0))) {
            ## remove suffix
            matches_ambivalent_NoSuffix <- str_remove_all(
              string = input$matches_ambivalentSearch,
              pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
            )
            module_rv$df[[1]]$text_summarized[text_summarized_NoSuffix %in% matches_ambivalent_NoSuffix &
                                                module_rv$df[[1]]$value == 10] <-
              paste0(input$supordinateWordSearch, "_ambivalent")

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
                       input$supordinateWordSearch,
                       " (",
                       paste0(matches_ambivalent_NoSuffix, collapse = " // "),
                       ")"
                     ))
          }


          #print(module_rv$df[[1]])
          #print("tmpWordsSummarized:")
          #print(tmpWordsSummarized)
          tmp_list = vector(mode = "list", length = 4)
          tmp_list[[1]] = as.character(as.POSIXct(Sys.time()))
          tmp_list[[2]] = tmpWordsSummarized
          tmp_list[[3]] = input$supordinateWordSearch
          tmp_list[[4]] = input$regularExp
          names(tmp_list) <-
            c("time",
              "wordsFound",
              "supordinateWord",
              "regularExpression")
          globals$protocol$searchTerms[[length(globals$protocol$searchTerms) + 1]] <-
            tmp_list

          #> change condition
          globals$condition <-
            c(globals$condition, "searchTerms")
        })




        ## update text field value of "supordinateWordSearch":
        observe({
            if (!identical(input$matches_positiveSearch, character(0))) {
                updateTextInput(session, "supordinateWordSearch",
                    value = str_remove_all(string = input$matches_positiveSearch[1], pattern = "_positive$|_negative$|_neutral$|_ambivalent$"))
            } else if (!identical(input$matches_negativeSearch, character(0))) {
                updateTextInput(session, "supordinateWordSearch",
                    value = str_remove_all(string = input$matches_negativeSearch[1], pattern = "_positive$|_negative$|_neutral$|_ambivalent$"))
            } else if (!identical(input$matches_neutralSearch, character(0))) {
                updateTextInput(session, "supordinateWordSearch",
                    value = str_remove_all(string = input$matches_neutralSearch[1], pattern = "_positive$|_negative$|_neutral$|_ambivalent$"))
            } else if (!identical(input$matches_ambivalentSearch, character(0))) {
                updateTextInput(session, "supordinateWordSearch",
                    value = str_remove_all(string = input$matches_ambivalentSearch[1], pattern = "_positive$|_negative$|_neutral$|_ambivalent$"))
            }
        })


        ## show already used words
        output$usedwWordsSearch <- renderDataTable({
              # https://stackoverflow.com/questions/27153979/converting-nested-list-unequal-length-to-data-frame
            indx <- sapply(module_rv$usedWordsAM, length)
            outdat <- as.data.frame(do.call(rbind,lapply(module_rv$usedWordsAM, `length<-`,
                                          max(indx))))
            t(outdat)
          })


        output$Nodes_unique_afterSearch <- renderText({
          tmp_text_summarized <- str_remove_all(string = module_rv$df[[1]]$text_summarized, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
          length(unique(tmp_text_summarized))
        })





      ############
      # synonyms, word2vec
      ############
      #> UI general
      observeEvent(input$Synonyms_wordVec, {
        ## change UI
        outUI$elements <- tagList(
          tags$h2("Summarize by searching for synonyms or word2vec models"),
          tags$br(),
     tags$div(
            HTML(
              "This module option is divided in two parts: you can search for similar group of terms  (a) by simply searching for synonyms
               and / or
               (b) by applying a word2vec model (neural network model which had learned word associations from a large corpus of text).
               <br>    
               <br>
               <b>Important: This module option can handle only concepts written with a <u>single word, which exsists</u>.</b>
               <br>
               Therefore, it could be necessary to summarize your concepts manually / check spelling errors and 
              inconsistent writing of concepts before using this module.        
              <br>
              Please click on: "
            ),
            style = "font-size:14px"
          ),
          tags$br(),
          div(
          style = "margin: 0 auto; width: 50%; text-align:center;",
          actionButton(
            ns("a_synonyms"),
            HTML('(a)<br>Searching for<br>Synonyms'),
            style = "width: 250px; height: 150px; font-size: 22px;"
          ),
          actionButton(
            ns("b_word2vec"),
            HTML('(b)<br>Applying a<br>word2vec Model'),
            style = "width: 250px; height: 150px; margin-left:20px; font-size: 22px;"
          )
        ),
        tags$br(),
        tags$p(
              "Your CAM dataset has at the moment ",
              tags$b(textOutput(ns(
                "numOneWords"
              ), inline = TRUE)),
              " unique concepts, which contains only one word, which are ",
              tags$b(textOutput(ns(
                "numOneWordsPercentage"
              ), inline = TRUE)),
              " percent of your total unique concepts."
            ),
        )
      })


      #> Server synonyms, word2vec general
      observeEvent(input$Synonyms_wordVec, {
          if (is.null(module_rv$df)) {
          showModal(
            modalDialog(
              title = "Run Approximate matching or Searching terms",
              paste0("You need to run the approximate matching or searching terms function before you 
              can use the module option."),
              easyClose = TRUE,
              footer = tagList(modalButton("Ok"))
            )
        )
          }else{
            tmpOneWords <- str_split(string = unique(module_rv$df[[1]]$text), pattern = " ", simplify = TRUE)

            output$numOneWords <- renderText({
              sum(rowSums(x = tmpOneWords != "") == 1)
              })

            output$numOneWordsPercentage <- renderText({
              round(x = sum(rowSums(x = tmpOneWords != "") == 1) / nrow(tmpOneWords), digits = 2) * 100
              })
          }
          })


       #> UI a_synonyms
      observeEvent(input$a_synonyms, {
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


      #> Server synonyms
      #> Server
synonym_rv <-
  reactiveValues(
    protocolCounter = 1L,
          counterPos = 0L,
          counterNeg = 0L,
          counterNeutral = 0L,
          counterAmbi = 0L,
    df = NULL,
    protocol = NULL,
    usedWords = list(),
    skip = FALSE
  )

## create data set for approximate matching
reducedSynonymList <-
  eventReactive(c(
    input$a_synonymsStart), {

    message("The choosen language is ",
            input$a_synonymsLanugage)


    ## get data input:
    if(any(colnames(module_rv$df) == "text_summarized")){
      # remove all suffix
      tmp_text <- str_remove_all(string = module_rv$df[[1]]$text_summarized, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
      }else{
        tmp_text <- module_rv$df[[1]]$text
      }
    
    if (input$a_synonymsStart == 1) {
      synonym_rv$df <- module_rv$df
    }

    ## get raw synonym list
    raw_SynonymList <- RawSynonymList(vectorWords = tmp_text) # !!! IF language
    
    ## out perentage of matches
    output$a_synonymsPercentageFound <- renderText({
      raw_SynonymList[[2]]
      })

    ## reset counter:
    synonym_rv$counterPos <- 0L
    synonym_rv$counterNeg <- 0L
        synonym_rv$counterNeutral <- 0L
            synonym_rv$counterAmbi <- 0L
  
    ## get reduced synonym list
reduced_SynonymList <- SummarizedSynonymList(listSynonyms = raw_SynonymList[[1]])
for(i in 1:length(reduced_SynonymList)){
  reduced_SynonymList <- SummarizedSynonymList(listSynonyms = reduced_SynonymList)
}

reduced_SynonymList
  })


        ## number of groups of synonyms
      output$a_groupsSynonyms <- renderText({
   req(reducedSynonymList())
    length(reducedSynonymList())
      })


    ## number ofrounds to click:
     output$a_groupsSynonymsRound <- renderText({
        length(reducedSynonymList()) - synonym_rv$counterPos + 1
      })
        ## number of nodes after summarizing words using synonyms
      output$a_groupsSynonyms <- renderText({
   req(reducedSynonymList())
    length(reducedSynonymList())
      })



      ## implement skip functionality
      observeEvent(input$a_synonymsClickSkip, {
        synonym_rv$skip <- TRUE
      })


### create labels for bucket list
# > words with POSITIVE valence:
labels_positive_synonym <-
  eventReactive(c(
    input$a_synonymsStart,
    input$a_synonymsClickSummarize,
    input$a_synonymsClickSkip
  ),
  {
    req(reducedSynonymList())
    synonym_rv$counterPos <- synonym_rv$counterPos + 1
    
    print("Counter positive:")
    print(synonym_rv$counterPos)

    ## get text variable
    tmp_text <- str_remove_all(string = module_rv$df[[1]]$text_summarized, 
                               pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
    tmp_text <- tolower(x = tmp_text)
    
    
    ## avoid error
    if (synonym_rv$counterPos <= length(reducedSynonymList())) {
      labels_out <-
        reducedSynonymList()[[synonym_rv$counterPos]]
      
      # print(module_rv$df[[1]][tmp_text %in% labels_out, ])
      
      labels_list <- list()
      for (i in 1:length(labels_out)) {
        tmp_dat <-
          module_rv$df[[1]][tmp_text == labels_out[i], ]
        
        
        for(w in unique(tmp_dat$text_summarized)){
          tmp_dat_w <- tmp_dat[tmp_dat$text_summarized == w, ]
          tmp_value <-
            tmp_dat_w$value[tmp_dat_w$value > 0 &
                              tmp_dat_w$value < 10]
          
          ## ignore zero value (if words are too similar)
          if (length(tmp_value) != 0) {
            ## compute N
            tmp_N <- length(tmp_value)
            ## compute mean
            tmp_mean <-
              round(x = mean(tmp_value), digits = 2)
            ## compute SD
            tmp_SD <-
              round(x = sd(tmp_value), digits = 2)
            
            tmp_word <- str_remove_all(string = w, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
            labels_list[[tmp_word]] <-
              htmltools::tags$div(paste0(
                tmp_word,
                "   (N=",
                tmp_N,
                ", M=",
                tmp_mean,
                ", SD=",
                tmp_SD,
                ")"
              ))
          }
        }
      }
      
      
      ## reset skip condition
      synonym_rv$skip <- FALSE
      
      labels_list
    } else {
      print("maximum reached")
      synonym_rv$counterPos <- synonym_rv$counterPos - 1
      NULL
    }
  })




### create labels for bucket list
# > words with NEGATIVE valence:
labels_negative_synonym <-
  eventReactive(c(
    input$a_synonymsStart,
    input$a_synonymsClickSummarize,
    input$a_synonymsClickSkip
  ),
  {
    req(reducedSynonymList())
        synonym_rv$counterNeg <- synonym_rv$counterNeg + 1
    
    ## get text variable
    tmp_text <- str_remove_all(string = module_rv$df[[1]]$text_summarized, 
                               pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
    tmp_text <- tolower(x = tmp_text)
    
    
    ## avoid error
    if (synonym_rv$counterNeg <= length(reducedSynonymList())) {
      labels_out <-
        reducedSynonymList()[[synonym_rv$counterNeg]]
      
      labels_list <- list()
      for (i in 1:length(labels_out)) {
        tmp_dat <-
          module_rv$df[[1]][tmp_text == labels_out[i], ]
        
        
        for(w in unique(tmp_dat$text_summarized)){
          tmp_dat_w <- tmp_dat[tmp_dat$text_summarized == w, ]
          tmp_value <- tmp_dat_w$value[tmp_dat_w$value < 0]
          
          ## ignore zero value (if words are too similar)
          if (length(tmp_value) != 0) {
            ## compute N
            tmp_N <- length(tmp_value)
            ## compute mean
            tmp_mean <-
              round(x = mean(tmp_value), digits = 2)
            ## compute SD
            tmp_SD <-
              round(x = sd(tmp_value), digits = 2)
            
            tmp_word <- str_remove_all(string = w, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
            labels_list[[tmp_word]] <-
              htmltools::tags$div(paste0(
                tmp_word,
                "   (N=",
                tmp_N,
                ", M=",
                tmp_mean,
                ", SD=",
                tmp_SD,
                ")"
              ))
          }
        }
      }
      
      ## reset skip condition
      synonym_rv$skip <- FALSE
      
      labels_list
    } else {
      print("maximum reached")
            synonym_rv$counterNeg <- synonym_rv$counterNeg - 1
      NULL
    }
  })




### create labels for bucket list
# > words with NEUTRAL valence:
labels_neutral_synonym <-
  eventReactive(c(
    input$a_synonymsStart,
    input$a_synonymsClickSummarize,
    input$a_synonymsClickSkip
  ),
  {
    req(reducedSynonymList())
            synonym_rv$counterNeutral <- synonym_rv$counterNeutral + 1
    
    ## get text variable
    tmp_text <- str_remove_all(string = module_rv$df[[1]]$text_summarized, 
                               pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
    tmp_text <- tolower(x = tmp_text)
    
    
    ## avoid error
    if (synonym_rv$counterNeutral <= length(reducedSynonymList())) {
      labels_out <-
        reducedSynonymList()[[synonym_rv$counterNeutral]]
      
      labels_list <- list()
      for (i in 1:length(labels_out)) {
        tmp_dat <-
          module_rv$df[[1]][tmp_text == labels_out[i], ]
        
        
        for(w in unique(tmp_dat$text_summarized)){
          tmp_dat_w <- tmp_dat[tmp_dat$text_summarized == w, ]
          tmp_value <- tmp_dat_w$value[tmp_dat_w$value == 0]
          
          ## ignore zero value (if words are too similar)
          if (length(tmp_value) != 0) {
            ## compute N
            tmp_N <- length(tmp_value)
            ## compute mean
            tmp_mean <-
              round(x = mean(tmp_value), digits = 2)
            ## compute SD
            tmp_SD <-
              round(x = sd(tmp_value), digits = 2)
            
            tmp_word <- str_remove_all(string = w, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
            labels_list[[tmp_word]] <-
              htmltools::tags$div(paste0(
                tmp_word,
                "   (N=",
                tmp_N,
                ", M=",
                tmp_mean,
                ", SD=",
                tmp_SD,
                ")"
              ))
          }
        }
      }
      
      
      ## reset skip condition
      synonym_rv$skip <- FALSE
      
      labels_list
    } else {
      print("maximum reached")
                  synonym_rv$counterNeutral <- synonym_rv$counterNeutral - 1
      NULL
    }
  })


### create labels for bucket list
# > words with AMBIVALENT valence:
labels_ambivalent_synonym <-
  eventReactive(c(
    input$a_synonymsStart,
    input$a_synonymsClickSummarize,
    input$a_synonymsClickSkip
  ),
  {
    req(reducedSynonymList())
                synonym_rv$counterAmbi <- synonym_rv$counterAmbi + 1
    
    ## get text variable
    tmp_text <- str_remove_all(string = module_rv$df[[1]]$text_summarized, 
                               pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
    tmp_text <- tolower(x = tmp_text)
    
    
    ## avoid error
    if (synonym_rv$counterAmbi <= length(reducedSynonymList())) {
      labels_out <-
        reducedSynonymList()[[synonym_rv$counterAmbi]]
      
      labels_list <- list()
      for (i in 1:length(labels_out)) {
        tmp_dat <-
          module_rv$df[[1]][tmp_text == labels_out[i], ]
        
        
        for(w in unique(tmp_dat$text_summarized)){
          tmp_dat_w <- tmp_dat[tmp_dat$text_summarized == w, ]
          tmp_value <- tmp_dat_w$value[tmp_dat_w$value == 10]
          
          ## ignore zero value (if words are too similar)
          if (length(tmp_value) != 0) {
            ## compute N
            tmp_N <- length(tmp_value)
            ## compute mean
            tmp_mean <-
              round(x = mean(tmp_value), digits = 2)
            ## compute SD
            tmp_SD <-
              round(x = sd(tmp_value), digits = 2)
            
            tmp_word <- str_remove_all(string = w, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
            labels_list[[tmp_word]] <-
              htmltools::tags$div(paste0(
                tmp_word,
                "   (N=",
                tmp_N,
                ", M=",
                tmp_mean,
                ", SD=",
                tmp_SD,
                ")"
              ))
          }
        }
      }
      
      
      ## reset skip condition
      synonym_rv$skip <- FALSE
      
      labels_list
    } else {
      print("maximum reached")
                        synonym_rv$counterAmbi <- synonym_rv$counterAmbi - 1
      NULL
    }
  })



      ## number of nodes after summarizing words using synonyms
      output$Nodes_unique_afterSynonyms <- renderText({
        tmp_text_summarized <- str_remove_all(string = module_rv$df[[1]]$text_summarized, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
        length(unique(tmp_text_summarized))
      })



 ## update text field value of "a_synonymsSupordinateWord":
      getSupordinateWord_synonyms <- function(){
        if (!identical(input$matches_positive_synonyms, character(0))) {
          updateTextInput(
            session,
            "a_synonymsSupordinateWord",
            value = str_remove_all(
              string = input$matches_positive_synonyms[1],
              pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
            )
          )
        } else if (!identical(input$matches_negative_synonyms, character(0))) {
          updateTextInput(
            session,
            "a_synonymsSupordinateWord",
            value = str_remove_all(
              string = input$matches_negative_synonyms[1],
              pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
            )
          )
        } else if (!identical(input$matches_neutral_synonyms, character(0))) {
          updateTextInput(
            session,
            "a_synonymsSupordinateWord",
            value = str_remove_all(
              string = input$matches_neutral_synonyms[1],
              pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
            )
          )
        } else if (!identical(input$matches_ambivalent_synonyms, character(0))) {
          updateTextInput(
            session,
            "a_synonymsSupordinateWord",
            value = str_remove_all(
              string = input$matches_ambivalent_synonyms[1],
              pattern = "_positive$|_negative$|_neutral$|_ambivalent$"
            )
          )
        }
      }
      
      observe({
       getSupordinateWord_synonyms()
      })



      ## Render bucket list
      output$a_synonymsBucketlist <- renderUI({
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
            labels = labels_positive_synonym(),
            input_id = ns("matches_positive_synonyms"),
            options = sortable_options(multiDrag = TRUE)
          ),
          add_rank_list(
            text = "suggested synonymous words with NEGATIVE valence:",
            labels = labels_negative_synonym(),
            input_id = ns("matches_negative_synonyms"),
            options = sortable_options(multiDrag = TRUE)
          ),
          add_rank_list(
            text = "suggested synonymous words with NEUTRAL valence:",
            labels = labels_neutral_synonym(),
            input_id = ns("matches_neutral_synonyms"),
            options = sortable_options(multiDrag = TRUE)
          ),
          add_rank_list(
            text = "suggested synonymous words with AMBIVALENT valence:",
            labels = labels_ambivalent_synonym(),
            input_id = ns("matches_ambivalent_synonyms"),
            options = sortable_options(multiDrag = TRUE)
          )
        )
      })




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


    print("tmp_protocol")
    print(tmp_protocol)
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





       #> UI b_word2vec
      observeEvent(input$b_word2vec, {
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




      #> Server word2vec




      ############
      # words not summarized
      ############
      #> UI
      observeEvent(input$wordsNotSummarized, {
        ## change UI
        outUI$elements <- tagList(
          tags$h2("Get all words you do not have summarized yet"),
          tags$br(),
          tags$div(
            HTML(
              "When using the module options Approximate matching OR Searching terms OR wordVec you are summarizing the concepts and thereby you decreasing the total number
              of unique concepts in the dataset. During this process it is highly likely that you will miss some concepts, which can be checked if you click on click on the button
              get words. Additionally, you can choose if you only want to get the concepts with a specific valence (default is all concepts). Please click only once and wait few seconds:"
            ),
            style = "font-size:14px"
          ),
          tags$br(),
           div(
            style = "margin: 0 auto; width: 100%; text-align:left;",
            div(
              style = "display: inline-block; vertical-align: top;",
                                   selectInput(ns("selectValence"), NULL, c("all",
                                                      "positive",
                                                      "negative",
                                                      "neutral",
                                                      "ambivalent"), width = "200px"),

            ),
            actionButton(
              ns("clickGetWords"),
              "get words",
              style = "display: inline-block;"
            ),
          ),
          tags$p(
              "Your CAM dataset contains ",
              tags$b(textOutput(ns(
                "numWordsNotSummarized"
              ), inline = TRUE)),
              " unique concepts in total, which you do NOT have summarized."
            ),
          verbatimTextOutput(ns("outGetWords")),
        )
      })


    #> Server
      observeEvent(input$clickGetWords, {
          print(input$selectValence)
          if (is.null(module_rv$df)) {
          showModal(
            modalDialog(
              title = "Run Approximate matching or Searching terms",
              paste0("You need to run the approximate matching or searching terms function before you 
              can check if you have any missing words"),
              easyClose = TRUE,
              footer = tagList(modalButton("Ok"))
            )
        )
          }
      output$outGetWords <- renderPrint({
        if (!is.null(module_rv$df)) {

          if(input$selectValence == "all"){
            tmp <- str_subset(string = unique(module_rv$df[[1]]$text_summarized), pattern = "_positive$|_negative$|_neutral$|_ambivalent$", negate = TRUE)
            tmp
          }else if(input$selectValence == "positive"){
            tmp <- module_rv$df[[1]]$text_summarized[module_rv$df[[1]]$value > 0 & module_rv$df[[1]]$value < 10]
            tmp <- str_subset(string = unique(tmp), pattern = "_positive$|_negative$|_neutral$|_ambivalent$", negate = TRUE)
            tmp
          }else if(input$selectValence == "negative"){
            tmp <- module_rv$df[[1]]$text_summarized[module_rv$df[[1]]$value < 0]
            tmp <- str_subset(string = unique(tmp), pattern = "_positive$|_negative$|_neutral$|_ambivalent$", negate = TRUE)
            tmp
          }else if(input$selectValence == "neutral"){
            tmp <- module_rv$df[[1]]$text_summarized[module_rv$df[[1]]$value == 0]
            tmp <- str_subset(string = unique(tmp), pattern = "_positive$|_negative$|_neutral$|_ambivalent$", negate = TRUE)
            tmp
          }else if(input$selectValence == "ambivalent"){
            tmp <- module_rv$df[[1]]$text_summarized[module_rv$df[[1]]$value == 10]
            tmp <- str_subset(string = unique(tmp), pattern = "_positive$|_negative$|_neutral$|_ambivalent$", negate = TRUE)
            tmp
          }
          }else{
            "Please use one of the module options before checking if you have any missing words."
            }
       })
            })

      output$numWordsNotSummarized <- renderText({
       if (!is.null(module_rv$df)){
            tmp <- str_subset(string = unique(module_rv$df[[1]]$text_summarized), pattern = "_positive$|_negative$|_neutral$|_ambivalent$", negate = TRUE)
            length(tmp)
       }
      })



      ############
      # Reliability
      ############
      #> UI
      observeEvent(input$reliability, {
        ## change UI
        outUI$elements <- tagList(
          tags$h2("Get word lists for raters and subsequently compute reliability coefficients"),
          tags$br(),
          tags$div(
            HTML(
              "This module option is divided in two parts: (a) you can create word lists of your concepts, which can be summarized by (independet)
              raters and subsequently (b) based on multiple summarized wordlists (at least 2) you can compute reliability coefficients.
              <br>
              Please click on: "
            ),
            style = "font-size:14px"
          ),
          tags$br(),
                  div(
          style = "margin: 0 auto; width: 50%; text-align:center;",
          actionButton(
            ns("a_createWordlists"),
            HTML('(a)<br>Create<br>Wordlists'),
            style = "width: 250px; height: 150px; font-size: 22px;"
          ),
          actionButton(
            ns("b_computeReliability"),
            HTML('(b)<br>Compute<br>Reliability'),
            style = "width: 250px; height: 150px; margin-left:20px; font-size: 22px;"
          )
        )
        )
      })


      #> UI (a) create word lists of your concepts
      observeEvent(input$a_createWordlists, {
        ## change UI
        outUI$elements <- tagList(
          tags$h2("(a) Create word lists of your concepts for raters"),
          tags$br(),
          tags$div(
            HTML(
              "Here you can create word lists of your concepts, which can be summarized by (independet) raters. It is important, that
              you, in accordance with sending them the wordlist, inform your raters how to summarize the respective word list, possible text:"
            ),
            style = "font-size:14px"
          ),
          tags$br(),
          tags$div(
            HTML(
              "Please read the following text carefully before you start summarizing the concepts:
              <br>
              <b>Goal</b>: The purpose of the rating process is to train you as raters to summarize the given wordlist into superordinate
              or thematic terms as best as possible. This process is so important, because based on your reliable process of summarizing
              the written concepts, these concepts will be partially or fully summarized (aggregated) later on.
              <br>
              <br>
              <b>How to</b>: To summarize the concepts, please do the following: In the A (Words) you will see the concepts, which have the
              participants written in our study (raw data). Your goal is now to summarize these concepts into superordinate or
              thematic terms, which you have to write in column B (Superordinate). Please, based on your knowledge of the overall study, use as
              few superordinate concepts as possible. In general you should <u>not summarize the predefined concepts</u>.
              <br>
              <br>
              <b>Objectivity</b>: During the process it is of central importance that you are consistent and reliable in your ratings.
              If you facing any problems or you have open questions regarding single concepts, please write them down in the respective
              comment column C. Please do <u>not talk to other raters</u> during this process, because the independence of the raters
              is a crucial assumption. It is important that you remain objective and impartial.
              <br>
              <br>
              <b>Training</b>: After you have summarized the concepts you will get feedback and in group discussions, there will be
              opportunities for training and improvement where necessary. Conflicting concepts are discussed together."
            )
          , style="border: 2px solid black; padding: 10px;"),
          tags$br(),

          tags$h3("Create word lists of your concepts"),
          tags$h4("Settings:"),
          tags$div(
            HTML(
              "Please select how many percent of your overall wordlist your raters should summarize (the number of words, which have
              to be summarized are shown, when you moving the slider):"
            ),
            style = "font-size:14px"
          ),
                  sliderInput(ns("a_perctWordlist"), "Percentage of words to summarize (1-100):",
                  min = 1, max = 100,
                  value = 10, width = "50%"),

          tags$p(
              "Your raters need to summarize ",
              tags$b(textOutput(ns(
                "a_wordsToSummarize"
              ), inline = TRUE)),
              " unique concepts."
            ),
                      tags$br(),
         tags$div(
            HTML(
              "Please select the ordering of the word list, if words should be split by given valence and if comments should be visible 
              for your raters (recommended to keep default settings):"
            ),
            style = "font-size:14px"
          ),
          div(
            style = "margin: 0 auto; width: 100%; text-align:left;",
            div(
              style = "display: inline-block; vertical-align: top; width: 25%; padding:10px;",
			  # specify order
                       tags$div(HTML("Please specify the order of your wordlist:"), style="font-size:14px"),
                   radioButtons(ns("a_Wordlist_Order"), label = NULL, c("alphabetic", "frequency"), selected = "alphabetic"),
            ),
      div(
              style = "display: inline-block; vertical-align: top; width: 25%; padding:10px;",
			  # specify split valence
		                     tags$div(HTML("Do you want to split your summarized words by the valence (X_positive, X_negative, ...)?"), style="font-size:14px"),
                   radioButtons(ns("a_Wordlist_Split"), label = NULL, c("yes", "no"), selected = "no"),
            ),
      div(
              style = "display: inline-block; vertical-align: top; width: 25%; padding:10px;",
			  # specify including comments
		                     tags$div(HTML("Do you want to include comments in your wordlist?"), style="font-size:14px"),
                   radioButtons(ns("a_Wordlist_Comments"), label = NULL, c("yes", "no"), selected = "yes"),
            ),
          ),
   tags$br(),
            actionButton(
              ns("a_createWordlistOut"),
              "create wordlist",
              style = "display: inline-block;"
            ),
                                tags$div(
            HTML(
              "<i>Remark: to download the wordlist for your raters please click on the global download button (top right) after 
              you have clicked on create wordlist.</i>"
            ),
            style = "font-size:14px"
          ),
                      tags$br(),
          tags$h4("Wordlist for raters to summarize:"),
          dataTableOutput(ns("a_wordlistTable")),
        )
      })

    #> Server
        reliability_a_rv <-
        reactiveValues(
          numConcepts = NULL
        )


      observeEvent(input$a_perctWordlist, {
        ## check if global data set is loaded
        if (is.null(module_rv$df)) {
          showModal(
            modalDialog(
              title = "Run Approximate matching or Searching terms",
              paste0("You need to run the approximate matching or searching terms function before you can create a wordlist."),
              easyClose = TRUE,
              footer = tagList(modalButton("Ok"))
            )
        )
        }

      ## number of nodes after summarizing words
      output$a_wordsToSummarize <- renderText({
        if(any(colnames(module_rv$df) == "text_summarized")){
          tmp_text <- str_remove_all(string = module_rv$df[[1]]$text_summarized, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
        }else{
          tmp_text <- module_rv$df[[1]]$text
        }
          tmp_text <- unique(x = tmp_text)
          tmp_proportion <- ceiling(x = length(tmp_text) * input$a_perctWordlist / 100)

          ## set reactive value
          reliability_a_rv$numConcepts <- tmp_proportion

          tmp_proportion
      })
      })

      ## create wordlist
        wordlist <- eventReactive(input$a_createWordlistOut, {
        if(any(colnames(module_rv$df) == "text_summarized")){
          tmp_text <- str_remove_all(string = module_rv$df[[1]]$text_summarized, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
        }else{
          tmp_text <- module_rv$df[[1]]$text
        }
        tmp_text <- unique(x = tmp_text)
        wordsOut <- sample(x = tmp_text, size = reliability_a_rv$numConcepts, replace = FALSE)

        # print(wordsOut)



        ## define settings: 
        # > split by valence
        if(input$a_Wordlist_Split == "no"){
          tmp_splitByValence = FALSE
        }else{
          tmp_splitByValence = TRUE
        }
        # > include comments
        if(input$a_Wordlist_Comments == "no"){
          tmp_includeComments = FALSE
        }else{
          tmp_includeComments = TRUE
        }


        CAMwordlist <- create_wordlist(
          dat_nodes = module_rv$df[[1]],
          dat_merged = module_rv$df[[3]],
          order = input$a_Wordlist_Order,
          splitByValence = tmp_splitByValence,
          comments = tmp_includeComments,
          rater = TRUE
          )

          # print(head(CAMwordlist))
          CAMwordlist <- CAMwordlist[CAMwordlist$Words %in% wordsOut, ]
          # print(head(CAMwordlist))


          #> change condition
          globals$condition <-
            c(globals$condition, "wordlistRatersCreated")
          #> save as global
          globals$wordlistRaters <- CAMwordlist

        CAMwordlist
      })

      output$a_wordlistTable <- renderDataTable({
            wordlist()
      })

      #> UI (a) create word lists of your concepts
      observeEvent(input$b_computeReliability, {
        ## change UI
        outUI$elements <- tagList(
          tags$h2("(b) Compute reliability coefficients"),
          tags$br(),
          tags$div(
            HTML(
              "Based on multiple summarized wordlists (at least 2) you can compute two reliability coefficients."
            ),
            style = "font-size:14px"
          ),
          tags$br(),

          tags$h3("Compute reliability coefficients"),
          tags$h4("Upload:"),
          tags$div(
            HTML(
              "Please upload all your wordlists you have received by your raters (at least 2). It is important to note that every concepts
              need to be summarized - no missings in column B (Superordinate):"
            ),
            style = "font-size:14px"
          ),
      fileInput(
          ns("b_upload"),
          NULL,
          accept = c(".xlsx"),
          placeholder = "upload wordlists from raters",
          multiple = TRUE, width = "30%"
        ),
        tags$div(
          HTML(
            "Please wait a few seconds until the data is processed. You have uploaded the
                              following wordlist(s) - maximum four file names are shown:"
          )
        ),
        tableOutput(ns("b_file")),
          tags$br(),
          tags$h4("Output reliability coefficients:"),
                                        tags$div(
            HTML(
              "<i>Remark: to download a overall wordlist, which combines all the single wordlists of your 
              raters, please click on the global download button (top right) after computing the reliability coefficients.</i>"
            ),
            style = "font-size:14px"
          ),
          tags$br(),
          tags$h5("Cohens Kappa (Light's Kappa):"),
          tags$div(
            HTML(
              "Cohens Kappa is pairwise computed by assuming a perfect match of overlapping group of words, which have been 
              summarized (r0001r0002... is indicating which groups of words have been summarized):"
            ),
            style = "font-size:14px"
          ),
          verbatimTextOutput(ns("b_CohensKappaMatrix")),
                    tags$div(
            HTML(
              "Cohens Kappa is pairwise computed by maximizing overlapping words, which have been 
              summarized (search for the largest overlap when allocating words for every group of raters):"
            ),
            style = "font-size:14px"
          ),
          verbatimTextOutput(ns("b_CohensKappaMatrixMaximized")),
                              tags$div(
            HTML(
              "Summary statistics of Cohens Kappa by (a) assuming a perfect match of overlapping group of words and 
              (b) maximizing overlapping words:"
            ),
            style = "font-size:14px"
          ),
          verbatimTextOutput(ns("b_CohensKappaSummary")),
          tags$br(),
          tags$h5("Fleiss Kappa:"),
                                        tags$div(
            HTML(
              "Fleiss Kappa for different groups of overlapping words (plus computing category-wise Kappa):"
            ),
            style = "font-size:14px"
          ),
          verbatimTextOutput(ns("b_FleissKappaOverlapping")),
                                                  tags$div(
            HTML(
              "> for different word combinations given superordinate words (sorted by highest frequency):"
            ),
            style = "font-size:14px"
          ),
          verbatimTextOutput(ns("b_FleissKappaOverlapping_Descriptives")),
          tags$br(),
                                                  tags$div(
            HTML(
              "Fleiss Kappa for given superordinate words:"
            ),
            style = "font-size:14px"
          ),
          verbatimTextOutput(ns("b_FleissKappaSuperordinate")),
          tags$h5("Overall rater list:"),
          dataTableOutput(ns("b_wordlistTableOverall")),
        )
      })



    #> Server
    ### get the extensions and show file names
    ext <- reactive({
      # wait for upload
      req(input$b_upload)

      # show uploaded file(s)
      output$b_file <- renderTable({
        if (nrow(input$b_upload) > 4) {
          rbind(input$b_upload[1:4,], c("...", "...", "...", "..."))
        } else {
          input$b_upload
        }
      })

      # extract file ending
      tools::file_ext(input$b_upload$name)
    })


   ### create data sets of wordlists
    data <- reactive({
      # wait for upload
      req(ext())
      message("uploaded file(s) - extensions:", ext())

      if (all(stringr::str_detect(string = ext(), pattern = "xlsx"))) {
        ## check: at least 2 datasets
        if (length(ext()) < 2) {
          showModal(
            modalDialog(
              title = "Wrong number of datasets",
              paste0(
                "Please upload at least two wordlists (.xlsx files)"
              ),
              easyClose = TRUE,
              footer = tagList(modalButton("Ok"))
            )
          )
          return(NULL)
        } else {
        ## go on !!!

        # Words	Superordinate Comment	raw	percent	mean_valence	sd_valence	mean_degree	sd_degree
          files <- list()
          for (i in 1:length(input$b_upload[, 1])) {
               files[[i]] <-
               xlsx::read.xlsx2(file = input$b_upload[[i, "datapath"]],
              sheetIndex = 1)
            }

          return(files)

        }
      } else {
          showModal(
            modalDialog(
              title = "Wrong type of dataset(s)",
              paste0(
                "Please upload a only .xlsx files (Excel)"
              ),
              easyClose = TRUE,
              footer = tagList(modalButton("Ok"))
            )
          )
          return(NULL)
      }
      
    })


    observeEvent(input$b_upload, {
      req(ext())
      req(data())

      tmp_namesRater <- input$b_upload[, 1]
      tmp_namesRater <- str_remove(string = tmp_namesRater, pattern = ".xlsx$")
 
      ## Cohens Kappa pairwise
                cohensKappas <- computeCohensKappa(files = data(), numberRaters = length(data()))
          rownames(x = cohensKappas) <- tmp_namesRater
          colnames(x = cohensKappas) <- tmp_namesRater
        output$b_CohensKappaMatrix <- renderPrint({
          round(x = cohensKappas, digits = 2)
        })  
      ## Cohens Kappa pairwise maximized
                cohensKappasMaximized <- computeCohensKappaMaximized(files = data(), numberRaters = length(data()))
          rownames(x = cohensKappasMaximized) <- tmp_namesRater
          colnames(x = cohensKappasMaximized) <- tmp_namesRater

        output$b_CohensKappaMatrixMaximized <- renderPrint({
          round(x = cohensKappasMaximized, digits = 2)
        }) 

        output$b_CohensKappaSummary <- renderPrint({
          out_summaryCohensKappa <- list(
            mean(cohensKappas[lower.tri(x = cohensKappas, diag = FALSE)]),
          mean(cohensKappasMaximized[lower.tri(x = cohensKappasMaximized, diag = FALSE)])
          )
          names(out_summaryCohensKappa) <- c("a", "b")
          out_summaryCohensKappa
        }) 


      ovallRaterList <- getOverallRaterList(files = data(),
                    orderAlphabetically = TRUE,
                    raterNames = tmp_namesRater)

      #> change condition
      globals$condition <- c(globals$condition, "wordlistOverallRated")
      #> save as global
      globals$wordlistOverallRated <- ovallRaterList

      ## output overall table
      output$b_wordlistTableOverall <- renderDataTable({
        ovallRaterList
      })

      ## Fleiss Kappa for different groups of overlapping words
      output$b_FleissKappaOverlapping <- renderPrint({
        tmp <- ovallRaterList[, str_subset(string = colnames(ovallRaterList),
                                  pattern = "Rating_")]
        kappam.fleiss(ratings = tmp, detail=TRUE)
      })


      # > for different word combinations given superordinate words (sorted by highest frequency)
      output$b_FleissKappaOverlapping_Descriptives <- renderPrint({
        ## table of word counts sorted
        number_wordCom <- sort(table(unlist(ovallRaterList[, str_subset(string = colnames(ovallRaterList),
          pattern = "Rating_")])), decreasing = TRUE)

        ## wide to long data
        tmp_Ratings <- ovallRaterList[, c("Words", str_subset(string = colnames(ovallRaterList),
                                                            pattern = "Rating_"))]
        tmp_Ratings <- tmp_Ratings %>%
          gather(variable, value, -Words)
                                                            
        tmp_Superordinate <- ovallRaterList[, str_subset(string = colnames(ovallRaterList),
                                                            pattern = "Superordinate_")]
        tmp_Superordinate <- tmp_Superordinate %>%
          gather(variable, value)
          
        tmp_Ratings$Superordinate <- tmp_Superordinate$value
        tmp_Ratings$variable <- str_remove(string = tmp_Ratings$variable, pattern = "Rating_")
        colnames(tmp_Ratings) <- c("Words", "Raters", "Rating", "Superordinate")
        
        ## print output
        for(w in 1:length(number_wordCom)){
          tmp_data <- data.frame(Raters = tmp_Ratings$Raters[tmp_Ratings$Rating %in% names(number_wordCom)[w]],
                         Superordinates = tmp_Ratings$Superordinate[tmp_Ratings$Rating %in% names(number_wordCom)[w]],
                         Words = tmp_Ratings$Words[tmp_Ratings$Rating %in% names(number_wordCom)[w]])

          cat("\nword combination: ", names(number_wordCom)[w],
          "\n      , # of Raters:", length(unique(tmp_data$Raters)),
          ", # of Superordinates:", length(unique(tmp_data$Superordinate)),
          ", # of words:", number_wordCom[w], "\n")
          print(tmp_data)
          }
      })




      ## Fleiss Kappa for given superordinate words
      output$b_FleissKappaSuperordinate <- renderPrint({
        tmp <- ovallRaterList[, str_subset(string = colnames(ovallRaterList),
                                  pattern = "Superordinate")] 
       kappam.fleiss(ratings = tmp, detail=TRUE)
      })



    })


      ############
      # information summarize terms
      ############
      #> UI
      observeEvent(input$informationSummarizeTerms, {
        ## change UI
        outUI$elements <-
          tagList(tags$h2("Module Specific Information"),
                  tags$div(
                    HTML('The options for this module are the following:'),
                    tags$ul(tags$li(
                      HTML(
                        '<b>Approximate matching:</b> By using approximate string matching you can compute the string distances between all your unique concepts
              in the dataset (using optimal string aligment) to find words, which have been written slightly differently.'
                      )
                    ),
                    tags$li(
                      HTML(
                        '<b>Searching terms:</b> Use regular expressions to search your CAM concepts for semantically identical or similar terms. Regular expression is a sequence of characters
                        that specifies a search pattern in text. If you want to try out different regular expressions you can have a look for example at
                        <a href="https://regex101.com/" target="_blank">https://regex101.com/</a>.'
                      )
                    ),
                    tags$li(
                      HTML('<b>wordVec:</b> to be implemented.')
                    ),
                            tags$li(
                      HTML(
                        '<b>not summarized:</b> Check which words you do not have summarized using the module options Approximate matching OR Searching terms OR wordVec (internally
                        the App is writing a suffix to the summarized words like "_positive" to split the so summarized words according to their valence).'
                      )
                    ),
                            tags$li(
                      HTML(
                        '<b>Reliability:</b> Download all your unique concepts as wordlists (EXCEL file) and let different raters summarize the concepts to compute scores for inter-rater-reliability.
                        .....'
                      )
                    )
                    )
                  ))
      })




      return(list(df = reactive({module_rv$df}), # return global df
            protocolAM   = reactive({am_rv$protocol}),
            protocolST   = reactive({search_rv$protocol}))) # return protocols

    })
  }
