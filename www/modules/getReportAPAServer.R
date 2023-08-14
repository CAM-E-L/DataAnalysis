getReportAPAServer <-
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
        h1("Get Report Module"),
        tags$br(),
        tags$br(),
        HTML(
          'To start the module please click on one of the module options on the sidebar panel. The options for
                     this module are the following:'
        ),
        tags$ul(tags$li(HTML(
          '<b>Get report:</b> ...'
        )))
      ))

      ## set output
      output$uploadOutGetReport <- renderUI({
        outUI$elements
      })

      ################################
      # single module options
      ################################
      ###### getReport
      #> UI
      observeEvent(input$getReport, {
        ## change UI
        outUI$elements <- tagList(
          tags$h2("Get Report"),
          tags$div(
            tags$br(),
            tags$div(
              HTML(
                "After choosing if you want to include statistics for single concepts, please click on the get report button.
          Please click only once and wait few seconds. It is recommended that you summarise all your concepts before getting the report:"
              ),
          style = "font-size:14px"
            ),
          tags$h3("Your Settings:"),
          div(
            style = "margin: 0 auto; width: 100%; text-align:left;",
            div(
              style = "display: inline-block; vertical-align: top; width: 35%; padding:5px;",
			  # specify number of predefined concepts
                       tags$div(HTML("Please provide the number of predefined concepts in your study (=starting concepts):"), style="font-size:14px"),
                      numericInput(ns("numPredefinedConcepts"), label = NULL, 
            value = 0, min = 0, max = Inf, step = 1)
            ),
            div(
              style = "display: inline-block; vertical-align: top; width: 55%; padding:5px;",
            #  get statistics of individual concepts
            tags$div(HTML("Select the concepts for which you want to receive additional statistics (alphabetically sorted):"), style="font-size:14px"),
            uiOutput(ns("statsIndividualConcepts")),
            ),
          ),
          tags$h3("Get Report:"),
          actionButton(ns("clickGetReport"), "get report"),
          HTML(
              "<br><i>To download the report download all your files globally using the button top right. Please <b>adjust the report</b> according to your specific needs!</i>"
            ),
          tags$br(),
          tags$br(),
          tags$div(
            htmlOutput(ns("overallReportOut")),
            style = "font-size:16px; border: 2px solid black; padding: 20px; width: 80%",
            id = ns("overallReport")
          ),
          tags$br(),
          tags$div(
            HTML("<b>Statistics of individual concepts</b><br>"),
            tags$div(id = ns("listIndividualConcepts")),
            id = ns("individualConcepts"),
            style = "font-size:16px; border: 2px solid black; padding: 20px; width: 80%"
          ),
          )
        )
      })

      #> Server
      module_rv <- reactiveValues(renamedIdenticalTerms = FALSE, renamedIdenticalTermsData = NULL)

        # input validator for settings
        iv <- InputValidator$new()
        iv$add_rule("numPredefinedConcepts", sv_between(0, Inf))

        output$statsIndividualConcepts <- renderUI({
          selectInput(ns("statsIndividualConcepts_input"),
                    NULL,
                      choices = as.list(uniqueConcepts_Report()), width = "50%",   multiple = TRUE
          )
        })

        ## choices individual concepts
        uniqueConcepts_Report <- reactive({
          req(dataCAM())

         # tmp_text <- str_remove_all(string = globals$dataCAMsummarized[[1]]$text_summarized, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
         
         tmp_nodes <- globals$dataCAMsummarized[[1]]
         tmp_nodes$text_summarized <- str_remove_all(string =tmp_nodes$text_summarized, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
            for(i in 1:length(unique(tmp_nodes$CAM))){
              if(any(table(tmp_nodes$text_summarized[tmp_nodes$CAM == unique(tmp_nodes$CAM)[i]]) > 1)){
                tmp_nodes <- rename_identicalTerms(dat_nodes = tmp_nodes, drawn_CAM = globals$drawnCAM(), 
                removeSuffix = TRUE) # changed input
                print("internally rename_identicalTerms() applied")
                tmp_nodes$text_summarized[str_detect(string = tmp_nodes$text_summarized, pattern = "_[:digit:]*$")] <- NA
                module_rv$renamedIdenticalTerms = TRUE
                module_rv$renamedIdenticalTermsData = tmp_nodes
                break
              }else{
             module_rv$renamedIdenticalTerms = FALSE
              }
            }

          sort(unique(tmp_nodes$text_summarized))
        })


      reportOut <- eventReactive(c(input$clickGetReport), {
        
        message("The value of input$numPredefinedConcepts is ", input$numPredefinedConcepts)    
        message("The value of input$statsIndividualConcepts_input is ", input$statsIndividualConcepts_input)    

        ### Description of data set ###
        providedNumberPredefinedConcepts <- input$numPredefinedConcepts


        ### overall report ###
        shinyjs::show(id = "overallReport")

        out_numCAMs <- length(globals$protocol$currentCAMs) + length(globals$protocol$deletedCAMs)

        # print("globals$protocol$deletedCAMs")
        # print(globals$protocol$deletedCAMs)
        out_excCAMs <- length(globals$protocol$deletedCAMs)

        out_excCAMsPercent <-
          round(x = out_excCAMs / out_numCAMs * 100, digits = 0)

        if(is.null(globals$dataNetworkIndicators)){
          print("compute_indicatorsCAM() - get report")
            globals$dataNetworkIndicators <- compute_indicatorsCAM(drawn_CAM = drawnCAM(),
                                  micro_degree = NULL,
                                  micro_valence = NULL,
                                  micro_centr_clo = NULL,
                                  micro_transitivity = NULL,
                                  largestClique = FALSE)
        }

        ## number of concepts
        out_averageConcepts <- round(x = mean(globals$dataNetworkIndicators$num_nodes_macro), digits = 2)
        out_sdConcepts <- round(x = sd(globals$dataNetworkIndicators$num_nodes_macro), digits = 2)

        out_positiveConceptsPercent <- paste0(round(x = sum(globals$dataNetworkIndicators$num_nodes_pos_macro) / sum(globals$dataNetworkIndicators$num_nodes_macro) * 100, digits = 0), "%")
        out_negativeConceptsPercent <- paste0(round(x = sum(globals$dataNetworkIndicators$num_nodes_neg_macro) / sum(globals$dataNetworkIndicators$num_nodes_macro) * 100, digits = 0), "%")
        out_neutralConceptsPercent <- paste0(round(x = sum(globals$dataNetworkIndicators$num_nodes_neut_macro) / sum(globals$dataNetworkIndicators$num_nodes_macro) * 100, digits = 0), "%")
        out_ambivalentConceptsPercent <- paste0(round(x = sum(globals$dataNetworkIndicators$num_nodes_ambi_macro) / sum(globals$dataNetworkIndicators$num_nodes_macro) * 100, digits = 0), "%")

        ## number of connectors
        out_averageConnectors <- round(x = mean(globals$dataNetworkIndicators$num_edges_macro), digits = 2)
        out_sdConnectors <- round(x = sd(globals$dataNetworkIndicators$num_edges_macro), digits = 2)

        out_connectorsAgreeingPercent <-   paste0(round(x = sum(globals$dataNetworkIndicators$num_edges_solid_macro) / sum(globals$dataNetworkIndicators$num_edges_macro) * 100, digits = 0), "%")
        out_connectorsDisagreeingPercent <-   paste0(round(x = sum(globals$dataNetworkIndicators$num_edges_dashed_macro) / sum(globals$dataNetworkIndicators$num_edges_macro) * 100, digits = 0), "%")

        out_connectorsBidirectionalPercent <-  paste0(round(x = sum(globals$dataCAMsummarized[[2]]$isBidirectional == 1) / nrow(globals$dataCAMsummarized[[2]]) * 100, digits = 0), "%")
        out_connectorsUnidirectionalPercent <-  paste0(round(x = sum(globals$dataCAMsummarized[[2]]$isBidirectional == 0) / nrow(globals$dataCAMsummarized[[2]]) * 100, digits = 0), "%")

        ## average valence
        out_averageValence <- round(x = mean(globals$dataNetworkIndicators$mean_valence_macro), digits = 2)
        out_sdValence <- round(x = sd(globals$dataNetworkIndicators$mean_valence_macro), digits = 2)



        ## deleted predefined concepts
        if(all(globals$dataCAMsummarized[[1]]$participantCAM == "noID")){
          nodes_notDeleted <- globals$dataCAMsummarized[[1]][globals$dataCAMsummarized[[1]]$CAM %in% globals$protocol$currentCAMs, ]
        }else{
          #> use participantCAM ID
          nodes_notDeleted <- globals$dataCAMsummarized[[1]][globals$dataCAMsummarized[[1]]$participantCAM %in% globals$protocol$currentCAMs, ]
        }

        vector_nonDeleted <- rep(x = FALSE, times = length(unique(nodes_notDeleted$CAM)))
        for(c in 1:length(unique(nodes_notDeleted$CAM))){
          tmp_CAM <- nodes_notDeleted[nodes_notDeleted$CAM %in% unique(nodes_notDeleted$CAM)[c], ]
          
          if(sum(tmp_CAM$predefinedConcept) - providedNumberPredefinedConcepts != 0){
            # print(c)
            # print(sum(tmp_CAM$predefinedConcept))
            vector_nonDeleted[c] <- TRUE
          }
        }


        out_deletedPredefinedConceptsCAMsPercent <-  paste0(round(x = sum(vector_nonDeleted) / length(vector_nonDeleted) * 100, digits = 0), "%")

        ### Summarizing concepts ###
        out_numConcepts <- length(globals$dataCAMsummarized[[1]]$text)
        out_numConceptsUnique <- length(unique(globals$dataCAMsummarized[[1]]$text))

        out_numConceptsSummarizedUnique <- length(unique(globals$dataCAMsummarized[[1]]$text_summarized))


        ### summarizing functions
        # print("globals$protocol")
        # print(globals$protocol)

        out_numApproximateMatching <- length(globals$protocol$approximateMatching)
        out_numSearchTerms <- length(globals$protocol$searchTerms)
        out_numFindSynonyms <- length(globals$protocol$findSynonyms)
        out_numModelwordVec <- length(globals$protocol$modelwordVec)





        ### individual concepts ###
        shinyjs::html("listIndividualConcepts", "", add = FALSE)

        if(length(input$statsIndividualConcepts_input) > 0){
          shinyjs::show(id = "individualConcepts")
          
          tmp_name_degree <- paste0("degreetot_micro_", str_replace_all(string=input$statsIndividualConcepts_input, pattern=" ", repl=""))
          tmp_name_valence <- paste0("valence_micro_", str_replace_all(string=input$statsIndividualConcepts_input, pattern=" ", repl=""))

          # print(tmp_name_degree)
          # print(tmp_name_valence)

          print("compute_indicatorsCAM() - get report -> micro indicators")
          if(module_rv$renamedIdenticalTerms){
CAMdrawn_renamed <- draw_CAM(dat_merged = globals$dataCAMsummarized[[3]],
                     dat_nodes = module_rv$renamedIdenticalTermsData,ids_CAMs = "all",
                     plot_CAM = FALSE,
                     useCoordinates = TRUE,
                     relvertexsize = 3,
                     reledgesize = 1)
                     
                     
                     tmp_microIndicators <- compute_indicatorsCAM(drawn_CAM = CAMdrawn_renamed,
                                  micro_degree = input$statsIndividualConcepts_input,
                                  micro_valence = input$statsIndividualConcepts_input,
                                  micro_centr_clo = NULL,
                                  micro_transitivity = NULL,
                                  largestClique = FALSE)
          }else{
          tmp_microIndicators <- compute_indicatorsCAM(drawn_CAM = drawnCAM(),
                                  micro_degree = input$statsIndividualConcepts_input,
                                  micro_valence = input$statsIndividualConcepts_input,
                                  micro_centr_clo = NULL,
                                  micro_transitivity = NULL,
                                  largestClique = FALSE)
          }



        for(i in 1:length(tmp_name_degree)){
          tmp_nameIndividualConcept <- input$statsIndividualConcepts_input[i]

          # print("tmp_microIndicators[, tmp_name_valence[i]]")
          # print(tmp_microIndicators[, tmp_name_valence[i]])
          tmp_averageIndividualConcept <- round(x = mean(tmp_microIndicators[, tmp_name_valence[i]], na.rm = TRUE), digits = 2)
          tmp_sdIndividualConcept <- round(x = sd(tmp_microIndicators[, tmp_name_valence[i]], na.rm = TRUE), digits = 2)
          tmp_drawnInCAMs <-  sum(!is.na(tmp_microIndicators[, tmp_name_valence[i]]))
          tmp_drawnInCAMsPercent <-  paste0(round(x = tmp_drawnInCAMs / nrow(tmp_microIndicators) * 100, digits = 0), "%")

          tmp_degreeIndividualConcept <- round(x = mean(tmp_microIndicators[, tmp_name_degree[i]], na.rm = TRUE), digits = 2)
          tmp_degreeSdIndividualConcept <- round(x = sd(tmp_microIndicators[, tmp_name_degree[i]], na.rm = TRUE), digits = 2)

          ## add indivvidual concept
          tmp_individualConcept <- paste0(
            '<ul><li>The concept "',
            tmp_nameIndividualConcept,
            '" has a average valence of ',
            tmp_averageIndividualConcept,
            ' (SD=',
            tmp_sdIndividualConcept,
            ') and was drawn in ',
            tmp_drawnInCAMs,
            ' (<span style="margin-left: -0px; margin-right: -0px">',
            tmp_drawnInCAMsPercent,
            "</span>) of the CAMs.",
            'The average degree is ',
            tmp_degreeIndividualConcept,
            ' (SD=',
            tmp_degreeSdIndividualConcept,
            ').',
            '</li></ul>'
          )
          shinyjs::html("listIndividualConcepts", tmp_individualConcept, add = TRUE)
        }
        }else{
          shinyjs::hide(id = "individualConcepts")
        }

        ### print report:
        output$overallReportOut <- renderUI({
          HTML(
            "<b>Description of dataset</b><br>",
            "In total we collected",
            out_numCAMs,
            "CAMs, of which",
            out_excCAMs,
            '(<span style="margin-left: -5px; margin-right: -5px">',
            out_excCAMsPercent,
            "</span>%)",
            "CAMs were excluded from further analysis.",
            "Participants drew on average",
            out_averageConcepts,
            '(SD=<span style="margin-left: -5px; margin-right: -5px">',
            out_sdConcepts,
            "</span>)",
            "concepts (whereby",
            out_positiveConceptsPercent,
            # neg, neut, ambivalent
            "were positive,",
            out_negativeConceptsPercent,
            "negative,",
            out_neutralConceptsPercent,
            "neutral and",
            out_ambivalentConceptsPercent,
            "ambivalent).",
            "Please note that the technical settings required participants to draw at least XXX concepts.",
            "On average",
            out_averageConnectors,
            '(SD=<span style="margin-left: -5px; margin-right: -5px">',
            out_sdConnectors,
            "</span>)",
            "connectors were drawn.",
            out_connectorsAgreeingPercent,
            "of connectors were agreeing and",
            out_connectorsDisagreeingPercent,
            "disagreeing. Furthermore",
            out_connectorsBidirectionalPercent,
            "of connectors were bidirectional and",
            out_connectorsUnidirectionalPercent,
            "unidirectional.  The valence for the concepts range from [-3,-1] for negative and [1,3] for positive concepts, with ambivalent and neutral concepts being assigned a value of 0.",
            "The mean average valence over all the CAMs was",
            out_averageValence,
            '(SD=<span style="margin-left: -5px; margin-right: -5px">',
            out_sdValence,
            "</span>).",
            "In",
            out_deletedPredefinedConceptsCAMsPercent,
            "of the non-deleted CAMs one or more of the predefined concepts were removed by the participants.",

            "<br><br><b>Summarizing concepts</b><br>",
            "We summarized the CAMs using the dedicated CAM-App. The CAM-App generates a protocol, which tracks every summarizing step, so that the summarizing process is completely transparent. The",
            out_numConceptsUnique,
            "rawn unique concepts",
            '(<span style="margin-left: -5px;">',
            out_numConcepts,
            "</span> in total)",
            "were summarized to",
            out_numConceptsSummarizedUnique,
            "concepts using",
            out_numApproximateMatching,
            'times the "approximate matching",',
            out_numSearchTerms,
            'times the "searching terms",',
            out_numFindSynonyms,
            'times the "search for synonyms" and',
            out_numModelwordVec,
            'times the "apply word2vec model" functionalities.'
          )
        })


        # enable InputValidator for the settings
        iv$enable()
      })


      observeEvent(c(input$clickGetReport), {
        shinyjs::hide(id = "overallReport")
        shinyjs::hide(id = "individualConcepts")

        if (input$clickGetReport > 0) {
          reportOut()
        }
      })





      ###### information
      observeEvent(input$informationGetReport, {
        ## change UI
        outUI$elements <- tagList(tags$h2("Module Specific Information"),
                                  tags$div(
                                    HTML('The options for this module are the following:'),
                                    tags$ul(tags$li(HTML(
                                      '<b>Get report:</b> ... blub'
                                    )))
                                  ))
      })

    })
  }
