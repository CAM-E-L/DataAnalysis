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
            HTML(
              "<i>To download the report download all your files globally using the button top right. Please adjust the report according to your specific needs!</i>"
            ),
            tags$br(),
            tags$div(
              HTML(
                "After choosing if you want to include statistics for single concepts, please click on the get report button.
          Please click only once and wait few seconds. It is recommended that you summarise all your concepts before getting the report:"
              ),
          style = "font-size:14px"
            ),
          tags$h3("Include statistics for single concepts:"),


          tags$h3("Get Report:"),
          actionButton(ns("clickGetReport"), "get report"),
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
      # module_rv <- reactiveValues(numCAM = NULL)
      reportOut <- eventReactive(c(input$clickGetReport), {
        ### Description of data set ###
        providedNumberPredefinedConcepts <- 4 # input$xxx


        ### overall report ###
        shinyjs::show(id = "overallReport")

        out_numCAMs <- length(globals$protocol$currentCAMs) + length(globals$protocol$deletedCAMs)

        print("globals$protocol$deletedCAMs")
        print(globals$protocol$deletedCAMs)
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

        out_deletedPredefinedConceptsCAMsPercent <- 9999 # providedNumberPredefinedConcepts



        ### Summarizing concepts ###
        out_numConcepts <- length(globals$dataCAMsummarized[[1]]$text)
        out_numConceptsUnique <- length(unique(globals$dataCAMsummarized[[1]]$text))

        out_numConceptsSummarizedUnique <- length(unique(globals$dataCAMsummarized[[1]]$text_summarized))



        ###individual concepts ###
        shinyjs::show(id = "individualConcepts") ## IF

        tmp_nameIndividualConcept <- "acceptability"
        tmp_averageIndividualConcept <- 2.7
        tmp_sdIndividualConcept <- 1.13
        tmp_drawnInCAMsPercent <-  "98%"


        shinyjs::html("listIndividualConcepts", "", add = FALSE)

        tmp_individualConcept <- paste0(
          '<ul><li>The concept "',
          tmp_nameIndividualConcept,
          '" has a average valence of ',
          tmp_averageIndividualConcept,
          ' (SD=',
          tmp_sdIndividualConcept,
          ') and was drawn in ',
          tmp_drawnInCAMsPercent,
          ' of the CAMs.</li></ul>'
        )
        shinyjs::html("listIndividualConcepts", tmp_individualConcept, add = TRUE)
        shinyjs::html("listIndividualConcepts", tmp_individualConcept, add = TRUE)



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
            "concepts."
          )
        })
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
