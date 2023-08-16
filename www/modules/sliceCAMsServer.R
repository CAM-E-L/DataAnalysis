
sliceCAMsServer <-
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
        h1("Slice CAMs module"),
        tags$br(),
        tags$br(),
        HTML(
          'To start the module please click on one of the module options on the sidebar panel. The options for
                     this module are the following:'
        ),
        tags$ul(
          tags$li(HTML('<b>slice CAMs:</b> ...')),
          tags$li(HTML('<b>get slice CAMs descriptives:</b> ...'))
        )
      ))

      ## set output
      output$uploadOutSliceCAMs <- renderUI({
        outUI$elements
      })

      ################################
      # single module options
      ################################
      ###### neighborhoodIndicators
      #> UI
      observeEvent(input$sliceCAMs, {

        ## change UI
        outUI$elements <- tagList(
          tags$h2("Slice your CAMs according to different criteria"),
          tags$br(),

          tags$div(
            HTML("<i>Remark: If you have a CAM structure, which can be separated (e.g. pre-defined opposing concepts) the CAMs can be automatically
          sliced according to two possible criteria: (a) delete a connection between two concepts, and / or (b) delete a concept. </i>"),
          tags$br(),
          HTML("<b>After seperating a CAM applying (a) and / or (b) what are your central concepts?</b><br>
          Remark: The list is sorted alphabetically, by pressing a letter key or writing
          you can search for specific words."),
          style="font-size:14px"),
          uiOutput(ns("selectCentralWords_SC")),
          tags$div(
            HTML("<b>(a) Remove connection</b><br>
          Remark: Please specify only 2 concepts here between which you want to remove the connection (if one exists)."),
          style="font-size:14px"),
          uiOutput(ns("selectRemoveConnection_SC")),
          tags$div(
            HTML("<b>(b) Remove concept</b><br>
          Remark: Please specify only 1 concept here  which you want to delete (if it exists)."),
          style="font-size:14px"),
          uiOutput(ns("selectRemoveConcept_SC")),
          tags$div(
            HTML("Click on button to run function to slice your CAMs. Please click only once and
                      wait few seconds:"),
            style="font-size:14px"),
          actionButton(ns("clickSliceCAMs"), "Slice CAMs"),
          tags$p(
            "In total applying your settings your were able to slice ",
            tags$b(textOutput(ns("slicedCAMs_num"), inline = TRUE)), " CAMs, which are ",
            tags$b(textOutput(ns("slicedCAMs_perc"), inline = TRUE)), "% of your total CAM data set."),
          tags$div(HTML("Side by side is the original CAM and the sliced CAM:"), style="font-size:14px"),
          fluidRow(
            column(width = 5,
                   tags$b("original CAM:"),
                   plotOutput(ns("plot_original"), width = "95%")
            ),
            column(width = 5,
                   tags$b("sliced CAM:"),
                   plotOutput(ns("plot_slicedCombined"), width = "95%")
            )),
          tags$br(),
          tags$div(HTML("<i>To download seperated files of your defined central concepts (e.g. to subsequently summarize separately), please download all your files globally using the button top right.</i>"),
                   style="font-size:14px")
        )
      })

      #> Server
      ## choices CAMs for selectInput
      uniqueConceptsSliceCAMs <- reactive({
        req(dataCAM())
        tmp_text <- str_remove_all(string = globals$dataCAMsummarized[[1]]$text_summarized, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
        # tmp_text <- names(sort(x = table(tmp_text), decreasing = TRUE))
        sort(unique(tmp_text))
      })


      output$selectCentralWords_SC <- renderUI({
        selectInput(ns("CentralWords_SC"),
                    NULL,
                    choices = as.list(uniqueConceptsSliceCAMs()), width = "50%",   multiple = TRUE, selectize = TRUE
        )
      })

      output$selectRemoveConnection_SC <- renderUI({
        selectInput(ns("RemoveConnection_SC"),
                    NULL,
                    choices = as.list(uniqueConceptsSliceCAMs()), width = "50%",   multiple = TRUE, selectize = TRUE
        )
      })

      output$selectRemoveConcept_SC <- renderUI({
        selectInput(ns("RemoveConcept_SC"),
                    NULL,
                    choices = as.list(uniqueConceptsSliceCAMs()), width = "50%",   multiple = TRUE, selectize = TRUE
        )
      })

      slicedCAMs_combined_out <- eventReactive(input$clickSliceCAMs, {
        req(drawnCAM())


        ## show modal and break if not exactly two concepts have been defined
        if(length(input$CentralWords_SC) != 2){
          showModal(
            modalDialog(
              title = "Error while trying to slice your CAMs",
              "It appears that you not have defined exactly 2 central concepts.",
              easyClose = TRUE,
              footer = tagList(modalButton("Ok"))
            )
          )

          return(NULL)
        }


        tmp_RemoveConnection_SC = NULL
        tmp_RemoveConcept_SC = NULL

        # set inputs for slice function
        if(!is.null(input$RemoveConnection_SC) & !is.null(input$RemoveConcept_SC)){
          tmp_RemoveConnection_SC = input$RemoveConnection_SC
          tmp_RemoveConcept_SC = input$RemoveConcept_SC
        }else if(!is.null(input$RemoveConnection_SC)){
          tmp_RemoveConnection_SC = input$RemoveConnection_SC
        }else if(!is.null(input$RemoveConcept_SC)){
          tmp_RemoveConcept_SC = input$RemoveConcept_SC
        }

        ## show modal and break if RemoveConcept_SC violated
        if(!is.null(input$RemoveConcept_SC) & length(input$RemoveConcept_SC) != 1){
          showModal(
            modalDialog(
              title = "Error while trying to slice your CAMs",
              "It appears that you have defined more than 1 concept to delete (not possible).",
              easyClose = TRUE,
              footer = tagList(modalButton("Ok"))
            )
          )

          return(NULL)
        }

        ## show modal and break if RemoveConnection_SC violated
        if(!is.null(input$RemoveConnection_SC) & length(input$RemoveConnection_SC) != 2){
          showModal(
            modalDialog(
              title = "Error while trying to slice your CAMs",
              "It appears that you not have defined exactly 2 concept to remove the connection between (not possible).",
              easyClose = TRUE,
              footer = tagList(modalButton("Ok"))
            )
          )
          return(NULL)
        }


        slicedCAMs_combined <- sliceAllCAMs_combined(CAMfilesList = globals$dataCAMsummarized,
                                                     drawnCAMs = drawnCAM(),
                                                     connectionToRemove = tmp_RemoveConnection_SC,
                                                     nodeToRemove = tmp_RemoveConcept_SC,
                                                     centralConceptsSubgraphs = input$CentralWords_SC,
                                                     plot = FALSE)

        if(nrow(slicedCAMs_combined[[1]]) > 1){
          tmp_merged <- slicedCAMs_combined[[3]][slicedCAMs_combined[[3]]$CAM.x %in% unique(slicedCAMs_combined[[3]]$CAM.x)[1], ]
          tmp_nodes <- slicedCAMs_combined[[1]][slicedCAMs_combined[[1]]$CAM %in% unique(slicedCAMs_combined[[1]]$CAM)[1], ]

          tmp_c12 <- draw_CAM(dat_merged = tmp_merged,
                              dat_nodes = tmp_nodes, ids_CAMs = "all",
                              plot_CAM = FALSE,
                              useCoordinates = TRUE,
                              relvertexsize = 3,
                              reledgesize = 1)

          ## plot CAMs
          output$plot_original <- renderPlot({
            plot(drawnCAM()[[names(tmp_c12)[1]]], edge.arrow.size = .4,
                 layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = 0.1,
                 vertex.size = 10, vertex.label.cex = .9)
          })

          output$plot_slicedCombined <- renderPlot({
            plot(tmp_c12[[1]], edge.arrow.size = .4,
                 layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = 0.1,
                 vertex.size = 10, vertex.label.cex = .9)
          })


          ## get seperated data sets
          globals$dataSlicedCAMs_seperated <- sliceAllCAMs_seperated(slicedCAMs = slicedCAMs_combined,
                                                                     centralConceptsSubgraphs = input$CentralWords_SC,
                                                                     plot = FALSE)
          globals$namingSlicedCAMs <- input$CentralWords_SC
        }


        slicedCAMs_combined
      })



      ## number of sliced CAMs
      output$slicedCAMs_num <-  renderText({
        req(slicedCAMs_combined_out())
        length(unique(slicedCAMs_combined_out()[[1]]$CAM))
      })

      ## percentage of sliced CAMs
      output$slicedCAMs_perc <-  renderText({
        req(slicedCAMs_combined_out())
        round(x = length(unique(slicedCAMs_combined_out()[[1]]$CAM)) /  length(unique(globals$dataCAMsummarized[[1]]$CAM)) * 100, digits = 0)
      })







      ###### neighborhoodIndicatorsDescriptives
      #> UI
      observeEvent(input$sliceCAMsDescriptives, {

        ## change UI
        outUI$elements <- tagList(
          tags$h2("Compute descriptives statistics of sliced CAMs"),
          tags$br(),
          tags$div(
            HTML("If you have successfully sliced CAMs you will see an APA table with multiple summary statistics:"),
            style="font-size:14px"),
          htmlOutput(ns("text_c1")),
          dataTableOutput(ns("APAtable_slicedCAMs_c1")),
          tags$br(),
          htmlOutput(ns("text_c2")),
          dataTableOutput(ns("APAtable_slicedCAMs_c2")),
          tags$br(),
          tags$div(
            HTML("If you have successfully sliced CAMs you will see two violin plots regarding (i) overall mean valence and
          (ii) number of drawn concepts:"),
          style="font-size:14px"),
          tags$b("(i) overall mean valence"),
          plotOutput(ns("plot_meanValence_SC"), width = "600px", height = "800px"),
          tags$b("(ii) number of drawn concepts"),
          plotOutput(ns("plot_numConcepts_SC"), width = "600px", height = "800px")
        )
      })


      #> Server
      networkInd_c12 <- eventReactive(input$sliceCAMsDescriptives, {
        if(!is.null(globals$dataSlicedCAMs_seperated)){
          CAMdrawn_c1 <- draw_CAM(dat_merged = globals$dataSlicedCAMs_seperated[[3]],
                                  dat_nodes = globals$dataSlicedCAMs_seperated[[1]], ids_CAMs = "all",
                                  plot_CAM = FALSE,
                                  useCoordinates = TRUE,
                                  relvertexsize = 3,
                                  reledgesize = 1)

          CAMdrawn_c2 <- draw_CAM(dat_merged = globals$dataSlicedCAMs_seperated[[6]],
                                  dat_nodes = globals$dataSlicedCAMs_seperated[[4]], ids_CAMs = "all",
                                  plot_CAM = FALSE,
                                  useCoordinates = TRUE,
                                  relvertexsize = 3,
                                  reledgesize = 1)


          tmp_microIndicators_c1 <- compute_indicatorsCAM(drawn_CAM = CAMdrawn_c1,
                                                          micro_degree = NULL,
                                                          micro_valence = NULL,
                                                          micro_centr_clo = NULL,
                                                          micro_transitivity = NULL,
                                                          largestClique = FALSE)


          tmp_microIndicators_c2 <- compute_indicatorsCAM(drawn_CAM = CAMdrawn_c2,
                                                          micro_degree = NULL,
                                                          micro_valence = NULL,
                                                          micro_centr_clo = NULL,
                                                          micro_transitivity = NULL,
                                                          largestClique = FALSE)

          tmp_microIndicators_c12 <- rbind(tmp_microIndicators_c1, tmp_microIndicators_c2)
          tmp_microIndicators_c12$group <- rep(globals$namingSlicedCAMs, each = nrow(tmp_microIndicators_c1))
          tmp_microIndicators_c12
        }
      })

      ## APA tables

      output$text_c1 <- renderUI({
        HTML(
          paste0(
            'APA table for the following central concept: ',
            globals$namingSlicedCAMs[1]
          )
        )
      })


      output$APAtable_slicedCAMs_c1 <- renderDataTable({
        req(networkInd_c12())
        getDescriptives(dataset = networkInd_c12()[networkInd_c12()$group == globals$namingSlicedCAMs[1], ],
                        nameAPAtable = NULL)
      })


      output$text_c2 <- renderUI({
        HTML(
          paste0(
            'APA table for the following central concept: ',
            globals$namingSlicedCAMs[2]
          )
        )
      })

      output$APAtable_slicedCAMs_c2 <- renderDataTable({
        req(networkInd_c12())
        getDescriptives(dataset = networkInd_c12()[networkInd_c12()$group == globals$namingSlicedCAMs[2], ],
                        nameAPAtable = NULL)
      })






      ## plot paired t-tests
      output$plot_meanValence_SC <- renderPlot({
        req(networkInd_c12())
        p_meanValence <- ggstatsplot::ggwithinstats(
          data = networkInd_c12(),
          x = group,
          y = mean_valence_macro,
          type = "parametric", ## type of statistical test
          xlab = "Central Concepts", ## label for the x-axis
          ylab = "Mean Valence", ## label for the y-axis
          title = "Comparison of mean valence between choosen central concepts"
        ) + ## modifying the plot further
          ggplot2::scale_y_continuous(
            limits = c(-3, 3),
            breaks = -3:3
          )
        p_meanValence
      })

      output$plot_numConcepts_SC <- renderPlot({
        req(networkInd_c12())
        p_numConcepts <- ggstatsplot::ggwithinstats(
          data = networkInd_c12(),
          x = group,
          y = num_nodes_macro,
          type = "parametric", ## type of statistical test
          xlab = "Central Concepts", ## label for the x-axis
          ylab = "Number of Drawn Concepts", ## label for the y-axis
          title = "Comparison of number of drawn concepts between choosen central concepts"
        )
        p_numConcepts
      })

      ###### information
      observeEvent(input$informationSliceCAMs, {
        ## change UI
        outUI$elements <- tagList(
          tags$h2("Module Specific Information"),
          tags$div(
            HTML('The options for this module are the following:'),
            tags$ul(
              tags$li(HTML('<b>slice CAMs:</b> ...')),
              tags$li(HTML('<b>get sliced CAMs descriptives:</b> ...'))
            )
          )
        )
      })

    })
  }
