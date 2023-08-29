
networkIndicatorsServer <-
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
        h1("Network Indicators module"),
        tags$br(),
        tags$br(),
        HTML(
          'To start the module please click on one of the module options on the sidebar panel. The options for
                     this module are the following:'
        ),
              tags$ul(
              tags$li(HTML('<b>get network indicators:</b>  Compute 33 different network indicators (e.g., mean valence, density etc.) on an overall CAM level (macro). Additionally, 
              select one or several concepts and calculate network indicators on an individual concept level (micro).')),
              tags$li(HTML('<b>get network descriptives:</b> Get a summary of network statistics you have calculated, get an APA-formatted table of statistics, get a correlation plot 
              between different network indicators and search for significant correlations between network indicators.')),
              tags$li(HTML('<b>get neighborhood indicators:</b> Compute several variants (in total six variants) of average valences over groups of concepts.')),
              tags$li(HTML('<b>get neighborhood descriptives:</b> Get a summary of neighborhood statistics you have calculated, get an APA-formatted table of statistics and get a correlation plot between 
              different neighborhood indicators.'))
            )
      ))

      ## set output
      output$uploadOutNetworkIndicators <- renderUI({
        outUI$elements
      })

      ################################
      # single module options
      ################################
        ###### networkIndicators
        #> UI
        observeEvent(input$networkIndicators, {

          ## change UI
          outUI$elements <- tagList(
          tags$h2("Compute network indicators"),
          tags$br(),
                  uiOutput(ns("selectMicro_NI")),
        tags$div(
          HTML("Click on the button to run a function to get the network indicators of your CAMs. Please click only once and
                      wait few seconds:"),
                      style="font-size:14px"),
        actionButton(ns("clickNetworkIndicators"), "Compute network indicators"),
                tags$p(
            "You computed the network indicators of ",
            tags$b(textOutput(ns("numCAMsDrawnNI"), inline = TRUE), " CAMs.")
        ),
        tags$div(HTML("Dynamic table of network indicators:"), style="font-size:14px"),
        dataTableOutput(ns("networkIndicatorsTable")),
        tags$br(),
        htmlOutput(ns("textTermsMultiple_NI")),
        tags$br(),
        tags$div(HTML("<i>To download the network indicators, download all your files globally using the button top right.</i>"),
        style="font-size:14px")
          )
        })


        #> Server
            networkIndicators <- eventReactive(input$clickNetworkIndicators, {
            req(drawnCAM())



            if(is.null(input$micro_NI)){
            tmp_NI <- compute_indicatorsCAM(drawn_CAM = drawnCAM(),
                                  micro_degree = NULL,
                                  micro_valence = NULL,
                                  micro_centr_clo = NULL,
                                  micro_transitivity = NULL,
                                  largestClique = FALSE)
            }else{
              tmp_NI <- compute_indicatorsCAM(drawn_CAM = drawnCAM(),
                                  micro_degree = input$micro_NI,
                                  micro_valence = input$micro_NI,
                                  micro_centr_clo = input$micro_NI,
                                  micro_transitivity = input$micro_NI,
                                  largestClique = FALSE)
            }


            #> change condition
            globals$condition <- c(globals$condition, "networkIndicators")
            #> add global data set
    globals$dataNetworkIndicators <- tmp_NI
            tmp_NI
        })

        ## number of CAMs
        output$numCAMsDrawnNI <-  renderText({
            nrow(networkIndicators())
        })

        ## dynamic table network indicators
        output$networkIndicatorsTable <- renderDataTable({
            networkIndicators()
        })


        ## choices CAMs for selectInput
        uniqueConcepts_NI <- reactive({
          req(dataCAM())

         tmp_text <- str_remove_all(string = globals$dataCAMsummarized[[1]]$text_summarized, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")

          sort(unique(tmp_text))
        })


        output$selectMicro_NI <- renderUI({
          selectInput(ns("micro_NI"),
                      "For which concepts do you want to get micro indicators?",
                      choices = as.list(uniqueConcepts_NI()), width = "50%",   multiple = TRUE
          )
        })


        output$textTermsMultiple_NI <- renderUI({
          req(dataCAM())

          if(!is.null(input$micro_NI)){
            ## get number of times concept was drawn
            tmp_nodes <- globals$dataCAMsummarized[[1]]
            tmp_nodes$text_summarized <-
              str_remove_all(string = tmp_nodes$text_summarized,
                             pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
            tmp_mat <- table(tmp_nodes$CAM, tmp_nodes$text_summarized)

            tmp_vector_Xtimes <- NULL
            h = 1
            for (i in 1:length(input$micro_NI)) {
              input$micro_NI[i]

              if (any(tmp_mat[, colnames(tmp_mat) == input$micro_NI[i]] > 1)) {
                tmp_vector_Xtimes[h] <- input$micro_NI[i]
                h = h + 1
              }
            }


          if (!is.null(tmp_vector_Xtimes)) {
            print("tmp_vector_Xtimes")
            print(tmp_vector_Xtimes)

            text_out <- paste0(
                  '<b style="color:red;">The following (summarized) concept(s) was / were drawn multiple times within an individual CAM:</b> ',
                  paste0(tmp_vector_Xtimes, collapse = " // "))
          } else {
            text_out <- paste0(' ')
          }
          } else {
            text_out <- paste0(' ')
          }

          HTML(text_out)
        })





        ###### networkIndicatorsDescriptives
        #> UI
        observeEvent(input$networkIndicatorsDescriptives, {

          ## change UI
          outUI$elements <- tagList(
          tags$h2("Compute descriptive statistics of network indicators"),
          tags$br(),
                  tags$div(
          HTML("If you have computed the network indicators you will see a dynamic table with multiple summary statistics:"),
                      style="font-size:14px"),
          dataTableOutput(ns("APAtable_NIdes")),
                    tags$br(),
                  tags$div(
          HTML("If you have computed the network indicators you will see a plot of the correlational matrix of
          your numeric network indicators (Pearson correlation):"),
                      style="font-size:14px"),
                     plotOutput(ns("corPlot_NIdes"), width = "100%", height = "600px"),

        tags$div(
         HTML("You can search single or multiple network indicators for significant correlations (Pearson correlation):"),
                      style="font-size:14px"),
          uiOutput(ns("selectSearchSigCorr_NIdes")),
        dataTableOutput(ns("SigCorrelations_NIdes"))



          )
        })


        #> Server
      output$APAtable_NIdes <- renderDataTable(
        if (!is.null(globals$dataNetworkIndicators)) {
          getDescriptives(dataset = globals$dataNetworkIndicators, nameAPAtable = NULL)
          }else{
            NULL
            }
        )



        ## plot CAM
        output$corPlot_NIdes <- renderPlot({
     if (!is.null(globals$dataNetworkIndicators)) {
      ggcorrplot::ggcorrplot(corr = cor(globals$dataNetworkIndicators[, unlist(lapply(globals$dataNetworkIndicators, is.numeric))]),
           hc.order = FALSE, type = "lower", lab = TRUE, lab_size = 2,
           title = "Correlation Plot of Network Indicators")
          }else{
            NULL
            }
        })



                ## choices CAMs for selectInput
        uniqueNumericNI_NIdes <- reactive({

                  if (!is.null(globals$dataNetworkIndicators)) {
                      vec_names <- globals$dataNetworkIndicators %>%
    select_if(Negate(is.character)) %>%
    colnames()
    vec_names
                  }else{
                    NULL
                  }
        })

        output$selectSearchSigCorr_NIdes <- renderUI({
          selectInput(ns("SearchSigCorr_NIdes"),
                      "For which network indicators do you want to search for significiant correlations?",
                      choices = as.list(uniqueNumericNI_NIdes()), width = "50%",   multiple = TRUE
          )
        })


       ## dynamic table network indicators
        output$SigCorrelations_NIdes <- renderDataTable({
        if(is.null(input$SearchSigCorr_NIdes)){
          print("Please specifiy network indicators to check for significant correlations.")
        }else{
        if (!is.null(globals$dataNetworkIndicators)) {

  des_sigCorr(indicatos_CAM = globals$dataNetworkIndicators, vars = input$SearchSigCorr_NIdes)
          }else{
            print("Please compute network indicators before checking for significant correlations.")
            }
        }
        })




        ###### neighborhoodIndicators
        #> UI
        observeEvent(input$neighborhoodIndicators, {

          ## change UI
          outUI$elements <- tagList(
          tags$h2("Compute neighborhood indicators of single concepts"),
          tags$br(),

        tags$div(
          HTML("<b>For which concepts do you want to get neighborhood indicators?</b><br>
          Remark: The list is sorted alphabetically, by pressing writing in the field 
          you can search for specific concepts."),
                      style="font-size:14px"),
                  uiOutput(ns("selectNeighborhood_NI")),
                       tags$div(
                  HTML("<b>Remove connection</b><br>
          Remark: Please specify only two concepts here between which you want to remove the connection (if one exists)."),
                      style="font-size:14px"),
                  uiOutput(ns("selectRemoveConnection_NI")),
                                         tags$div(
                  HTML("<b>Remove concept</b><br>
          Remark: Please specify only one concept here  which you want to delete (if it exists)."),
                      style="font-size:14px"),
                  uiOutput(ns("selectRemoveConcept_NI")),
        tags$div(
          HTML("Click on the button to run a function to get the neighborhood indicators of your CAMs. Please click only once and
                      wait few seconds:"),
                      style="font-size:14px"),
        actionButton(ns("clickNeighborhoodIndicators"), "Compute neighborhood indicators"),
                tags$p(
            "You computed the neighborhood indicators of ",
            tags$b(textOutput(ns("numCAMsDrawnNI"), inline = TRUE), " CAMs.")
        ),
        tags$div(HTML("Dynamic table of neighborhood indicators:"), style="font-size:14px"),
        dataTableOutput(ns("neighborhoodIndicatorsTable")),
         tags$br(),
        htmlOutput(ns("textTermsMultiple_NeighborhoodInd")),
        tags$br(),
        tags$div(HTML("<i>To download the neighborhood indicators, download all your files globally using the button top right.</i>"),
        style="font-size:14px")
          )
        })

        #> Server
                ## choices CAMs for selectInput
        uniqueConceptsNeighborhood_NI <- reactive({
          req(dataCAM())
  tmp_text <- str_remove_all(string = globals$dataCAMsummarized[[1]]$text_summarized, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
 # tmp_text <- names(sort(x = table(tmp_text), decreasing = TRUE))
 sort(unique(tmp_text))
        })



        output$selectNeighborhood_NI <- renderUI({
          selectInput(ns("neighborhood_NI"),
                      NULL,
                      choices = as.list(uniqueConceptsNeighborhood_NI()), width = "50%",   multiple = TRUE, selectize = TRUE
          )
        })

        output$selectRemoveConnection_NI <- renderUI({
          selectInput(ns("removeConnection_NI"),
                      NULL,
                      choices = as.list(uniqueConceptsNeighborhood_NI()), width = "50%",   multiple = TRUE, selectize = TRUE
          )
        })

                output$selectRemoveConcept_NI <- renderUI({
          selectInput(ns("removeConcept_NI"),
                      NULL,
                      choices = as.list(uniqueConceptsNeighborhood_NI()), width = "50%",   multiple = TRUE, selectize = TRUE
          )
        })

NeighborhoodIndicators <- eventReactive(input$clickNeighborhoodIndicators, {
  req(drawnCAM())

  print("input$neighborhood_NI")
  #print(Encoding(input$neighborhood_NI))
  print(input$neighborhood_NI)

  if(is.null(input$neighborhood_NI)){
return(NULL)
  }else{
            tmp_removeConnection_NI = NULL
            tmp_removeConcept_NI = NULL
            tmp_sliceCAMbool = FALSE
            # set inputs for slice function
      if(!is.null(input$removeConnection_NI) & !is.null(input$removeConcept_NI)){
            tmp_removeConnection_NI = input$removeConnection_NI
            tmp_removeConcept_NI = input$removeConcept_NI
                        tmp_sliceCAMbool = TRUE
      }else if(!is.null(input$removeConnection_NI)){
            tmp_removeConnection_NI = input$removeConnection_NI
                                    tmp_sliceCAMbool = TRUE
      }else if(!is.null(input$removeConcept_NI)){
            tmp_removeConcept_NI = input$removeConcept_NI
                                    tmp_sliceCAMbool = TRUE
      }


print("tmp_removeConnection_NI")
print(tmp_removeConnection_NI)
print("tmp_removeConcept_NI")
print(tmp_removeConcept_NI)
print("tmp_sliceCAMbool")
print(tmp_sliceCAMbool)
## compute network neighborhood indicators
                  tmp_NI <- compute_neighborhoodIndicatorsCAM(drawn_CAM = drawnCAM(),
                                   weightSecondOrder = .5,
                                   consideredConcepts = input$neighborhood_NI,
                                              sliceCAMbool = tmp_sliceCAMbool,
                                              removeConnectionCAM = tmp_removeConnection_NI,
                                              removeNodeCAM = tmp_removeConcept_NI)

                                                #> change condition
  globals$condition <- c(globals$condition, "networkNeighborhoodIndicators")
  #> add global data set
  globals$dataNetworkNeighborhoodIndicators <- tmp_NI
  tmp_NI
  }



})

## dynamic table network indicators
output$neighborhoodIndicatorsTable <- renderDataTable({
  NeighborhoodIndicators()
})


 output$textTermsMultiple_NeighborhoodInd <- renderUI({
          req(dataCAM())

          if(!is.null(input$neighborhood_NI)){
            ## get number of times concept was drawn
            tmp_nodes <- globals$dataCAMsummarized[[1]]
            tmp_nodes$text_summarized <-
              str_remove_all(string = tmp_nodes$text_summarized,
                             pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
            tmp_mat <- table(tmp_nodes$CAM, tmp_nodes$text_summarized)

            tmp_vector_Xtimes <- NULL
            h = 1
            for (i in 1:length(input$neighborhood_NI)) {
              input$neighborhood_NI[i]

              if (any(tmp_mat[, colnames(tmp_mat) == input$neighborhood_NI[i]] > 1)) {
                tmp_vector_Xtimes[h] <- input$neighborhood_NI[i]
                h = h + 1
              }
            }


          if (!is.null(tmp_vector_Xtimes)) {
            print("tmp_vector_Xtimes")
            print(tmp_vector_Xtimes)

            text_out <- paste0(
                  '<b style="color:red;">The following (summarized) concept(s) was / were drawn multiple times within an individual CAM (not possible to compute neighborhood indicators):</b> ',
                  paste0(tmp_vector_Xtimes, collapse = " // "))
          } else {
            text_out <- paste0(' ')
          }
          } else {
            text_out <- paste0(' ')
          }

          HTML(text_out)
        })





        ###### neighborhoodIndicatorsDescriptives
        #> UI
        observeEvent(input$neighborhoodIndicatorsDescriptives, {

          ## change UI
          outUI$elements <- tagList(
          tags$h2("Compute descriptive statistics of neighborhood indicators"),
          tags$br(),
                  tags$div(
          HTML("If you have computed the neighborhood indicators you will a dynamic table with multiple summary statistics:"),
                      style="font-size:14px"),
          dataTableOutput(ns("APAtable_NIdes_neighborhood")),
                    tags$br(),
                  tags$div(
          HTML("If you have computed the neighborhood indicators you will see a plot of the correlational matrix of
          your numeric network indicators (Pearson correlation):"),
                      style="font-size:14px"),
                     plotOutput(ns("corPlot_NIdes_neighborhood"), width = "100%", height = "600px"),
          )
        })


        #> Server
      output$APAtable_NIdes_neighborhood <- renderDataTable(
        if (!is.null(globals$dataNetworkNeighborhoodIndicators)) {
          getDescriptives(dataset = globals$dataNetworkNeighborhoodIndicators, nameAPAtable = NULL)
          }else{
            NULL
            }
        )



        ## plot CAM
        output$corPlot_NIdes_neighborhood <- renderPlot({
     if (!is.null(globals$dataNetworkNeighborhoodIndicators)) {
      ggcorrplot::ggcorrplot(corr = cor(globals$dataNetworkNeighborhoodIndicators[, unlist(lapply(globals$dataNetworkNeighborhoodIndicators, is.numeric))]),
           hc.order = FALSE, type = "lower", lab = TRUE, lab_size = 3,
           title = "Correlation Plot of Neighborhood Network Indicators")
          }else{
            NULL
            }

        })

        ###### information
        observeEvent(input$informationNetworkIndicators, {
          ## change UI
          outUI$elements <- tagList(
            tags$h2("Module Specific Information"),
            tags$div(
              HTML('The options for this module are the following:'),
             tags$ul(
              tags$li(HTML('<b>get network indicators:</b>  Compute 33 different network indicators (e.g., mean valence, density etc.) on an overall CAM level (macro). Additionally, 
              select one or several concepts and calculate network indicators on an individual concept level (micro). Please check the online documentation for detailed information: 
               <a href="https://camtools-documentation.readthedocs.io/en/master/CAM-App/#compute-network-indicators" target="_blank">https://camtools-documentation.readthedocs.io/en/master/CAM-App/#compute-network-indicators</a>.')),
              tags$li(HTML('<b>get network descriptives:</b> Get a summary of network statistics you have calculated, get an APA-formatted table of statistics, get a correlation plot 
              between different network indicators and search for significant correlations between network indicators.')),
              tags$li(HTML('<b>get neighborhood indicators:</b> Compute several variants (in total six variants) of average valences over groups of concepts. 
              Please check the online documentation for detailed information: 
               <a href="https://camtools-documentation.readthedocs.io/en/master/CAM-App/#compute-neighborhood-network-indicators" target="_blank">https://camtools-documentation.readthedocs.io/en/master/CAM-App/#compute-neighborhood-network-indicators</a>.')),
              tags$li(HTML('<b>get neighborhood descriptives:</b> Get a summary of neighborhood statistics you have calculated, get an APA-formatted table of statistics and get a correlation plot between 
              different neighborhood indicators.'))
            )
          )
          )
        })

    })
    }
