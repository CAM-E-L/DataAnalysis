
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
          'To start the module please click on one of the the module option on the sidebar panel. The options for
                     this module are the following:'
        ),
              tags$ul(
              tags$li(HTML('<b>get network indicators:</b> ...')),
              tags$li(HTML('<b>get network descriptives:</b> ...'))
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
          HTML("Click on button to run function to get the network indicators of your CAMs. Please click only once and
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
        tags$div(HTML("<i>To download the network indicators download all your files globally using the button top right.</i>"),
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
                                  largestClique = FALSE)
            }else{
              tmp_NI <- compute_indicatorsCAM(drawn_CAM = drawnCAM(),
                                  micro_degree = input$micro_NI,
                                  micro_valence = input$micro_NI,
                                  micro_centr_clo = input$micro_NI,
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


        ###### networkIndicatorsDescriptives
        #> UI
        observeEvent(input$networkIndicatorsDescriptives, {

          ## change UI
          outUI$elements <- tagList(
          tags$h2("Compute descriptives statistics of network indicators"),
          tags$br(),
                  tags$div(
          HTML("If you have computed the network indicators you will see an APA table with multiple summary statistics:"),
                      style="font-size:14px"),
          uiOutput(ns("APAtable_NIdes")),
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
      output$APAtable_NIdes <- renderText(
        if (!is.null(globals$dataNetworkIndicators)) {

          
          getDescriptives(dataset = globals$dataNetworkIndicators, nameAPAtable = NULL)
          }else{
            NULL
            }
        )



        ## plot CAM
        output$corPlot_NIdes <- renderPlot({
     if (!is.null(globals$dataNetworkIndicators)) {
      psych::cor.plot(r = globals$dataNetworkIndicators[, unlist(lapply(globals$dataNetworkIndicators, is.numeric))], xlas = 2)
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
          Remark: The list is sorted alphabetically, by pressing a letter key or writing 
          you can search for specific words."),
                      style="font-size:14px"),
                  uiOutput(ns("selectNeighborhood_NI")),
                       tags$div(
                  HTML("<b>Remove connection</b><br>
          Remark: Please specify only 2 concepts here between which you want to remove the connection (if one exists)."),
                      style="font-size:14px"),
                  uiOutput(ns("selectRemoveConnection_NI")),
                                         tags$div(
                  HTML("<b>Remove concept</b><br>
          Remark: Please specify only 1 concept here  which you want to delete (if it exists)."),
                      style="font-size:14px"),
                  uiOutput(ns("selectRemoveConcept_NI")),
        tags$div(
          HTML("Click on button to run function to get the neighborhood indicators of your CAMs. Please click only once and
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
        tags$div(HTML("<i>To download the neighborhood indicators download all your files globally using the button top right.</i>"),
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
  print(Encoding(input$neighborhood_NI))
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





        ###### neighborhoodIndicatorsDescriptives
        #> UI
        observeEvent(input$neighborhoodIndicatorsDescriptives, {

          ## change UI
          outUI$elements <- tagList(
          tags$h2("Compute descriptives statistics of neighborhood indicators"),
          tags$br(),
                  tags$div(
          HTML("If you have computed the neighborhood indicators you will see an APA table with multiple summary statistics:"),
                      style="font-size:14px"),
          uiOutput(ns("APAtable_NIdes_neighborhood")),
                    tags$br(),
                  tags$div(
          HTML("If you have computed the neighborhood indicators you will see a plot of the correlational matrix of
          your numeric network indicators (Pearson correlation):"),
                      style="font-size:14px"),
                     plotOutput(ns("corPlot_NIdes_neighborhood"), width = "100%", height = "600px"),
          )
        })


        #> Server
      output$APAtable_NIdes_neighborhood <- renderText(
        if (!is.null(globals$dataNetworkNeighborhoodIndicators)) {
          getDescriptives(dataset = globals$dataNetworkNeighborhoodIndicators, nameAPAtable = NULL)
          }else{
            NULL
            }
        )



        ## plot CAM
        output$corPlot_NIdes_neighborhood <- renderPlot({
     if (!is.null(globals$dataNetworkNeighborhoodIndicators)) {
      psych::cor.plot(r = globals$dataNetworkNeighborhoodIndicators[, 
      unlist(lapply(globals$dataNetworkNeighborhoodIndicators, is.numeric))], xlas = 2)
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















        ###### information
        observeEvent(input$informationNetworkIndicators, {
          ## change UI
          outUI$elements <- tagList(
            tags$h2("Module Specific Information"),
            tags$div(
              HTML('The options for this module are the following:'),
             tags$ul(
              tags$li(HTML('<b>get network indicators:</b> ...')),
              tags$li(HTML('<b>get network descriptives:</b> ...'))
            )
          )
          )
        })

    })
    }
