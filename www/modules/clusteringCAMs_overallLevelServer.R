clusteringCAMs_overallLevelServer <-
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
        h1("Clustering CAMs - on overall level"),
        tags$br(),
        tags$br(),
        HTML(
          'To start the module please click on one of the module options on the sidebar panel. The options for
                     this module are the following:'
        ),
              tags$ul(
              tags$li(HTML('<b>get network similarities:</b> ...'))
                          )
      ))

      ## set output
      output$uploadOutClusteringCAMsOverall <- renderUI({
        outUI$elements
      })

      ################################
      # single module options
      ################################
        ###### networkSimilarities
        #> UI
        observeEvent(input$networkSimilarities, {

          ## change UI
          outUI$elements <- tagList(
          tags$h2("compute network similarities"),
          tags$br(),
         tags$i("Coming soon. Network similarity algorithms will be implemented in the future (e.g. NetSmile, Random Walk approaches) in a seperate article...")
          )
        })


        #> Server
         


         
        ###### information
        observeEvent(input$informationClusteringCAMsOverall, {
          ## change UI
          outUI$elements <- tagList(
            tags$h2("Module Specific Information"),
            tags$div(
              HTML('The options for this module are the following:'),
              tags$ul(
                tags$li(HTML('<b>get network similarities:</b> <i>coming soon</i>'))
            )
          )
          )
        })

    })
    }