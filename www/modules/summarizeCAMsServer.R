summarizeCAMsServer <-
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
        h1("Summarize CAMs Module"),
        tags$br(),
        tags$br(),
        HTML(
          'To start the module please click on one of the the module option on the sidebar panel. The options for
                     this module are the following:'
        ),
              tags$ul(
              tags$li(HTML('<b>aggregateCAMs:</b> By creating a so called “canonical adjacency matrix” CAMs according to different criteria (all CAMs, CAMs of a certain group) are aggregated, whereby the size of the concept and the thickness of the connection is proportional to the frequency of the drawn concepts and the pairwise connections respectively.'))
            )
      ))

      ## set output
      output$uploadOutSummarizeCAMs <- renderUI({
        outUI$elements
      })

      ################################
      # single module options
      ################################
        ###### wordlist_overall
        #> UI
        observeEvent(input$aggregateCAMs, {

          ## change UI
          outUI$elements <- tagList(
          tags$h2("aggregate CAMs"),
          tags$br(),
          )
        })


        #> Server
         
         

        

        ###### information
        observeEvent(input$informationSummarizeCAMs, {
          ## change UI
          outUI$elements <- tagList(
            tags$h2("Module Specific Information"),
            tags$div(
              HTML('The options for this module are the following:'),
              tags$ul(
              tags$li(HTML('<b>aggregateCAMs:</b> By creating a so called “canonical adjacency matrix” CAMs according to different criteria (all CAMs, CAMs of a certain group) are aggregated, whereby the size of the concept and the thickness of the connection is proportional to the frequency of the drawn concepts and the pairwise connections respectively.'))
            )
          )
          )
        })

    })
    }