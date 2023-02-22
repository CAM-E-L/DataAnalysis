wordOutputs_overallServer <-
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
        h1("Word Outputs Module - overall"),
        tags$br(),
        tags$br(),
        HTML(
          'To start the module please click on one of the the module option on the sidebar panel. The options for
                     this module are the following:'
        ),
              tags$ul(
              tags$li(HTML('<b>get wordlist:</b> ...')),
              tags$li(HTML('<b>get wordcloud:</b> ...'))
            )
      ))

      ## set output
      output$uploadOutWordsOverall <- renderUI({
        outUI$elements
      })

      ################################
      # single module options
      ################################
        ###### wordlist_overall
        #> UI
        observeEvent(input$wordlist_overall, {

          ## change UI
          outUI$elements <- tagList(
          tags$h2("Compute Wordlists"),
          tags$br(),
          )
        })


        #> Server
         

       ###### wordClouds_overall
        #> UI
        observeEvent(input$wordClouds_overall, {

          ## change UI
          outUI$elements <- tagList(
          tags$h2("Compute Word Clouds"),
          tags$br(),
          )
        })


        #> Server
         

        

        ###### information
        observeEvent(input$informationWordsOverall, {
          ## change UI
          outUI$elements <- tagList(
            tags$h2("Module Specific Information"),
            tags$div(
              HTML('The options for this module are the following:'),
              tags$ul(
              tags$li(HTML('<b>get wordlist:</b> ...')),
              tags$li(HTML('<b>get wordcloud:</b> ...'))
            )
          )
          )
        })

    })
    }