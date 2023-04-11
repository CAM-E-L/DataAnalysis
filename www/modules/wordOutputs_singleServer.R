wordOutputs_singleServer <-
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
        h1("Word Outputs Module - single"),
        tags$br(),
        tags$br(),
        HTML(
          'To start the module please click on one of the the module option on the sidebar panel. The options for
                     this module are the following:'
        ),
              tags$ul(
              tags$li(HTML('<b>get table, pie chart:</b> Create a table (APA 7 format) and pie chart for every summarized superordinate concept in your data set seperately')),
              tags$li(HTML('<b>get summary statistics:</b> Get summary statistics (e.g. percantage of time word appears in CAMs) for every summarized superordinate concept in your data set seperately'))
            )
      ))

      ## set output
      output$uploadOutWordsSingle <- renderUI({
        outUI$elements
      })

      ################################
      # single module options
      ################################
        ###### wordlist_overall
        #> UI
        observeEvent(input$getTablePie_single, {

          ## change UI
          outUI$elements <- tagList(
          tags$h2("get table, pie chart"),
          tags$br(),
          )
        })


        #> Server
         

       ###### wordClouds_overall
        #> UI
        observeEvent(input$getSummaryStats_single, {

          ## change UI
          outUI$elements <- tagList(
          tags$h2("get summary statistics"),
          tags$br(),
          )
        })


        #> Server
         
        ###### information
        observeEvent(input$informationWordsSingle, {
          ## change UI
          outUI$elements <- tagList(
            tags$h2("Module Specific Information"),
            tags$div(
              HTML('The options for this module are the following:'),
              tags$ul(
                tags$li(HTML('<b>get table, pie chart:</b> Create a table (APA 7 format) and pie chart for every summarized superordinate concept in your data set seperately')),
                tags$li(HTML('<b>get summary statistics:</b> Get summary statistics (e.g. percantage of time word appears in CAMs) for every summarized superordinate concept in your data set seperately'))
            )
          )
          )
        })

    })
    }