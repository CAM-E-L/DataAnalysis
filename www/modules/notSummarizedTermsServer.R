
notSummarizedTermsServer <-
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
        h1("Non-Summarized (ns) terms module"),
        tags$br(),
        tags$br(),
        HTML(
          'To start the module please click on one of the the module option on the sidebar panel. The options for
                     this module are the following:'
        ),
        tags$ul(
          tags$li(
            HTML(
              '<b>get non-summarized terms:</b> During the process of summarizing your data it is highly likely that you will miss some 
              concepts, which can be printed out using this function.'
            )
          ),
          tags$li(
            HTML(
              '<b>click through non-summarized terms:</b> All concepts, which you have missed / not seen using the "summarize terms" functions 
              can be summarized concept by concept clicking through the non-summarized terms.'
            )
          ),
          tags$li(
            HTML(
              '<b>Information:</b> Further information regarding this module.'
            )
          )
        )
      ))
      
      ## set output
      output$NST_out <- renderUI({
        outUI$elements
      })











      ############
      # information non-summarized terms
      ############
      #> UI
      observeEvent(input$NST_information, {
        ## change UI
        outUI$elements <-
          tagList(tags$h2("Module Specific Information"),
                  tags$div(
                    HTML('The options for this module are the following:'),
        tags$ul(
          tags$li(
            HTML(
              '<b>get non-summarized terms:</b> During the process of summarizing your data it is highly likely that you will miss some 
              concepts, which can be printed out using this function. Additionally, you can choose if you only want to get the concepts with a 
              specific valence (default is all concepts).'
            )
          ),
          tags$li(
            HTML(
              '<b>click through non-summarized terms:</b> All concepts, which you have missed / not seen using the "summarize terms" functions 
              can be summarized concept by concept clicking through the non-summarized terms.'
            )
          ),
          )
          ))
      })



    })
}

