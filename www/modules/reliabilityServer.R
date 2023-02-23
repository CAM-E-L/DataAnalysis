
reliabilityServer <-
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
        h1("Reliability module"),
        tags$br(),
        tags$br(),
        HTML(
          'To start the module please click on one of the the module option on the sidebar panel. The options for
                     this module are the following:'
        ),
        tags$ul(
          tags$li(
            HTML(
              '<b>Train Reliability:</b> Here you can create word lists of your concepts, which can be summarized by (independet) raters.'
            )
          ),
          tags$li(
            HTML(
              '<b>Get Reliability:</b> Based on multiple summarized wordlists (at least 2) you can compute multiple reliability coefficients.'
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
      output$reliability_out <- renderUI({
        outUI$elements
      })
      
      
            ############
      # information summarize terms
      ############
      #> UI
      observeEvent(input$reliability_information, {
        ## change UI
        outUI$elements <-
          tagList(tags$h2("Module Specific Information"),
                  tags$div(
                    HTML('The options for this module are the following:'),
        tags$ul(
                   tags$li(
            HTML(
              '<b>Train Reliability:</b> Here you can create word lists of your concepts, which can be summarized by (independet) raters. 
              It is important, that you, in accordance with sending them the wordlist, inform your raters how to summarize the 
              respective word list, for a possible text see respective module.' 
            )
          ),
          tags$li(
            HTML(
              '<b>Get Reliability:</b> Based on multiple summarized wordlists (at least 2) you can compute multiple reliability coefficients: 
              Cohens Kappa (Lights Kappa) by (a) assuming a perfect match of overlapping group of words or (b) by maximizing overlapping words. Also 
              category-wise Fleiss Kappa is computed.'
            )
          ),
        )
                  ))
      })


    })
}

