
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
          tags$li(
            HTML(
              '<b>Approximate matching:</b> By using approximate string matching you can compute the string distances between all your unique concepts
              in the dataset (using optimal string alignment) to find words, which have been written slightly differently.'
            )
          ),
          tags$li(
            HTML(
              '<b>Searching terms:</b> Use regular expression to search your CAM data set for similar terms.'
            )
          ),
          tags$li(HTML('<b>wordVec:</b> to be implemented.')),
          tags$li(
            HTML(
              '<b>not summarized:</b> Check which words you do not have summarized.'
            )
          ),
          tags$li(
            HTML(
              '<b>Reliability:</b> Download all your unique concepts as wordlists (EXCEL file) and let different raters summarize the concepts to compute scores for inter-rater-reliability.'
            )
          ),
          tags$li(
            HTML(
              '<b>Information:</b> Further information regarding the module.'
            )
          )
        )
      ))

      ## set output
      output$uploadOutNetworkIndicators <- renderUI({
        outUI$elements
      })

      ################################
      # single module options
      ################################

    })
    }