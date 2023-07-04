
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
          'To start the module please click on one of the module options on the sidebar panel. The options for
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
              '<b>click through non-summarized terms:</b> all concepts, which you have missed / not seen using the "summarize terms" functions 
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

      ################################
      # single module options
      ################################


      ###############################
      ###############################
      ###############################

      ###### get not summarized words ######
      #> UI
      observeEvent(input$NST_getNonSummarized, {
               ## change UI
        outUI$elements <- tagList(
          tags$h2("Get all words you do not have summarized yet"),
          tags$br(),
          tags$div(
            HTML(
              'When using the module options of "summarize" you are summarizing the concepts and thereby you decreasing the total number
              of unique concepts in the dataset. During this process it is highly likely that you will miss some concepts, which can 
              be checked if you click on click on the button get words. 
              Additionally, you can choose if you only want to get the concepts with a specific valence 
              (default is all concepts). Please click only once and wait few seconds:'
            ),
            style = "font-size:14px"
          ),
          tags$br(),
           div(
            style = "margin: 0 auto; width: 100%; text-align:left;",
            div(
              style = "display: inline-block; vertical-align: top;",
                                   selectInput(ns("selectValence"), NULL, c("all",
                                                      "positive",
                                                      "negative",
                                                      "neutral",
                                                      "ambivalent"), width = "200px"),

            ),
            actionButton(
              ns("clickGetWords"),
              "get words",
              style = "display: inline-block;"
            ),
          ),
          tags$p(
              "Your CAM dataset contains ",
              tags$b(textOutput(ns(
                "numWordsNotSummarized"
              ), inline = TRUE)),
              " unique concepts in total, which you do NOT have summarized."
            ),
          verbatimTextOutput(ns("outGetWords")),
        )
      })

     #> Server
           ###############################
      ### get non-summarized words
      observeEvent(input$clickGetWords, {
        req(drawnCAM())
                 message("Selected valence is ",
                    input$selectValence)

output$outGetWords <- renderPrint({
  ## depending on choosen valence
  if(input$selectValence == "all"){
    tmp <- str_subset(string = unique(globals$dataCAMsummarized[[1]]$text_summarized), 
                      pattern = "_positive$|_negative$|_neutral$|_ambivalent$", negate = TRUE)
  }else if(input$selectValence == "positive"){
    tmp <- globals$dataCAMsummarized[[1]]$text_summarized[globals$dataCAMsummarized[[1]]$value > 0 & 
                                                            globals$dataCAMsummarized[[1]]$value < 10]
    tmp <- str_subset(string = unique(tmp), pattern = "_positive$|_negative$|_neutral$|_ambivalent$", negate = TRUE)
  }else if(input$selectValence == "negative"){
    tmp <- globals$dataCAMsummarized[[1]]$text_summarized[globals$dataCAMsummarized[[1]]$value < 0]
    tmp <- str_subset(string = unique(tmp), pattern = "_positive$|_negative$|_neutral$|_ambivalent$", negate = TRUE)
  }else if(input$selectValence == "neutral"){
    tmp <- globals$dataCAMsummarized[[1]]$text_summarized[globals$dataCAMsummarized[[1]]$value == 0]
    tmp <- str_subset(string = unique(tmp), pattern = "_positive$|_negative$|_neutral$|_ambivalent$", negate = TRUE)
  }else if(input$selectValence == "ambivalent"){
    tmp <- globals$dataCAMsummarized[[1]]$text_summarized[globals$dataCAMsummarized[[1]]$value == 10]
    tmp <- str_subset(string = unique(tmp), pattern = "_positive$|_negative$|_neutral$|_ambivalent$", negate = TRUE)
  }
  
  tmp
})
})
###############################

      ###############################
      ### show number of
      # > non summarized concepts
      output$numWordsNotSummarized <- renderText({
  if (!is.null(globals$dataCAMsummarized)){
    tmp <- str_subset(string = unique(globals$dataCAMsummarized[[1]]$text_summarized), 
                      pattern = "_positive$|_negative$|_neutral$|_ambivalent$", negate = TRUE)
    length(tmp)
  }
})
      ###############################






      ###############################
      ###############################
      ###############################

      ###### click trough not summarized words ######
      #> UI
      observeEvent(input$NST_clickThroughNonSummarized, {
               ## change UI
        outUI$elements <- tagList(
          tags$h2("Click through all non summarized words"),
          tags$br(),
          tags$div(
            HTML(
              'Using this function you can click trough all non summarized words, which you have missed by using the module options of 
              "summarize". However, this process should still be theoretically driven and you should not too hastily summarize concepts:'
            ),
            style = "font-size:14px"
          ),
          tags$br(),
          actionButton(ns("clickThrough"), "start clicking trough"),
          tags$p(
              "Your current CAM dataset contains ",
              tags$b(textOutput(ns(
                "numWordsNotSummarized"
              ), inline = TRUE)),
              " unique concepts in total, which you do NOT have summarized."
            )
        )
      })


    #> Server
           ###############################
      ### start looping through
      observeEvent(input$clickThrough, {
        req(drawnCAM())
          showModal(
            modalDialog(
              title = "Wait for it...",
              paste0("Will be implemented soon."),
              easyClose = TRUE,
              footer = tagList(modalButton("Ok"))
            )
        )
      })


###############################

      ###############################
      ### show number of
      # > see global
      ###############################


      ############
      # information summarize terms
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
              concepts, which can be printed out using this function.'
            )
          ),
          tags$li(
            HTML(
              '<b>click through non-summarized terms:</b> all concepts, which you have missed / not seen using the "summarize terms" functions 
              can be summarized concept by concept clicking through the non-summarized terms. Theoretically driven...'
            )
          ),
          tags$li(
            HTML(
              '<b>Information:</b> Further information regarding this module.'
            )
          )
        )
                  ))
      })

    })
}


