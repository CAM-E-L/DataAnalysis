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
          'To start the module please click on one of the module options on the sidebar panel. The options for
                     this module are the following:'
        ),
              tags$ul(
              tags$li(HTML('<b>get wordlist:</b> create a wordlist with summary statistics for each concept (mean / SD valence, mean / SD degree).')),
              tags$li(HTML('<b>get word cloud:</b> create a word cloud of all your concepts in the dataset, whereby the colors indicate the concept’s mean valence.'))
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
          tags$h2("Wordlists"),
          tags$br(),
                    tags$div(HTML("After choosing your settings, please click on the 
                    create wordlist button. 
          Please click only once and wait few seconds:"), style="font-size:14px"),
            tags$h3("Your Settings:"),
                        tags$br(),
                   tags$div(
            HTML(
              "Please select the ordering of the word list, if words should be split by given valence and if comments should be visible 
              (recommended to keep default settings):"
            ),
            style = "font-size:14px"
          ),
          div(
            style = "margin: 0 auto; width: 100%; text-align:left;",
            div(
              style = "display: inline-block; vertical-align: top; width: 23%; padding:5px;",
			  # specify order
                       tags$div(HTML("Please specify the order of your wordlist:"), style="font-size:14px"),
                   radioButtons(ns("order_setting_WordsOverall"), label = NULL, c("alphabetic", "frequency"), selected = "alphabetic"),
            ),
      div(
              style = "display: inline-block; vertical-align: top; width: 23%; padding:5px;",
			  # specify including comments
		                     tags$div(HTML("Do you want to include comments in your wordlist?"), style="font-size:14px"),
                   radioButtons(ns("comments_setting_WordsOverall"), label = NULL, c("yes", "no"), selected = "yes"),
            ),
                  div(
              style = "display: inline-block; vertical-align: top; width: 23%; padding:5px;",
			  # specify split valence
		                     tags$div(HTML("Do you want to create a wordlist of your summarized concepts?"), style="font-size:14px"),
                   radioButtons(ns("summarizedWordlist_setting_WordsOverall"), label = NULL, c("yes", "no"), selected = "yes"),
            ),
                  div(
              style = "display: inline-block; vertical-align: top; width: 23%; padding:5px;",
			  # specify split valence
		                     tags$div(HTML("Do you want to split your summarized words by the valence (X_positive, X_negative, ...)?"), style="font-size:14px"),
                   radioButtons(ns("split_setting_WordsOverall"), label = NULL, c("yes", "no"), selected = "no"),
            ),
          ),
                    tags$h3("Compute Wordlists"),
                       tags$br(),
            actionButton(
              ns("createWordlist_WordsOverall"),
              "create wordlist",
              style = "display: inline-block;"
            ),
                                tags$div(
            HTML(
              "<i>Remark: to download the wordlist please click on the global download button (top right) after 
              you have clicked on create wordlist.</i>"
            ),
            style = "font-size:14px"
          ),
                      tags$br(),
          tags$h4("Overall wordlist:"),
          dataTableOutput(ns("wordlistTable_WordsOverall")),
          )
        })


        #> Server

         observeEvent(input$summarizedWordlist_setting_WordsOverall, {
          print(input$summarizedWordlist_setting_WordsOverall)
          if(input$summarizedWordlist_setting_WordsOverall == "no"){
            updateRadioButtons(session, "split_setting_WordsOverall", selected = "no")
          }
  })


## create wordlist
wordlist <- eventReactive(input$createWordlist_WordsOverall, {
          req(drawnCAM())
  ## define settings: 
  # > include comments
  if(input$comments_setting_WordsOverall == "no"){
    tmp_includeComments = FALSE
  }else{
    tmp_includeComments = TRUE
  }
  
    if(input$summarizedWordlist_setting_WordsOverall == "no"){
    tmp_useSummarized = FALSE
    tmp_splitByValence = FALSE # if no summarized words are used, no split by valence
  }else{
    tmp_useSummarized = TRUE
    # > split by valence
  if(input$split_setting_WordsOverall == "no"){
    tmp_splitByValence = FALSE
  }else{
    tmp_splitByValence = TRUE
  }
  }


  CAMwordlist <- create_wordlist(
    dat_nodes = globals$dataCAMsummarized[[1]],
    dat_merged = globals$dataCAMsummarized[[3]],
    useSummarized = tmp_useSummarized,
    order = input$order_setting_WordsOverall,
    splitByValence = tmp_splitByValence,
    comments = tmp_includeComments,
    raterSubsetWords = NULL,
    rater = FALSE
  )


  #> change condition
  globals$condition <-
    c(globals$condition, "wordlistOverallCreated")
  #> save as global
  globals$wordlistOverall <- CAMwordlist
  
  CAMwordlist
})


output$wordlistTable_WordsOverall <- renderDataTable({
  wordlist()
})
  
       ###### wordClouds_overall
        #> UI
        observeEvent(input$wordClouds_overall, {

          ## change UI
          outUI$elements <- tagList(
          tags$h2("Wordclouds"),
          tags$br(),
          tags$div(HTML('After choosing your settings, please click on the 
                    "create word cloud" button. 
          Please click only once and wait few seconds:'), style="font-size:14px"),
          tags$h3("Your Settings:"),
                        tags$br(),
                   tags$div(
            HTML(
              "Please select the minimum frequency of concepts, which have been drawn, to be plotted
               and the maximum number of words to be plotted. Define if the summarized or non-summarized words should be plotted 
               (recommended to keep default settings):"
            ),
            style = "font-size:14px"
          ),
          div(
            style = "margin: 0 auto; width: 100%; text-align:left;",
            div(
              style = "display: inline-block; vertical-align: top; width: 25%; padding:10px;",
			  # specify order
                       tags$div(HTML("Please select the minimum frequency of concepts, which have been drawn, to be plotted in the word cloud:"), style="font-size:14px"),
              numericInput(ns("numMinimum_setting_WordsOverall"), label = NULL, 
            value = 2, min = 1, max = Inf, step = 1)
                                   ),
      div(
              style = "display: inline-block; vertical-align: top; width: 25%; padding:10px;",
			  # specify split valence
		                     tags$div(HTML("Please select the maximum number of words to be plotted in the word cloud:"), style="font-size:14px"),
              numericInput(ns("numMaximum_setting_WordsOverall"), label = NULL, 
            value = 200, min = 1, max = Inf, step = 1)
            ),
      div(
              style = "display: inline-block; vertical-align: top; width: 25%; padding:10px;",
			  # specify including comments
		                     tags$div(HTML("Do you want to plot the summarized concepts?"), style="font-size:14px"),
                   radioButtons(ns("summarized_setting_WordsOverall"), label = NULL, c("yes", "no"), selected = "yes"),
            ),
          ),
                    tags$h3("Create word cloud"),
                       tags$br(),
            actionButton(
              ns("createWordcloud_WordsOverall"),
              "create word cloud",
              style = "display: inline-block;"
            ),
                      tags$br(),
          tags$h4("Word cloud:"),
                  plotOutput(ns("plotWordcloud_WordsOverall"), width = "100%", height = "600px"),

          )
        })


  #> Server
## create wordlist
observeEvent(input$createWordcloud_WordsOverall, {
          req(drawnCAM())

## create wordlist
  # define settings: 
  # > use summarized
  if(input$summarized_setting_WordsOverall == "no"){
    tmp_useSummarized = FALSE
  }else{
    tmp_useSummarized = TRUE
  }

            wordlist_wordcloud <- create_wordlist(
    dat_nodes = globals$dataCAMsummarized[[1]],
    dat_merged = globals$dataCAMsummarized[[3]],
                           useSummarized = tmp_useSummarized,
    order = "frequency",
    splitByValence = FALSE,
    comments = FALSE,
    raterSubsetWords = NULL,
    rater = FALSE
  )

        ## plot wordcloud
        output$plotWordcloud_WordsOverall <- renderPlot({
            colors_vec <- rep(NA, times = nrow(wordlist_wordcloud))
            colors_vec <- ifelse(test = wordlist_wordcloud$mean_valence > 2, yes = "darkgreen",
            no = ifelse(test = wordlist_wordcloud$mean_valence > 1, yes = "green",
            no = ifelse(test = wordlist_wordcloud$mean_valence > 0, yes = "lightgreen",
            no = ifelse(test = wordlist_wordcloud$mean_valence < -2, yes = "darkred",
            no = ifelse(test = wordlist_wordcloud$mean_valence < -1, yes = "red",
            no = ifelse(test = wordlist_wordcloud$mean_valence < 0, yes = "indianred1",  no = "yellow"))))))
            # print(colors_vec)
            # print(table(colors_vec))
            wordcloud::wordcloud(words = wordlist_wordcloud$Words, freq = wordlist_wordcloud$raw, min.freq = input$numMinimum_setting_WordsOverall,
                      max.words=input$numMaximum_setting_WordsOverall, random.order=FALSE, colors=as.character(colors_vec), ordered.colors=TRUE, scale=c(1,0.50))
            # , rot.per=0.35, colors=brewer.pal(8, "Dark2")
        })

})






        ###### information
        observeEvent(input$informationWordsOverall, {
          ## change UI
          outUI$elements <- tagList(
            tags$h2("Module Specific Information"),
            tags$div(
              HTML('The options for this module are the following:'),
              tags$ul(
           tags$li(HTML('<b>get wordlist:</b> create a wordlist with summary statistics for each concept (mean / SD valence, mean / SD degree). 
           You could select the ordering of the word list, if words should be split by given valence and if comments should be visible.')),
              tags$li(HTML('<b>get word cloud:</b> create a word cloud of all your concepts in the dataset, whereby the colors indicate the concept’s mean valence. 
              You could select the minimum frequency concepts have been drawn to be plotted, the maximum number of words to be plotted and define if the summarized or non-summarized words should be plotted.'))
            )
          )
          )
        })

    })
    }