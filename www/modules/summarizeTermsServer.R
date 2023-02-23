
summarizeTermsServer <-
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
        h1("Summarize terms module"),
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
                    tags$li(
            HTML(
              '<b>Search for Synonyms:</b> Summarize concepts by searching for synonyms.'
            )
          ),
                    tags$li(
            HTML(
              '<b>Apply word2vec model:</b> The word2vec is a neural network, which learnd word associations from a large corpus of text. 
              Based on these word associations get groups of similar terms (e.g. "happiness" is similar to "joy").'
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
      output$ST_out <- renderUI({
        outUI$elements
      })


            ################################
      # single module options
      ################################
        ###### approximate matching ######
                #> UI
observeEvent(input$ST_approxMatch, {
  ## change UI
  outUI$elements <- tagList(
    tags$h2("Summarize by approximate matching"),
    tags$br(),
    tags$div(
      HTML(
        "By using approximate string matching you can compute the string distances between all your unique concepts
              in the dataset (using optimal string aligment) to find words, which have been written slightly differently."
      ),
      style = "font-size:14px"
    ),
    tags$br(),
    tags$h3("Your Settings:"),
    tags$div(
      HTML(
        "Please select the maximum string distance (recommended: less than five) and
                click on button to start the approximate string matching. Please click only once and wait few seconds:"
      ),
      style = "font-size:14px"
    ),
    div(
      style = "margin: 0 auto; width: 100%; text-align:left;",
      div(
        style = "display: inline-block; vertical-align: top;",
        numericInput(
          ns("maxStringDis"),
          NULL,
          value = 2,
          width = "70px",
          min = 1,
          max = 6
        )
      ),
      actionButton(
        ns("clickAproxStriMatch"),
        "start approximate string matching",
        style = "display: inline-block;"
      ),
    ),
    tags$p(
      "Your uploaded CAM dataset contains ",
      tags$b(textOutput(ns(
        "Nodes_unique"
      ), inline = TRUE), " unique concepts"),
      " , whereby on your choosen setting ",
      tags$b(
        textOutput(ns("Nodes_matches"), inline = TRUE),
        " cases, where you can apply approximate string matching, were found."
      )),
    tags$br(),
    tags$p(
      "Word for which matches were found: ",
      tags$b(textOutput(
        ns("Nodes_matchesSinWor"), inline = TRUE
      )),
      HTML("&nbsp;&nbsp;"),
      " > ",
      tags$b(textOutput(
        ns("Nodes_matchesRounds"), inline = TRUE
      )),
      " more words to summarize"
    ),
    tags$br(),
    tags$div(
      HTML("Keep the words you want to summarize in the right lists:"),
      style = "font-size:18px"
    ),
    tags$div(
      HTML(
        '<i>The first word is automatically selected. If it contains a spelling error, you can correct it manually.
                                Important: all words will be saved in the following format: Word_positive for words with positive valence,
                                Word_negative for words with negative valence and so on.
                                 If you do not want to summarize a specific word please move it to the most left column.
                                 If you do not want to summarize any words just click on "skip":</i>'
      ),
      style = "font-size:14px"
    ),
    htmlOutput(ns("bucketlist")),
    fluidRow(
      column(
        width = 6,
        offset = 5,
        textInput(
          ns("supordinateWord"),
          "Superordinate word:",
          placeholder = "all words, which are not in the most left column"
        ),
        actionButton(ns("clickSummarize"), "summarize"),
        actionButton(ns("clickSkip"), "skip")
      )
    ),
    tags$br(),
    tags$p(
      "Your CAM dataset contains ",
      tags$b(textOutput(ns(
        "Nodes_unique_after"
      ), inline = TRUE)),
      " unique concepts AFTER summarzing."
    ),
    tags$div(
      HTML(
        'The following table shows you which words you have already summarized. You can use the search functionalities of the table:'
      ),
      style = "font-size:14px"
    ),
    dataTableOutput(ns("alreadyUsedWords")),
  )
})

      #> Server



        ###### searching terms ######
                #> UI
      observeEvent(input$ST_searchTerms, {
        ## change UI
        outUI$elements <-
          tagList(tags$h2("Summarize by searching terms"),
                  tags$div(
                    HTML(
                      "By using so called regular expression you can search your CAM concepts for semantically identical or similar terms
              (high likeness of their meaning). For more details see Information."
                    ),
              style = "font-size:14px"
                  ),
              tags$br(),
              div(
                style = "margin: 0 auto; width: 100%; text-align:left;",
                div(
                  style = "display: inline-block;",
                  textInput(ns("regularExp"), "Regular expression to search:", placeholder = "write a regexp",  width = "250px",)
                ),
                actionButton(
                  ns("regularExpSearch"),
                  "search",
                  style = "display: inline-block; width: 140px; height: 35px;"
                ),
              ),
              tags$p(
                "Your uploaded CAM dataset contains ",
                tags$b(textOutput(ns("Nodes_uniqueSearch"), inline = TRUE), " unique concepts")
              ),
              tags$br(),
              tags$div(
                HTML("Keep the words you want to summarize in the right lists:"),
                style = "font-size:18px"
              ),
              tags$div(
                HTML(
                  '<i>The first word is automatically selected. If it contains a spelling error, you can correct it manually.
                                Important: all words will be saved in the following format: Word_positive for words with positive valence,
                                Word_negative for words with negative valence and so on.
                                 If you do not want to summarize a specific word please move it to the most left column.
                                 If you do not want to summarize any words just do not click on "summarize" and start a new search:</i>'
                ),
                style = "font-size:14px"
              ),
              htmlOutput(ns("bucketlistSearch")),
              fluidRow(
                column(width = 6, offset = 5,
                       textInput(ns("supordinateWordSearch"), "Superordinate word:",
                                 placeholder = "all words, which are not in the most left column"),
                       actionButton(ns("clickSummarizeSearch"), "summarize")

                )
              ),
              tags$br(),


              tags$p(
                "Your CAM dataset contains ",
                tags$b(textOutput(ns("Nodes_unique_afterSearch"), inline = TRUE)),
                " unique concepts AFTER summarzing."),
              tags$div(
                HTML(
                  'The following table shows you which words you have already summarized. You can use the search functionalities of the table:'
                ),
                style = "font-size:14px"
              ),
              dataTableOutput(ns("usedwWordsSearch")),

          )
      })

      #> Server

        ###### synonyms ######
                #> UI
      observeEvent(input$ST_synonyms, {
        ## change UI
        outUI$elements <- tagList(
          tags$h2("(a) Summarize concepts by searching for synonyms"),
          tags$br(),
          tags$div(
            HTML(
              'Here you can summarize concepts by searching for synonyms, whereby for words written in
              <ul>
                <li>English the <a href="https://dictionary.reverso.net/english-synonyms/" target="_blank">Reverso Online Dictionary</a></li>
                <li><i>other languages will be implemented</i></li>
              </ul>
              <br>
               are applied:'
            ),
            style = "font-size:14px"
          ),
          tags$br(),
          tags$h3("Your settings:"),
          tags$div(
            HTML(
              "Please select the language of your CAM dataset and click
                click on the button to start summarizing your concepts by searching for synonyms.
                Please click only once and wait few seconds:"
            ),
            style = "font-size:14px"
          ),
          div(
            style = "margin: 0 auto; width: 100%; text-align:left;",
            div(
              style = "display: inline-block; vertical-align: top;",
              selectInput(ns("a_synonymsLanugage"), NULL, c("English",
                                                            "no other implemented"), selected = "English", width = "200px")
            ),
            actionButton(
              ns("a_synonymsStart"),
              "search for synonyms",
              style = "display: inline-block;"
            ),
          ),
          tags$h3("Synonyms found:"),
          tags$p(
            "By searching for synonyms ",
            tags$b(textOutput(ns(
              "a_synonymsPercentageFound"
            ), inline = TRUE)),
            " percent of your unique concepts have been found in the synonym database."
          ),
          tags$p(
            "Your uploaded CAM dataset contains ",
            tags$b(textOutput(ns(
              "Nodes_unique"
            ), inline = TRUE), " unique concepts"),
            " , whereby on your current set of unique concepts (! only single words considered) ",
            tags$b(
              textOutput(ns("a_groupsSynonyms"), inline = TRUE),
              " groups of synonyms"
            ),
            " have been found."
          ),
          tags$br(),
          tags$p(
            HTML("&nbsp;&nbsp;"),
            " > ",
            tags$b(textOutput(
              ns("a_groupsSynonymsRound"), inline = TRUE
            )),
            " more groups of synonyms to look at."
          ),
          tags$br(),
          tags$div(
            HTML("Keep the words you want to summarize in the right lists:"),
            style = "font-size:18px"
          ),
          tags$div(
            HTML(
              '<i>The first word is automatically selected as a possible superordinate word. If it contains a spelling error, you can
                correct it manually.
                <br>
                Important: all words will be saved in the following format: Word_positive for words with positive valence,
                 Word_negative for words with negative valence and so on.
                 If you do not want to summarize a specific word please move it to the most left column.
                 If you do not want to summarize any words of the found synonyms just click on "skip":</i>'
            ),
            style = "font-size:14px"
          ),
          htmlOutput(ns("a_synonymsBucketlist")),
          fluidRow(
            column(
              width = 6,
              offset = 5,
              textInput(
                ns("a_synonymsSupordinateWord"),
                "Superordinate word:",
                placeholder = "all words, which are not in the most left column"
              ),
              actionButton(ns("a_synonymsClickSummarize"), "summarize"),
              actionButton(ns("a_synonymsClickSkip"), "skip")
            )
          ),
          tags$br(),
          tags$p(
            "Your CAM dataset contains ",
            tags$b(textOutput(ns(
              "Nodes_unique_afterSynonyms"
            ), inline = TRUE)),
            " unique concepts AFTER summarzing."
          ),
          tags$div(
            HTML(
              'The following table shows you which words you have already summarized. You can use the search functionalities of the table:'
            ),
            style = "font-size:14px"
          ),
          dataTableOutput(ns("alreadyUsedWordsSynonyms")),
        )
      })

      #> Server

        ###### word2vec ######
                #> UI
 observeEvent(input$ST_wordVec, {
        ## change UI
        outUI$elements <- tagList(
          tags$h2("(b) Summarize concepts by applying a word2vec model"),
          tags$br(),
          tags$div(
            HTML(
              "Remark: To use this functionality you need to compute the pairwise similarity between concepts (written with a single word) using
              the provided Python code on GitHub. It is not possible to implement this procedure online, because trained word2vec models are
              relatively large (> 500mb) and it is technically difficult to implement Python Code within a Shiny Application written in R.
              <br>
              <br>
              To run the word2vec model, please do the following three steps: (1) download your summarized words as a text file using the button below
              and (2) download run the Python script (see GitHub link) and (3) upload the so computed pairwise similarities to the Shiny App."
            ),
            style = "font-size:14px"
          ),
          tags$br(),
          tags$h3("(1) Download summarized words:"),
          tags$h3("(2) Download and run Python script:"),
          tags$div(
            HTML(
              "...:"
            ),
            style = "font-size:14px"
          ),
          tags$h3("(3) Upload the computed pairwise similarities:"),
          tags$div(
            HTML(
              "...:"
            ),
            style = "font-size:14px"
          ),
          tags$h4("Pairwise similarities (correlation matrix):"),
          tags$h4("Suggested similar groups of concepts by computing a hierarchical clustering:"),
        )
      })

      #> Server



      ############
      # information summarize terms
      ############
      #> UI
      observeEvent(input$ST_information, {
        ## change UI
        outUI$elements <-
          tagList(tags$h2("Module Specific Information"),
                  tags$div(
                    HTML('The options for this module are the following:'),
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
                    tags$li(
            HTML(
              '<b>Search for Synonyms:</b> Summarize concepts by searching for synonyms.'
            )
          ),
                    tags$li(
            HTML(
              '<b>Apply word2vec model:</b> The word2vec is a neural network, which learnd word associations from a large corpus of text. 
              Based on these word associations get groups of similar terms (e.g. "happiness" is similar to "joy").'
            )
          )
        )
                  ))
      })


    })
  }
