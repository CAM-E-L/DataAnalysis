summarizeTermsUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(class = "sidebar",
             column(1, tags$b("Module Options:"),


                    actionButton(inputId =  ns("approxMatch"), label = HTML('Approximate<br>matching'),
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar"),
                    actionButton(inputId =  ns("searchTerms"), label = "Searching terms",
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar"),
                    actionButton(inputId = ns("Synonyms_wordVec"), label = HTML("Synonyms/<br>word2vec"),
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar"),

                    actionButton(inputId = ns("wordsNotSummarized"), label = "not summarized",
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar", style="background-color: #f3f14f;"),

                    actionButton(inputId = ns("reliability"), label = HTML("Train and Check<br>Reliability"),
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar", style="background-color: #c6e2ff;"),

                    actionButton(inputId = ns("informationSummarizeTerms"), label = "Information",
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar")
             ),

       
             column(11,
                    uiOutput(ns("summarizeTermsOut"))
             )

    )
  )
}

