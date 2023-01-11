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
                    actionButton(inputId = ns("wordVec"), label = "word2vec",
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar"),

                    actionButton(inputId = ns("wordsNotSummarized"), label = "not summarized",
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar", style="background-color: #93ffb7;"),

                    actionButton(inputId = ns("reliability"), label = "Reliability",
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar", style="background-color: #fdff5f;"),

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

