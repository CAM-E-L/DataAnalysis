summarizeTermsUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(class = "sidebar",
             column(1, tags$b("Module Options:"),


actionButton(inputId =  ns("ST_approxMatch"), label = HTML('Approximate<br>matching'),
             icon = icon(name = "angle-right", lib = "font-awesome"),
             class = "btn-sidebar"),
actionButton(inputId =  ns("ST_searchTerms"), label = "Searching terms",
             icon = icon(name = "angle-right", lib = "font-awesome"),
             class = "btn-sidebar"),
actionButton(inputId = ns("ST_synonyms"), label = HTML("Search for<br>Synonyms"),
             icon = icon(name = "angle-right", lib = "font-awesome"),
             class = "btn-sidebar"),
actionButton(inputId = ns("ST_wordVec"), label = HTML("Apply word2vec<br>model"),
             icon = icon(name = "angle-right", lib = "font-awesome"),
             class = "btn-sidebar"),
actionButton(inputId = ns("ST_information"), label = "Information",
             icon = icon(name = "angle-right", lib = "font-awesome"),
             class = "btn-sidebar")
             ),

       
             column(11,
                    uiOutput(ns("ST_out"))
             )

    )
  )
}

