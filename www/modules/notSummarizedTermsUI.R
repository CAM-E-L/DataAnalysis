notSummarizedTermsUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(class = "sidebar",
             column(1, tags$b("Module Options:"),

actionButton(inputId =  ns("NST_getNonSummarized"), label = HTML('get ns<br>terms'),
             icon = icon(name = "angle-right", lib = "font-awesome"),
             class = "btn-sidebar"),
actionButton(inputId =  ns("NST_clickThroughNonSummarized"), label = HTML('click through<br>ns terms'),
             icon = icon(name = "angle-right", lib = "font-awesome"),
             class = "btn-sidebar"),
actionButton(inputId = ns("NST_information"), label = "Information",
             icon = icon(name = "angle-right", lib = "font-awesome"),
             class = "btn-sidebar")

             ),

       
             column(11,
                    uiOutput(ns("NST_out"))
             )

    )
  )
}

