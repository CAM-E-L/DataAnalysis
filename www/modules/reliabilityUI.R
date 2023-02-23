reliabilityUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(class = "sidebar",
             column(1, tags$b("Module Options:"),


actionButton(inputId =  ns("reliability_train"), label = HTML('Train<br>Reliability'),
             icon = icon(name = "angle-right", lib = "font-awesome"),
             class = "btn-sidebar"),
actionButton(inputId =  ns("reliability_get"), label = HTML('Get<br>Reliability'),
             icon = icon(name = "angle-right", lib = "font-awesome"),
             class = "btn-sidebar"),
actionButton(inputId = ns("reliability_information"), label = "Information",
             icon = icon(name = "angle-right", lib = "font-awesome"),
             class = "btn-sidebar")
             ),

       
             column(11,
                    uiOutput(ns("reliability_out"))
             )

    )
  )
}

