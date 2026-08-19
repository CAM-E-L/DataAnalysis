wordOutputs_singleUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(class = "sidebar",
             column(1, tags$b("Module Options:"),

                   actionButton(inputId =  ns("concepts_single"), label = HTML('<b>Concept by<br>Concept</b>'),
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar"),
                   actionButton(inputId =  ns("concepts_overview"), label = HTML('<b>Overview of<br>Concepts</b>'),
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar"),
                    actionButton(inputId = ns("informationWordsSingle"), label = "Information",
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar")
             ),
             column(11,
                    uiOutput(ns("uploadOutWordsSingle"))
             )
    )
  )
}

