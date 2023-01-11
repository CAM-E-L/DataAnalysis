drawUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(class = "sidebar",
             column(1, tags$b("Module Options:"),


                    actionButton(inputId =  ns("drawCAMR"), label = HTML('<b>Draw R</b>'),
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar"),
                    actionButton(inputId =  ns("drawCAMJS"), label = "Draw JS",
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar"),
                    actionButton(inputId = ns("informationDraw"), label = "Information",
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar")
             ),
             column(11,
                    uiOutput(ns("uploadOutDraw"))
             )

    )
  )
}

