wordOutputs_singleUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(class = "sidebar",
             column(1, tags$b("Module Options:"),

                    actionButton(inputId =  ns("getTablePie_single"), label = HTML('<b>get table<br>pie chart</b>'),
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar"),
                    actionButton(inputId =  ns("getSummaryStats_single"), label = HTML('get summary<br>statistics'),
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

