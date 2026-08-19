sliceCAMsUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(class = "sidebar",
             column(1, tags$b("Module Options:"),

                   actionButton(inputId =  ns("sliceCAMs"), label = HTML('<b>Slice CAMs</b>'),
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar"),
                   actionButton(inputId =  ns("sliceCAMsDescriptives"), label = HTML('Get Sliced CAMs<br>Descriptives'),
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar"),
                    actionButton(inputId = ns("informationSliceCAMs"), label = "Information",
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar")
             ),
             column(11,
                    uiOutput(ns("uploadOutSliceCAMs"))
             )

    )
  )
}

