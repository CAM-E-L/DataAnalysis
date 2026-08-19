networkIndicatorsUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(class = "sidebar",
             column(1, tags$b("Module Options:"),

                   actionButton(inputId =  ns("networkIndicators"), label = HTML('<b>Get Network<br>Indicators</b>'),
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar"),
                   actionButton(inputId =  ns("networkIndicatorsDescriptives"), label = HTML('Get Network<br>Descriptives'),
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar"),
                   actionButton(inputId =  ns("neighborhoodIndicators"), label = HTML('<b>Get Neighborhood<br>Indicators</b>'),
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar"),
                   actionButton(inputId =  ns("neighborhoodIndicatorsDescriptives"), label = HTML('Get Neighborhood<br>Descriptives'),
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar"),
                    actionButton(inputId = ns("informationNetworkIndicators"), label = "Information",
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar")
             ),
             column(11,
                    uiOutput(ns("uploadOutNetworkIndicators"))
             )

    )
  )
}

