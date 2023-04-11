clusteringCAMs_overallLevelUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(class = "sidebar",
             column(1, tags$b("Module Options:"),

                    actionButton(inputId =  ns("networkSimilarities"), label = HTML('<b>get network<br>similarities</b>'),
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar"),
                    actionButton(inputId = ns("informationClusteringCAMsOverall"), label = "Information",
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar")
             ),
             column(11,
                    uiOutput(ns("uploadOutClusteringCAMsOverall"))
             )
    )
  )
}

