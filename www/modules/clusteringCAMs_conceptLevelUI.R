clusteringCAMs_conceptLevelUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(class = "sidebar",
             column(1, tags$b("Module Options:"),

                    actionButton(inputId =  ns("ConceptCooccurrences"), label = HTML('<b>Concept<br>co-occurrences</b>'),
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar"),
                    actionButton(inputId =  ns("ValenceCooccurrences"), label = HTML('Valence<br>co-occurrences'),
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar"),
                    actionButton(inputId = ns("informationClusteringCAMsConceptLevel"), label = "Information",
                                 icon = icon(name = "angle-right", lib = "font-awesome"),
                                 class = "btn-sidebar")
             ),
             column(11,
                    uiOutput(ns("uploadOutClusteringCAMsConceptLevel"))
             )
    )
  )
}