uploadUI <- function(id) {
    ns <- NS(id)

    tagList(
        fluidRow(class = "sidebar",
                 column(1, tags$b("Module Options:"),


                        actionButton(inputId =  ns("uploadData"), label = HTML('<b>Upload</b>'),
                                     icon = icon(name = "angle-right", lib = "font-awesome"),
                                     class = "btn-sidebar"),
                        actionButton(inputId =  ns("getDescriptive"), label = "Descriptive",
                                     icon = icon(name = "angle-right", lib = "font-awesome"),
                                     class = "btn-sidebar"),
                        actionButton(inputId =  ns("getProtocolStats"), label = "Protocol stats",
                                     icon = icon(name = "angle-right", lib = "font-awesome"),
                                     class = "btn-sidebar"),
                        actionButton(inputId = ns("cleanValence"), label = "Clean Valence",
                                     icon = icon(name = "angle-right", lib = "font-awesome"),
                                     class = "btn-sidebar"),
                        actionButton(inputId = ns("information"), label = "Information",
                                     icon = icon(name = "angle-right", lib = "font-awesome"),
                                     class = "btn-sidebar")
                        ),
                 column(11,
                        uiOutput(ns("uploadOut"))
                 )

        )
    )
}
