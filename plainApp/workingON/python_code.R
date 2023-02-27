library(reticulate)
# use_condaenv("r-reticulate")
# py_install("matplotlib")
library(shiny)

ui <- fluidPage(
  plotOutput(outputId = "plot01")
)

server <- function(input, output){

  # Import module
  plt <- import("matplotlib.pyplot")

  output$plot01 <- renderPlot({
    plt$plot(1:10)
    plt$show()
  })
}

shinyApp(ui, server)
