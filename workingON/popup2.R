library(shiny)
library(keys)

hotkeys <- c(
  "1",
  "command+shift+k",
  "up up down down left right left right b a enter"
)

ui <- fluidPage(
  useKeys(),
  keysInput("keys", hotkeys)
)

server <- function(input, output, session) {
  observeEvent(input$keys, {
    print(input$keys)
  })
}

shinyApp(ui, server)
