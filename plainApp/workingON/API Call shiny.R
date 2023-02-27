# https://stackoverflow.com/questions/54556135/is-it-possible-to-send-data-to-a-shiny-app
library(shiny)
library(httr)
library(dplyr)

ui <- fluidPage(
  tableOutput("tbl")
)

server <- function(input, output, session) {
  output$tbl <- renderTable( {
    # GET request from an API
    req <- httr::GET(url = "https://www.openthesaurus.de/synonyme/search?q=test&format=application/json")
    print(req)
    req_parsed <- httr::content(req, type = "application/json")
  print(req_parsed)
    # Convert to data.frame
    dplyr::bind_rows(req_parsed)
  })
}

shinyApp(ui, server)
